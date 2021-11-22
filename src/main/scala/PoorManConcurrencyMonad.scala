object PoorManConcurrencyMonad {

  // FUNCTIONAL PEARLS Poor Man􏱔s Concurrency Monad

  trait Monad[F[_]] {
    def *[A, B](fa: F[A])(k: A => F[B]): F[B]
    def `return`[A](a: A): F[A]
  }

  // for do notation
  implicit class MonadSyntax[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = M.*(fa)(f)
    def map[B](f: A => B): F[B]        = M.*(fa)(a => M.`return`(f(a)))
  }

  // example monad with output (writer monad)
  trait Writer[F[_]] {
    def write(s: String): F[Unit]
  }

  case class W[T](value: T, s: String) {
    // run function
    val output: String = s
  }

  implicit val instanceWriterW: Writer[W] with Monad[W] = new Writer[W] with Monad[W] {
    def *[A, B](fa: W[A])(k: A => W[B]): W[B] = {
      val W(a, s)  = fa
      val W(b, s1) = k(a)
      W(b, s ++ s1)
    }

    def `return`[A](a: A): W[A] = W(a, "")

    def write(s: String): W[Unit] = W((), s)
  }

  // monad transformers
  trait MonadTrans[G[_[_], _]] {
    def lift[F[_]: Monad, A](fa: F[A]): G[F, A]
  }

  // continuation
  case class C[M[_], A](k: (A => Action[M]) => Action[M])

  // C is the continuation monad transformer
  implicit def instanceContinuationMonad[M[_]]: Monad[C[M, *]] =
    new Monad[C[M, *]] {
//      val M = implicitly[Monad[M]] // do we need this?
      def *[A, B](fa: C[M, A])(k: A => C[M, B]): C[M, B] = {
        val C(f) = fa
        C(c => f(a => k(a).k(c)))
      }

      def `return`[A](a: A): C[M, A] = C(c => c(a))
    }

  sealed trait Action[_[_]]
  case class Atom[M[_]](action: M[Action[M]])              extends Action[M]
  case class Fork[M[_]](left: Action[M], right: Action[M]) extends Action[M]
  case class Stop[M[_]]()                                  extends Action[M]

  def action[M[_]: Monad, A](cma: C[M, A]): Action[M] =
    cma.k(a => Stop[M])

  // this turns a generic computation M[A] into an atomic action in C
  def atom[M[_]: Monad, A](ma: M[A]): C[M, A] =
    C(cb => Atom(ma.map(a => cb(a))))

  def stop[M[_]: Monad, A]: C[M, A] =
    C(cb => Stop())

  def par[M[_]: Monad, A](p1: C[M, A], p2: C[M, A]): C[M, A] =
    C(cb => Fork(p1.k(cb), p2.k(cb)))

  def fork[M[_]: Monad, A](cma: C[M, A]): C[M, Unit] =
    C(cb => Fork(action(cma), cb(())))

  implicit def instanceMonadTransformerC: MonadTrans[C] =
    new MonadTrans[C] {
      def lift[F[_]: Monad, A](fa: F[A]): C[F, A] = atom(fa)
    }

  // now we have a defined way to construct actions of type C[F, A]
  // and we need a way to interpret concurrently running actions
  // semantics with round robin

  def round[M[_]](actions: List[Action[M]])(implicit MonadM: Monad[M]): M[Unit] = {
    actions match {
      case Nil                => MonadM.`return`(())
      case Atom(ma) :: as     => ma.flatMap(act => round(as ++ List(act)))
      case Fork(a1, a2) :: as => round(as ++ List(a1, a2))
      case Stop() :: as       => round(as)
    }
  }

  def run[M[_]: Monad, A](cma: C[M, A]): M[Unit] =
    round(List(action(cma)))

  // concurrent output example

  implicit def instanceWriterC[F[_]](implicit WriterF: Writer[F], MonadF: Monad[F]): Writer[C[F, *]] =
    new Writer[C[F, *]] {
      def write(s: String): C[F, Unit] = instanceMonadTransformerC.lift(WriterF.write(s))
    }

}

object Example extends App {

  import PoorManConcurrencyMonad._

  def loop[F[_]](s: String, count: Int = 10)(implicit WriterF: Writer[F], M: Monad[F]): F[Unit] =
    if (count <= 0) M.`return`(()) else WriterF.write(s).flatMap(_ => loop[F](s, count - 1))

  def example[F[_]](implicit WriterC: Writer[C[F, *]], MonadF: Monad[F]): C[F, Unit] = {
    for {
      _ <- WriterC.write("start!\n")
      _ <- fork(loop[C[F, *]]("fish\n"))
      _ <- loop[C[F, *]]("cat\n")
    } yield ()
  }

  def runExample =
    run(example[W]).output

  println(runExample)
}
