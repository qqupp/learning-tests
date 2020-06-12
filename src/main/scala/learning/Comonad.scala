package learning

object Comonad {

  trait Monad[M[_]] {
    def map[A, B](m: M[A])(f: A => B): M[B]
    def unit[A](a: A): M[A]
    def join[A](mma: M[M[A]]): M[A]
    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B] = join(map(m)(f))
  }

  // Monad must respect monad laws assoc and left/right id

  /*
      Ability to ask for a value

      meaning of join is to pass the same context
      of type R to both the outer scope and the inner scope
   */
  case class Reader[R, A](run: R => A)
  def ask[R]: Reader[R, R] = Reader(r => r)


  def monadReaderInstance[R]: Monad[Reader[R, ?]] = new Monad[Reader[R, ?]] {
    def map[A, B](m: Reader[R, A])(f: A => B): Reader[R, B] = Reader( r =>  f(m.run(r)))
    def unit[A](a: A): Reader[R, A] = Reader(_ => a)
    def join[A](mma: Reader[R, Reader[R, A]]): Reader[R, A] =
     Reader(r => mma.run(r).run(r) )
  }


  /*
      Ability to write a value

      meaning of join is to concatenate the log in the outer and inner scopes
   */
  case class Writer[W, A](value: A, log: W)
  def tell[W](w: W): Writer[W, Unit] = Writer((), w)

  trait Monoid[A] {
    def unit: A
    def combine(a1: A, a2: A): A
  }

  def monadWriterInstance[W : Monoid]: Monad[Writer[W, ?]] = new Monad[Writer[W, ?]] {
    def map[A, B](m: Writer[W, A])(f: A => B): Writer[W, B] = Writer(f(m.value), m.log)
    def unit[A](a: A): Writer[W, A] = Writer(a, implicitly[Monoid[W]].unit)
    def join[A](mma: Writer[W, Writer[W, A]]): Writer[W, A] = {
      val newLog = implicitly[Monoid[W]].combine(mma.log, mma.value.log)
      Writer(mma.value.value, newLog)
    }
  }

  /*
      Abitily to get and set a value

      meaning of join is to thread the state from the outer to the inner scopes
   */
  case class State[S, A](run: S => (A, S))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def monadStateInstance[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    def map[A, B](m: State[S, A])(f: A => B): State[S, B] = State( s => { val (a, s1) = m.run(s); (f(a), s1) })
    def unit[A](a: A): State[S, A] = State( s => (a, s) )
    def join[A](mma: State[S, State[S, A]]): State[S, A] =
      State{ s =>
        val (ma, s1) = mma.run(s)
        val (a, s2) = ma.run(s1)
        (a, s2)
      }
  }


  /*
    Ability to present a value or not
   */
  sealed trait Opt[+X]
  case class OptSome[X](value: X) extends Opt[X]
  case object OptNone extends Opt[Nothing]

  val monadOptionInstance: Monad[Opt] = new Monad[Opt] {
    def map[A, B](m: Opt[A])(f: A => B): Opt[B] = m match {
      case OptNone => OptNone
      case OptSome(v) => OptSome(f(v))
    }
    def unit[A](a: A): Opt[A] = OptSome(a)
    def join[A](mma: Opt[Opt[A]]): Opt[A] = mma match {
      case OptNone => OptNone
      case OptSome(ma) => ma
    }
  }

  /*
      A comonad presents the inverse operations for unit and join
      prefixed with co-

    def unit[A](a: A): M[A]
    def join[A](mma: M[M[A]]): M[A]
   */

  trait Comonad[M[_]] {
    def map[A, B](m: M[A])(f: A => B): M[B]
    def counit[A](m: M[A]): A
    def cojoin[A](ma: M[A]): M[M[A]]
  }


  /*
    identity comonad
   */
  case class ID[A](value: A)

  val comonadIdInstance: Comonad[ID] = new Comonad[ID] {
    def map[A, B](m: ID[A])(f: A => B): ID[B] = ID(f(m.value))
    def counit[A](m: ID[A]): A = m.value
    def cojoin[A](ma: ID[A]): ID[ID[A]] = ID(ma)
  }

  /*
    Reader Comonad
   */
  case class CoReader[R, A](extract: A, ask: R)

  def comonadCoReaderInstance[R]: Comonad[CoReader[R, ?]] = new Comonad[CoReader[R, ?]] {
    def map[A, B](m: CoReader[R, A])(f: A => B): CoReader[R, B] =
      CoReader(f(m.extract), m.ask)

    def counit[A](m: CoReader[R, A]): A =
      m.extract

    def cojoin[A](ma: CoReader[R, A]): CoReader[R, CoReader[R, A]] =
      CoReader(ma, ma.ask)
  }

  /*
    Writer Comonad
   */
  case class CoWriter[W, A](tell: W => A)

  def comonadCoWriterInstance[W: Monoid]: Comonad[CoWriter[W, ?]] = new Comonad[CoWriter[W, ?]] {
    def map[A, B](m: CoWriter[W, A])(f: A => B): CoWriter[W, B] =
      CoWriter(w => f(m.tell(w)))

    def counit[A](m: CoWriter[W, A]): A = m.tell(implicitly[Monoid[W]].unit)

    def cojoin[A](ma: CoWriter[W, A]): CoWriter[W, CoWriter[W, A]] =
      CoWriter(w1 => CoWriter( w2 => ma.tell(implicitly[Monoid[W]].combine(w1, w2)) ) )
  }


}
