package simulatedHigherKinds


/*
  emulates higher kinded types through Higher interface
 */
trait FunctorTypeClass[F] {
  def map[A, B](fa: Higher[F, A])(f: A => B): Higher[F, B]
}


object Functors {
  def map[F, A, B](tc: FunctorTypeClass[F])(fa: Higher[F, A])(f: A => B): Higher[F, B]  =
    tc.map(fa)(f)

}


