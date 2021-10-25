package functionalbuilder

import functionalbuilder.Transformable.Transformable

object Transformable extends LowerPriority {

  /*
     Typeclass that represents the ability for an entity A to be transformed into  C given you provide a component B
   */
  trait Transformable[A, B, C] {
    def transform(component: B, origin: A): C
  }

  implicit def baseCase[A, B, C]: Transformable[A => B, A, B] =
    new Transformable[A => B, A, B] {
      def transform(component: A, origin: A => B): B = origin(component)
    }

  /*
    Functional Builder
    creates an entity in steps

    Add the ability to provide arguments non in sequential order to higher order functions

    val f: Int => Boolean => Double => String =
      i => b => d => s"$i $b $d"

    val result: String =
      f
       .provide(4.0)
       .provide(false)
       .provide(10)

    val partialApplication: Int => Double => String =
      f.provide(false)
   */
  implicit class TransformableOps[A, B](f: A => B) {
    def provide[Z, C](component: Z)(implicit ev: Transformable[A => B, Z, C]): C = ev.transform(component, f)
  }

}

trait LowerPriority {

  implicit def inductiveCase[A, B, C, D](implicit
      tr: Transformable[B, C, D]
  ): Transformable[A => B, C, A => D] =
    new Transformable[A => B, C, A => D] {
      def transform(component: C, origin: A => B): A => D =
        a => tr.transform(component, origin(a))
    }

}
