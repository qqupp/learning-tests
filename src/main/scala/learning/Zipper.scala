package learning

import scala.annotation.tailrec


object Zipper extends  App {

  trait Tree[T]
  case class Item[T](t: T) extends Tree[T]
  case class Section[T](l: List[Tree[T]]) extends Tree[T]

  trait Path[T]
  case class Top[T]() extends Path[T]
  case class Node[T](
                      left:  List[Tree[T]],
                      up:    Path[T],
                      right: List[Tree[T]]
             ) extends Path[T]

  case class Loc[T](current: Tree[T], path: Path[T])


  /*
    eg parsing "a * b + c * d" as a tree would result in

    Section( List(
                  Section( List(
                               Item(a),
                               Item(*),
                               Item(b)
                          )),
                  Item(+),
                  Section( List(
                               Item(c),
                               Item(*),
                               Item(d)
                          ))
             ))

    )

    The location of the second multiplication sign in the tree is:

    Loc( Item(*) ,
          Node(
                left = List( Item(c) ),
                up   =  Node(
                              left  = List(
                                          Item(+),
                                          Section( List(Item(a), Item(*), Item(b)) )
                                        )
                              up    = Top
                              right = List()
                          )))

                ritght = List( Item(d) )
          )
   */

  def go_left[T](loc: Loc[T]): Option[Loc[T]] =
    loc.path match {
      case Top() => None
      case Node(Nil, _, _ ) => None
      case Node(l:: left, up, right) =>
        Some(
          Loc(l,
            Node(left, up, loc.current :: right))
        )
    }

  def go_right[T](loc: Loc[T]): Option[Loc[T]] =
    loc.path match {
      case Top() => None
      case Node(_, _, Nil) => None
      case Node(left, up, r:: right)  =>
        Some(
          Loc(r,
            Node(loc.current:: left, up, right)
          )
        )
    }

  def go_up[T](loc: Loc[T]): Option[Loc[T]] =
    loc.path match {
      case Top() => None
      case Node(left, up, right) =>
        Some(
          Loc(
            Section(left.reverse ++ (loc.current :: right)),
            up
          )
        )
    }

  def go_down[T](loc: Loc[T]): Option[Loc[T]] =
    loc.current match {
      case Item(x) => None
      case Section(Nil) => None
      case Section(t:: trees) =>
        Some(
          Loc(
            t,
            Node(Nil, loc.path , trees)
          )
        )
    }


  def nth[T](loc: Loc[T], n: Int): Option[Loc[T]] = {
    @tailrec
    def go[T](loc: Option[Loc[T]], n: Int): Option[Loc[T]] =
      (loc, n) match {
        case (Some(l), 1)          => go_down(l)
        case (Some(l), n) if n > 0 => go(go_right(l), n - 1)
        case _ => None
      }

    go(Some(loc), n)
  }


  def change[T](loc: Loc[T], replaceCurrent: Tree[T]): Loc[T] =
    Loc(replaceCurrent, loc.path)

  def insert_right[T](loc: Loc[T], newTree: Tree[T]): Option[Loc[T]] =
    loc.path match {
      case Node(left, up, right) =>
        Some(
          Loc(loc.current, Node(left, up, newTree :: right))
        )
      case Top() => None
    }

  def insert_left[T](loc: Loc[T], newTree: Tree[T]): Option[Loc[T]] =
    loc.path match {
      case Node(left, up, right) =>
        Some(
          Loc(loc.current, Node(newTree:: left, up, right))
        )
      case Top() => None
    }


  def insert_down[T](loc: Loc[T], newTree: Tree[T]): Option[Loc[T]] =
    loc.current match {
      case Item(_) => None
      case Section(ll) =>
        Some(
          Loc(Section(newTree::ll), loc.path)
        )
    }


}
