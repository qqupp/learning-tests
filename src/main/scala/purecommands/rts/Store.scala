package purecommands.rts

trait Store[Loc, T] {
  def emptyStore(t: T): Store[Loc, T]
  def allocate(t: T): (Loc, Store[Loc, T])
  def update(loc: Loc, t: T): Store[Loc, T]
  def applyStore(loc: Loc): T
}
