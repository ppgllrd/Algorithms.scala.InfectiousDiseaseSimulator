/********************************************************************
 * Events occurring during the simulation
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

trait Event {
  val time: Double
  def isValid: Boolean = true
}

case class Redraw(time: Double) extends Event

case class Collision(time: Double // time that event is scheduled to occur
                     , ia: Individual, ib: Individual // particles involved in event
                    ) extends Event {
  // collision counts at event creation
  private val iaCollisions = ia.collisions
  private val ibCollisions = ib.collisions

  // has any new collision occurred between when event was created and now?
  override def isValid: Boolean =
    !ia.isDead && !ib.isDead &&
      (ia.collisions == iaCollisions) &&
      (ib.collisions == ibCollisions)
}

trait WallCollision extends Event {
  val i: Individual
  // collision counts at event creation
  private val iCollisions = i.collisions

  // has any new collision occurred between when event was created and now?
  override def isValid: Boolean =
    !i.isDead && i.collisions == iCollisions
}

case class HorizontalWallCollision(time: Double, i: Individual) extends WallCollision

case class VerticalWallCollision(time: Double, i: Individual) extends WallCollision

case class EndInfection(time: Double, p: Individual) extends Event

