/********************************************************************
 * Priority queue of events
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

object PriorityQueue {
  // scala.collection.mutable.PriorityQueue is largest first priority queue,
  // hence we need to negate time to preserve temporal order
  private val ord: Ordering[Event] = Ordering.by(-_.time)

  def apply(timeLimit: Double): PriorityQueue =
    new PriorityQueue(timeLimit)
}

class PriorityQueue(timeLimit: Double) {
  // cannot use inheritance as mutable.PriorityQueue is sealed
  private val pq = scala.collection.mutable.PriorityQueue[Event]()(PriorityQueue.ord)

  def size: Int = pq.size

  def isEmpty: Boolean = pq.isEmpty

  def nonEmpty: Boolean = pq.nonEmpty

  def enqueue(ev: Event): Unit = {
    if(ev.time <= timeLimit)
      pq.enqueue(ev)
  }

  def clear(): Unit = {
    pq.clear()
  }

  def dequeue(): Event =
    pq.dequeue()
}
