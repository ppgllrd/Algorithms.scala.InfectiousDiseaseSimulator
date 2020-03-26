/********************************************************************
 * Main simulator
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

import java.awt.Color
import java.awt.geom.Rectangle2D

import scala.swing.Graphics2D

object Simulator {
  def apply(window: Drawable, conf: Configuration) =
    new Simulator(window, conf)
}

class Simulator(window: Drawable, conf: Configuration) {
  private val period = 1.0 / conf.Hz

  // priority queue of events
  private val pq = PriorityQueue(conf.timeLimit)

  // simulation clock time
  private var time = 0.0

  // random generator
  private val rnd = Random(conf.seed)

  private val individuals = new Array[Individual](conf.populationSz)

  def initializePopulation(): Unit = {
    // initialize population with non-colliding random individuals
    for(i <- 0 until individuals.length){
      var ok = false
      while(!ok) {
        val ind = Individual.random(rnd, conf)
        individuals(i) = ind
        ok = (0 until i).forall(j => !individuals(j).collidesWith(ind))
      }
    }

    // select random initial infected individual
    val n = rnd.uniform(individuals.length)
    infect(individuals(n))
  }

  // updates priority queue with all new events for individual ia
  private def predictCollisions(ia: Individual): Unit = {
    if(!ia.isDead) {
      // individual-individual collisions
      for(i <- individuals) {
        val t = time + ia.timeToHit(i)
        pq.enqueue(Collision(t, ia, i))
      }

      // individual-wall collisions
      val tx = time + ia.timeToHitVerticalWall
      pq.enqueue(VerticalWallCollision(tx, ia))

      val ty = time + ia.timeToHitHorizontalWall
      pq.enqueue(HorizontalWallCollision(ty, ia))
    }
  }

  private def statistics: (Int, Int, Int, Int) =  {
    var infected = 0
    var dead = 0
    var exposed = 0
    for(i <- individuals)
      if(i.isInfected)
        infected += 1
      else if(i.isDead)
        dead += 1
      else if(i.canGetInfected)
        exposed += 1
    val alive = individuals.length - dead
    val nonInfected = alive - infected
    return (alive, dead, infected, exposed)
  }

  // redraw all particles
  private val top = BoundingBox.top.toInt-10
  private val bottom = BoundingBox.bottom.toInt+25
  private val left = BoundingBox.left.toInt+5

  private object history {
    private val values = Array.fill[Double](conf.timeLimit.toInt)(0)
    def update(time: Double, value: Double): Unit = {
      values(time.toInt) = value
    }
    def drawOn(g2D: Graphics2D): Unit = {
      g2D.setColor(Color.red)
      for(i <- 0 until time.toInt)
        g2D.fill(new Rectangle2D.Double(left+i*3, top-values(i), 3, values(i)))
    }
  }

  private def redraw(): Unit = {
    val (alive, dead, infected, exposed) = statistics
    val percentInfected = 100.0 * infected / alive
    history(time) = 100.0 * infected / alive

    window.drawWith{ g2D =>
      g2D.setFont(Fonts.mono)
      g2D.setColor(Color.red)
      g2D.drawString(f"Infected: $infected%3d($percentInfected%6.2f%%)  Dead: $dead%4d", left, bottom)
      g2D.setColor(Color.blue)
      g2D.drawString(f"Non-infected: $exposed%3d", left+400, bottom)
      g2D.setColor(Color.black)
      g2D.drawString(f"Time: $time%.2f", left+650, bottom)

      history.drawOn(g2D)

      for(p <- individuals)
        p.drawOn(g2D)

      BoundingBox.drawOn(g2D)
    }
  }

  /**
   * Simulates the system of particles for the specified amount of time.
   */

  def infect(p: Individual): Unit = {
    p.infect()
    pq.enqueue(EndInfection(time + rnd.normal(conf.timeInfectious), p))
  }

  def simulate(): Unit = {
    // make priority queue empty
    pq.clear()

    // reset simulation time
    time = 0

    // initialize population
    initializePopulation()

    // predict collisions for all individuals
    for(i <- individuals)
      predictCollisions(i)

    // initial redraw event
    Redraw.time = 0
    pq.enqueue(Redraw)

    var t0 = System.currentTimeMillis()

    // the main event-driven simulation loop
    while(pq.nonEmpty) { // get event, discard if invalidated
      val ev = pq.dequeue()
      if(ev.isValid) {
        // update individual positions
        for(i <- individuals)
          i.move(ev.time - time)

        // update simulation clock
        time = ev.time

        // process event
        ev match {
          case Redraw =>
            val delay = System.currentTimeMillis() - t0
            redraw()
            Thread.sleep((10 - delay) max 1)

            // schedule next redraw event
            Redraw.time += period
            pq.enqueue(Redraw)
            t0 = System.currentTimeMillis()

          case Collision(t, ia, ib) =>
            // ia-ib collision

            if(ia.isInfected && ib.canGetInfected) {
              val infection = rnd.bernoulli(conf.probInfection)
              if(infection)
                infect(ib)

            } else if(ib.isInfected && ia.canGetInfected) {
              val infection = rnd.bernoulli(conf.probInfection)
              if(infection)
                infect(ia)
            }

            ia.bounceOff(ib)
            // predict new collisions for pa and pb
            predictCollisions(ia)
            predictCollisions(ib)

          case HorizontalWallCollision(t, i) =>
            // individual-wall collision
            i.bounceOffHorizontalWall()
            predictCollisions(i)

          case VerticalWallCollision(t, i) =>
            // individual-wall collision
            i.bounceOffVerticalWall()
            predictCollisions(i)

          case EndInfection(t, p) =>
            val die = rnd.bernoulli(conf.probDying)
            p.endInfection(die)
        }
      }
    }
  }
}


