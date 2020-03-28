/********************************************************************
 * Configuration and main program
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

import scala.swing.SimpleSwingApplication

case class Configuration(seed: Int
                         , Hz: Int // Hz is redraw frequency: number of redraw events per clock tick
                         , populationSz: Int
                         , velocitySigma: Double
                         , timeLimit: Double // execution time limit
                         , probInfection: Double
                         , probDying: Double
                         , timeInfectious: Double
                        )

object DefaultConfiguration extends Configuration(
  seed = 0
  , Hz = 48
  , populationSz = 500
  , velocitySigma = 25
  , timeLimit = 1000
  , probInfection = 1.0 / 3
  , probDying = 0.1
  , timeInfectious = 15
  )


object Main extends SimpleSwingApplication {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  def top = GUI()
}