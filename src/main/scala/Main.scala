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
  seed = 1
  , Hz = 48
  , populationSz = 800
  , velocitySigma = 10
  , timeLimit = 10000
  , probInfection = 1.0 / 3
  , probDying = 0.2
  , timeInfectious = 10
  )


object Main extends SimpleSwingApplication {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)

  def top = GUI()
}