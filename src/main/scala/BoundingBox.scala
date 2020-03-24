/********************************************************************
 * Bounding box around population
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

import java.awt.geom.Rectangle2D
import java.awt.{BasicStroke, Color, Graphics2D, Stroke}

object BoundingBox {
  val width = 1000.0
  val height = 500.0

  val left = -width/2
  val right = width/2
  val top = -height/2
  val bottom = height/2

  def drawOn(g2D: Graphics2D): Unit = {
    g2D.setColor(Color.darkGray)
    g2D.setStroke(new BasicStroke(3))
    g2D.draw(new Rectangle2D.Double(left,top,width,height))
  }
}