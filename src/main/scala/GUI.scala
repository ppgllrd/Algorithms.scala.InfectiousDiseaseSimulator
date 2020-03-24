/********************************************************************
 * Graphical User Interface
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

import java.awt.geom.AffineTransform
import java.awt.{Color, Dimension, RenderingHints}

import javax.swing.BorderFactory

import scala.swing._
import scala.swing.event.ButtonClicked

trait Drawable {
  def drawWith(procedure : Graphics2D => Unit): Unit
}

object Fonts {
  val mono = new Font(Font.Monospaced, java.awt.Font.PLAIN, 15)
  val sans8 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 8)
  val sans14 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 14)
  val default = sans14
}

class IntField(v: Int) extends TextField {
  text = v.toString
  columns = 7
  horizontalAlignment = Alignment.Right
  font = Fonts.default

  def value: Int =
    try {
      text.toInt
    } catch {
      case e: Exception =>
        text = "0"
        0
    }
}

trait NumericSlider[A] extends Slider {
  val from: Int
  val to: Int
  def format(n: Int): String
  def fromInt(n: Int): A
  def toInt(v: A): Int

  min = from
  max = to
  majorTickSpacing = (to - from + 1) / 10
  minorTickSpacing = majorTickSpacing / 5
  paintTicks = true

  val lbs = List.range(from, to+1, majorTickSpacing)
  labels = lbs.zip(lbs.map(x =>
    new Label{
      text = format(x)
      font = Fonts.sans8
    })).toMap
  paintLabels = true

  def contents: A = fromInt(value)
  def contents_=(v: A) =
    value = toInt(v)
}

class DoubleSlider(v: Double, val from: Int, val to: Int) extends NumericSlider[Double] {
  def format(n: Int): String = s"$n"
  def fromInt(n: Int): Double = n
  def toInt(v: Double): Int = v.toInt

  contents = v
}

class ProbSlider(v: Double) extends DoubleSlider(v, 0, 100) {
  override def format(n: Int): String = f"${n/100.0}%.1f"
  override def fromInt(n: Int): Double = n/100.0
  override def toInt(v: Double): Int = (v*100).toInt
}

class IntSlider(v: Int, val from: Int, val to: Int) extends NumericSlider[Int] {
  def format(n: Int): String = s"$n"
  def fromInt(n: Int): Int = n
  def toInt(v: Int): Int = v

  contents = v
}


class Description extends Label {
  font = Fonts.default
}

object GUI {
  def apply(): GUI =
    new GUI()
}

class GUI() extends MainFrame with Drawable {
  title = "Simulator"

  private var drawingProcedure = (g2D: Graphics2D) => {}
  private def setDrawingProcedure(procedure : Graphics2D => Unit): Unit = {
    drawingProcedure =
      g2D => {
        val w = canvas.size.width
        val h = canvas.size.height

        val transform = new AffineTransform()
        transform.translate(w/2, h/2)
        g2D.transform(transform)
        procedure(g2D)
      }
  }
  setDrawingProcedure(g2D => BoundingBox.drawOn(g2D))

  def update(): Unit = {
    canvas.repaint()
  }

  def drawWith(procedure : Graphics2D => Unit): Unit = {
    setDrawingProcedure(procedure)
    update()
  }

  private val canvas = new Label {
    opaque = true
    background = Color.white
    peer.setDoubleBuffered(false)

    preferredSize = new Dimension((1.02*BoundingBox.width).toInt, (1.4*BoundingBox.height).toInt)

    override def paintComponent(g2D: Graphics2D) {
      super.paintComponent(g2D)
      g2D.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      drawingProcedure(g2D)
    }
  }

  private val seedTooltip = "Seed for this random simulation"
  private val seedLabel = new Description {
    text = "Seed:"
    tooltip = seedTooltip
  }
  private val seedIntField = new IntField(DefaultConfiguration.seed) {
    tooltip = seedTooltip
  }
  private val randomSeedButton = new Button{
    text = "Random"
    tooltip = "Generate a random seed"
    font = Fonts.default
  }

  private val popTooltip = "Number of individuals in population"
  private val popSizeLabel = new Description {
    text = "Population size:"
    tooltip = popTooltip
  }
  private val popSizeSlider = new IntSlider(DefaultConfiguration.populationSz, 0, 1500) {
    tooltip = popTooltip
  }

  private val velTooltip = "Velocity of individuals in normal distributed with μ=0 and this value as σ"
  private val velLabel = new Description {
    text = "Velocity variation:"
    tooltip = velTooltip
  }
  private val velSlider = new DoubleSlider(DefaultConfiguration.velocitySigma, 0, 100) {
    tooltip = velTooltip
  }

  private val probInfectTooltip = "Probability of getting infected after contacting an infected individual"
  private val probInfectLabel = new Description {
    text = "Probability Infection:"
    tooltip = probInfectTooltip
  }
  private val probInfectSlider = new ProbSlider(DefaultConfiguration.probInfection) {
    tooltip = probInfectTooltip
  }

  private val probDieTooltip = "Probability of dying after getting infected"
  private val probDieLabel = new Description {
    text = "Probability dying:"
    tooltip = probDieTooltip
  }
  private val probDieSlider = new ProbSlider(DefaultConfiguration.probDying) {
    tooltip = probDieTooltip
  }

  private val infectiousTooltip = "Time an individual remains infectious to others is normal distributed around this value(μ) with σ=1"
  private val infectiousLabel = new Description {
    text = "Time infectious:"
    tooltip = infectiousTooltip
  }
  private val infectiousSlider = new DoubleSlider(DefaultConfiguration.timeInfectious, 0, 100){
    tooltip = infectiousTooltip
  }

  private val HzTootlTip = "Number of redraw events per clock tick"
  private val HzLabel = new Description {
    text = "Redraw frequency:"
    tooltip = HzTootlTip
  }
  private val HzSlider = new IntSlider(DefaultConfiguration.Hz, 0, 60) {
    tooltip = HzTootlTip
  }

  val aboutButton = new Button {
    text = "About..."
    tooltip = "Show information about this program"
    font = Fonts.default
  }

  private val toDisable = List(seedIntField, randomSeedButton, popSizeSlider, velSlider, probInfectSlider, probDieSlider, infectiousSlider, HzSlider, aboutButton)

  private val window = this

  private object startButton extends Button {
    val startText = "Start"
    text = startText
    tooltip = s"$text running simulation"
    font = Fonts.default

    var threadOpt: Option[Thread] = None

    def onPress(): Unit = {
      threadOpt match {
        case None =>
          val thread = new Thread {
            override def run {
              val s = seedIntField.value
              val p = popSizeSlider.contents
              val pbInfect = probInfectSlider.contents
              val pbDying = probDieSlider.contents
              val vel = velSlider.contents
              val infectious = infectiousSlider.contents
              val Hz = HzSlider.contents

              // create a simulator and simulate
              val conf = DefaultConfiguration.copy(seed = s, Hz = Hz, velocitySigma = vel, populationSz = p, probInfection = pbInfect, probDying = pbDying, timeInfectious = infectious)
              val simulator = Simulator(window, conf)
              simulator.simulate()
            }
          }
          thread.start()
          threadOpt = Some(thread)
          text = "Stop"
          tooltip = s"$text running simulation"
          toDisable.foreach(_.enabled = false)

        case Some(thread) =>
          thread.stop()
          threadOpt = None
          text = startText
          tooltip = s"$text running simulation"
          toDisable.foreach(_.enabled = true)
      }
    }
  }

  private val controls = new GridPanel(11,2) {
    border = BorderFactory.createEmptyBorder(5,5,5,5)
    contents ++= List(
        new FlowPanel() {
          contents ++= Seq(seedLabel, seedIntField)
        }
      , new FlowPanel() {
          contents += randomSeedButton
        }
      , popSizeLabel, popSizeSlider
      , velLabel, velSlider
      , probInfectLabel, probInfectSlider
      , infectiousLabel, infectiousSlider
      , probDieLabel, probDieSlider
      , HzLabel, HzSlider
      , Swing.VStrut(20), Swing.VStrut(20)
      , new FlowPanel() {
          contents += startButton
        }
      , Swing.VStrut(0)
      , Swing.VStrut(60), Swing.VStrut(60)
      , new FlowPanel() {
          contents += aboutButton
      }, Swing.VStrut(0)
    )
  }

  contents = new BorderPanel {
    layout += new BoxPanel(Orientation.Vertical) {
      contents += Swing.VGlue
      contents += controls
      contents += Swing.VGlue
    } -> BorderPanel.Position.West
    layout += canvas -> BorderPanel.Position.Center
  }

  listenTo(randomSeedButton, startButton, aboutButton)
  reactions += {
    case ButtonClicked(`randomSeedButton`) =>
      seedIntField.text = Random.uniform(Int.MaxValue).toString

    case ButtonClicked(`startButton`) =>
      startButton.onPress()

    case ButtonClicked(`aboutButton`) =>
      Dialog.showMessage(window
        , "Infectious disease simulator.\n\n" +
          "@ José E. Gallardo, 2020.\n\n" +
          "Partly based on Java Event-Driven Simulator by Robert Sedgewick and Kevin Wayne."
        , s"About ${window.title}...")
  }

  visible = true
  pack()
  startButton.requestFocus()
}
