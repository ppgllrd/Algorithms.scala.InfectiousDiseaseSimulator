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
import javax.swing.plaf.FontUIResource
import javax.swing.UIManager

import scala.swing._
import scala.swing.event.{ButtonClicked, ValueChanged}

trait Drawable {
  def drawWith(procedure : Graphics2D => Unit): Unit
}

object Fonts {
  val mono = new Font(Font.Monospaced, java.awt.Font.PLAIN, 14)
  val sans8 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 8)
  val sans10 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 10)
  val sans11 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 10)
  val sans14 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 14)
  val default = sans11
}

class IntField(v: Int) extends TextField {
  horizontalAlignment = Alignment.Right

  def value: Int =
    try {
      text.toInt
    } catch {
      case e: Exception =>
        text = "0"
        0
    }

  def value_=(v: Int): Unit = {
    text = v.toString
  }

  value_=(v)
}

abstract class NumericSlider[A] extends BoxPanel(Orientation.Horizontal) {
  protected def formatLabel(n: A): String
  protected def formatText(n: A): String = formatLabel(n)
  protected def fromInt(n: Int): A
  protected def toInt(v: A): Int

  private val slider = new Slider
  private val textField = new TextField {
    font = Fonts.sans10
    columns = 4
    horizontalAlignment = Alignment.Right
    editable = false
    maximumSize = new Dimension(0,15)
  }

  private def config(): Unit = {
    slider.majorTickSpacing = (slider.max - slider.min + 1) / 10
    slider.minorTickSpacing = slider.majorTickSpacing / 5
    slider.paintTicks = true

    slider.labels = {
      val lbs = List.range(slider.min, slider.max+1, slider.majorTickSpacing)
      lbs.zip(lbs.map(x =>
        new Label{
          text = formatLabel(fromInt(x))
          font = Fonts.sans8
        })).toMap
    }
    slider.paintLabels = true
  }

  def min: Int = slider.min
  def min_=(n: Int): Unit = {
    slider.min = n
    config()
  }

  def max: Int = slider.max
  def max_=(n: Int): Unit = {
    slider.max = n
    config()
  }

  contents += textField
  contents += Swing.HStrut(2)
  contents += slider

  def value: A = fromInt(slider.value)
  def value_=(v: A): Unit = {
    slider.value = toInt(v)
    textField.text = formatText(v)
  }

  override def enabled: Boolean = slider.enabled
  override def enabled_=(e: Boolean): Unit = {
    slider.enabled_=(e)
    textField.enabled_=(e)
  }

  override def tooltip: String = slider.tooltip
  override def tooltip_=(s: String): Unit = {
    slider.tooltip_=(s)
    textField.tooltip_=(s)
  }

  listenTo(slider)
  reactions += {
    case ValueChanged(_) =>
      textField.text = formatText(value)
  }

  config()
}

class DoubleSlider(v: Double, from: Int, to: Int) extends NumericSlider[Double] {
  def formatLabel(v: Double): String = s"$v"
  def fromInt(n: Int): Double = n
  def toInt(v: Double): Int = v.toInt

  min = from
  max = to
  value = v
}

class ProbSlider(v: Double) extends DoubleSlider(v, 0, 100) {
  override def formatLabel(v: Double): String = f"$v%.1f"
  override def formatText(v: Double): String = f"$v%.2f"
  override def fromInt(n: Int): Double = n/100.0
  override def toInt(v: Double): Int = (v*100).toInt
}

class IntSlider(v: Int, from: Int, to: Int) extends NumericSlider[Int] {
  def formatLabel(v: Int): String = s"$v"
  def fromInt(n: Int): Int = n
  def toInt(v: Int): Int = v

  min = from
  max = to
  value = v
}

object GUI {
  def apply(): GUI =
    new GUI()

  private def setGUIFont(f: FontUIResource): Unit = {
    val keys = UIManager.getDefaults.keys
    while(keys.hasMoreElements) {
      val key = keys.nextElement
      val value = UIManager.get(key)
      if(value.isInstanceOf[FontUIResource])
        UIManager.put(key, f)
    }
  }
  setGUIFont(new FontUIResource(Fonts.default))
}

class GUI() extends MainFrame with Drawable {
  title = "Simulator"
  resizable = false

  private var _scale = 1.0
  private val scaleTooltip = "Scale for simulation window"
  private val scaleLabel = new Label("Simulation scale:") {
    tooltip = scaleTooltip
  }
  private val scaleSlider = new ProbSlider(_scale)  {
    tooltip = scaleTooltip
  }

  def scale: Double = _scale
  def scale_=(sc: Double): Unit = {
    _scale = sc
    scaleSlider.value = sc
    canvas.preferredSize = new Dimension((1.02*scale*BoundingBox.width).toInt, (1.4*scale*BoundingBox.height).toInt)
    pack()
  }

  private var drawingProcedure = (g2D: Graphics2D) => {}
  private def setDrawingProcedure(procedure : Graphics2D => Unit): Unit = {
    drawingProcedure =
      g2D => {
        val w = canvas.size.width
        val h = canvas.size.height

        val transform = new AffineTransform()
        transform.translate(w/2, h/2)
        transform.scale(scale, scale)
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

    override def paintComponent(g2D: Graphics2D) {
      super.paintComponent(g2D)
      g2D.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      drawingProcedure(g2D)
    }
  }

  private val seedTooltip = "Seed for this random simulation"
  private val seedLabel = new Label("Seed:") {
    tooltip = seedTooltip
  }
  private val seedIntField = new IntField(DefaultConfiguration.seed) {
    tooltip = seedTooltip
    columns = 10
  }
  private val randomSeedButton = new Button("Random") {
    tooltip = "Generate a random seed"
  }

  private val popTooltip = "Number of individuals in population"
  private val popSizeLabel = new Label("Population size:") {
    tooltip = popTooltip
  }
  private val popSizeSlider = new IntSlider(DefaultConfiguration.populationSz, 0, 1500) {
    tooltip = popTooltip
  }

  private val velTooltip = "Velocity of individuals in normal distributed with μ=0 and this value as σ"
  private val velLabel = new Label("Velocity variance:") {
    tooltip = velTooltip
  }
  private val velSlider = new IntSlider(DefaultConfiguration.velocitySigma.toInt, 0, 100) {
    tooltip = velTooltip
  }

  private val probInfectTooltip = "Probability of getting infected after contacting another infected individual"
  private val probInfectLabel = new Label("Infection rate:") {
    tooltip = probInfectTooltip
  }
  private val probInfectSlider = new ProbSlider(DefaultConfiguration.probInfection) {
    tooltip = probInfectTooltip
  }

  private val probDieTooltip = "Probability of dying after getting infected"
  private val probDieLabel = new Label("Death rate:") {
    tooltip = probDieTooltip
  }
  private val probDieSlider = new ProbSlider(DefaultConfiguration.probDying) {
    tooltip = probDieTooltip
  }

  private val infectiousTooltip = "Time an individual remains infectious to others is normal distributed around this value(μ) with σ=1"
  private val infectiousLabel = new Label("Time infectious:") {
    tooltip = infectiousTooltip
  }
  private val infectiousSlider = new IntSlider(DefaultConfiguration.timeInfectious.toInt, 0, 100){
    tooltip = infectiousTooltip
  }

  private val HzTootlTip = "Number of redraw events per clock tick"
  private val HzLabel = new Label("Redraw frequency:") {
    tooltip = HzTootlTip
  }
  private val HzSlider = new IntSlider(DefaultConfiguration.Hz, 0, 60) {
    tooltip = HzTootlTip
  }

  private val aboutButton = new Button("About...") {
    tooltip = "Show information about this program"
  }

  private val toDisable = List(seedIntField, randomSeedButton, popSizeSlider, velSlider, probInfectSlider, probDieSlider, infectiousSlider, HzSlider, aboutButton)

  private val window = this

  private object startButton extends Button {
    val startText = "Start"
    text = startText
    tooltip = s"$text running simulation"

    var threadOpt: Option[Thread] = None

    def onPress(): Unit = {
      threadOpt match {
        case None =>
          val thread = new Thread {
            override def run {
              val s = seedIntField.value
              val p = popSizeSlider.value
              val pbInfect = probInfectSlider.value
              val pbDying = probDieSlider.value
              val vel = velSlider.value
              val infectious = infectiousSlider.value
              val Hz = HzSlider.value

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

  private val grid = new GridBagPanel {
    def constraints( x: Int, y: Int
                   , gridwidth: Int = 1, gridheight: Int = 1
                   , weightx: Double = 0.0, weighty: Double = 0.0
                   , fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None
                   , insets: Insets = new Insets(5,1,5,1)
                   , anchor: GridBagPanel.Anchor.Value = GridBagPanel.Anchor.West
                   ) : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c.insets = insets
      c.anchor = anchor
      c
    }

    border = BorderFactory.createEmptyBorder(10,5,50,2)
    add(seedLabel, constraints(0, 0))
    add(new FlowPanel() {
          hGap = 0
          contents += seedIntField
          contents += Swing.HStrut(5)
          contents += randomSeedButton
        }
      , constraints(1, 0))

    add(popSizeLabel, constraints(0, 1))
    add(popSizeSlider, constraints(1, 1))

    add(velLabel, constraints(0, 2))
    add(velSlider, constraints(1, 2))

    add(probInfectLabel, constraints(0, 3))
    add(probInfectSlider, constraints(1, 3))

    add(infectiousLabel, constraints(0, 4))
    add(infectiousSlider, constraints(1, 4))

    add(probDieLabel, constraints(0, 5))
    add(probDieSlider, constraints(1, 5))

    add(HzLabel, constraints(0, 6))
    add(HzSlider, constraints(1, 6))

    add(startButton, constraints(0, 7, weighty = 2))

    add(scaleLabel, constraints(0, 8))
    add(scaleSlider, constraints(1, 8))

    add(aboutButton, constraints(0, 9))
  }

  contents = new BorderPanel {
    layout += new BoxPanel(Orientation.Vertical){
                border = BorderFactory.createEtchedBorder()
                contents += grid
              } -> BorderPanel.Position.West
    layout += canvas -> BorderPanel.Position.Center
  }

  listenTo(randomSeedButton, startButton, aboutButton, scaleSlider)
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

    case ValueChanged(`scaleSlider`) =>
      window.scale = scaleSlider.value
  }

  visible = true
  scale_=(0.8)
  startButton.requestFocus()
}
