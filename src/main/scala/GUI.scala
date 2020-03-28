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
  val canvas: Component
}

object Fonts {
  val mono = new Font(Font.Monospaced, java.awt.Font.PLAIN, 14)
  val sans8 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 8)
  val sans10 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 10)
  val sans11 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 10)
  val sans14 = new Font(Font.SansSerif, java.awt.Font.PLAIN, 14)
  val default = sans11
}

object Colors {
  val nonInfected = new Color(0, 0, 220)
  val infected = Color.red
  val recovered = new Color(0, 200, 0)
  val dead = new Color(50, 50, 50)

  def withAlpha(c: Color, alpha: Int): Color =
    new Color(c.getRed, c.getGreen, c.getBlue, alpha)
}

class IntField(v: Int = 0) extends TextField {
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
    maximumSize = new Dimension(0, 15)
  }
  private val numericSlider = this

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
    case ValueChanged(`slider`) =>
      textField.text = formatText(value)
      publish(new ValueChanged(numericSlider)) // propagate outwards
  }

  config()
}

class DoubleSlider(v: Double, from: Int, to: Int) extends NumericSlider[Double] {
  def this(from: Int, to: Int) {
    this(0, from, to)
  }
  def formatLabel(v: Double): String = s"$v"
  def fromInt(n: Int): Double = n
  def toInt(v: Double): Int = v.toInt

  min = from
  max = to
  value = v
}

class ProbSlider(v: Double = 0) extends DoubleSlider(v, 0, 100) {
  override def formatLabel(v: Double): String = f"$v%.1f"
  override def formatText(v: Double): String = f"$v%.2f"
  override def fromInt(n: Int): Double = n/100.0
  override def toInt(v: Double): Int = (v*100).toInt
}

class IntSlider(v: Int, from: Int, to: Int) extends NumericSlider[Int] {
  def this(from: Int, to: Int) {
    this(0, from, to)
  }
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
        transform.translate(w/2, 1.15*h/2)
        transform.scale(scale, scale)
        g2D.transform(transform)
        procedure(g2D)
      }
  }

  def update(): Unit = {
    canvas.repaint()
  }

  def drawWith(procedure : Graphics2D => Unit): Unit = {
    setDrawingProcedure(procedure)
    update()
  }

  val canvas: Label = new Label {
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
  private val seedIntField = new IntField() {
    tooltip = seedTooltip
    columns = 10
  }
  private val randomSeedButton = new Button("Random") {
    tooltip = "Generate a random seed"
  }

  private val populationSzTooltip = "Number of individuals in population"
  private val populationSzLabel = new Label("Population size:") {
    tooltip = populationSzTooltip
  }
  private val populationSzSlider = new IntSlider(0, 1500) {
    tooltip = populationSzTooltip
  }

  private val velocityTooltip = "Velocity of individuals in normal distributed with μ=0 and this value as σ"
  private val velocityLabel = new Label("Velocity variance:") {
    tooltip = velocityTooltip
  }
  private val velocitySlider = new IntSlider(0, 100) {
    tooltip = velocityTooltip
  }

  private val probInfectionTooltip = "Probability of getting infected after contacting another infected individual"
  private val probInfectionLabel = new Label("Infection rate:") {
    tooltip = probInfectionTooltip
  }
  private val probInfectionSlider = new ProbSlider() {
    tooltip = probInfectionTooltip
  }

  private val probDyingTooltip = "Probability of dying after getting infected"
  private val probDyingLabel = new Label("Death rate:") {
    tooltip = probDyingTooltip
  }
  private val probDyingSlider = new ProbSlider() {
    tooltip = probDyingTooltip
  }

  private val timeInfectiousTooltip = "Time an individual remains infectious to others is normal distributed around this value(μ) with σ=1"
  private val timeInfectiousLabel = new Label("Time infectious:") {
    tooltip = timeInfectiousTooltip
  }
  private val timeInfectiousSlider = new IntSlider(0, 100){
    tooltip = timeInfectiousTooltip
  }

  private val HzTootlTip = "Number of redraw events per clock tick"
  private val HzLabel = new Label("Redraw frequency:") {
    tooltip = HzTootlTip
  }
  private val HzSlider = new IntSlider(0, 60) {
    tooltip = HzTootlTip
  }

  private val aboutButton = new Button("About...") {
    tooltip = "Show information about this program"
  }

  private val toDisable = Array(seedIntField, randomSeedButton
    , populationSzSlider, velocitySlider, probInfectionSlider
    , probDyingSlider, timeInfectiousSlider, HzSlider
    , aboutButton)

  private val window = this

  def configuration: Configuration =
    Configuration(
        seed = seedIntField.value
      , Hz = HzSlider.value
      , populationSz = populationSzSlider.value
      , velocitySigma = velocitySlider.value
      , timeLimit = DefaultConfiguration.timeLimit
      , probInfection= probInfectionSlider.value
      , probDying = probDyingSlider.value
      , timeInfectious = timeInfectiousSlider.value
      )

  def configuration_=(conf: Configuration): Unit = {
    seedIntField.value = conf.seed
    HzSlider.value = conf.Hz
    populationSzSlider.value = conf.populationSz
    velocitySlider.value = conf.velocitySigma.toInt
    probInfectionSlider.value = conf.probInfection
    probDyingSlider.value = conf.probDying
    timeInfectiousSlider.value = conf.timeInfectious.toInt
  }

  private object startButton extends Button {
    val startText = "Start"
    text = startText

    def setTooptip(): Unit =
      tooltip = s"$text running simulation"
    setTooptip()

    var threadOpt: Option[Thread] = None

    def onClick(): Unit = {
      threadOpt match {
        case None =>
          val thread = new Thread {
            override def run {
              // create a simulator and simulate
              val conf = configuration
              val simulator = Simulator(window, conf)
              simulator.simulate()
            }
          }
          thread.start()
          threadOpt = Some(thread)
          text = "Stop"
          setTooptip()
          toDisable.foreach(_.enabled = false)

        case Some(thread) =>
          thread.stop()
          threadOpt = None
          text = startText
          setTooptip()
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
        }, constraints(1, 0))

    add(populationSzLabel, constraints(0, 1))
    add(populationSzSlider, constraints(1, 1))

    add(velocityLabel, constraints(0, 2))
    add(velocitySlider, constraints(1, 2))

    add(probInfectionLabel, constraints(0, 3))
    add(probInfectionSlider, constraints(1, 3))

    add(timeInfectiousLabel, constraints(0, 4))
    add(timeInfectiousSlider, constraints(1, 4))

    add(probDyingLabel, constraints(0, 5))
    add(probDyingSlider, constraints(1, 5))

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
      seedIntField.value = Random.uniform(Int.MaxValue)

    case ButtonClicked(`startButton`) =>
      startButton.onClick()

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
  setDrawingProcedure(g2D => BoundingBox.drawOn(g2D))
  scale_=(0.8)
  configuration_=(DefaultConfiguration)
  startButton.requestFocus()
}
