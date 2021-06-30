package ucesoft.zx.spectrum

import ucesoft.zx.{Clock, ClockEvent, Log, Model, ZXComponent, ZXComponentType}
import ucesoft.zx.audio.{AY38912, Speaker}
import ucesoft.zx.cpu.Z80
import ucesoft.zx.gpu.{Palette, ULA, ULAPlus}
import ucesoft.zx.joystick.JoystickBridge
import ucesoft.zx.keyboard.{DefaultKeyboardMapper, Keyboard, _128KeyboardMapper}
import ucesoft.zx.misc.Preferences
import ucesoft.zx.mmu.MMU
import ucesoft.zx.tape.TZXTape

import java.io.{File, FileReader, FileWriter, IOException}
import java.util.Properties

class Spectrum(errorHandler: Throwable => Unit) extends ZXComponent {
  override val componentID: String = "ZX Computer"
  override val componentType: ZXComponentType.Type = ZXComponentType.MOTHERBOARD
  protected val CONFIGURATION_FILENAME = "ZX.config"

  // PREFERENCES ===============================================================
  val preferences = new Preferences
  val configuration = new Properties()
  private[this] var _configurationFile : File = _
  // COMPONENTS ================================================================
  val clk = Clock.setSystemClock(Some(errorHandler),false)(mainLoop _)
  val keyboard = new Keyboard(DefaultKeyboardMapper)
  val tape = new TZXTape
  val joystick = new JoystickBridge
  val speaker = new Speaker(44100,100)
  val ay = new AY38912(44100,500)
  val mmu = new MMU(keyboard,speaker,ay,joystick,tape)(preferences)
  val z80 = new Z80(mmu,mmu)
  val ula = new ULA(mmu,irqHandler _)
  // MODEL =====================================================================
  var model : Model.Value = Model._48K

  // CONSTRUCTOR ===============================================================
  configure
  // ===========================================================================

  // ULA IRQ handler ULA => Z80
  private def irqHandler(irqLow:Boolean) : Unit = z80.irq(irqLow,0xFF)

  def configurationFile : File = _configurationFile

  def saveConfiguration : Unit = {
    val conf = new FileWriter(configurationFile)
    configuration.store(conf,"ZX configuration file")
    conf.close()
  }

  def savePreferences : Unit = {
    preferences.save(configuration)
    saveConfiguration
  }

  override def reset: Unit = {
    clk.maximumSpeed = false
  }

  private def configure : Unit = {
    // LOAD configuration
    // ZX HOME ==========================================================================
    var zxHome = System.getProperty("zx.home")
    if (zxHome == null) {
      zxHome = scala.util.Properties.userHome
      println(s"Warning: zx.home env variable not set. Default zx home is $zxHome")
    }
    else zxHome = new File(new File(zxHome),"conf").toString
    // PROPERTIES =======================================================================

    val configHome = System.getProperty("zx.config",zxHome)
    _configurationFile = new File(new File(configHome),CONFIGURATION_FILENAME)
    if (_configurationFile.exists) {
      try {
        configuration.load(new FileReader(_configurationFile))
      }
      catch {
        case _:IOException =>
          setDefaultProperties
      }
    }
    else setDefaultProperties
  }

  override def init: Unit = {
    add(clk)
    add(keyboard)
    add(tape)
    add(joystick)
    add(speaker)
    add(ay)
    add(mmu)
    add(z80)
    add(ula)

    mmu.setChips(z80,ula)
  }

  private def mainLoop(cycles:Long) : Unit = {
    z80.clock
  }

  protected def setDefaultProperties : Unit = {

  }

  override def modelChanged(model: Model.Value,resetRequest:Boolean): Unit = {
    if (this.model != model) {
      this.model = model

      model match {
        case Model._48K|Model._16K =>
          clk.setClockHz(3500000)
          keyboard.mapper = DefaultKeyboardMapper
        case _ =>
          clk.setClockHz(3546900)
          keyboard.mapper = _128KeyboardMapper
      }
      ula.modelChanged(model,resetRequest)
      mmu.modelChanged(model,resetRequest)

      if (resetRequest) resetComponent
    }
  }

  // API =============================================================
  def setWarpMode(mode:Boolean) : Unit = {
    clk.maximumSpeed = mode
  }

  def insertKeyCodes(codes:Int*) : Unit = {
    clk.pause
    val size = ula.getDisplaySize
    val waitCycles = ((size.width >> 1) * size.height * 1.5).toInt
    var cycle = clk.nextCycles + waitCycles
    for(c <- codes) {
      clk.schedule(new ClockEvent("Key",cycle,_ => {
        mmu.load(23560,c) // LAST_K
        mmu.load(23611,mmu.peek(23611) | 0x20) // FLAGS
      }))
      cycle += waitCycles + 1
    }
    clk.play
  }

  def setULAPlus(enabled: Boolean,play:Boolean = true) : Unit = {
    clk.pause
    ULAPlus.enabled = enabled
    if (!enabled) ula.setPalette(Palette.DEFAULT)
    if (play) clk.play
  }
}
