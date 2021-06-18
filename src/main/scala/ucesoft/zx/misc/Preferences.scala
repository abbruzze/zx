package ucesoft.zx.misc

import java.util.Properties
import scala.collection.mutable

object Preferences {
  // GENERAL ======================================================
  val XY = "frame.xy"
  val AUTOSAVE_PREFERENCES = "pref.autosave"
  // ROMS =========================================================
  val _48K_ROM_PREF = "48k-rom-path"
  val _128K_0_ROM_PREF = "128k-rom-0-path"
  val _128K_1_ROM_PREF = "128k-rom-1-path"
  val _128K_PLUS2_0_ROM_PREF = "128k+2-rom-0-path"
  val _128K_PLUS2_1_ROM_PREF = "128k+2-rom-1-path"
  val _128K_PLUS2A_0_ROM_PREF = "128k+2A-rom-0-path"
  val _128K_PLUS2A_1_ROM_PREF = "128k+2A-rom-1-path"
  val _128K_PLUS2A_2_ROM_PREF = "128k+2A-rom-2-path"
  val _128K_PLUS2A_3_ROM_PREF = "128k+2A-rom-3-path"
  val _128K_PLUS3_0_ROM_PREF = "128k+3-rom-0-path"
  val _128K_PLUS3_1_ROM_PREF = "128k+3-rom-1-path"
  val _128K_PLUS3_2_ROM_PREF = "128k+3-rom-2-path"
  val _128K_PLUS3_3_ROM_PREF = "128k+3-rom-3-path"
  val IFI_ROM_PREF = "ifI-rom-path"
  // JOYSTICK =====================================================
  val JOY_UP_KEY = "joy-up-key"
  val JOY_DOWN_KEY = "joy-down-key"
  val JOY_LEFT_KEY = "joy-left-key"
  val JOY_RIGHT_KEY = "joy-right-key"
  val JOY_FIRE_KEY = "joy-fire-key"
  val JOY_TYPE = "joy-type"
  // MODEL ========================================================
  val MODEL = "model"
  // VIDEO ========================================================
  val SNOW_EFFECT_ENABLED = "snow-effect"
  val ULAPLUS_ENABLED = "ulaplus"
  val PAL_SCANLINES_ENABLED = "pal-scanlines"
  val FULLS_SCREEN = "full-screen"
  // TAPE =========================================================
  val TAPE_FAST_LOADER_ENABLED = "tape-fastload"
  val TAPE_AUTOPLAY_ENABLED = "tape-autoplay"
  val TAPE_AUTOSTOP_ENABLED = "tape-autostop"
  val TAPE_WARPMODE_ENABLED = "tape-warpmode"
  // AUDIO ========================================================
  val AUDIO_AY_ON48K_ENABLED = "ay-on-48k"
  val AUDIO_AY_MODE = "ay-mode"
  // MOUSE ========================================================
  val MOUSE_ENABLED = "kempstone-mouse"
  // INTERFACE I ==================================================
  val IFI_ENABLED = "interface-1"
  val MICRODRIVE_SECTORS = "microdrive-sectors"
  val MICRODRIVE_WRITE_CHANGES = "microdrive-write-changes"
  // VIDEO ========================================================
  val ZOOM_FACTOR = "zoom-x"
  // LEC ==========================================================
  val LEC = "lec"
  // ==============================================================
  class PreferenceIllegalArgumentException(msg:String) extends Exception(msg)

  trait PreferenceChangeListener {
    def preferenceHasChanged(pref:Preference[_]) : Unit
  }

  trait PreferenceConv[T] {
    def convert(value:String) : T
    val consume : Int
    val default : T
  }

  implicit object StringPreferenceConv extends PreferenceConv[String] {
    def convert(value:String) = value
    val default = ""
    val consume = 1
  }
  implicit object IntPreferenceConv extends PreferenceConv[Int] {
    def convert(value:String) = value.toInt
    val default = 0
    val consume = 1
  }
  implicit object BooleanPreferenceConv extends PreferenceConv[Boolean] {
    def convert(value:String) = value.toBoolean
    val default = false
    val consume = 0
  }

  case class Preference[T](cmdLine:String,description:String,var value:T,enumerated:Set[T] = Set.empty[T],canBeSaved : Boolean = true)(listener : T => Unit)(implicit prefConv:PreferenceConv[T]) {
    private var listeners : List[PreferenceChangeListener] = Nil
    private[Preferences] var adjusting = true
    private var loaded = false
    private[misc] var internal = false

    def isAdjusting : Boolean = adjusting
    def isLoaded : Boolean = loaded

    listeners ::= new PreferenceChangeListener {
      override def preferenceHasChanged(pref: Preference[_]): Unit = listener(value)
    }

    def addChangeListener(l:PreferenceChangeListener) : Unit = if (!listeners.contains(l)) listeners ::= l

    def notifyListeners : Unit = for(l <- listeners) l.preferenceHasChanged(this)

    def load(p:Properties) : Unit = {
      p.getProperty(cmdLine) match {
        case null =>
        case v => load(v)
      }
    }

    def load(from:String) : Unit = load(prefConv.convert(from))

    def load(v:T,notify:Boolean = true) : Unit = {
      loaded = true
      val prevValue = value
      value = v
      if (!enumerated.isEmpty && !enumerated.contains(value)) {
        val badValue = value
        value = prevValue
        throw new PreferenceIllegalArgumentException(s"Bad value '$badValue' for option '$cmdLine'. Expected: ${enumerated.mkString(",")}")
      }
      if (prevValue != value && notify) notifyListeners
    }

    def save(p:Properties) : Unit = {
      if (loaded && canBeSaved) p.setProperty(cmdLine,if (value != null) value.toString else "")
    }

    def consume : Int = prefConv.consume
  }
}

class Preferences {
  import Preferences._

  private[this] val prefs = new collection.mutable.ListBuffer[Preference[_]]

  def add[T](cmdLine:String,description:String,value:T,enumerated:Set[T] = Set.empty[T],canBeSaved:Boolean = true)(listener : T => Unit)(implicit prefConv:PreferenceConv[T]) : Preference[T] = {
    val p = Preference(cmdLine,description,value,enumerated,canBeSaved)(listener)
    prefs += p
    p
  }

  def addInternal[T](cmdLine:String,value:T,enumerated:Set[T] = Set.empty[T])(listener : T => Unit)(implicit prefConv:PreferenceConv[T]) : Preference[T] = {
    val p = Preference(cmdLine,"",value,enumerated,false)(listener)
    p.internal = true
    prefs += p
    p
  }

  def save(p:Properties) : Unit = {
    for(s <- preferences) s.save(p)
  }

  def remove(cmdLine:String) : Unit = {
    prefs.find(_.cmdLine == cmdLine).foreach( prefs -= _ )
  }

  def apply[T](cmdLine:String) : Option[T] = {
    prefs.find(_.cmdLine == cmdLine) match {
      case Some(p) if p.isLoaded => Some(p.value.asInstanceOf[T])
      case _ => None
    }
  }

  def get[T](cmdLine:String) : Option[Preference[T]] = prefs.find(_.cmdLine == cmdLine).asInstanceOf[Option[Preference[T]]]

  def update[T](cmdLine:String,value:T) : Unit = prefs.find(_.cmdLine == cmdLine).foreach(_.asInstanceOf[Preference[T]].load(value))

  def updateWithoutNotify[T](cmdLine:String,value:T) : Unit = prefs.find(_.cmdLine == cmdLine).foreach(_.asInstanceOf[Preference[T]].load(value,false))

  def checkForHelp(args:Array[String]) : Boolean = args.length == 1 && (args(0) == "--help" || args(0) == "-h" || args(0) == "-help")

  def printUsage(fileDescr:String)  : Unit = {
    println(s"Usage: [settings] [$fileDescr]")
    for(s <- prefs.filterNot(_.internal)) {
      val opt = if (s.cmdLine.length > 20) s.cmdLine else s.cmdLine + (" " * (20 - s.cmdLine.length))
      println(s"--$opt${s.description}")
    }
  }

  def preferences : List[Preference[_]] = prefs.toList

  def parseAndLoad(args:Array[String],props:Properties) : Option[String] = {
    var p = 0
    val found : collection.mutable.Set[String] = new mutable.HashSet[String]
    while (p < args.length && args(p).startsWith("--")) {
      try {
        prefs find {
          _.cmdLine == args(p).substring(2)
        } match {
          case Some(s) if s.consume == 0 =>
            if (p + 1 < args.length && !args(p + 1).startsWith("--")) {
              s.load(args(p + 1))
              p += 2
            }
            else {
              s.load("true")
              p += 1
            }
            found += s.cmdLine
          case Some(s) if s.consume == 1 =>
            if (p + 1 < args.length) s.load(args(p + 1))
            else throw new PreferenceIllegalArgumentException("Value for setting " + args(p) + " not found")
            p += 2
            found += s.cmdLine
          case _ =>
            throw new PreferenceIllegalArgumentException("Invalid setting: " + args(p))
        }
      }
      catch {
        case e:Throwable =>
          throw new PreferenceIllegalArgumentException(s"error while applying command option ${args(p)}: ${e.getMessage}")
      }
    }
    // check props
    val filtered = prefs.filterNot(p => found.contains(p.cmdLine))
    filtered.foreach(_.load(props))

    // stop adjusting
    for(p <- prefs) p.adjusting = false

    if (p < args.length) Some(args(p)) else None
  }
}

