package ucesoft.zx.gpu

import ucesoft.zx.ZXComponent
import ucesoft.zx.ZXComponentType

import java.util.Properties

object ULAPlus extends ZXComponent {
  override val componentID: String = "ULA+"
  override val componentType: ZXComponentType.Type = ZXComponentType.CHIP

  val PALETTE : Array[Int] = Array.ofDim[Int](64)
  var enabled = false
  var active = false
  var activeListener : (Boolean,Boolean) => Unit = _

  private[this] val GRTABLE = Array(0x00,0x24,0x49,0x6D,0x92,0xB6,0xDB,0xFF)
  private[this] val BTABLE = Array(0x00,0x6D,0xB6,0xFF)
  private[this] var group = 0
  private[this] var paletteEntry = 0
  private[this] var lastData = 0

  @inline private def GRBtoRGB(grb:Int) : Int = {
    val r = GRTABLE((grb >> 2) & 7)
    val g = GRTABLE((grb >> 5) & 7)
    val b = BTABLE(grb & 3)
    0xFF << 24 | r << 16 | g << 8 | b
  }

  def getPaletteIndex(attribute:Int,ink:Boolean) : Int = {
    val fb = (attribute & 0xC0) >> 2
    val _0123 = if (ink) attribute & 0x7 else (attribute >> 3) & 0x07 | 0x8
    fb | _0123
  }

  private def storePalette(index:Int,palette:Int) : Unit = {
    PALETTE(index) = GRBtoRGB(palette)
  }

  def writeRegisterPort(value:Int) : Unit = {
    group = value >> 6
    if (group == 0) paletteEntry = value & 0x3F
  }

  def writeDataPort(value:Int) : Unit = {
    if (group == 0) storePalette(paletteEntry,value)
    else {
      active = (value & 1) == 1
      if (activeListener != null) activeListener(active,true)
    }
    lastData = value
  }

  def readDataPort : Int = {
    if (group == 1) return if (active) 1 else 0
    lastData
  }

  override def reset: Unit = {
    active = false
    if (activeListener != null) activeListener(false,enabled)
    group = 0
    lastData = 0
  }

  override def hardReset: Unit = {
    reset
    enabled = false
    if (activeListener != null) activeListener(false,false)
  }

  override def getProperties: Properties = {
    properties.setProperty("Enabled",enabled.toString)
    properties.setProperty("Active",active.toString)
    properties
  }

  override def init: Unit = {}
}
