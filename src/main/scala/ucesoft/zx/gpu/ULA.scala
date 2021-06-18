package ucesoft.zx.gpu

import ucesoft.zx.ZXComponentType.Type
import ucesoft.zx.{ChipID, Clock, Model, ZXComponent, ZXComponentType}
import ucesoft.zx.cpu.Memory

import java.awt.Dimension
import java.util.Properties
import scala.annotation.switch

object ULA {
  case class GFXClip(x1:Int,y1:Int,x2:Int,y2:Int)
  private val SCANLINE_COLORS : Array[Int] = {
    val colors = Array.ofDim[Int](256)
    for(c <- 0 until colors.length) colors(c) = (c * 0.80).toInt
    colors
  }
}


class ULA(mem:Memory, irq : Boolean => Unit) extends ZXComponent {
  override val componentType: Type = ZXComponentType.VIDEO
  override val componentID: String = "ULA"

  import ULA._

  private[this] final val LEFT_BORDER = 48
  private[this] final val RIGHT_BORDER = 48
  private[this] final val DISPLAY_X_SIZE = 256
  private[this] final val DISPLAY_Y_SIZE = 192
  private[this] final val TOP_BORDER = 48
  private[this] final val BOTTOM_BORDER = 56

  private[this] var clip : GFXClip = ULA48K.DEFAULT_CLIP


  private[this] var gfxYOffset,attrYOffset = 0
  private[this] var gfx,gfxLatch,attrLatch = 0
  private[this] var borderColor,borderLatchColor = 0
  private[this] var borderLatched = false
  private[this] var ink,paper = 0
  private[this] var flashOn = false
  private[this] var flashCounter = 0

  private[this] var palette : Array[Int] = Palette.DEFAULT

  private[this] var cycle,xDisplay,yDisplay,pixelCycle = 0
  private[this] var raster = 0 // 0 - 312 lines
  private[this] var vBorderOn = true
  private[this] var contended = false
  private[this] var displayArea = false
  private[this] var bus,lastBus = 0xFF

  private[this] var pixels : Array[Int] = _
  private[this] var display : Display = _

  private[this] var minModX,maxModX,minModY,maxModY = 0
  private[this] var pixelMod = false

  private[this] var irqCycle = 0L
  private[this] lazy val clk = Clock.systemClock

  private[this] var applyScanLineEffect = false

  private abstract class ULAType {
    val DEFAULT_CLIP : GFXClip

    final def clock : Unit = {
      (pixelCycle & 0x07 : @switch) match {
        case 0 =>
          attrLatch = bus
          bus = 0xFF
          contended = false
        case 1 =>
          bus = 0xFF
        case 2 =>
          contended = pixelCycle < 122
        case 3 =>
          applyAttr
        case 4 =>
          contended = pixelCycle < 124
          if (contended) bus = fetchGfx
        case 5 =>
          if (contended) {
            bus = fetchAttr
          }
        case 6 =>
          if (contended) {
            attrLatch = bus
            bus = fetchGfx
          }
        case 7 =>
          if (contended) {
            applyAttr
            bus = fetchAttr
          }
          else displayArea = false
      }
    }

    protected def setCoord : Unit = {
      val y345 = (yDisplay & 0x38) << 2
      gfxYOffset = 0x0000 | y345 | (yDisplay & 0x07) << 8 | (yDisplay & 0xC0) << 5
      attrYOffset = 0x1800 | y345 | (yDisplay & 0xC0) << 2
      pixelCycle = 0
    }

    def checkCycle : Unit
    def checkRaster : Unit
    def canDrawScreen : Boolean
    def lineSize : Int
    def totalLines : Int
    def cyclesPerLine: Int
  }

  private object ULA48K extends ULAType {
    final val HSYNC = 96
    final val YSYNC = 16
    final val IRQ_LEN = 32
    final val LINE_SIZE = HSYNC + LEFT_BORDER + DISPLAY_X_SIZE + RIGHT_BORDER
    final val CYCLES_PER_LINE: Int = LINE_SIZE >> 1
    final val TOTAL_LINES: Int = DISPLAY_Y_SIZE + TOP_BORDER + BOTTOM_BORDER + YSYNC

    final val RASTER_TOP_BORDER_CHECK = TOP_BORDER + YSYNC
    final val RASTER_BOTTOM_BORDER_CHECK = TOP_BORDER + YSYNC + DISPLAY_Y_SIZE
    final val DEFAULT_CLIP : GFXClip = GFXClip(HSYNC + LEFT_BORDER - 32,YSYNC + TOP_BORDER - 24,HSYNC + LEFT_BORDER + DISPLAY_X_SIZE + 32,YSYNC + TOP_BORDER + DISPLAY_Y_SIZE + 24)

    private[this] final val FIRST_VISIBLE_CYCLE = HSYNC >> 1
    private[this] final val FIRST_GFX_CYCLE = (HSYNC + LEFT_BORDER) >> 1
    private[this] final val CONTENDED_CHECK_CYCLE = FIRST_GFX_CYCLE - 6
    private[this] final val IRQ_ENABLED_CHECK_CYCLE = FIRST_GFX_CYCLE - 5
    private[this] final val IRQ_DISABLED_CHECK_CYCLE = IRQ_ENABLED_CHECK_CYCLE + IRQ_LEN
    private[this] final val GFX_FETCH_CYCLE = FIRST_GFX_CYCLE - 4
    private[this] final val ATTR_FETCH_CYCLE = FIRST_GFX_CYCLE - 3
    private[this] final val NEXT_GFX_FETCH_CYCLE = FIRST_GFX_CYCLE - 2
    private[this] final val NEXT_ATTR_FETCH_CYCLE = FIRST_GFX_CYCLE - 1

    override final def canDrawScreen : Boolean = cycle >= FIRST_VISIBLE_CYCLE && raster >= YSYNC
    override final def lineSize: Int = LINE_SIZE
    override final def totalLines: Int = TOTAL_LINES
    override final def cyclesPerLine: Int = CYCLES_PER_LINE

    override final def checkRaster : Unit = {
      raster match {
        case RASTER_TOP_BORDER_CHECK =>
          vBorderOn = false
          xDisplay = 0
          yDisplay = 0
          pixelCycle = 0
        case RASTER_BOTTOM_BORDER_CHECK =>
          vBorderOn = true
        case TOTAL_LINES =>
          raster = 0
          drawFrame
        case _ =>
      }
    }

    override final def checkCycle: Unit = {
      if (borderLatched && ((cycle & 0x3) == 0)) {
        borderLatched = false
        borderColor = borderLatchColor
      }

      (cycle : @switch) match {
        case 0 =>
          if (!vBorderOn) setCoord
        case CONTENDED_CHECK_CYCLE =>
          contended = !vBorderOn
        case IRQ_ENABLED_CHECK_CYCLE =>
          if (raster == 0) {
            irq(true)
            irqCycle = clk.currentCycles
          }
        case GFX_FETCH_CYCLE =>
          if (contended) bus = fetchGfx
        case ATTR_FETCH_CYCLE =>
          if (contended) bus = fetchAttr
        case NEXT_GFX_FETCH_CYCLE =>
          if (contended) {
            attrLatch = bus
            bus = fetchGfx
          }
        case NEXT_ATTR_FETCH_CYCLE =>
          if (contended) {
            applyAttr
            bus = fetchAttr
          }
        case IRQ_DISABLED_CHECK_CYCLE =>
          if (raster == 0) irq(false)
        case FIRST_GFX_CYCLE =>
          displayArea = contended
        case _ =>
      }
    }
  }
  private object ULA128K extends ULAType {
    final val HSYNC = 96 + 8
    final val YSYNC = 15
    final val IRQ_LEN = 36
    final val LINE_SIZE = HSYNC + LEFT_BORDER + DISPLAY_X_SIZE + RIGHT_BORDER
    final val CYCLES_PER_LINE: Int = LINE_SIZE >> 1
    final val TOTAL_LINES: Int = DISPLAY_Y_SIZE + TOP_BORDER + BOTTOM_BORDER + YSYNC

    final val RASTER_TOP_BORDER_CHECK = TOP_BORDER + YSYNC
    final val RASTER_BOTTOM_BORDER_CHECK = TOP_BORDER + YSYNC + DISPLAY_Y_SIZE
    final val DEFAULT_CLIP : GFXClip = GFXClip(HSYNC + LEFT_BORDER - 32,YSYNC + TOP_BORDER - 24,HSYNC + LEFT_BORDER + DISPLAY_X_SIZE + 32,YSYNC + TOP_BORDER + DISPLAY_Y_SIZE + 24)

    private[this] final val FIRST_VISIBLE_CYCLE = HSYNC >> 1
    private[this] final val FIRST_GFX_CYCLE = (HSYNC + LEFT_BORDER) >> 1
    private[this] final val CONTENDED_CHECK_CYCLE = FIRST_GFX_CYCLE - 6
    private[this] final val GFX_FETCH_CYCLE = FIRST_GFX_CYCLE - 4
    private[this] final val IRQ_ENABLED_CHECK_CYCLE = GFX_FETCH_CYCLE
    private[this] final val IRQ_DISABLED_CHECK_CYCLE = IRQ_ENABLED_CHECK_CYCLE + IRQ_LEN
    private[this] final val ATTR_FETCH_CYCLE = FIRST_GFX_CYCLE - 3
    private[this] final val NEXT_GFX_FETCH_CYCLE = FIRST_GFX_CYCLE - 2
    private[this] final val NEXT_ATTR_FETCH_CYCLE = FIRST_GFX_CYCLE - 1

    override final def canDrawScreen : Boolean = cycle >= FIRST_VISIBLE_CYCLE && raster >= YSYNC
    override final def lineSize: Int = LINE_SIZE
    override final def totalLines: Int = TOTAL_LINES
    override final def cyclesPerLine: Int = CYCLES_PER_LINE

    override final def checkRaster : Unit = {
      raster match {
        case RASTER_TOP_BORDER_CHECK =>
          vBorderOn = false
          xDisplay = 0
          yDisplay = 0
          pixelCycle = 0
        case RASTER_BOTTOM_BORDER_CHECK =>
          vBorderOn = true
        case TOTAL_LINES =>
          raster = 0
          drawFrame
        case _ =>
      }
    }

    override final def checkCycle: Unit = {
      if (borderLatched && ((cycle & 0x3) == 0)) {
        borderLatched = false
        borderColor = borderLatchColor
      }

      (cycle : @switch) match {
        case 0 =>
          if (!vBorderOn) setCoord
        case CONTENDED_CHECK_CYCLE =>
          contended = !vBorderOn
        case GFX_FETCH_CYCLE =>
          if (raster == 0) {
            irq(true)
            irqCycle = clk.currentCycles
          }
          if (contended) bus = fetchGfx
        case ATTR_FETCH_CYCLE =>
          if (contended) bus = fetchAttr
        case NEXT_GFX_FETCH_CYCLE =>
          if (contended) {
            attrLatch = bus
            bus = fetchGfx
          }
        case NEXT_ATTR_FETCH_CYCLE =>
          if (contended) {
            applyAttr
            bus = fetchAttr
          }
        case IRQ_DISABLED_CHECK_CYCLE =>
          if (raster == 0) irq(false)
        case FIRST_GFX_CYCLE =>
          displayArea = contended
        case _ =>
      }
    }
  }

  private[this] var ulaType : ULAType = ULA48K

  override def modelChanged(model: Model.Value,resetRequest:Boolean): Unit = {
    model match {
      case Model._48K|Model._16K =>
        ulaType = ULA48K
      case Model._128K | Model._128K_PLUS2 | Model._128K_PLUS3 | Model._128K_PLUS2A =>
        ulaType = ULA128K
    }
    clip = ulaType.DEFAULT_CLIP
    reset
  }

  def getRasterLine : Int = raster

  def setScanLineEffect(sle:Boolean) : Unit = applyScanLineEffect = sle

  def getIRQCycle : Long = irqCycle

  def setDisplay(display:Display) : Unit = {
    this.display = display
    pixels = display.displayMem

    display.setClipArea(clip.x1,clip.y1,clip.x2,clip.y2)
  }

  def getDisplaySize : Dimension = new Dimension(ulaType.lineSize,ulaType.totalLines)

  def isContended : Boolean = contended

  def getBorderColor : Int = borderColor
  def setBorderColor(_color:Int) : Unit = {
    var color = _color
    if (ULAPlus.active) {
        color = ULAPlus.getPaletteIndex(color << 3,false)
    }

    borderLatchColor = color
    borderLatched = true
  }

  def getClip : GFXClip = clip
  /*
  def setClip(clip:GFXClip) : Unit = {
    this.clip = GFXClip(ulaType.HSYNC + LEFT_BORDER - clip.x1,ulaType.YSYNC + TOP_BORDER - clip.y1,ulaType.HSYNC + LEFT_BORDER + DISPLAY_X_SIZE + clip.x2,ulaType.YSYNC + TOP_BORDER + DISPLAY_Y_SIZE + clip.y2)
    display.setClipArea(this.clip.x1,this.clip.y1,this.clip.x2,this.clip.y2)
  }
  */

  @inline private def fetchGfx : Int = mem.read(gfxYOffset | xDisplay & 0x1F,ChipID.ULA)
  @inline private def fetchAttr : Int = {
    gfxLatch = bus
    val a = mem.read(attrYOffset | xDisplay & 0x1F,ChipID.ULA)
    xDisplay += 1
    a
  }

  final def floatingBus : Int = bus
  final def lastFloatingBus : Int = (lastBus >> 16) & 0xFF

  override def getProperties: Properties = {
    properties.setProperty("Raster line",raster.toString)
    properties.setProperty("Line cycle",cycle.toString)
    properties.setProperty("Bus value",bus.toString)
    properties.setProperty("Contended",contended.toString)
    properties.setProperty("Width in pixels",ulaType.lineSize.toString)
    properties.setProperty("Height (lines)",ulaType.totalLines.toString)
    properties
  }

  def setPalette(palette:Array[Int]) : Unit = {
    this.palette = palette
  }

  override def init : Unit = {
    add(ULAPlus)
  }

  override def reset : Unit = {
    gfxYOffset = 0
    attrYOffset = 0
    gfx = 0
    gfxLatch = 0
    attrLatch = 0
    borderLatched = false
    flashOn = false
    flashCounter = 0
    cycle = 0
    xDisplay = 0
    yDisplay = 0
    pixelCycle = 0
    raster = 0
    vBorderOn = true
    contended = false
    displayArea = false
    bus = 0xFF
    lastBus = 0xFF
    pixelMod = false
  }

  final def clock : Unit = {
    ulaType.checkCycle

    if (ulaType.canDrawScreen) {
      if (displayArea) {
        val color1 = if ((gfx & 0x80) > 0) ink else paper
        gfx <<= 1
        val color2 = if ((gfx & 0x80) > 0) ink else paper
        gfx <<= 1
        draw2Pixels(color1, color2)

        ulaType.clock

        pixelCycle += 1
      }
      else draw2Pixels(borderColor, borderColor)
    }

    cycle += 1
    if (cycle == ulaType.cyclesPerLine) {
      cycle = 0
      if (!vBorderOn) {
        yDisplay += 1
        xDisplay = 0
      }

      raster += 1
      ulaType.checkRaster
    }
    lastBus = lastBus << 8 | bus
  }

  @inline private def drawFrame : Unit = {
    //display.showFrame(clip.x1,clip.y1,clip.x2,clip.y2)
    if (pixelMod) display.showFrame(minModX,minModY,maxModX,maxModY)
    else display.showFrame(-1,0,0,0)
    //println(s"Draw ($minModX,$minModY)-($maxModX,$maxModY) $pixelMod")
    minModY = -1
    minModX = Integer.MAX_VALUE
    maxModX = 0
    pixelMod = false
    flashCounter += 1
    if (flashCounter == 16) {
      flashCounter = 0
      flashOn ^= true
    }
  }

  @inline private def applyAttr : Unit = {
    gfx = gfxLatch
    if (ULAPlus.active) {
      ink = ULAPlus.getPaletteIndex(attrLatch,true)
      paper = ULAPlus.getPaletteIndex(attrLatch,false)
    }
    else {
      val bright = (attrLatch & 0x40) >> 3
      ink = attrLatch & 0x07 | bright
      paper = (attrLatch >> 3) & 0x07 | bright

      if (flashOn && (attrLatch & 0x80) > 0) gfx ^= 0xFF
    }
  }

  @inline private def draw2Pixels(color1:Int,color2:Int) : Unit = {
    val x = cycle << 1
    val base = raster * ulaType.lineSize + x
    val c1 = scanLineEffect(palette(color1))
    val c2 = scanLineEffect(palette(color2))
    if (pixels(base) != c1) {
      checkCoords(x)
      pixels(base) = c1
    }
    if (pixels(base + 1) != c2) {
      checkCoords(x + 1)
      pixels(base + 1) = c2
    }
  }

  @inline private def scanLineEffect(color:Int) : Int = {
    if (!applyScanLineEffect || (raster & 1) == 0) color
    else {
      val r = SCANLINE_COLORS((color >> 16) & 0xFF)
      val g = SCANLINE_COLORS((color >> 8) & 0xFF)
      val b = SCANLINE_COLORS(color & 0xFF)
      0xFF << 24 | r << 16 | g << 8 | b
    }
  }

  @inline private def checkCoords(x:Int) : Unit = {
    if (x < minModX) minModX = x
    else if (x > maxModX) maxModX = x
    if (minModY == -1) minModY = raster
    maxModY = raster
    pixelMod = true
  }
}
