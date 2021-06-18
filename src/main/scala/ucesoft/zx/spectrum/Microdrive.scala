package ucesoft.zx.spectrum

import ucesoft.zx.{Clock, ClockEvent, ZXComponent, ZXComponentType}
import ucesoft.zx.format.MDR

import java.util.Properties

class Microdrive(val id:Int,driveListener:MicrodriveListener) extends ZXComponent {
  override val componentID: String = s"Microdrive#$id"
  override val componentType: ZXComponentType.Type = ZXComponentType.MICRODRIVE

  final val transferCycles = 168 // 12us/bit (1 byte = 96 us => 3.5 * 96 = 336 cycles

  private[this] var mdr : MDR = _
  private[this] var gapWriting = false
  private[this] val clk = Clock.systemClock

  override def getProperties: Properties = {
    properties.setProperty("Cart:",if (mdr != null) mdr.getCartName else "empty")
    properties
  }

  def stopMotor : Unit = {
    if (mdr != null) {
      /*
      mdr.findEntries match {
        case Some(MDR.Entries(name, entries)) =>
          println(s"$name : #${entries.size} ${entries.filter(_.name.isDefined).map(_.name.get).toSet.mkString(",")}")
        case None =>
          println("Not found")
      }
       */

      if (mdr.isModified) mdr.save

      mdr.moveToNextGap
    }
    clk.cancel("MDR_GAP")
  }

  def insertCart(mdr:MDR) : Unit = {
    this.mdr = mdr
    driveListener.mdrInserted(mdr,id)
    this.mdr.setMicrodriveListener(driveListener)
  }

  def ejectCart : Unit = {
    mdr = null
    driveListener.mdrEjected(id)
    clk.cancel("MDR_GAP")
  }

  def mdrInserted : Boolean = mdr != null

  override def reset: Unit = {
    if (mdr != null) mdr.reset
    gapWriting = false
  }

  override def init: Unit = {}

  private def writeGAP(cycles:Long) : Unit = {
    if (gapWriting) {
      mdr.writeGap
      clk.schedule(new ClockEvent("MDR_GAP", cycles + transferCycles, writeGAP _))
    }
  }

  def writeControl(value:Int) : Unit = {
    if (mdr != null) {
      val EW = (value >> 2) & 3
      EW match {
        case 1 if !gapWriting => // ERASE = ON, WRITE = OFF
          gapWriting = true
          clk.schedule(new ClockEvent("MDR_GAP",clk.currentCycles + transferCycles,writeGAP _))
        case 0|3 if gapWriting => // ERASE = ON, WRITE = ON or ERASE = OFF, WRITE = OFF
          gapWriting = false
          clk.cancel("MDR_GAP")
        case _ =>
      }
    }
  }

  def writeData(data:Int) : Unit = {
    if (mdr != null) mdr.write(data)
  }

  def readData : Int = {
    if (mdr == null) return 0xFF
    mdr.read
  }

  def readStatus : Int = {
    if (gapWriting) return 0xFB

    if (mdr == null) 0xF5 else mdr.readStatus
  }
}
