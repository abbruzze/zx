package ucesoft.zx.spectrum

import ucesoft.zx.format.MDR
import ucesoft.zx.rs232.RS232
import ucesoft.zx.{ZXComponent, ZXComponentType}

class IF_I extends ZXComponent with MicrodriveListener {
  override val componentID: String = "Interface I"
  override val componentType: ZXComponentType.Type = ZXComponentType.INTERFACE

  private final val CTS_MASK = 0xEF
  private final val DTR_MASK = 0xF7
  private final val TX_MASK = 0x7F
  private final val RX_MASK = 0x01

  private[this] var comms_data = 0

  private[this] var sr = 0
  private[this] var selectedDrive = 0
  private[this] var driveListeners : List[MicrodriveListener] = Nil
  val drives = (for(id <- 1 to 8) yield new Microdrive(id,this)).toArray
  val rs232 : RS232 = new RS232

  def addDriveListener(dl:MicrodriveListener) : Unit = driveListeners ::= dl

  override def selectedDrive(drive: Int): Unit = for(l <- driveListeners) l.selectedDrive(drive)
  override def posChanged(pos: Int): Unit = for(l <- driveListeners) l.posChanged(pos)
  override def mdrInserted(mdr: MDR,driveID:Int): Unit = for(l <- driveListeners) l.mdrInserted(mdr,driveID)
  override def mdrEjected(driveID:Int): Unit = for(l <- driveListeners) l.mdrEjected(driveID)

  override def reset: Unit = {
    sr = 0
    selectedDrive = 0
  }
  override def init: Unit = {
    add(rs232)
    for(d <- drives) add(d)
  }

  // MDR Port ==================================================
  def writeMDRData(value:Int) : Unit = {
    if (selectedDrive > 0) drives(selectedDrive - 1).writeData(value)
  }
  def readMDRData : Int = {
    if (selectedDrive == 0) 0xFF
    else drives(selectedDrive - 1).readData
  }
  // Control Port ==============================================
  def writeControlPort(value:Int) : Unit = {
    comms_data = value & 1
    if ((value & 2) == 0) {
      val bit = 1 - comms_data
      sr = (sr << 1) | bit
      selectDrive(sr & 0xFF)
    }
    if (selectedDrive > 0) drives(selectedDrive - 1).writeControl(value)
    rs232.cts((value & CTS_MASK) == CTS_MASK)
  }
  def readControlPort : Int = {
    var status = 0xFF
    if (selectedDrive > 0) status = drives(selectedDrive - 1).readStatus
    if (!rs232.dtr) status &= DTR_MASK
    status
  }
  // RS232 Port ================================================
  def writeRS232Port(value:Int) : Unit = {
    if (comms_data == 1) rs232.rxData((value & RX_MASK) == 0)
  }

  def readRS232Port : Int = {
    var value = 0xFF
    if (rs232.txData) value &= TX_MASK

    value
  }

  protected def selectDrive(driveMask:Int) : Unit = {
    val drive = driveMask match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 4 => 3
      case 8 => 4
      case 16 => 5
      case 32 => 6
      case 64 => 7
      case 128 => 8
    }
    if (drive != selectedDrive && selectedDrive != 0) drives(selectedDrive - 1).stopMotor
    selectedDrive = drive
    selectedDrive(selectedDrive)
  }

  def transferCycles : Int = drives(0).transferCycles
}
