package ucesoft.zx.mmu

import ucesoft.zx.ChipID.ID
import ucesoft.zx.audio.{AY38912, Speaker}
import ucesoft.zx.{ChipID, Clock, Model, ZXComponent, ZXComponentType}
import ucesoft.zx.cpu.{BankedMemory, Z80}
import ucesoft.zx.gpu.{Palette, ULA, ULAPlus}
import ucesoft.zx.joystick.Joystick
import ucesoft.zx.keyboard.Keyboard
import ucesoft.zx.misc.{MouseCage, Preferences}
import ucesoft.zx.spectrum.IF_I
import ucesoft.zx.tape.{TAPE_PLAY, TAPE_STOP, Tape, TapeBlockInfo, TapeListener}

import java.util.Properties

class MMU(keyboard:Keyboard,
          speaker:Speaker,
          ay:AY38912,
          joystick:Joystick,
          tape:Tape)(implicit pref:Preferences) extends ZXComponent with BankedMemory with Z80.IOMemory {
  override val componentID: String = "Main memory"
  override val componentType: ZXComponentType.Type = ZXComponentType.MEMORY
  override val name: String = "Main Memory"

  var tapeFastLoadEnabled = true
  var tapeAutoPlay = true
  var tapeAutostop = false

  var interfaceIEnabled = false
  var mouseEnabled = false
  var mouseActive = false
  val ifI = new IF_I

  private[this] var ay48kEnabled = true
  private[this] var ayEnabled = true
  private[this] val banks : Array[Array[Int]] = Array.ofDim[Int](8,0x4000)
  private[this] val mem : Array[Array[Int]] = Array.ofDim[Array[Int]](4)
  private[this] var model : Model.Value = Model._48K
  private[this] var z80 : Z80 = _
  private[this] var ula : ULA = _
  private[this] val clk = Clock.systemClock
  private[this] var snow = false
  private[this] var snowAllowed = true
  private[this] var snowAddress = 0
  private[this] val bankContention = Array.ofDim[Boolean](4)
  private[this] var rom48KEnabled = true
  private[this] var _7FFDRegistry = 0
  private[this] var _1FFDRegistry = 0
  private[this] var _48kROM = Array.ofDim[Int](16384)
  private[this] var _128kROM = Array.ofDim[Int](2,16384)
  private[this] var _128kPlus2ROM = Array.ofDim[Int](2,16384)
  private[this] var _128kPlus3AROM = Array.ofDim[Int](4,16384)
  private[this] var _128kPlus2AROM = Array.ofDim[Int](4,16384)
  private[this] var INTERFACE_I_ROM = Array.ofDim[Int](8192)
  private[this] var interfaceIROMActive = false
  private[this] var _16K = false
  //private[this] var ayCurrentRegister = 0
  private[this] var lecEnabled = false
  private[this] var lecActive = false
  private[this] var lecBank = 0
  private[this] var lecRAM : Array[Array[Int]] = _

  def setAYOn48KEnabled(enabled:Boolean) : Unit = {
    this.ay48kEnabled = enabled
    if (model == Model._48K || model == Model._48K) ayEnabled = ay48kEnabled
    else ayEnabled = true
  }

  def isAYEnabled : Boolean = ayEnabled

  def setChips(z80:Z80,ula:ULA) : Unit = {
    this.z80 = z80
    this.ula = ula
  }

  def setLecEnabled(enabled:Boolean) : Unit = {
    lecEnabled = enabled
    lecActive = false
    lecBank = 0
    if (enabled) {
      lecRAM = Array.ofDim[Int](32,16384)
      mem(2) = lecRAM(30)
      mem(3) = lecRAM(31)
    }
    else {
      lecRAM = null
      mem(0) = _48kROM
      for (b <- 1 to 3) mem(b) = banks(b - 1)
    }
  }

  def setSnowAllowed(allowed:Boolean) : Unit = snowAllowed = allowed

  override def getProperties: Properties = {
    properties.setProperty("Model",model.toString)
    properties.setProperty("0x7FFD port",_7FFDRegistry.toString)
    properties.setProperty("0x1FFD port",_1FFDRegistry.toString)
    properties.setProperty("AY enabled",ayEnabled.toString)
    properties.setProperty("Tape fastload enabled",tapeFastLoadEnabled.toString)
    properties.setProperty("Tape autoplay enabled",tapeAutoPlay.toString)
    properties.setProperty("Tape autostop enabled",tapeAutostop.toString)
    properties
  }

  // ===================== Chips loop =========================================
  @inline private def loop(cycles:Int) : Unit = {
    var c = cycles
    val warpMode = clk.maximumSpeed
    while (c > 0) {
      ula.clock
      if (!warpMode) {
        speaker.clock(clk.currentCycles)
        if (ayEnabled && (clk.currentCycles & 1) == 0) ay.clock(clk.currentCycles)
      }
      tape.cycle
      clk.tick
      c -= 1
    }
  }
  // ===================== I/O ================================================
  private def ioCheckContended[@specialized(Int,Unit) T](addressHI: Int, addressLO: Int,value:Int,iof : (Int,Int,Int) => T) : T = {
    val hicontSet = bankContention(addressHI >> 6)
    val lowcontSet = (addressLO & 1) != 0
    if (!hicontSet && !lowcontSet) { // N:1, C:3
      loop(1)
      rwcontention
      val in = iof(addressHI,addressLO,value)
      loop(3)
      in
    }
    else
    if (!hicontSet && lowcontSet) { // N:4
      loop(2)
      val in = iof(addressHI,addressLO,value)
      loop(2)
      in
    }
    else
    if (hicontSet && !lowcontSet) { // C:1, C:3
      loop(1)
      rwcontention
      loop(1)
      val in = iof(addressHI,addressLO,value)
      loop(2)
      in
    }
    else { // C:1, C:1, C:1, C:1
      var c = 4
      var in : T = null.asInstanceOf[T]
      while (c > 0) {
        rwcontention
        loop(1)
        if (c == 4) in = iof(addressHI,addressLO,value)
        c -= 1
      }
      in
    }
  }
  override def in(addressHI: Int, addressLO: Int): Int = ioCheckContended(addressHI,addressLO,0,port_in _)
  override def out(addressHI: Int, addressLO: Int, value: Int): Unit = ioCheckContended(addressHI,addressLO,value,port_out _)

  @inline private def port_in(addressHI: Int, addressLO: Int,dummy:Int) : Int = {
    val address = addressHI << 8 | addressLO
    val ulaPort = (address & 1) == 0
    // AY ========================================================================
    if (ayEnabled && (address & 0xC002) == 0xC000) {
      /*
      if (ayCurrentRegister == 14 && model == Model._128K) { // AY I/O Port A
        var rs232 = 0xFF
        if (_128RS232.dtr) rs232 &= 0xBF
        if (!_128RS232.txData) rs232 &= 0x7F
        return rs232
      }
      else
       */
      return ay.getData
    }
    // ULA =======================================================================
    if (ulaPort) {
      speaker.setOn(tape.ear > 0)
      val port = if (tape.isPlaying) (keyboard.select(addressHI) & 0x1F) | tape.ear | 0xA0 else (keyboard.select(addressHI) & 0x1F) | 0xA0 | (if (speaker.isMic && (model != Model._128K_PLUS3 && model != Model._128K_PLUS2A)) 0x40 else 0)
      return port
    }
    // JOYSTICK ===================================================================
    if (addressLO == joystick.port) return joystick.getData
    // MOUSE ======================================================================
    if (mouseEnabled && mouseActive) {
      val maddress = address >> 5
      if ((maddress & 0x38) == 0x38) return 0xFF - (MouseCage.y & 0xFF)
      if ((maddress & 0x18) == 0x18) return MouseCage.x & 0xFF
      if ((maddress & 0x10) == 0x10) {
        val bt = MouseCage.buttonPressed
        var mask = 0xFF
        if ((bt & MouseCage.RIGHT_BUTTON) > 0) mask &= 0xFE
        if ((bt & MouseCage.LEFT_BUTTON) > 0) mask &= 0xFD
        if ((bt & MouseCage.MIDDLE_BUTTON) > 0) mask &= 0xFB
        return mask
      }
    }
    // IFI =======================================================================
    if (interfaceIEnabled && (model != Model._128K_PLUS3 && model != Model._128K_PLUS2A)) {
      (addressLO >> 3) & 3 match {
        case 1 /*0xEF*/ => // CONTROL PORT
          return ifI.readControlPort
        case 0 /*0xE7*/ => // DATA PORT
          val data = ifI.readMDRData
          loop(ifI.transferCycles)
          return data
        case 2 /*0xF7*/ => // RS232 PORT
          return ifI.readRS232Port
        case _ =>
      }
    }
    // ULA+ =======================================================================
    if (ULAPlus.enabled && (address & 0x4004) == 0x4000) {
      return ULAPlus.readDataPort
    }
    // FLOATING BUS ===============================================================
    if (model == Model._128K_PLUS3 || model == Model._128K_PLUS2A) return 0xFF

    val bank = address >> 14
    if (bankContention(bank)) ula.lastFloatingBus else ula.floatingBus
  }

  @inline private def port_out(addressHI: Int, addressLO: Int,value:Int) : Unit = {
    val address = addressHI << 8 | addressLO
    val ulaPort = (address & 1) == 0
    // LEC =======================================================================
    if (lecEnabled && (address & 2) == 0) {
      lecActive = (value & 0x80) > 0
      lecBank = (value & 0x8) | (value >> 4) & 7
      //println(s"LECBANK=$lecBank active=$lecActive")
      setLecMemory
      return
    }
    // AY ========================================================================
    if (ayEnabled && (address & 0x8002) == 0x8000) {
      if ((address & 0x4000) != 0) {
        //ayCurrentRegister = value
        ay.setRegister(value)
      }
      else {
        /*
        if (ayCurrentRegister == 14 && model == Model._128K) { // AY I/O Port A
          _128RS232.rxData((value & 0x8) == 0x0)
          _128RS232.cts((value & 0x4) == 0x4)
        }
        else*/
        ay.setData(value)
      }
      return
    }
    // 128 & 128+X ===============================================================
    if (((address & 0x8002) == 0 && (model == Model._128K || model == Model._128K_PLUS2)) ||
        (address & 0xC002) == 0x4000 && (model == Model._128K_PLUS3 || model == Model._128K_PLUS2A)) {
      // 128K: Bit 5: If set, memory paging will be disabled and further output to this port will be ignored until the computer is reset.
      if ((_7FFDRegistry & 0x20) > 0) return

      set7FFDRegistry(value)
    }
    else if ((address & 0xF002) == 0x1000 && (model == Model._128K_PLUS3 || model == Model._128K_PLUS2A)) set1FFDRegister(value)
    // ULA =======================================================================
    if (ulaPort) {
      ula.setBorderColor(value & 7)
      speaker.setOn((value & 0x10) > 0)
      speaker.setVolume((value >> 3) & 3)
      speaker.setMic((value & 0x8) > 0)
      tape.setEAR((value & 0x8) > 0)

      return
    }
    // IFI =======================================================================
    if (interfaceIEnabled && (model != Model._128K_PLUS3 && model != Model._128K_PLUS2A)) {
      (addressLO >> 3) & 3 match {
        case 1 /*0xEF*/ => // CONTROL PORT
          //if ((value & 2) == 0) println(s"CLK ${1-(value & 1)} r/w=${(value >> 2) & 1} erase)${(value >> 3) & 1}")
          ifI.writeControlPort(value)
          return
        case 0 /*0xE7*/ => // DATA PORT
          ifI.writeMDRData(value)
          loop(ifI.transferCycles)
          return
        case 2 /*0xF7*/ => // RS232 PORT
          ifI.writeRS232Port(value)
          return
        case _ =>
      }
    }
    // ULA+ ======================================================================
    if (ULAPlus.enabled && (address & 0x0004) == 0) {
      if ((address & 0x4000) == 0) ULAPlus.writeRegisterPort(value)
      else {
        val lastActive = ULAPlus.active
        ULAPlus.writeDataPort(value)
        val active = ULAPlus.active
        if (lastActive != active) {
          if (active) ula.setPalette(ULAPlus.PALETTE)
          else ula.setPalette(Palette.DEFAULT)
        }
      }
      return
    }
  }

  final def set1FFDRegister(value:Int) : Unit = {
    _1FFDRegistry = value
    setPlusXAMemory
  }

  final def set7FFDRegistry(value:Int) : Unit = {
    _7FFDRegistry = value

    model match {
      case Model._128K_PLUS3 | Model._128K_PLUS2A =>
        setPlusXAMemory
      case _ =>
        // 128K: Bits 0-2: RAM page (0-7) to map into memory at 0xc000.
        val bank = value & 7
        mem(3) = banks(bank)

        bankContention(3) = bank == 1 || bank == 3 || bank == 5 || bank == 7
        // 128K: Bit 4: ROM select. ROM 0 is the 128k editor and menu system; ROM 1 contains 48K BASIC
        val rom0 = (value >> 4) & 1
        mem(0) = model match {
          case Model._128K =>
            _128kROM(rom0)
          case Model._128K_PLUS2 =>
            _128kPlus2ROM(rom0)
        }
        rom48KEnabled = rom0 != 0
    }
  }

  private def setPlusXAMemory : Unit = {
    val specialMode = (_1FFDRegistry & 1) == 1

    if (specialMode) {
      rom48KEnabled = false
      val mode = (_1FFDRegistry >> 1) & 3
      mode match {
        case 0 =>
          mem(0) = banks(0)
          mem(1) = banks(1)
          mem(2) = banks(2)
          mem(3) = banks(3)
          java.util.Arrays.fill(bankContention, false)
        case 1 =>
          mem(0) = banks(4)
          mem(1) = banks(5)
          mem(2) = banks(6)
          mem(3) = banks(7)
          java.util.Arrays.fill(bankContention, true)
        case 2 =>
          mem(0) = banks(4)
          mem(1) = banks(5)
          mem(2) = banks(6)
          mem(3) = banks(3)
          bankContention(0) = true
          bankContention(1) = true
          bankContention(2) = true
          bankContention(3) = false
        case 3 =>
          mem(0) = banks(4)
          mem(1) = banks(7)
          mem(2) = banks(6)
          mem(3) = banks(3)
          bankContention(0) = true
          bankContention(1) = true
          bankContention(2) = true
          bankContention(3) = false
      }
    }
    else { // normal mode
      val bank = _7FFDRegistry & 7
      mem(1) = banks(5)
      mem(2) = banks(2)
      mem(3) = banks(bank)
      bankContention(0) = false
      bankContention(1) = false
      bankContention(2) = false
      bankContention(3) = bank == 1 || bank == 3 || bank == 5 || bank == 7

      val rom0 = ((_7FFDRegistry >> 4) & 1) | ((_1FFDRegistry >> 1) & 2)
      mem(0) = model match {
        case Model._128K_PLUS2A =>
          _128kPlus2AROM(rom0)
        case Model._128K_PLUS3 =>
          _128kPlus3AROM(rom0)
      }
      rom48KEnabled = rom0 == 3 //TODO : check
    }
  }

  final def get7FFDRegistry : Int = _7FFDRegistry

  override def internalOperation(cycles:Int,address:Int = 0) : Unit = {
    if (address != 0) {
      val bank = address >> 14
      val contended = bankContention(bank)
      var c = cycles
      while (c > 0) {
        if (contended && ula.isContended) rwcontention
        loop(1)
        c -= 1
      }
    }
    else loop(cycles)
  }

  // ===================== R/W ================================================
  override def peekBank(bank: Int, address: Int): Int = banks(bank)(address)
  override def loadBank(bank: Int, address: Int, value: Int): Unit = banks(bank)(address) = value

  @inline private def rwcontention : Unit = {
    while (ula.isContended) {
      loop(1)
      z80.ctx.setAdditionalClockCycles(1)
    }
  }

  @inline private def readULA(_address:Int) : Int = {
    // 128K: Bit 3: Select normal (0) or shadow (1) screen to be displayed. The normal screen is in bank 5, whilst the shadow screen is in bank 7. Note that this does not affect the memory between 0x4000 and 0x7fff, which is always bank 5
    val bank = model match {
      case Model._48K|Model._16K =>
        0
      case _ =>
        if ((_7FFDRegistry & 8) == 0) 5 else 7
    }

    var address = _address
    if (snow) address = address & 0xFF00 | snowAddress & 0xFF
    banks(bank)(address & 0x3FFF)
  }

  @inline private def readCPU(address:Int) : Int = {
    val z80Pins = z80.pins
    val refresh = (z80Pins & 2) > 0
    val m1Fetch = !refresh && (z80Pins & 1) > 0
    val dummyRead = (z80Pins & 4) > 0

    val cycles = if (dummyRead) 0 else if (m1Fetch || refresh) 2 else 3
    //println(s"READ $cycles M1=${z80.isM1Fetch} R=${z80.isRefresh} dummy=${z80.isDummyRead}")
    val bank = address >> 14
    // SPECTRUM 16K
    if (_16K && bank > 1) return 0xFF

    val contended = !dummyRead && bankContention(bank)
    snow = false
    if (contended && !refresh && ula.isContended) rwcontention
    else
    if (snowAllowed && contended && refresh) {
      snow = true
      snowAddress = address
    }

    if (cycles > 0) loop(cycles)

    if (address == 0x556 && rom48KEnabled && m1Fetch) { // LOAD routine
      if (tapeAutoPlay) tape.setState(TAPE_PLAY)
      if (tapeFastLoadEnabled && tape.fastLoad(this,z80)) return 201
    }

    if (interfaceIEnabled && m1Fetch && !lecActive && (model != Model._128K_PLUS3 && model != Model._128K_PLUS2A)) {
      address match {
        case 0x0008|0x1708 if !interfaceIROMActive =>
          interfaceIROMActive = true
          rom48KEnabled = false
        case 0x700 if interfaceIROMActive =>
          interfaceIROMActive = false
          rom48KEnabled = (model == Model._48K || model == Model._16K) || ((_7FFDRegistry >> 4) & 1) != 0
          return INTERFACE_I_ROM(0x700)
        case _ =>
      }
    }

    if (interfaceIROMActive && address < 0x4000) {
      return INTERFACE_I_ROM(address & 0x1FFF)
    }

    mem(bank)(address & 0x3FFF)
  }

  override final def read(address: Int, chipID: ID): Int = {
    chipID match {
      case ChipID.CPU =>
        readCPU(address)
      case ChipID.ULA =>
        readULA(address)
    }
  }

  override final def peek(address: Int, chipID: ID): Int = mem(address >> 14)(address & 0x3FFF)

  override final def write(address: Int, value: Int, chipID: ID): Unit = {
    val bank = address >> 14
    // SPECTRUM 16K
    if (_16K && bank > 1) return

    val contended = bankContention(bank)
    if (contended && ula.isContended) rwcontention

    loop(3)

    if (bank > 0 || (_1FFDRegistry & 1) == 1 || lecActive) mem(bank)(address & 0x3FFF) = value
  }

  override final def load(address: Int, value: Int, chipID: ID): Unit = mem(address >> 14)(address & 0x3FFF) = value

  // ==========================================================================

  private def setLecMemory : Unit = {
    if (lecActive) {
      val bank = lecBank << 1
      mem(0) = lecRAM(bank)
      mem(1) = lecRAM(bank + 1)
      rom48KEnabled = false
    }
    else {
      mem(0) = _48kROM
      mem(1) = banks(0)
      rom48KEnabled = true
    }
  }

  override def modelChanged(model: Model.Value,resetRequest:Boolean): Unit = {
    this.model = model
    java.util.Arrays.fill(bankContention, false)
    bankContention(1) = true

    this.model match {
      case Model._16K =>
        mem(0) = _48kROM
        for (b <- 1 to 3) mem(b) = banks(b - 1)
        rom48KEnabled = true
        snowAllowed = true
        setAYOn48KEnabled(ay48kEnabled)
      case Model._48K =>
        if (lecEnabled) setLecMemory
        else {
          mem(0) = _48kROM
          for (b <- 1 to 3) mem(b) = banks(b - 1)
          rom48KEnabled = true
        }
        snowAllowed = true
        setAYOn48KEnabled(ay48kEnabled)
      case Model._128K =>
        //rom48KEnabled = false
        mem(0) = _128kROM((_7FFDRegistry >> 4) & 1)
        mem(1) = banks(5)
        mem(2) = banks(2)
        mem(3) = banks(0)
        snowAllowed = true
        setAYOn48KEnabled(false)
      case Model._128K_PLUS2 =>
        mem(0) = _128kPlus2ROM((_7FFDRegistry >> 4) & 1)
        mem(1) = banks(5)
        mem(2) = banks(2)
        mem(3) = banks(0)
        snowAllowed = true
        setAYOn48KEnabled(false)
      case Model._128K_PLUS3 | Model._128K_PLUS2A =>
        setPlusXAMemory
        snowAllowed = false
        setAYOn48KEnabled(false)
    }

    _16K = model == Model._16K
  }

  override def init : Unit = {
    _48kROM = ROMs.get48KROM
    _128kROM = ROMs.get128KROMs
    _128kPlus2ROM = ROMs.get128KPlus2ROMs
    _128kPlus3AROM = ROMs.get128KPlus3ROMs
    _128kPlus2AROM = ROMs.get128KPlus2AROMs
    INTERFACE_I_ROM = ROMs.getIFIROM

    add(ifI)

    reset
    tape.addTapeListener(new TapeListener {
      override def blockChanged(block: TapeBlockInfo): Unit = {
        if (tapeAutostop) tape.setState(TAPE_STOP)
      }
    })
  }

  override def hardReset: Unit = {
    for(b <- banks) java.util.Arrays.fill(b,0)
    interfaceIROMActive = false

    _7FFDRegistry = 0
    _1FFDRegistry = 0

    modelChanged(model,false)

    snow = false
  }

  override def reset: Unit = {
    for(b <- banks) java.util.Arrays.fill(b,0)
    interfaceIROMActive = false

    if ((_7FFDRegistry & 0x20) == 0) _7FFDRegistry = 0
    _1FFDRegistry = 0

   modelChanged(model,false)

    snow = false
  }
}
