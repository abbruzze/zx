package ucesoft.zx.format

import ucesoft.zx.Model
import ucesoft.zx.cpu.{BankedMemory, Z80}
import ucesoft.zx.gpu.ULA
import ucesoft.zx.spectrum.Spectrum

class Z80Format extends SnapshotFileFormat {
  val extension = ".z80"
  def apply(bs:Array[Int],spectrum:Spectrum) : SnapshotResult = {
    restoreZ80State(bs,spectrum.z80,spectrum.ula)
    val compressed = (bs(12) & 0x20) > 0
    val v2 = bs(6) == 0 && bs(7) == 0
    var hwModel : Model.Value = Model._48K
    var ayEnabled = false
    var ifI = false

    if (v2) {
      ayEnabled = (bs(37) & 4) > 0
      ifI = bs(36) == 0xFF
      hwModel = decodeHWModel(bs(34),(bs(30) | bs(31) << 8) != 23)

      spectrum.mmu.modelChanged(hwModel,true)
      if (hwModel == Model._128K) {
        spectrum.mmu.set7FFDRegistry(bs(35))
      }

      spectrum.ay.reset
      for(i <- 39 until (39 + 16)) {
        spectrum.ay.setRegister(i)
        spectrum.ay.setData(bs(i))
      }
      spectrum.ay.setRegister(bs(38))

      spectrum.z80.ctx.PC = bs(32) | bs(33) << 8

      var ptr = 0
      while (ptr < bs.length) {
        ptr = restoreMemory(ptr,true,true,bs,spectrum.mmu,hwModel)
      }
    }
    else {
      spectrum.mmu.modelChanged(hwModel,true)
      restoreMemory(0,compressed,false,bs,spectrum.mmu,hwModel)
    }

    SnapshotResult(hwModel,ifI,ayEnabled)
  }

  private def decodeHWModel(hw:Int,v3:Boolean) : Model.Value = {
    if (v3) {
      hw match {
        case 0 => Model._48K
        case 1|5 => throw new IllegalArgumentException("IF1 not supported")
        case 2 => throw new IllegalArgumentException("SamRAM not supported")
        case 3|6 => throw new IllegalArgumentException("M.G.T not supported")
        case 4 => Model._128K
      }
    }
    else {
      hw match {
        case 0 => Model._48K
        case 1|4 => throw new IllegalArgumentException("IF1 not supported")
        case 2 => throw new IllegalArgumentException("SamRAM not supported")
        case 3 => Model._128K
        case _ => throw new IllegalArgumentException("Bad hw model for version 2")
      }
    }
  }

  private def restoreMemory(oldDataPtr:Int,compressed:Boolean,v2:Boolean,bs:Array[Int],mem:BankedMemory,model:Model.Value) : Int = {
    var dataPtr = oldDataPtr
    var endPtr = 0
    var address = 0
    var bank = -1

    if (v2) {
      var dataLength = 0
      var block = 0
      if (oldDataPtr == 0) {
        val addHeaderLength = 2 + (bs(30) | bs(31) << 8)
        dataLength = bs(addHeaderLength + 30) | bs(addHeaderLength + 31) << 8
        if (dataLength == 0xFFFF) dataLength = 0x4000
        block = bs(addHeaderLength + 32)
        dataPtr = addHeaderLength + 33
      }
      else {
        dataLength = bs(dataPtr) | bs(dataPtr + 1) << 8
        if (dataLength == 0xFFFF) dataLength = 0x4000
        block = bs(dataPtr + 2)
        dataPtr += 3
      }

      endPtr = dataPtr + dataLength
      val (a,b) = block match {
        case 3 =>
          if (model == Model._48K) throw new IllegalArgumentException(s"Block type $block not supported for 48K")
          else (0,0)
        case 4 => if (model == Model._48K) (0x8000,-1) else (0,1)
        case 5 =>  if (model == Model._48K) (0xC000,-1) else (0,2)
        case 6 =>
          if (model == Model._48K) throw new IllegalArgumentException(s"Block type $block not supported for 48K")
          else (0,3)
        case 7 =>
          if (model == Model._48K) throw new IllegalArgumentException(s"Block type $block not supported for 48K")
          else (0,4)
        case 8 => if (model == Model._48K) (0x4000,-1) else (0,5)
        case 9 =>
          if (model == Model._48K) throw new IllegalArgumentException(s"Block type $block not supported for 48K")
          else (0,6)
        case 10 =>
          if (model == Model._48K) throw new IllegalArgumentException(s"Block type $block not supported for 48K")
          else (0,7)
        case _ => throw new IllegalArgumentException(s"Block type $block not supported")
      }
      address = a
      bank = b
    }
    else {
      dataPtr = 30
      address = 0x4000
      endPtr = bs.length - 4 // 00 ED ED 00 block end
    }

    var EDFound = false
    while (dataPtr < endPtr) {
      val b = bs(dataPtr)
      dataPtr += 1
      if (compressed && b == 0xED) {
        if (EDFound) {
          EDFound = false
          var xx = bs(dataPtr) ; dataPtr += 1
          val yy = bs(dataPtr) ; dataPtr += 1
          while (xx > 0 && dataPtr < bs.length) {
            if (bank == -1) mem.load(address,yy) else mem.loadBank(bank,address,yy)
            xx -= 1
            address += 1
          }
        }
        else EDFound = true
      }
      else {
        if (EDFound) {
          if (bank == -1) mem.load(address,0xED) else mem.loadBank(bank,address,0xED)
          address += 1
          EDFound = false
        }
        if (bank == -1) mem.load(address,b) else mem.loadBank(bank,address,b)
        address += 1
      }
    }
    dataPtr
  }

  private def restoreZ80State(bs:Array[Int],z80:Z80,ula:ULA) : Unit = {
    import z80.ctx._

    A = bs(0)
    F = bs(1)
    C = bs(2)
    B = bs(3)
    L = bs(4)
    H = bs(5)
    PC = bs(6) | bs(7) << 8
    SP = bs(8) | bs(9) << 8
    I = bs(10)
    R = bs(11) & 0x7F | (bs(12) & 1) << 7
    ula.setBorderColor((bs(12) >> 1) & 7)
    E = bs(13)
    D = bs(14)
    C1 = bs(15)
    B1 = bs(16)
    E1 = bs(17)
    D1 = bs(18)
    L1 = bs(19)
    H1 = bs(20)
    A1 = bs(21)
    F1 = bs(22)
    IYL = bs(23)
    IYH = bs(24)
    IXL = bs(25)
    IXH = bs(26)
    IFF1 = bs(27) & 1
    IFF2 = bs(28) & 1
    im = bs(29) & 2
    //if (im == 0) im = 1 // TODO
  }
}
