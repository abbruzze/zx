package ucesoft.zx.cpu

import ucesoft.zx.ChipID.ID
import ucesoft.zx.trace.{BreakType, CpuStepInfo, NoBreak, TraceListener}
import ucesoft.zx.{Chip, ChipID, Log}

import scala.language.implicitConversions
import java.io.PrintWriter
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.util.Properties

object Z80 {
  @inline private def hex2(data: Int) = "%02X".format(data & 0xffff)
  @inline private def hex4(data: Int) = "%04X".format(data & 0xffff)
  @inline private def WORD(h:Int,l:Int) = ((h << 8) | l) & 0xFFFF

  trait IOMemory {
    def in(addressHI:Int,addressLO:Int) : Int
    def out(addressHI:Int,addressLO:Int,value:Int) : Unit
    def internalOperation(cycles:Int,address:Int = 0) : Unit = {}
  }

  object EmptyIOMemory extends IOMemory {
    def in(addressHI:Int,addressLO:Int) = 0
    def out(addressHI:Int,addressLO:Int,value:Int) : Unit = {}
  }

  class Context(val mem:Memory,val io:IOMemory) {
    var A1,F1,H1,L1,D1,E1,B1,C1 = 0
    var halted = false
    var im = 0
    var A = 0
    var B = 0
    var C = 0
    var D = 0
    var E = 0
    var F = 0
    var H = 0
    var L = 0
    var I = 0
    var R = 0
    var IX = 0
    var IY = 0
    var IFF1 = 0
    var IFF2 = 0
    var PC = 0
    var SP = 0
    var memptr = 0xFFFF
    var Q,lastQ = false
    private[this] var delayInt = false
    private[this] var additionalClockCycles = 0
    var isIndexX = true
    var lastWrite = 0

    final def copyQ : Unit = {
      lastQ = Q
      Q = false
    }

    // state
    def saveState(out:ObjectOutputStream) : Unit = {
      out.writeInt(im)
      out.writeInt(A1)
      out.writeInt(B1)
      out.writeInt(C1)
      out.writeInt(D1)
      out.writeInt(E1)
      out.writeInt(F1)
      out.writeInt(H1)
      out.writeInt(L1)
      out.writeInt(A)
      out.writeInt(B)
      out.writeInt(C)
      out.writeInt(D)
      out.writeInt(E)
      out.writeInt(F)
      out.writeInt(H)
      out.writeInt(L)
      out.writeInt(I)
      out.writeInt(R)
      out.writeInt(IX)
      out.writeInt(IY)
      out.writeInt(IFF1)
      out.writeInt(IFF2)
      out.writeInt(PC)
      out.writeInt(SP)
      out.writeBoolean(halted)
      out.writeBoolean(delayInt)
      out.writeInt(additionalClockCycles)
      out.writeInt(memptr)
      out.writeBoolean(Q)
    }
    def loadState(in:ObjectInputStream) : Unit = {
      im = in.readInt
      A1 = in.readInt
      B1 = in.readInt
      C1 = in.readInt
      D1 = in.readInt
      E1 = in.readInt
      F1 = in.readInt
      H1 = in.readInt
      L1 = in.readInt
      A = in.readInt
      B = in.readInt
      C = in.readInt
      D = in.readInt
      E = in.readInt
      F = in.readInt
      H = in.readInt
      L = in.readInt
      I = in.readInt
      R = in.readInt
      IX = in.readInt
      IY = in.readInt
      IFF1 = in.readInt
      IFF2 = in.readInt
      PC = in.readInt
      SP = in.readInt
      halted = in.readBoolean
      delayInt = in.readBoolean
      additionalClockCycles = in.readInt
      memptr = in.readInt
      Q = in.readBoolean
    }

    final def setAdditionalClockCycles(acs:Int) : Unit = additionalClockCycles += acs
    final def getAdditionalClockSycles : Int = {
      val acs = additionalClockCycles
      additionalClockCycles = 0
      acs
    }

    final def mustDelayInt : Boolean = delayInt
    final def setDelayInt(value:Boolean) : Unit = delayInt = value

    /*
    | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    |---|---|---|---|---|---|---|---|
    | S | Z | Y | H | X |P/V| N | C |
    |---|---|---|---|---|---|---|---|
    */
    final val CFLAG = 0x1
    final val NFLAG = 0x2
    final val PFLAG = 0x4
    final val XFLAG = 0x8
    final val HFLAG = 0x10
    final val YFLAG = 0x20
    final val ZFLAG = 0x40
    final val SFLAG = 0x80

    // ========================================= FLAGS =========================================================
    // pre calculated Sign, Zero and Parity flags for 0-255 values
    final val SZP : Array[Int] = {
      val szip = Array.ofDim[Int](0x100)
      for(i <- 0 to 0xFF) {
        if ((i & 0x80) > 0) szip(i) |= SFLAG
        if (i == 0) szip(i) |= ZFLAG
        var p = 0
        var j = i
        for(_ <- 0 until 8) {
          p ^= j & 0x1
          j >>= 1
        }
        szip(i) |= (if (p != 0) 0 else PFLAG)
      }
      szip
    }

    final def zero: Int = F & ZFLAG
    final def sign: Int = F & SFLAG
    final def carry: Int = F & CFLAG
    final def negative: Int = F & NFLAG
    final def parity: Int = F & PFLAG
    final def half: Int = F & HFLAG
    final def xf : Int = F & XFLAG
    final def yf : Int = F & YFLAG

    final def setZero(set:Boolean): Unit = if (set) F |= ZFLAG else F &= ~ZFLAG
    final def setSign(set:Boolean): Unit = if (set) F |= SFLAG else F &= ~SFLAG
    final def setCarry(set:Boolean): Unit = if (set) F |= CFLAG else F &= ~CFLAG
    final def setNegative(set:Boolean): Unit = if (set) F |= NFLAG else F &= ~NFLAG
    final def setParity(set:Boolean): Unit = if (set) F |= PFLAG else F &= ~PFLAG
    final def setHalf(set:Boolean): Unit = if (set) F |= HFLAG else F &= ~HFLAG
    final def setXY(result:Int) : Unit = F = (F & 0xD7) | result & 0x28

    // =========================================================================================================

    final def incPC(value:Int = 1) : Unit = PC = (PC + value) & 0xFFFF

    final def AF1: Int = WORD(A1,F1)
    final def HL1: Int = WORD(H1,L1)
    final def DE1: Int = WORD(D1,E1)
    final def BC1: Int = WORD(B1,C1)

    final def IR: Int = WORD(I,R)
    final def AF: Int = WORD(A,F)
    final def HL: Int = WORD(H,L)
    final def BC: Int = WORD(B,C)
    final def DE: Int = WORD(D,E)
    final def AF_=(w:Int): Unit = {
      A = (w >> 8) & 0xFF
      F = w & 0xFF
    }
    final def HL_=(w:Int): Unit = {
      H = (w >> 8) & 0xFF
      L = w & 0xFF
    }
    final def BC_=(w:Int) : Unit = {
      B = (w >> 8) & 0xFF
      C = w & 0xFF
    }
    final def DE_=(w:Int) : Unit = {
      D = (w >> 8) & 0xFF
      E = w & 0xFF
    }
    final def AF1_=(w:Int): Unit = {
      A1 = (w >> 8) & 0xFF
      F1 = w & 0xFF
    }
    final def HL1_=(w:Int): Unit = {
      H1 = (w >> 8) & 0xFF
      L1 = w & 0xFF
    }
    final def BC1_=(w:Int) : Unit = {
      B1 = (w >> 8) & 0xFF
      C1 = w & 0xFF
    }
    final def DE1_=(w:Int) : Unit = {
      D1 = (w >> 8) & 0xFF
      E1 = w & 0xFF
    }
    final def incDecBC(inc:Boolean) : Unit = {
      if (inc) {
        C += 1
        if (C == 0x100) {
          C = 0
          B = (B + 1) & 0xFF
        }
      }
      else {
        C -= 1
        if (C == -1) {
          C = 0xFF
          B = (B - 1) & 0xFF
        }
      }
    }
    final def incDecDE(inc:Boolean) : Unit = {
      if (inc) {
        E += 1
        if (E == 0x100) {
          E = 0
          D = (D + 1) & 0xFF
        }
      }
      else {
        E -= 1
        if (E == -1) {
          E = 0xFF
          D = (D - 1) & 0xFF
        }
      }
    }
    final def incDecHL(inc:Boolean) : Unit = {
      if (inc) {
        L += 1
        if (L == 0x100) {
          L = 0
          H = (H + 1) & 0xFF
        }
      }
      else {
        L -= 1
        if (L == -1) {
          L = 0xFF
          H = (H - 1) & 0xFF
        }
      }
    }

    final def incDecSP(inc:Boolean) : Unit = if (inc) SP = (SP + 1) & 0xFFFF else SP = (SP - 1) & 0xFFFF
    @inline private def incDecIX(inc:Boolean) : Unit = if (inc) IX = (IX + 1) & 0xFFFF else IX = (IX - 1) & 0xFFFF
    @inline private def incDecIY(inc:Boolean) : Unit = if (inc) IY = (IY + 1) & 0xFFFF else IY = (IY - 1) & 0xFFFF

    final def incDecIndex(inc:Boolean) : Unit = if (isIndexX) incDecIX(inc) else incDecIY(inc)

    @inline private def IX_+(d:Int): Int = (IX + d.asInstanceOf[Byte]) & 0xFFFF
    @inline private def IY_+(d:Int): Int = (IY + d.asInstanceOf[Byte]) & 0xFFFF

    final def INDEX_+(d:Int,iop:Boolean = true): Int = {
      if (iop) io.internalOperation(5,PC)
      memptr = if (isIndexX) IX_+(d) else IY_+(d)
      memptr
    }
    final def INDEX : Int = if (isIndexX) IX else IY
    final def INDEX_=(value:Int) : Unit = if (isIndexX) IX = value else IY = value

    final def EX_SP_IX : Unit = {
      val tmp = readW(SP)
      io.internalOperation(1,(SP + 1) & 0xFFFF)
      writeW(SP,INDEX)
      io.internalOperation(2,SP)
      INDEX = tmp
      memptr = tmp
    }

    final def EX_SP_HL : Unit = {
      val tmp = readW(SP)
      io.internalOperation(1,(SP + 1) & 0xFFFF)
      writeW(SP,HL)
      io.internalOperation(2,SP)
      HL = tmp
      memptr = tmp
    }

    final def EX_AF  : Unit = {
      var tmp = A
      A = A1
      A1 = tmp
      tmp = F
      F = F1
      F1 = tmp
    }

    final def EX_DE_HL  : Unit = {
      var tmp = D
      D = H
      H = tmp
      tmp = E
      E = L
      L = tmp
    }

    final def EXX  : Unit = {
      // BC <=> BC'
      var tmp = B
      B = B1
      B1 = tmp
      tmp = C
      C = C1
      C1 = tmp
      // DE <=> DE'
      tmp = D
      D = D1
      D1 = tmp
      tmp = E
      E = E1
      E1 = tmp
      // HL <=> HL'
      tmp = H
      H = H1
      H1 = tmp
      tmp = L
      L = L1
      L1 = tmp
    }

    final def IXL: Int = IX & 0xFF
    final def IXH : Int = (IX >> 8) & 0xFF
    final def IYL: Int = IY & 0xFF
    final def IYH : Int = (IY >> 8) & 0xFF

    final def INDEX_L : Int = if (isIndexX) IXL else IYL
    final def INDEX_H : Int = if (isIndexX) IXH else IYH

    final def INDEX_L_=(value:Int) : Unit = if (isIndexX) IXL = value else IYL = value
    final def INDEX_H_=(value:Int) : Unit = if (isIndexX) IXH = value else IYH = value

    final def IXH_=(value:Int) : Unit = IX = ((value & 0xFF) << 8) | IX & 0xFF
    final def IXL_=(value:Int) : Unit = IX = IX & 0xFF00 | value & 0xFF
    final def IYH_=(value:Int) : Unit = IY = ((value & 0xFF) << 8) | IY & 0xFF
    final def IYL_=(value:Int) : Unit = IY = IY & 0xFF00 | value & 0xFF

    final def push(w:Int) : Unit = {
      SP = (SP - 1) & 0xFFFF
      mem.write(SP,(w >> 8) & 0xFF,ChipID.CPU)
      SP = (SP - 1) & 0xFFFF
      mem.write(SP,w & 0xFF,ChipID.CPU)
    }

    final def pop: Int = {
      var popped = mem.read(SP)
      SP = (SP + 1) & 0xFFFF
      popped |= mem.read(SP) << 8
      SP = (SP + 1) & 0xFFFF
      popped
    }

    final def byte(offset:Int): Int = mem.read((PC + offset) & 0xFFFF)
    final def word(offset:Int): Int = mem.read((PC + offset + 1) & 0xFFFF) << 8 | mem.read((PC + offset) & 0xFFFF)

    final def read(address:Int,iop:Int = 0): Int = {
      val r = mem.read(address)
      if (iop > 0) io.internalOperation(iop,address)
      r
    }
    final def readW(address:Int): Int = mem.read(address) | mem.read((address + 1) & 0xFFFF) << 8
    final def write(address:Int,value:Int): Unit = {
      mem.write(address,value,ChipID.CPU)
      lastWrite = value
    }
    final def writeW(address:Int,value:Int): Unit = {
      mem.write(address,value & 0xFF,ChipID.CPU)
      mem.write((address + 1) & 0xFFFF,(value >> 8) & 0xFF,ChipID.CPU)
    }

    final def reset  : Unit = {
      AF = 0xFFFF
      SP = 0xFFFF
      PC = 0
      IFF1 = 0
      IFF2 = 0
      im = 0
      halted = false
      IX = 0
      IY = 0
      BC = 0
      DE = 0
      HL = 0
      AF1 = 0
      BC1 = 0
      DE1 = 0
      HL1 = 0
      I = 0
      R = 0

      isIndexX = true
      memptr = 0xFFFF
    }

    override def toString = s"PC=${hex4(PC)} AF=${hex4(AF)} BC=${hex4(BC)} DE=${hex4(DE)} HL=${hex4(HL)} IX=${hex4(IX)} IY=${hex4(IY)} I=${hex2(I)} im=$im SP=${hex2(SP)} SZYHXPNC=${sr2String}"
    @inline private def sr2String = {
      val sb = new StringBuilder
      if (sign > 0) sb += 'S' else sb += '-'
      if (zero > 0) sb += 'Z' else sb += '-'
      if (yf > 0) sb += 'Y' else sb += '-'
      if (half > 0) sb += 'H' else sb += '-'
      if (xf > 0) sb += 'X' else sb += '-'
      if (parity > 0) sb += 'P' else sb += '-'
      if (carry > 0) sb += 'C' else sb += '-'
      sb.toString
    }

    // =============================================================
    final def add(value: Int): Unit = {
      val tmp = (A + value) & 0xFF
      F = SZP(tmp) ; Q = true
      setCarry(((A + value) & 0x100) > 0)
      setHalf(((A ^ value ^ tmp) & 0x10) > 0)
      setParity(((~(A ^ value)) & (A ^ tmp) & 0x80) > 0)
      A = tmp
      setXY(tmp)
    }

    final def adc(value: Int): Unit = {
      val oldCarry = carry
      val tmp = (A + value + oldCarry) & 0xFF
      F = SZP(tmp) ; Q = true
      setCarry(((A + value + oldCarry) & 0x100) > 0)
      setHalf(((A ^ value ^ tmp) & 0x10) > 0)
      setParity(((~(A ^ value)) & (A ^ tmp) & 0x80) > 0)
      A = tmp
      setXY(tmp)
    }

    final def sub(value: Int): Unit = {
      val tmp = (A - value) & 0xFF
      F = SZP(tmp) ; Q = true
      setNegative(true)
      setCarry(value > A)
      setHalf(((A ^ value ^ tmp) & 0x10) > 0)
      setParity(((A ^ value) & (A ^ tmp) & 0x80) > 0)
      A = tmp
      setXY(tmp)
    }

    final def sbc(value: Int): Unit = {
      val oldCarry = carry
      val tmp = (A - value - oldCarry) & 0xFF
      F = SZP(tmp) ; Q = true
      setNegative(true)
      setCarry(value + oldCarry > A)
      setHalf(((A ^ value ^ tmp) & 0x10) > 0)
      setParity(((A ^ value) & (A ^ tmp) & 0x80) > 0)
      A = tmp
      setXY(tmp)
    }

    final def and(value: Int): Unit = {
      A &= value
      F = SZP(A) ; Q = true
      setHalf(true)
      setXY(A)
    }

    final def xor(value: Int): Unit = {
      A ^= value
      F = SZP(A) ; Q = true
      setXY(A)
    }

    final def or(value: Int): Unit = {
      A |= value
      F = SZP(A) ; Q = true
      setXY(A)
    }

    final def cpl : Unit = {
      A = ~A & 0xFF
      setHalf(true)
      setNegative(true)
      setXY(A)
      Q = true
    }

    final def ccf : Unit = {
      val q = if (lastQ) F else 0
      F = F & 0xC4 | (if ((F & 1) != 0) 0x10 else 1) | ((q ^ F) | A) & 0x28
      Q = true
    }

    final def scf : Unit = {
      val q = if (lastQ) F else 0
      F = F & 0xC4 | ((q ^ F) | A) & 0x28 | 1
      Q = true
    }

    final def daa : Unit = {
      var c,d = 0
      if (A > 0x99 || carry > 0) {
        c = 1
        d = 0x60
      }
      if ((A & 0xF) > 9 || half != 0) d += 6
      val oldA = A
      A += (if (negative > 0) -d else d)
      A &= 0xFF
      F = SZP(A) | (A ^ oldA) & HFLAG | F & 0x2 | c ; Q = true
      setXY(A)
    }

    final def cp(value: Int): Unit = {
      val tmp = (A - value) & 0xFF
      F = SZP(tmp) ; Q = true
      setNegative(true)
      setCarry(value > A)
      setHalf(((A ^ value ^ tmp) & 0x10) > 0)
      setParity(((A ^ value) & (A ^ tmp) & 0x80) > 0)
      setXY(value)
    }

    final def addHLBC : Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = HL + BC
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ B) & 0x10) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def addHLDE : Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = HL + DE
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ D) & 0x10) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def addHLHL : Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = HL + HL
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ H) & 0x10) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def addHLSP : Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = HL + SP
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ ((SP >> 8) & 0xFF)) & 0x10) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def addIXBC : Unit = {
      memptr = (INDEX + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = INDEX + BC
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ ((INDEX >> 8) & 0xFF) ^ B) & 0x10) > 0)
      INDEX = tmp & 0xFFFF
      setXY(tmp >> 8)
      Q = true
    }

    final def addIXDE : Unit = {
      memptr = (INDEX + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = INDEX + DE
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ ((INDEX >> 8) & 0xFF) ^ D) & 0x10) > 0)
      INDEX = tmp & 0xFFFF
      setXY(tmp >> 8)
      Q = true
    }

    final def addIXSP : Unit = {
      memptr = (INDEX + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = INDEX + SP
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ ((INDEX >> 8) & 0xFF) ^ ((SP >> 8) & 0xFF)) & 0x10) > 0)
      INDEX = tmp & 0xFFFF
      setXY(tmp >> 8)
      Q = true
    }

    final def addIXIX : Unit = {
      memptr = (INDEX + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val tmp = INDEX + INDEX
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ ((INDEX >> 8) & 0xFF) ^ ((INDEX >> 8) & 0xFF)) & 0x10) > 0)
      INDEX = tmp & 0xFFFF
      setXY(tmp >> 8)
      Q = true
    }

    final def adcHL(value: Int): Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val valueH = (value >> 8) & 0xFF
      val tmp = HL + value + carry
      setZero((tmp & 0xFFFF) == 0)
      setSign((tmp & 0x8000) > 0)
      setNegative(false)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ valueH) & 0x10) > 0)
      setParity(((~(H ^ valueH)) & (valueH ^ (tmp >> 8)) & 0x80) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def sbcHL(value: Int): Unit = {
      memptr = (HL + 1) & 0xFFFF
      io.internalOperation(7,IR)
      val valueH = (value >> 8) & 0xFF
      val tmp = HL - value - carry
      setZero((tmp & 0xFFFF) == 0)
      setSign((tmp & 0x8000) > 0)
      setNegative(true)
      setCarry((tmp & 0x10000) > 0)
      setHalf((((tmp >> 8) ^ H ^ valueH) & 0x10) > 0)
      setParity(((H ^ (tmp >> 8)) & (H ^ valueH) & 0x80) > 0)
      HL = tmp
      setXY(tmp >> 8)
      Q = true
    }

    final def rotLC(value: Int) = {
      val h = (value & 0x80) >> 7
      val rot = (value << 1 | h) & 0xFF
      F = SZP(rot) ; Q = true
      setCarry(h > 0)
      setXY(rot)
      rot
    }

    final def rotRC(value: Int) = {
      val oldCarry = value & 0x01
      val rot = (value >> 1 | oldCarry << 7) & 0xFF
      F = SZP(rot) ; Q = true
      setCarry(oldCarry > 0)
      setXY(rot)
      rot
    }

    final def rotL(value: Int) = {
      val oldCarry = carry
      val h = (value & 0x80) >> 7
      val rot = (value << 1 | oldCarry) & 0xFF
      F = SZP(rot) ; Q = true
      setCarry(h > 0)
      setXY(rot)
      rot
    }

    final def rotR(value: Int) = {
      val oldCarry = carry
      val l = (value & 0x01)
      val rot = (value >> 1 | oldCarry << 7) & 0xFF
      F = SZP(rot) ; Q = true
      setCarry(l > 0)
      setXY(rot)
      rot
    }

    final def sla(value: Int, bit0: Int = 0) = {
      val h = (value & 0x80)
      val shift = bit0 | (value << 1) & 0xFF
      F = SZP(shift) ; Q = true
      setCarry(h > 0)
      setXY(shift)
      shift
    }

    final def sra(value: Int) = {
      val l = (value & 0x01)
      val h = (value & 0x80)
      val shift = (value >> 1 | h) & 0xFF
      F = SZP(shift) ; Q = true
      setCarry(l > 0)
      setXY(shift)
      shift
    }

    final def srl(value: Int) = {
      val l = value & 0x01
      val shift = (value >> 1) & 0xFF
      F = SZP(shift) ; Q = true
      setCarry(l > 0)
      setXY(shift)
      shift
    }

    final def rlca(): Unit = {
      val value = A
      val h = (value & 0x80) >> 7
      val rot = (value << 1 | h) & 0xFF
      setHalf(false)
      setNegative(false)
      setCarry(h > 0)
      A = rot
      setXY(A)
      Q = true
    }

    final def rrca(): Unit = {
      val value = A
      val oldCarry = value & 0x01
      val rot = (value >> 1 | oldCarry << 7) & 0xFF
      setHalf(false)
      setNegative(false)
      setCarry(oldCarry > 0)
      A = rot
      setXY(rot)
      Q = true
    }

    final def rla(): Unit = {
      val value = A
      val oldCarry = carry
      val h = value & 0x80
      val rot = (value << 1 | oldCarry) & 0xFF
      setHalf(false)
      setNegative(false)
      setCarry(h > 0)
      A = rot
      setXY(rot)
      Q = true
    }

    final def rra(): Unit = {
      val value = A
      val oldCarry = carry
      val l = value & 0x01
      val rot = (value >> 1 | oldCarry << 7) & 0xFF
      setHalf(false)
      setNegative(false)
      setCarry(l > 0)
      A = rot
      setXY(rot)
      Q = true
    }

    final def rld : Unit = {
      memptr = (HL + 1) & 0xFFFF
      val memHL = read(HL)
      io.internalOperation(4,HL)
      write(HL,(memHL & 0x0F) << 4 | A & 0x0F)
      A = A & 0xF0 | (memHL & 0xF0) >> 4
      F = SZP(A) | carry ; Q = true
      setHalf(false)
      setNegative(false)
      setXY(A)
    }

    final def rrd : Unit = {
      memptr = (HL + 1) & 0xFFFF
      val memHL = read(HL)
      io.internalOperation(4,HL)
      write(HL,(A & 0x0F) << 4 | (memHL & 0xF0) >> 4)
      A = A & 0xF0 | memHL & 0x0F
      F = SZP(A) | carry ; Q = true
      setHalf(false)
      setNegative(false)
      setXY(A)
    }

    final def bit(b: Int, value: Int, opType: Int = 0): Unit = {
      val isZero = (value & (1 << b)) == 0
      Q = true
      setZero(isZero)
      setParity(isZero)
      setHalf(true)
      setNegative(false)
      setSign(b == 7 && (value & 0x80) > 0)
      opType match {
        case 0 => // BIT b,r
          setXY(value)
        case 1 => // BIT b,(IX + d)
          //setXY(addr)
          setXY(memptr >> 8)
        case 2 => // BIT b,(HL)
          setXY(memptr >> 8)
      }
    }

    final def res(b:Int,value:Int) = value & ~(1 << b)
    final def set(b:Int,value:Int) = value | (1 << b)

    final def jre_e : Unit = {
      io.internalOperation(5,PC)
      val addr = (PC + 2 + byte(1).asInstanceOf[Byte]) & 0xFFFF
      PC = addr
      memptr = addr
    }

    final def jp_cond_nn(cond: Boolean): Unit = {
      val ofs = word(1)
      if (cond) PC = ofs else PC = (PC + 3) & 0xFFFF
      memptr = ofs
    }
    final def jr_cond_e(cond: Boolean): Unit = {
      val ofs = byte(1).asInstanceOf[Byte]
      if (cond) {
        io.internalOperation(5,PC)
        PC = (PC + 2 + ofs) & 0xFFFF
        memptr = PC
      }
      else {
        PC = (PC + 2) & 0xFFFF
        setAdditionalClockCycles(-5)
      }
    }

    final def call(addr: Int): Unit = {
      push((PC + 3) & 0xFFFF)
      PC = addr
      memptr = addr
    }

    final def call_cond_nn(cond: Boolean): Unit = {
      val tmp = word(1)
      if (cond) {
        io.internalOperation(1,(PC + 1) & 0xFFFF)
        call(tmp)
        setAdditionalClockCycles(7)
      }
      else {
        PC = (PC + 3) & 0xFFFF
        memptr = tmp
      }
    }

    final def ret_cond(cond: Boolean): Unit = {
      io.internalOperation(1,IR)
      if (cond) {
        PC = pop
        setAdditionalClockCycles(6)
      }
      else PC = (PC + 1) & 0xFFFF
    }

    final def retni(): Unit = {
      PC = pop
      IFF1 = IFF2
      memptr = PC
    }

    final def rst(pcl: Int): Unit = {
      io.internalOperation(1,IR)
      push((PC + 1) & 0xFFFF)
      PC = pcl
      memptr = PC
    }

    final def in_a_n : Unit = {
      val port = byte(1)
      memptr = ((A << 8) + port + 1) & 0xFFFF
      A = io.in(A,port)
    }

    final def in_r_c() = {
      val v = io.in(B,C)
      F = SZP(v) | carry ; Q = true
      setHalf(false)
      setNegative(false)
      setXY(v)
      memptr = (BC + 1) & 0xFFFF
      v
    }

    final def ini(inc: Boolean): Unit = {
      io.internalOperation(1,IR)
      val tmp = io.in(B,C)
      write(HL,tmp)
      incDecHL(inc)
      memptr = if (inc) (BC + 1) & 0xFFFF else (BC - 1) & 0xFFFF
      B = (B - 1) & 0xFF
      F = SZP(B) ; Q = true
      setNegative((tmp & 0x80) > 0)
      val tmp2 = if (inc) (tmp + C + 1) & 0xFF else (tmp + C - 1) & 0xFF
      val parity = (SZP((tmp2 & 0x07) ^ B) & 0x04) > 0
      setParity(parity)
      setHalf(tmp2 < tmp)
      setCarry(tmp2 < tmp)
      setXY(B)
    }

    final def out_c_r(value:Int) : Unit = {
      io.out(B,C,value)
      memptr = (BC + 1) & 0xFFFF
    }

    /*
   *  tmp := (hl), ((c)) := tmp, hl += 1,
      b -= 1 => flags, nf := tmp.7, tmp2 = tmp + l,
      pf := parity of [[tmp2 AND 0x07] XOR b],
      hf := cf := tmp2 > 255
   */
    final def outi(inc: Boolean): Unit = {
      io.internalOperation(1,IR)
      val tmp = read(HL)
      incDecHL(inc)
      B = (B - 1) & 0xFF
      io.out(B,C,tmp)
      memptr = if (inc) (BC + 1) & 0xFFFF else (BC - 1) & 0xFFFF
      F = SZP(B) ; Q = true
      setNegative((tmp & 0x80) > 0)
      val tmp2 = tmp + L
      val parity = (SZP((tmp2 & 0x07) ^ B) & 0x04) > 0
      setParity(parity)
      setHalf(tmp2 > 0xFF)
      setCarry(tmp2 > 0xFF)
      setXY(B)
    }

    final def neg : Unit = {
      val tmp = (0 - A) & 0xFF
      F = SZP(tmp) ; Q = true
      setNegative(true)
      setHalf(((A ^ tmp) & 0x10) > 0)
      setParity((A & tmp & 0x80) > 0)
      setCarry(A > 0)
      A = tmp
      setXY(tmp)
    }

    final def incR(deltaR:Int) = R = (R & 0x80) | (R + deltaR) & 0x7F

    final def ldi : Unit = {
      val tmp = read(HL)
      val tmp2 = A + tmp
      val de = DE
      write(DE,tmp) ; incDecDE(true) ; incDecHL(true) ; incDecBC(false)
      io.internalOperation(2,de)
      F = F & 0xC1 ; Q = true
      setParity(BC != 0)
      setNegative(false)
      F = F & 0xD7 | tmp2 & 0x8 | (tmp2 & 0x2) << 4
    }

    final def ldir : Unit = {
      val tmp = read(HL)
      val tmp2 = A + tmp
      val de = DE
      write(DE,tmp) ; incDecDE(true) ; incDecHL(true) ; incDecBC(false)
      io.internalOperation(2,de)
      F &= 0xC1 ; Q = true
      setParity(BC != 0)
      setHalf(false)
      if (BC == 0) {
        PC = (PC + 2) & 0xFFFF
        setAdditionalClockCycles(-5)
      }
      else {
        io.internalOperation(5,de)
        memptr = (PC + 1) & 0xFFFF
      }

      F = F & 0xD7 | tmp2 & 0x8 | (tmp2 & 0x2) << 4
    }

    final def ldd : Unit = {
      val tmp = read(HL)
      val tmp2 = A + tmp
      val de = DE
      write(DE,tmp) ; incDecDE(false) ; incDecHL(false) ; incDecBC(false)
      io.internalOperation(2,de)
      F &= 0xC1 ; Q = true
      setParity(BC != 0)
      setNegative(false)
      F = F & 0xD7 | tmp2 & 0x8 | (tmp2 & 0x2) << 4
    }

    final def lddr : Unit = {
      val tmp = read(HL)
      val tmp2 = A + tmp
      val de = DE
      write(DE,tmp) ; incDecDE(false) ; incDecHL(false) ; incDecBC(false)
      io.internalOperation(2,de)
      F &= 0xC1 ; Q = true
      setParity(BC != 0)
      setHalf(false)
      if (BC == 0) {
        PC = (PC + 2) & 0xFFFF
        setAdditionalClockCycles(-5)
      }
      else {
        io.internalOperation(5,de)
        memptr = (PC + 1) & 0xFFFF
      }
      F = F & 0xD7 | tmp2 & 0x8 | (tmp2 & 0x2) << 4
    }

    final def cpi : Unit = {
      val value = read(HL)
      io.internalOperation(5,HL)
      var cmp = (A - value) & 0xFF
      incDecHL(true) ; incDecBC(false)
      F = carry | SZP(cmp) ; Q = true
      setParity(BC != 0)
      setNegative(true)
      setHalf(((A ^ value ^ cmp) & 0x10) > 0)
      if (half > 0) cmp -= 1
      F = F & 0xD7 | cmp & 0x8 | (cmp & 0x2) << 4
      memptr = (memptr + 1) & 0xFFFF
    }

    final def cpir : Unit = {
      val value = read(HL)
      io.internalOperation(5,HL)
      var cmp = (A - value) & 0xFF
      val hl = HL
      incDecHL(true) ; incDecBC(false)
      F = carry | SZP(cmp) ; Q = true
      setParity(BC != 0)
      setNegative(true)
      setHalf(((A ^ value ^ cmp) & 0x10) > 0)
      if (BC == 0 || cmp == 0) {
        PC = (PC + 2) & 0xFFFF
        setAdditionalClockCycles(-5)
        memptr = (memptr + 1) & 0xFFFF
      }
      else {
        io.internalOperation(5,hl)
        memptr = (PC + 1) & 0xFFFF
      }
      if (half > 0) cmp -= 1
      F = F & 0xD7 | cmp & 0x8 | (cmp & 0x2) << 4
    }

    final def cpdr : Unit = {
      val value = read(HL)
      io.internalOperation(5,HL)
      var cmp = (A - value) & 0xFF
      val hl = HL
      incDecHL(false) ; incDecBC(false)
      F = carry | SZP(cmp) ; Q = true
      setParity(BC != 0)
      setNegative(true)
      setHalf(((A ^ value ^ cmp) & 0x10) > 0)
      if (BC == 0 || cmp == 0) {
        PC = (PC + 2) & 0xFFFF
        setAdditionalClockCycles(-5)
        memptr = (memptr - 1) & 0xFFFF
      }
      else {
        io.internalOperation(5,hl)
        memptr = (PC + 1) & 0xFFFF
      }
      if (half > 0) cmp -= 1
      F = F & 0xD7 | cmp & 0x8 | (cmp & 0x2) << 4
    }

    final def cpd : Unit = {
      val value = read(HL)
      io.internalOperation(5,HL)
      var cmp = (A - value) & 0xFF
      incDecHL(false) ; incDecBC(false)
      F = carry | SZP(cmp) ; Q = true
      setParity(BC != 0)
      setNegative(true)
      setHalf(((A ^ value ^ cmp) & 0x10) > 0)
      if (half > 0) cmp -= 1
      F = F & 0xD7 | cmp & 0x8 | (cmp & 0x2) << 4
      memptr = (memptr - 1) & 0xFFFF
    }

    final def djnz : Unit = {
      io.internalOperation(1,IR)
      val ofs = byte(1).asInstanceOf[Byte]
      B = (B - 1) & 0xFF
      if (B == 0) PC = (PC + 2) & 0xFFFF
      else {
        setAdditionalClockCycles(5)
        io.internalOperation(5,PC)
        PC = (PC + 2 + ofs) & 0xFFFF
        memptr = PC
      }
    }
  }
  private implicit def int2Array(x:Int): Array[Int] = Array(x)
  private implicit def tuple2Array(x:Tuple2[Int,Int]): Array[Int] = Array(x._1,x._2)
  private implicit def tuple3Array(x:Tuple3[Int,Int,Int]): Array[Int] = Array(x._1,x._2,x._3)
  private implicit def string2Mnem(s:String): (Memory, Int) => String = (Memory, Int) => s

  private class FD(exe : Context => Unit) extends (Context => Unit) {
    def apply(ctx : Context) : Unit = {
      try {
        ctx.isIndexX = false
        exe(ctx)
      }
      finally {
        ctx.isIndexX = true
      }
    }
  }

  private case class Opcode(opcodes:Array[Int],
                            cycles:Int,
                            size:Int,
                            getMnemonic : (Memory,Int) => String,
                            modifyPC:Boolean = false,
                            copyopcodes:Array[Int] = null)(val executeFunction:Context => Unit) {
    def disassemble(mem:Memory,pc:Int) : String = {
      val sb = new StringBuilder(hex4(pc) + "  ")
      var s = pc
      while (s < pc + size) {
        sb.append(hex2(mem.read(s & 0xFFFF)) + " ")
        s += 1
      }
      var spaces = 20 - sb.length
      while (spaces > 0) {
        sb += ' '
        spaces -= 1
      }
      sb.toString + getMnemonic(mem,pc)
    }
  }

  private val opcodes_1, opcodes_ed, opcodes_cb, opcodes_dd, opcodes_fd, opcodes_ddcb, opcodes_fdcb = Array.ofDim[Opcode](256)
  private val OPCODES = new collection.mutable.ListBuffer[Opcode]

  // ================================== LOAD 8 bit ===========================================================
  addOp(Opcode((0xED,0x57),9,2,"LD A,I") { ctx =>
    ctx.io.internalOperation(1,ctx.IR)
    ctx.A = ctx.I
    ctx.F = ctx.SZP(ctx.A) | ctx.carry ; ctx.Q = true
    ctx.setParity(ctx.IFF2 > 0)
    ctx.setXY(ctx.A)
  })
  addOp(Opcode((0xED,0x5F),9,2,"LD A,R") { ctx =>
    ctx.io.internalOperation(1,ctx.IR)
    ctx.A = ctx.R
    ctx.F = ctx.SZP(ctx.A) | ctx.carry ; ctx.Q = true
    ctx.setParity(ctx.IFF2 > 0)
    ctx.setXY(ctx.R)
  })
  // *** LD r,r'
  // **************
  addOp(Opcode(0x7F,4,1,"LD A,A") { ctx => /*ctx.A = ctx.A*/ })
  addOp(Opcode(0x78,4,1,"LD A,B") { ctx => ctx.A = ctx.B })
  addOp(Opcode(0x79,4,1,"LD A,C") { ctx => ctx.A = ctx.C })
  addOp(Opcode(0x7A,4,1,"LD A,D") { ctx => ctx.A = ctx.D })
  addOp(Opcode(0x7B,4,1,"LD A,E") { ctx => ctx.A = ctx.E })
  addOp(Opcode(0x7C,4,1,"LD A,H") { ctx => ctx.A = ctx.H })
  addOp(Opcode(0x7D,4,1,"LD A,L") { ctx => ctx.A = ctx.L })
  //
  addOp(Opcode(0x47,4,1,"LD B,A") { ctx => ctx.B = ctx.A })
  addOp(Opcode(0x40,4,1,"LD B,B") { ctx => /*ctx.B = ctx.B*/ })
  addOp(Opcode(0x41,4,1,"LD B,C") { ctx => ctx.B = ctx.C })
  addOp(Opcode(0x42,4,1,"LD B,D") { ctx => ctx.B = ctx.D })
  addOp(Opcode(0x43,4,1,"LD B,E") { ctx => ctx.B = ctx.E })
  addOp(Opcode(0x44,4,1,"LD B,H") { ctx => ctx.B = ctx.H })
  addOp(Opcode(0x45,4,1,"LD B,L") { ctx => ctx.B = ctx.L })
  //
  addOp(Opcode(0x4F,4,1,"LD C,A") { ctx => ctx.C = ctx.A })
  addOp(Opcode(0x48,4,1,"LD C,B") { ctx => ctx.C = ctx.B })
  addOp(Opcode(0x49,4,1,"LD C,C") { ctx => /*ctx.C = ctx.C*/ })
  addOp(Opcode(0x4A,4,1,"LD C,D") { ctx => ctx.C = ctx.D })
  addOp(Opcode(0x4B,4,1,"LD C,E") { ctx => ctx.C = ctx.E })
  addOp(Opcode(0x4C,4,1,"LD C,H") { ctx => ctx.C = ctx.H })
  addOp(Opcode(0x4D,4,1,"LD C,L") { ctx => ctx.C = ctx.L })
  //
  addOp(Opcode(0x57,4,1,"LD D,A") { ctx => ctx.D = ctx.A })
  addOp(Opcode(0x50,4,1,"LD D,B") { ctx => ctx.D = ctx.B })
  addOp(Opcode(0x51,4,1,"LD D,C") { ctx => ctx.D = ctx.C })
  addOp(Opcode(0x52,4,1,"LD D,D") { ctx => /*ctx.D = ctx.D*/ })
  addOp(Opcode(0x53,4,1,"LD D,E") { ctx => ctx.D = ctx.E })
  addOp(Opcode(0x54,4,1,"LD D,H") { ctx => ctx.D = ctx.H })
  addOp(Opcode(0x55,4,1,"LD D,L") { ctx => ctx.D = ctx.L })
  //
  addOp(Opcode(0x5F,4,1,"LD E,A") { ctx => ctx.E = ctx.A })
  addOp(Opcode(0x58,4,1,"LD E,B") { ctx => ctx.E = ctx.B })
  addOp(Opcode(0x59,4,1,"LD E,C") { ctx => ctx.E = ctx.C })
  addOp(Opcode(0x5A,4,1,"LD E,D") { ctx => ctx.E = ctx.D })
  addOp(Opcode(0x5B,4,1,"LD E,E") { ctx => /*ctx.E = ctx.E*/ })
  addOp(Opcode(0x5C,4,1,"LD E,H") { ctx => ctx.E = ctx.H })
  addOp(Opcode(0x5D,4,1,"LD E,L") { ctx => ctx.E = ctx.L })
  //
  addOp(Opcode(0x67,4,1,"LD H,A") { ctx => ctx.H = ctx.A })
  addOp(Opcode(0x60,4,1,"LD H,B") { ctx => ctx.H = ctx.B })
  addOp(Opcode(0x61,4,1,"LD H,C") { ctx => ctx.H = ctx.C })
  addOp(Opcode(0x62,4,1,"LD H,D") { ctx => ctx.H = ctx.D })
  addOp(Opcode(0x63,4,1,"LD H,E") { ctx => ctx.H = ctx.E })
  addOp(Opcode(0x64,4,1,"LD H,H") { ctx => /*ctx.H = ctx.H*/ })
  addOp(Opcode(0x65,4,1,"LD H,L") { ctx => ctx.H = ctx.L })
  //
  addOp(Opcode(0x6F,4,1,"LD L,A") { ctx => ctx.L = ctx.A })
  addOp(Opcode(0x68,4,1,"LD L,B") { ctx => ctx.L = ctx.B })
  addOp(Opcode(0x69,4,1,"LD L,C") { ctx => ctx.L = ctx.C })
  addOp(Opcode(0x6A,4,1,"LD L,D") { ctx => ctx.L = ctx.D })
  addOp(Opcode(0x6B,4,1,"LD L,E") { ctx => ctx.L = ctx.E })
  addOp(Opcode(0x6C,4,1,"LD L,H") { ctx => ctx.L = ctx.H })
  addOp(Opcode(0x6D,4,1,"LD L,L") { ctx => /*ctx.L = ctx.L*/ })
  // *** LD r,n
  // **************
  private def MNEMONIC_n(pattern:String,ofs:Int = 1) = (m:Memory,PC:Int) => pattern.format(hex2(m.read(PC + ofs)))
  addOp(Opcode(0x3E,7,2,MNEMONIC_n("LD A,%s")) { ctx => ctx.A = ctx.byte(1) })
  addOp(Opcode(0x06,7,2,MNEMONIC_n("LD B,%s")) { ctx => ctx.B = ctx.byte(1) })
  addOp(Opcode(0x0E,7,2,MNEMONIC_n("LD C,%s")) { ctx => ctx.C = ctx.byte(1) })
  addOp(Opcode(0x16,7,2,MNEMONIC_n("LD D,%s")) { ctx => ctx.D = ctx.byte(1) })
  addOp(Opcode(0x1E,7,2,MNEMONIC_n("LD E,%s")) { ctx => ctx.E = ctx.byte(1) })
  addOp(Opcode(0x26,7,2,MNEMONIC_n("LD H,%s")) { ctx => ctx.H = ctx.byte(1) })
  addOp(Opcode(0x2E,7,2,MNEMONIC_n("LD L,%s")) { ctx => ctx.L = ctx.byte(1) })
  // *** LD r,(HL)
  // **************
  addOp(Opcode(0x7E,7,1,"LD A,(HL)") { ctx => ctx.A = ctx.read(ctx.HL) })
  addOp(Opcode(0x46,7,1,"LD B,(HL)") { ctx => ctx.B = ctx.read(ctx.HL) })
  addOp(Opcode(0x4E,7,1,"LD C,(HL)") { ctx => ctx.C = ctx.read(ctx.HL) })
  addOp(Opcode(0x56,7,1,"LD D,(HL)") { ctx => ctx.D = ctx.read(ctx.HL) })
  addOp(Opcode(0x5E,7,1,"LD E,(HL)") { ctx => ctx.E = ctx.read(ctx.HL) })
  addOp(Opcode(0x66,7,1,"LD H,(HL)") { ctx => ctx.H = ctx.read(ctx.HL) })
  addOp(Opcode(0x6E,7,1,"LD L,(HL)") { ctx => ctx.L = ctx.read(ctx.HL) })
  // *** LD A,(BC)
  // **************
  addOp(Opcode(0x0A,7,1,"LD A,(BC)") { ctx =>
    ctx.A = ctx.read(ctx.BC)
    ctx.memptr = (ctx.BC + 1) & 0xFFFF
  })
  // *** LD A,(DE)
  // **************
  addOp(Opcode(0x1A,7,1,"LD A,(DE)") { ctx =>
    ctx.A = ctx.read(ctx.DE)
    ctx.memptr = (ctx.DE + 1) & 0xFFFF
  })
  // *** LD r,(IX + d)
  // **************
  private def MNEMONIC_IXY_d(pattern:String) = (m:Memory,PC:Int) => {
    val ofs = m.read(PC + 2).asInstanceOf[Byte]
    if (ofs > 0) pattern.format(" + " + hex2(ofs)) else pattern.format(" - " + hex2(-ofs))
  }
  addOp(Opcode((0xDD,0x7E),19,3,MNEMONIC_IXY_d("LD A,(IX %s)")) { ctx => ctx.A = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x46),19,3,MNEMONIC_IXY_d("LD B,(IX %s)")) { ctx => ctx.B = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x4E),19,3,MNEMONIC_IXY_d("LD C,(IX %s)")) { ctx => ctx.C = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x56),19,3,MNEMONIC_IXY_d("LD D,(IX %s)")) { ctx => ctx.D = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x5E),19,3,MNEMONIC_IXY_d("LD E,(IX %s)")) { ctx => ctx.E = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x66),19,3,MNEMONIC_IXY_d("LD H,(IX %s)")) { ctx => ctx.H = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  addOp(Opcode((0xDD,0x6E),19,3,MNEMONIC_IXY_d("LD L,(IX %s)")) { ctx => ctx.L = ctx.read(ctx.INDEX_+(ctx.byte(2))) })
  // *** LD A,(nn)
  // **************
  private def MNEMONIC_nn(pattern:String,ofs:Int = 1) = (m:Memory,PC:Int) => pattern.format(hex4(WORD(m.read(PC + ofs + 1),m.read(PC + ofs))))
  addOp(Opcode(0x3A,13,3,MNEMONIC_nn("LD A,(%s)")) { ctx =>
    val addr = ctx.word(1)
    ctx.A = ctx.read(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  // *** LD (HL),r
  // **************
  addOp(Opcode(0x77,7,1,"LD (HL),A") { ctx => ctx.write(ctx.HL,ctx.A) })
  addOp(Opcode(0x70,7,1,"LD (HL),B") { ctx => ctx.write(ctx.HL,ctx.B) })
  addOp(Opcode(0x71,7,1,"LD (HL),C") { ctx => ctx.write(ctx.HL,ctx.C) })
  addOp(Opcode(0x72,7,1,"LD (HL),D") { ctx => ctx.write(ctx.HL,ctx.D) })
  addOp(Opcode(0x73,7,1,"LD (HL),E") { ctx => ctx.write(ctx.HL,ctx.E) })
  addOp(Opcode(0x74,7,1,"LD (HL),H") { ctx => ctx.write(ctx.HL,ctx.H) })
  addOp(Opcode(0x75,7,1,"LD (HL),L") { ctx => ctx.write(ctx.HL,ctx.L) })
  // *** LD (HL),n
  // **************
  addOp(Opcode(0x36,10,2,MNEMONIC_n("LD (HL),%s")) { ctx => ctx.write(ctx.HL,ctx.byte(1)) })
  // *** LD (BC),A
  // **************
  addOp(Opcode(0x02,7,1,"LD (BC),A") { ctx =>
    ctx.write(ctx.BC,ctx.A)
    ctx.memptr = (ctx.BC + 1) & 0xFF | ctx.A << 8
  })
  // *** LD (DE),A
  // **************
  addOp(Opcode(0x12,7,1,"LD (DE),A") { ctx =>
    ctx.write(ctx.DE,ctx.A)
    ctx.memptr = (ctx.DE + 1) & 0xFF | ctx.A << 8
  })
  // *** LD (IX + d),r
  // **************
  addOp(Opcode((0xDD,0x77),19,3,MNEMONIC_IXY_d("LD (IX %s),A")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.A) })
  addOp(Opcode((0xDD,0x70),19,3,MNEMONIC_IXY_d("LD (IX %s),B")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.B) })
  addOp(Opcode((0xDD,0x71),19,3,MNEMONIC_IXY_d("LD (IX %s),C")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.C) })
  addOp(Opcode((0xDD,0x72),19,3,MNEMONIC_IXY_d("LD (IX %s),D")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.D) })
  addOp(Opcode((0xDD,0x73),19,3,MNEMONIC_IXY_d("LD (IX %s),E")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.E) })
  addOp(Opcode((0xDD,0x74),19,3,MNEMONIC_IXY_d("LD (IX %s),H")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.H) })
  addOp(Opcode((0xDD,0x75),19,3,MNEMONIC_IXY_d("LD (IX %s),L")) { ctx => ctx.write(ctx.INDEX_+(ctx.byte(2)),ctx.L) })
  // *** LD (IX + d),n
  // **************
  private def MNEMONIC_IXY_d_n(pattern:String) = (m:Memory,PC:Int) => {
    val ofs = m.read(PC + 2).asInstanceOf[Byte]
    val n = hex2(m.read(PC + 3))
    if (ofs > 0) pattern.format("+ " + hex2(ofs),n) else pattern.format("- " + hex2(-ofs),n)
  }
  addOp(Opcode((0xDD,0x36),19,4,MNEMONIC_IXY_d_n("LD (IX %s),%s")) { ctx =>
    val ofs = ctx.byte(2)
    val value = ctx.byte(3)
    ctx.io.internalOperation(2,ctx.PC)
    ctx.write(ctx.INDEX_+(ofs,false),value)
  })
  // *** LD (nn),A
  // **************
  addOp(Opcode(0x32,13,3,MNEMONIC_nn("LD (%s),A")) { ctx =>
    val addr = ctx.word(1)
    ctx.write(addr,ctx.A)
    ctx.memptr = (addr + 1) & 0xFF | ctx.A << 8
  })
  // *** LD I,A
  // **************
  addOp(Opcode((0xED,0x47),9,2,"LD I,A") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.I = ctx.A })
  // *** LD R,A
  // **************
  addOp(Opcode((0xED,0x4F),9,2,"LD R,A") { ctx => ctx.R = ctx.A ; ctx.io.internalOperation(1,ctx.IR) })
  // =========================================== LOAD 16 bit =================================================
  // *** LD dd,nn
  // **************
  addOp(Opcode(0x01,10,3,MNEMONIC_nn("LD BC,%s")) { ctx => ctx.BC = ctx.word(1) })
  addOp(Opcode(0x11,10,3,MNEMONIC_nn("LD DE,%s")) { ctx => ctx.DE = ctx.word(1) })
  addOp(Opcode(0x21,10,3,MNEMONIC_nn("LD HL,%s")) { ctx => ctx.HL = ctx.word(1) })
  addOp(Opcode(0x31,10,3,MNEMONIC_nn("LD SP,%s")) { ctx => ctx.SP = ctx.word(1) })
  // *** LD IX,nn
  // **************
  addOp(Opcode((0xDD,0x21),14,4,MNEMONIC_nn("LD IX,%s",2)) { ctx => ctx.INDEX = ctx.word(2) })
  // UNDOCUMENTED
  // *** LD IXH,n
  // **************
  addOp(Opcode((0xDD,0x26),11,3,MNEMONIC_nn("LD IXH,%s",2)) { ctx => ctx.INDEX_H = ctx.byte(2) })
  // UNDOCUMENTED
  // *** LD IXL,n
  // **************
  addOp(Opcode((0xDD,0x2E),11,3,MNEMONIC_n("LD IXL,%s",2)) { ctx => ctx.INDEX_L = ctx.byte(2) })
  // UNDOCUMENTED
  // *** LD B,IXH
  // **************
  addOp(Opcode((0xDD,0x44),8,2,"LD B,IXH") { ctx => ctx.B = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD B,IXL
  // **************
  addOp(Opcode((0xDD,0x45),8,2,"LD B,IXL") { ctx => ctx.B = ctx.INDEX_L })
  // UNDOCUMENTED
  // *** LD C,IXH
  // **************
  addOp(Opcode((0xDD,0x4C),8,2,"LD C,IXH") { ctx => ctx.C = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD C,IXL
  // **************
  addOp(Opcode((0xDD,0x4D),8,2,"LD C,IXL") { ctx => ctx.C = ctx.INDEX_L })
  // UNDOCUMENTED
  // *** LD D,IXH
  // **************
  addOp(Opcode((0xDD,0x54),8,2,"LD D,IXH") { ctx => ctx.D = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD B,IXL
  // **************
  addOp(Opcode((0xDD,0x55),8,2,"LD D,IXL") { ctx => ctx.D = ctx.INDEX_L })
  // UNDOCUMENTED
  // *** LD E,IXH
  // **************
  addOp(Opcode((0xDD,0x5C),8,2,"LD E,IXH") { ctx => ctx.E = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD C,IXL
  // **************
  addOp(Opcode((0xDD,0x5D),8,2,"LD E,IXL") { ctx => ctx.E = ctx.INDEX_L })
  // UNDOCUMENTED
  // *** LD IXH,B
  // **************
  addOp(Opcode((0xDD,0x60),8,2,"LD IXH,B") { ctx => ctx.INDEX_H = ctx.B })
  // UNDOCUMENTED
  // *** LD IXH,C
  // **************
  addOp(Opcode((0xDD,0x61),8,2,"LD IXH,C") { ctx => ctx.INDEX_H = ctx.C })
  // UNDOCUMENTED
  // *** LD IXH,D
  // **************
  addOp(Opcode((0xDD,0x62),8,2,"LD IXH,D") { ctx => ctx.INDEX_H = ctx.D })
  // UNDOCUMENTED
  // *** LD IXH,E
  // **************
  addOp(Opcode((0xDD,0x63),8,2,"LD IXH,E") { ctx => ctx.INDEX_H = ctx.E })
  // UNDOCUMENTED
  // *** LD IXH,IXH
  // **************
  addOp(Opcode((0xDD,0x64),8,2,"LD IXH,IXH") { ctx => /*ctx.INDEX_H = ctx.INDEX_H*/ })
  // UNDOCUMENTED
  // *** LD IXH,IXL
  // **************
  addOp(Opcode((0xDD,0x65),8,2,"LD IXH,IXL") { ctx => ctx.INDEX_H = ctx.INDEX_L })
  // UNDOCUMENTED
  // *** LD IXH,A
  // **************
  addOp(Opcode((0xDD,0x67),8,2,"LD IXH,A") { ctx => ctx.INDEX_H = ctx.A })
  // UNDOCUMENTED
  // *** LD IXL,B
  // **************
  addOp(Opcode((0xDD,0x68),8,2,"LD IXL,B") { ctx => ctx.INDEX_L = ctx.B })
  // UNDOCUMENTED
  // *** LD IXL,C
  // **************
  addOp(Opcode((0xDD,0x69),8,2,"LD IXL,C") { ctx => ctx.INDEX_L = ctx.C })
  // UNDOCUMENTED
  // *** LD IXL,D
  // **************
  addOp(Opcode((0xDD,0x6A),8,2,"LD IXL,D") { ctx => ctx.INDEX_L = ctx.D })
  // UNDOCUMENTED
  // *** LD IXL,E
  // **************
  addOp(Opcode((0xDD,0x6B),8,2,"LD IXL,E") { ctx => ctx.INDEX_L = ctx.E })
  // UNDOCUMENTED
  // *** LD IXL,IXH
  // **************
  addOp(Opcode((0xDD,0x6C),8,2,"LD IXL,IXH") { ctx => ctx.INDEX_L = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD IXL,IXL
  // **************
  addOp(Opcode((0xDD,0x6D),8,2,"LD IXL,IXL") { ctx => /*ctx.INDEX_L = ctx.INDEX_L*/ })
  // UNDOCUMENTED
  // *** LD IXL,A
  // **************
  addOp(Opcode((0xDD,0x6F),8,2,"LD IXL,A") { ctx => ctx.INDEX_L = ctx.A })
  // UNDOCUMENTED
  // *** LD A,IXH
  // **************
  addOp(Opcode((0xDD,0x7C),8,2,"LD A,IXH") { ctx => ctx.A = ctx.INDEX_H })
  // UNDOCUMENTED
  // *** LD A,IXL
  // **************
  addOp(Opcode((0xDD,0x7D),8,2,"LD A,IXL") { ctx => ctx.A = ctx.INDEX_L })

  // *** LD dd,(nn)
  // **************
  addOp(Opcode((0xED,0x4B),20,4,MNEMONIC_nn("LD BC,(%s)",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.BC = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  addOp(Opcode((0xED,0x5B),20,4,MNEMONIC_nn("LD DE,(%s)",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.DE = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  addOp(Opcode((0xED,0x6B),20,4,MNEMONIC_nn("LD HL,(%s)",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.HL = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  addOp(Opcode((0xED,0x7B),20,4,MNEMONIC_nn("LD SP,(%s)",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.SP = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  // *** LD HL,(nn)
  // **************
  addOp(Opcode(0x2A,16,3,MNEMONIC_nn("LD HL,(%s)")) { ctx =>
    val addr = ctx.word(1)
    ctx.HL = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  // *** LD IX,(nn)
  // **************
  addOp(Opcode((0xDD,0x2A),20,4,MNEMONIC_nn("LD IX,(%s)",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.INDEX = ctx.readW(addr)
    ctx.memptr = (addr + 1) & 0xFFFF
  })
  // *** LD SP,HL
  // **************
  addOp(Opcode(0xF9,6,1,"LD SP,HL") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.SP = ctx.HL })
  // *** LD SP,IX
  // **************
  addOp(Opcode((0xDD,0xF9),10,2,"LD SP,IX") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.SP = ctx.INDEX })
  // *** LD (nn),dd
  // **************
  addOp(Opcode((0xED,0x43),20,4,MNEMONIC_nn("LD (%s),BC",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.writeW(addr,ctx.BC)
    ctx.memptr = addr + 1
  })
  addOp(Opcode((0xED,0x53),20,4,MNEMONIC_nn("LD (%s),DE",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.writeW(addr,ctx.DE)
    ctx.memptr = addr + 1
  })
  addOp(Opcode((0xED,0x63),20,4,MNEMONIC_nn("LD (%s),HL",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.writeW(addr,ctx.HL)
    ctx.memptr = addr + 1
  })
  addOp(Opcode((0xED,0x73),20,4,MNEMONIC_nn("LD (%s),SP",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.writeW(addr,ctx.SP)
    ctx.memptr = addr + 1
  })
  addOp(Opcode((0xDD,0x22),20,4,MNEMONIC_nn("LD (%s),IX",2)) { ctx =>
    val addr = ctx.word(2)
    ctx.writeW(addr,ctx.INDEX)
    ctx.memptr = addr + 1
  })
  // *** LD (nn),HL
  // **************
  addOp(Opcode(0x22,16,3,MNEMONIC_nn("LD (%s),HL")) { ctx =>
    val addr = ctx.word(1)
    ctx.writeW(addr,ctx.HL)
    ctx.memptr = addr + 1
  })
  // *** PUSH AF
  // **************
  addOp(Opcode(0xF5,11,1,"PUSH AF") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.push(ctx.AF) })
  // *** PUSH BC
  // **************
  addOp(Opcode(0xC5,11,1,"PUSH BC") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.push(ctx.BC) })
  // *** PUSH DE
  // **************
  addOp(Opcode(0xD5,11,1,"PUSH DE") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.push(ctx.DE) })
  // *** PUSH HL
  // **************
  addOp(Opcode(0xE5,11,1,"PUSH HL") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.push(ctx.HL) })
  // *** PUSH IX
  // **************
  addOp(Opcode((0xDD,0xE5),15,2,"PUSH IX") { ctx => ctx.io.internalOperation(1,ctx.IR) ; ctx.push(ctx.INDEX) })
  // *** POP AF
  // **************
  addOp(Opcode(0xF1,10,1,"POP AF") { ctx => ctx.AF = ctx.pop })
  // *** POP BC
  // **************
  addOp(Opcode(0xC1,10,1,"POP BC") { ctx => ctx.BC = ctx.pop })
  // *** POP DE
  // **************
  addOp(Opcode(0xD1,10,1,"POP DE") { ctx => ctx.DE = ctx.pop })
  // *** POP HL
  // **************
  addOp(Opcode(0xE1,10,1,"POP HL") { ctx => ctx.HL = ctx.pop })
  // *** POP IX
  // **************
  addOp(Opcode((0xDD,0xE1),14,2,"POP IX") { ctx => ctx.INDEX = ctx.pop })
  // ======================================= Exchange ========================================================
  // *** EXX
  // **************
  addOp(Opcode(0xD9,4,1,"EXX") { ctx => ctx.EXX })
  // *** EX DE,HL
  // **************
  addOp(Opcode(0xEB,4,1,"EX DE,HL") { ctx => ctx.EX_DE_HL })
  // *** EX AF,AF'
  // **************
  addOp(Opcode(0x08,4,1,"EX AF,AF'") { ctx => ctx.EX_AF })
  // *** EX (SP),HL
  // **************
  addOp(Opcode(0xE3,19,1,"EX (SP),HL") { ctx => ctx.EX_SP_HL })
  // *** EX (SP),IX
  // **************
  addOp(Opcode((0xDD,0xE3),23,2,"EX (SP),IX") { ctx => ctx.EX_SP_IX })
  // ======================================= Block Transfer ==================================================
  // *** LDI
  // **************
  addOp(Opcode((0xED,0xA0),16,2,"LDI") { ctx => ctx.ldi })
  // *** LDIR
  // **************
  addOp(Opcode((0xED,0xB0),21,2,"LDIR",modifyPC = true) { ctx => ctx.ldir })
  // *** LDD
  // **************
  addOp(Opcode((0xED,0xA8),16,2,"LDD") { ctx => ctx.ldd })
  // *** LDDR
  // **************
  addOp(Opcode((0xED,0xB8),21,2,"LDDR",modifyPC = true) { ctx => ctx.lddr })
  // *** CPI
  // **************
  addOp(Opcode((0xED,0xA1),16,2,"CPI") { ctx => ctx.cpi })
  // *** CPIR
  // **************
  addOp(Opcode((0xED,0xB1),21,2,"CPIR",modifyPC = true) { ctx => ctx.cpir })
  // *** CPDR
  // **************
  addOp(Opcode((0xED,0xB9),21,2,"CPDR",modifyPC = true) { ctx => ctx.cpdr })
  // *** CPD
  // **************
  addOp(Opcode((0xED,0xA9),16,2,"CPD") { ctx => ctx.cpd })
  // ===================================== 8 bit arithmetic ==================================================
  // *** ADD A,r
  // **************
  addOp(Opcode(0x87,4,1,"ADD A,A") { ctx => ctx.add(ctx.A) })
  addOp(Opcode(0x80,4,1,"ADD A,B") { ctx => ctx.add(ctx.B) })
  addOp(Opcode(0x81,4,1,"ADD A,C") { ctx => ctx.add(ctx.C) })
  addOp(Opcode(0x82,4,1,"ADD A,D") { ctx => ctx.add(ctx.D) })
  addOp(Opcode(0x83,4,1,"ADD A,E") { ctx => ctx.add(ctx.E) })
  addOp(Opcode(0x84,4,1,"ADD A,H") { ctx => ctx.add(ctx.H) })
  addOp(Opcode(0x85,4,1,"ADD A,L") { ctx => ctx.add(ctx.L) })
  // *** ADD A,(HL)
  // **************
  addOp(Opcode(0x86,7,1,"ADD A,(HL)") { ctx => ctx.add(ctx.read(ctx.HL)) })
  // *** ADD A,(IX + d)
  // **************
  addOp(Opcode((0xDD,0x86),19,3,MNEMONIC_IXY_d("ADD A,(IX%s)")) { ctx => ctx.add(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** ADD A,n
  // **************
  addOp(Opcode(0xC6,7,2,MNEMONIC_n("ADD A,%s")) { ctx => ctx.add(ctx.byte(1)) })
  // *** ADC A,r
  // **************
  addOp(Opcode(0x8F,4,1,"ADC A,A") { ctx => ctx.adc(ctx.A) })
  addOp(Opcode(0x88,4,1,"ADC A,B") { ctx => ctx.adc(ctx.B) })
  addOp(Opcode(0x89,4,1,"ADC A,C") { ctx => ctx.adc(ctx.C) })
  addOp(Opcode(0x8A,4,1,"ADC A,D") { ctx => ctx.adc(ctx.D) })
  addOp(Opcode(0x8B,4,1,"ADC A,E") { ctx => ctx.adc(ctx.E) })
  addOp(Opcode(0x8C,4,1,"ADC A,H") { ctx => ctx.adc(ctx.H) })
  addOp(Opcode(0x8D,4,1,"ADC A,L") { ctx => ctx.adc(ctx.L) })
  // *** ADC A,(HL)
  // **************
  addOp(Opcode(0x8E,7,1,"ADC A,L") { ctx => ctx.adc(ctx.read(ctx.HL)) })
  // *** ADC A,(IX + d)
  // **************
  addOp(Opcode((0xDD,0x8E),19,3,MNEMONIC_IXY_d("ADC A,(IX%s)")) { ctx => ctx.adc(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** ADC A,n
  // **************
  addOp(Opcode(0xCE,7,2,MNEMONIC_n("ADC A,%s")) { ctx => ctx.adc(ctx.byte(1)) })
  // UNDOCUMENTED
  // *** ADC A,IXL
  // **************
  addOp(Opcode((0xDD,0x8D),8,2,"ADC IXL") { ctx => ctx.adc(ctx.INDEX_L) })
  // UNDOCUMENTED
  // *** ADC A,IXH
  // **************
  addOp(Opcode((0xDD,0x8C),8,2,"ADC IXH") { ctx => ctx.adc(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** ADD A,IXH
  // **************
  addOp(Opcode((0xDD,0x84),8,2,"ADD IXH") { ctx => ctx.add(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** ADD A,IXL
  // **************
  addOp(Opcode((0xDD,0x85),8,2,"ADD IXL") { ctx => ctx.add(ctx.INDEX_L) })
  // *** SUB r
  // **************
  addOp(Opcode(0x97,4,1,"SUB A") { ctx => ctx.sub(ctx.A) })
  addOp(Opcode(0x90,4,1,"SUB B") { ctx => ctx.sub(ctx.B) })
  addOp(Opcode(0x91,4,1,"SUB C") { ctx => ctx.sub(ctx.C) })
  addOp(Opcode(0x92,4,1,"SUB D") { ctx => ctx.sub(ctx.D) })
  addOp(Opcode(0x93,4,1,"SUB E") { ctx => ctx.sub(ctx.E) })
  addOp(Opcode(0x94,4,1,"SUB H") { ctx => ctx.sub(ctx.H) })
  addOp(Opcode(0x95,4,1,"SUB L") { ctx => ctx.sub(ctx.L) })
  // *** SUB (HL)
  // **************
  addOp(Opcode(0x96,7,1,"SUB (HL)") { ctx => ctx.sub(ctx.read(ctx.HL)) })
  // *** SUB (IX + d)
  // **************
  addOp(Opcode((0xDD,0x96),19,3,MNEMONIC_IXY_d("SUB (IX%s)")) { ctx => ctx.sub(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** SUB n
  // **************
  addOp(Opcode(0xD6,7,2,MNEMONIC_n("SUB %s")) { ctx => ctx.sub(ctx.byte(1)) })
  // *** SBC r
  // **************
  addOp(Opcode(0x9F,4,1,"SBC A,A") { ctx => ctx.sbc(ctx.A) })
  addOp(Opcode(0x98,4,1,"SBC A,B") { ctx => ctx.sbc(ctx.B) })
  addOp(Opcode(0x99,4,1,"SBC A,C") { ctx => ctx.sbc(ctx.C) })
  addOp(Opcode(0x9A,4,1,"SBC A,D") { ctx => ctx.sbc(ctx.D) })
  addOp(Opcode(0x9B,4,1,"SBC A,E") { ctx => ctx.sbc(ctx.E) })
  addOp(Opcode(0x9C,4,1,"SBC A,H") { ctx => ctx.sbc(ctx.H) })
  addOp(Opcode(0x9D,4,1,"SBC A,L") { ctx => ctx.sbc(ctx.L) })
  // *** SBC A,(HL)
  // **************
  addOp(Opcode(0x9E,7,1,"SBC A,(HL)") { ctx => ctx.sbc(ctx.read(ctx.HL)) })
  // *** SBC A,(IX + d)
  // **************
  addOp(Opcode((0xDD,0x9E),19,3,MNEMONIC_IXY_d("SBC A,(IX%s)")) { ctx => ctx.sbc(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** SBC n
  // **************
  addOp(Opcode(0xDE,7,2,MNEMONIC_n("SBC A,%s")) { ctx => ctx.sbc(ctx.byte(1)) })
  // UNDOCUMENTED
  // *** SUB IXH
  // **************
  addOp(Opcode((0xDD,0x94),8,2,"SUB IXH") { ctx => ctx.sub(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** SUB IXL
  // **************
  addOp(Opcode((0xDD,0x95),8,2,"SUB IXL") { ctx => ctx.sub(ctx.INDEX_L) })
  // UNDOCUMENTED
  // *** SBC IXH
  // **************
  addOp(Opcode((0xDD,0x9C),8,2,"SBC IXH") { ctx => ctx.sbc(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** SBC IXL
  // **************
  addOp(Opcode((0xDD,0x9D),8,2,"SBC IXL") { ctx => ctx.sbc(ctx.INDEX_L) })
  // *** AND r
  // **************
  addOp(Opcode(0xA7,4,1,"AND A") { ctx => ctx.and(ctx.A) })
  addOp(Opcode(0xA0,4,1,"AND B") { ctx => ctx.and(ctx.B) })
  addOp(Opcode((0xDD,0xA0),4,2,"AND B") { ctx => ctx.and(ctx.B) })
  addOp(Opcode(0xA1,4,1,"AND C") { ctx => ctx.and(ctx.C) })
  addOp(Opcode(0xA2,4,1,"AND D") { ctx => ctx.and(ctx.D) })
  addOp(Opcode(0xA3,4,1,"AND E") { ctx => ctx.and(ctx.E) })
  addOp(Opcode(0xA4,4,1,"AND H") { ctx => ctx.and(ctx.H) })
  addOp(Opcode(0xA5,4,1,"AND L") { ctx => ctx.and(ctx.L) })
  // UNDOCUMENTED
  // *** AND IXH
  addOp(Opcode((0xDD,0xA4),8,2,"AND IXH") { ctx => ctx.and(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** AND IXL
  addOp(Opcode((0xDD,0xA5),8,2,"AND IXL") { ctx => ctx.and(ctx.INDEX_L) })
  // **************
  // *** AND (HL)
  // **************
  addOp(Opcode(0xA6,7,1,"AND (HL)") { ctx => ctx.and(ctx.read(ctx.HL)) })
  // *** AND (IX + d)
  // **************
  addOp(Opcode((0xDD,0xA6),19,3,MNEMONIC_IXY_d("AND (IX%s)")) { ctx => ctx.and(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** AND n
  // **************
  addOp(Opcode(0xE6,7,2,MNEMONIC_n("AND %s")) { ctx => ctx.and(ctx.byte(1)) })
  // *** XOR r
  // **************
  addOp(Opcode(0xAF,4,1,"XOR A") { ctx => ctx.xor(ctx.A) })
  addOp(Opcode(0xA8,4,1,"XOR B") { ctx => ctx.xor(ctx.B) })
  addOp(Opcode(0xA9,4,1,"XOR C") { ctx => ctx.xor(ctx.C) })
  addOp(Opcode((0xDD,0xA9),4,2,"XOR C") { ctx => ctx.xor(ctx.C) })
  addOp(Opcode(0xAA,4,1,"XOR D") { ctx => ctx.xor(ctx.D) })
  addOp(Opcode(0xAB,4,1,"XOR E") { ctx => ctx.xor(ctx.E) })
  addOp(Opcode(0xAC,4,1,"XOR H") { ctx => ctx.xor(ctx.H) })
  addOp(Opcode(0xAD,4,1,"XOR L") { ctx => ctx.xor(ctx.L) })
  // *** XOR (HL)
  // **************
  addOp(Opcode(0xAE,7,1,"XOR (HL)") { ctx => ctx.xor(ctx.read(ctx.HL)) })
  // *** XOR (IX + d)
  // **************
  addOp(Opcode((0xDD,0xAE),19,3,MNEMONIC_IXY_d("XOR (IX%s)")) { ctx => ctx.xor(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** XOR n
  // **************
  addOp(Opcode(0xEE,7,2,MNEMONIC_n("XOR %s")) { ctx => ctx.xor(ctx.byte(1)) })
  // UNDOCUMENTED
  // *** XOR IXH
  addOp(Opcode((0xDD,0xAC),8,2,"XOR IXH") { ctx => ctx.xor(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** XOR IXL
  addOp(Opcode((0xDD,0xAD),8,2,"XOR IXL") { ctx => ctx.xor(ctx.INDEX_L) })
  // *** OR r
  // **************
  addOp(Opcode(0xB7,4,1,"OR A") { ctx => ctx.or(ctx.A) })
  addOp(Opcode(0xB0,4,1,"OR B") { ctx => ctx.or(ctx.B) })
  addOp(Opcode(0xB1,4,1,"OR C") { ctx => ctx.or(ctx.C) })
  addOp(Opcode(0xB2,4,1,"OR D") { ctx => ctx.or(ctx.D) })
  addOp(Opcode(0xB3,4,1,"OR E") { ctx => ctx.or(ctx.E) })
  addOp(Opcode(0xB4,4,1,"OR H") { ctx => ctx.or(ctx.H) })
  addOp(Opcode(0xB5,4,1,"OR L") { ctx => ctx.or(ctx.L) })
  // *** OR (HL)
  // **************
  addOp(Opcode(0xB6,7,1,"OR (HL)") { ctx => ctx.or(ctx.read(ctx.HL)) })
  // *** OR (IX + d)
  // **************
  addOp(Opcode((0xDD,0xB6),19,3,MNEMONIC_IXY_d("OR (IX%s)")) { ctx => ctx.or(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** OR n
  // **************
  addOp(Opcode(0xF6,7,2,MNEMONIC_n("OR %s")) { ctx => ctx.or(ctx.byte(1)) })
  // UNDOCUMENTED
  // *** OR IXH
  addOp(Opcode((0xDD,0xB4),8,2,"OR IXH") { ctx => ctx.or(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** OR IXL
  addOp(Opcode((0xDD,0xB5),8,2,"OR IXL") { ctx => ctx.or(ctx.INDEX_L) })
  // *** CP r
  // **************
  addOp(Opcode(0xBF,4,1,"CP A") { ctx => ctx.cp(ctx.A) })
  addOp(Opcode(0xB8,4,1,"CP B") { ctx => ctx.cp(ctx.B) })
  addOp(Opcode(0xB9,4,1,"CP C") { ctx => ctx.cp(ctx.C) })
  addOp(Opcode(0xBA,4,1,"CP D") { ctx => ctx.cp(ctx.D) })
  addOp(Opcode(0xBB,4,1,"CP E") { ctx => ctx.cp(ctx.E) })
  addOp(Opcode(0xBC,4,1,"CP H") { ctx => ctx.cp(ctx.H) })
  addOp(Opcode(0xBD,4,1,"CP L") { ctx => ctx.cp(ctx.L) })
  // *** CP (HL)
  // **************
  addOp(Opcode(0xBE,7,1,"CP (HL)") { ctx => ctx.cp(ctx.read(ctx.HL)) })
  // *** CP (IX + d)
  // **************
  addOp(Opcode((0xDD,0xBE),19,3,MNEMONIC_IXY_d("CP (IX%s)")) { ctx => ctx.cp(ctx.read(ctx.INDEX_+(ctx.byte(2)))) })
  // *** CP n
  // **************
  addOp(Opcode(0xFE,7,2,MNEMONIC_n("CP %s")) { ctx => ctx.cp(ctx.byte(1)) })
  // UNDOCUMENTED
  // *** CP IXH
  addOp(Opcode((0xDD,0xBC),8,2,"CP IXH") { ctx => ctx.cp(ctx.INDEX_H) })
  // UNDOCUMENTED
  // *** CP IXL
  addOp(Opcode((0xDD,0xBD),8,2,"CP IXL") { ctx => ctx.cp(ctx.INDEX_L) })
  // *** INC r
  // **************
  @inline private def incdecFlags(ctx:Context,regValueAfter:Int,inc:Boolean) : Unit = {
    val carry = ctx.carry
    ctx.F = ctx.SZP(regValueAfter) | carry ; ctx.Q = true
    if (inc) {
      ctx.setParity(regValueAfter == 0x80)
      ctx.setHalf((regValueAfter & 0x0F) == 0)
      ctx.setNegative(false)
    }
    else {
      ctx.setParity(regValueAfter == 0x7F)
      ctx.setHalf((regValueAfter & 0x0F) == 0x0F)
      ctx.setNegative(true)
    }
    ctx.setXY(regValueAfter)
  }
  addOp(Opcode(0x3C,4,1,"INC A") { ctx => ctx.A = (ctx.A + 1) & 0xFF ; incdecFlags(ctx,ctx.A,inc = true) })
  addOp(Opcode(0x04,4,1,"INC B") { ctx => ctx.B = (ctx.B + 1) & 0xFF ; incdecFlags(ctx,ctx.B,inc = true) })
  addOp(Opcode(0x0C,4,1,"INC C") { ctx => ctx.C = (ctx.C + 1) & 0xFF ; incdecFlags(ctx,ctx.C,inc = true) })
  addOp(Opcode(0x14,4,1,"INC D") { ctx => ctx.D = (ctx.D + 1) & 0xFF ; incdecFlags(ctx,ctx.D,inc = true) })
  addOp(Opcode(0x1C,4,1,"INC E") { ctx => ctx.E = (ctx.E + 1) & 0xFF ; incdecFlags(ctx,ctx.E,inc = true) })
  addOp(Opcode(0x24,4,1,"INC H") { ctx => ctx.H = (ctx.H + 1) & 0xFF ; incdecFlags(ctx,ctx.H,inc = true) })
  addOp(Opcode(0x2C,4,1,"INC L") { ctx => ctx.L = (ctx.L + 1) & 0xFF ; incdecFlags(ctx,ctx.L,inc = true) })
  // *** INC (HL)
  // **************
  addOp(Opcode(0x34,11,1,"INC (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = (ctx.read(adr) + 1) & 0xFF
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,tmp)
    incdecFlags(ctx,tmp,inc = true)
  })
  // *** INC (IX + d)
  // **************
  addOp(Opcode((0xDD,0x34),23,3,MNEMONIC_IXY_d("INC (IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.io.internalOperation(1,addr)
    val tmp = (ctx.read(addr) + 1) & 0xFF
    ctx.write(addr,tmp) ; incdecFlags(ctx,tmp,inc = true)
  })
  // UNDOCUMENTED
  // *** DEC IXH
  addOp(Opcode((0xDD,0x25),8,2,"DEC IXH") { ctx => ctx.INDEX_H = (ctx.INDEX_H - 1) & 0xFF ; incdecFlags(ctx,ctx.INDEX_H,inc = false) })
  // *** DEC IXL
  addOp(Opcode((0xDD,0x2D),8,2,"DEC IXL") { ctx => ctx.INDEX_L = (ctx.INDEX_L - 1) & 0xFF ; incdecFlags(ctx,ctx.INDEX_L,inc = false) })
  // UNDOCUMENTED
  // *** INC IXH
  addOp(Opcode((0xDD,0x24),8,2,"INC IXH") { ctx => ctx.INDEX_H = (ctx.INDEX_H + 1) & 0xFF ; incdecFlags(ctx,ctx.INDEX_H,inc = true) })
  // *** INC IXL
  addOp(Opcode((0xDD,0x2C),8,2,"INC IXL") { ctx => ctx.INDEX_L = (ctx.INDEX_L + 1) & 0xFF ; incdecFlags(ctx,ctx.INDEX_L,inc = true) })
  // *** DEC r
  // **************
  addOp(Opcode(0x3D,4,1,"DEC A") { ctx => ctx.A = (ctx.A - 1) & 0xFF ; incdecFlags(ctx,ctx.A,inc = false) })
  addOp(Opcode(0x05,4,1,"DEC B") { ctx => ctx.B = (ctx.B - 1) & 0xFF ; incdecFlags(ctx,ctx.B,inc = false) })
  addOp(Opcode(0x0D,4,1,"DEC C") { ctx => ctx.C = (ctx.C - 1) & 0xFF ; incdecFlags(ctx,ctx.C,inc = false) })
  addOp(Opcode(0x15,4,1,"DEC D") { ctx => ctx.D = (ctx.D - 1) & 0xFF ; incdecFlags(ctx,ctx.D,inc = false) })
  addOp(Opcode(0x1D,4,1,"DEC E") { ctx => ctx.E = (ctx.E - 1) & 0xFF ; incdecFlags(ctx,ctx.E,inc = false) })
  addOp(Opcode(0x25,4,1,"DEC H") { ctx => ctx.H = (ctx.H - 1) & 0xFF ; incdecFlags(ctx,ctx.H,inc = false) })
  addOp(Opcode(0x2D,4,1,"DEC L") { ctx => ctx.L = (ctx.L - 1) & 0xFF ; incdecFlags(ctx,ctx.L,inc = false) })
  // *** DEC (HL)
  // **************
  addOp(Opcode(0x35,11,1,"DEC (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = (ctx.read(adr) - 1) & 0xFF
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,tmp)
    incdecFlags(ctx,tmp,inc = false)
  })
  // *** DEC (IX + d)
  // **************
  addOp(Opcode((0xDD,0x35),23,3,MNEMONIC_IXY_d("DEC (IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    val tmp = (ctx.read(addr,1) - 1) & 0xFF
    ctx.write(addr,tmp) ; incdecFlags(ctx,tmp,inc = false)
  })
  // ==================================== 16 bit arithmetic ==================================================
  // *** ADD HL,rr
  // **************
  addOp(Opcode(0x09,11,1,"ADD HL,BC") { ctx => ctx.addHLBC })
  addOp(Opcode(0x19,11,1,"ADD HL,DE") { ctx => ctx.addHLDE })
  addOp(Opcode(0x29,11,1,"ADD HL,HL") { ctx => ctx.addHLHL })
  addOp(Opcode(0x39,11,1,"ADD HL,SP") { ctx => ctx.addHLSP })
  addOp(Opcode((0xDD,0x09),15,2,"ADD IX,BC") { ctx => ctx.addIXBC })
  addOp(Opcode((0xDD,0x19),15,2,"ADD IX,DE") { ctx => ctx.addIXDE })
  addOp(Opcode((0xDD,0x39),15,2,"ADD IX,SP") { ctx => ctx.addIXSP })
  addOp(Opcode((0xDD,0x29),15,2,"ADD IX,IX") { ctx => ctx.addIXIX })
  // *** ADC HL,rr
  // **************
  addOp(Opcode((0xED,0x4A),15,2,"ADC HL,BC") { ctx => ctx.adcHL(ctx.BC) })
  addOp(Opcode((0xED,0x5A),15,2,"ADC HL,DE") { ctx => ctx.adcHL(ctx.DE) })
  addOp(Opcode((0xED,0x6A),15,2,"ADC HL,HL") { ctx => ctx.adcHL(ctx.HL) })
  addOp(Opcode((0xED,0x7A),15,2,"ADC HL,SP") { ctx => ctx.adcHL(ctx.SP) })
  // *** SBC HL,rr
  // **************
  addOp(Opcode((0xED,0x42),15,2,"SBC HL,BC") { ctx => ctx.sbcHL(ctx.BC) })
  addOp(Opcode((0xED,0x52),15,2,"SBC HL,DE") { ctx => ctx.sbcHL(ctx.DE) })
  addOp(Opcode((0xED,0x62),15,2,"SBC HL,HL") { ctx => ctx.sbcHL(ctx.HL) })
  addOp(Opcode((0xED,0x72),15,2,"SBC HL,SP") { ctx => ctx.sbcHL(ctx.SP) })
  // *** INC rr
  // **************
  addOp(Opcode(0x03,6,1,"INC BC") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecBC(true) })
  addOp(Opcode(0x13,6,1,"INC DE") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecDE(true) })
  addOp(Opcode(0x23,6,1,"INC HL") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecHL(true) })
  addOp(Opcode(0x33,6,1,"INC SP") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecSP(true) })
  addOp(Opcode((0xDD,0x23),10,2,"INC IX") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecIndex(true) })
  // *** DEC rr
  // **************
  addOp(Opcode(0x0B,6,1,"DEC BC") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecBC(false) })
  addOp(Opcode(0x1B,6,1,"DEC DE") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecDE(false) })
  addOp(Opcode(0x2B,6,1,"DEC HL") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecHL(false) })
  addOp(Opcode(0x3B,6,1,"DEC SP") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecSP(false) })
  addOp(Opcode((0xDD,0x2B),10,2,"DEC IX") { ctx => ctx.io.internalOperation(2,ctx.IR) ; ctx.incDecIndex(false) })
  // ==================================== General Purpose Arithmetic and Control Groups ======================
  // *** NOP
  // **************
  private val NOP = Opcode(0x00,4,1,"NOP") { ctx => }
  addOp(NOP)
  // *** HALT
  // **************
  addOp(Opcode(0x76,4,1,"HALT",modifyPC = true) { ctx => ctx.halted = true })
  // *** EI
  // **************
  addOp(Opcode(0xFB,4,1,"EI") { ctx => ctx.IFF1 = 1 ; ctx.IFF2 = 1 ; ctx.setDelayInt(true) })
  // *** DI
  // **************
  addOp(Opcode(0xF3,4,1,"DI") { ctx => ctx.IFF1 = 0 ; ctx.IFF2 = 0 })
  // *** IM x
  // **************
  addOp(Opcode((0xED,0x46),8,2,"IM 0",false,Array(0x4E,0x6E,0x66)) { ctx => ctx.im = 0 })
  addOp(Opcode((0xED,0x56),8,2,"IM 1",false,Array(0x76)) { ctx => ctx.im = 1 })
  addOp(Opcode((0xED,0x5E),8,2,"IM 2",false,Array(0x7E)) { ctx => ctx.im = 2 })
  // *** CPL
  // **************
  addOp(Opcode(0x2F,4,1,"CPL") { ctx => ctx.cpl })
  // *** NEG
  // **************
  addOp(Opcode((0xED,0x44),8,2,"NEG",false,Array(0x54,0x64,0x74,0x4C,0x5C,0x6C,0x7C)) { ctx => ctx.neg })
  // *** CCF
  // **************
  addOp(Opcode(0x3F,4,1,"CCF") { ctx => ctx.ccf })
  // *** SCF
  // **************
  addOp(Opcode(0x37,4,1,"SCF") { ctx => ctx.scf })
  // *** DAA
  // **************
  addOp(Opcode(0x27,4,1,"DAA") { ctx => ctx.daa })
  // ==================================== Rotate and Shift Group =============================================
  // *** RLC r
  // **************
  addOp(Opcode((0xCB,0x07),8,2,"RLC A") { ctx => ctx.A = ctx.rotLC(ctx.A) })
  addOp(Opcode((0xCB,0x00),8,2,"RLC B") { ctx => ctx.B = ctx.rotLC(ctx.B) })
  addOp(Opcode((0xCB,0x01),8,2,"RLC C") { ctx => ctx.C = ctx.rotLC(ctx.C) })
  addOp(Opcode((0xCB,0x02),8,2,"RLC D") { ctx => ctx.D = ctx.rotLC(ctx.D) })
  addOp(Opcode((0xCB,0x03),8,2,"RLC E") { ctx => ctx.E = ctx.rotLC(ctx.E) })
  addOp(Opcode((0xCB,0x04),8,2,"RLC H") { ctx => ctx.H = ctx.rotLC(ctx.H) })
  addOp(Opcode((0xCB,0x05),8,2,"RLC L") { ctx => ctx.L = ctx.rotLC(ctx.L) })
  // *** RLC (HL)
  // **************
  addOp(Opcode((0xCB,0x06),15,2,"RLC (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotLC(tmp))
  })
  // *** RLC (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x06),23,4,MNEMONIC_IXY_d("RLC (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotLC(tmp))
  })
  // *** RRC r
  // **************
  addOp(Opcode((0xCB,0x0F),8,2,"RRC A") { ctx => ctx.A = ctx.rotRC(ctx.A) })
  addOp(Opcode((0xCB,0x08),8,2,"RRC B") { ctx => ctx.B = ctx.rotRC(ctx.B) })
  addOp(Opcode((0xCB,0x09),8,2,"RRC C") { ctx => ctx.C = ctx.rotRC(ctx.C) })
  addOp(Opcode((0xCB,0x0A),8,2,"RRC D") { ctx => ctx.D = ctx.rotRC(ctx.D) })
  addOp(Opcode((0xCB,0x0B),8,2,"RRC E") { ctx => ctx.E = ctx.rotRC(ctx.E) })
  addOp(Opcode((0xCB,0x0C),8,2,"RRC H") { ctx => ctx.H = ctx.rotRC(ctx.H) })
  addOp(Opcode((0xCB,0x0D),8,2,"RRC L") { ctx => ctx.L = ctx.rotRC(ctx.L) })
  // *** RRC (HL)
  // **************
  addOp(Opcode((0xCB,0x0E),15,2,"RRC (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotRC(tmp))
  })
  // *** RRC (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x0E),23,4,MNEMONIC_IXY_d("RRC (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotRC(tmp))
  })
  // *** RL r
  // **************
  addOp(Opcode((0xCB,0x17),8,2,"RL A") { ctx => ctx.A = ctx.rotL(ctx.A) })
  addOp(Opcode((0xCB,0x10),8,2,"RL B") { ctx => ctx.B = ctx.rotL(ctx.B) })
  addOp(Opcode((0xCB,0x11),8,2,"RL C") { ctx => ctx.C = ctx.rotL(ctx.C) })
  addOp(Opcode((0xCB,0x12),8,2,"RL D") { ctx => ctx.D = ctx.rotL(ctx.D) })
  addOp(Opcode((0xCB,0x13),8,2,"RL E") { ctx => ctx.E = ctx.rotL(ctx.E) })
  addOp(Opcode((0xCB,0x14),8,2,"RL H") { ctx => ctx.H = ctx.rotL(ctx.H) })
  addOp(Opcode((0xCB,0x15),8,2,"RL L") { ctx => ctx.L = ctx.rotL(ctx.L) })
  // *** RL (HL)
  // **************
  addOp(Opcode((0xCB,0x16),15,2,"RL (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotL(tmp))
  })
  // *** RL (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x16),23,4,MNEMONIC_IXY_d("RL (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotL(tmp))
  })
  // *** RR r
  // **************
  addOp(Opcode((0xCB,0x1F),8,2,"RR A") { ctx => ctx.A = ctx.rotR(ctx.A) })
  addOp(Opcode((0xCB,0x18),8,2,"RR B") { ctx => ctx.B = ctx.rotR(ctx.B) })
  addOp(Opcode((0xCB,0x19),8,2,"RR C") { ctx => ctx.C = ctx.rotR(ctx.C) })
  addOp(Opcode((0xCB,0x1A),8,2,"RR D") { ctx => ctx.D = ctx.rotR(ctx.D) })
  addOp(Opcode((0xCB,0x1B),8,2,"RR E") { ctx => ctx.E = ctx.rotR(ctx.E) })
  addOp(Opcode((0xCB,0x1C),8,2,"RR H") { ctx => ctx.H = ctx.rotR(ctx.H) })
  addOp(Opcode((0xCB,0x1D),8,2,"RR L") { ctx => ctx.L = ctx.rotR(ctx.L) })
  // *** RR (HL)
  // **************
  addOp(Opcode((0xCB,0x1E),15,2,"RR (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotR(tmp))
  })
  // *** RR (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x1E),23,4,MNEMONIC_IXY_d("RR (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.rotR(tmp))
  })
  // *** SLA r
  // **************
  addOp(Opcode((0xCB,0x27),8,2,"SLA A") { ctx => ctx.A = ctx.sla(ctx.A) })
  addOp(Opcode((0xCB,0x20),8,2,"SLA B") { ctx => ctx.B = ctx.sla(ctx.B) })
  addOp(Opcode((0xCB,0x21),8,2,"SLA C") { ctx => ctx.C = ctx.sla(ctx.C) })
  addOp(Opcode((0xCB,0x22),8,2,"SLA D") { ctx => ctx.D = ctx.sla(ctx.D) })
  addOp(Opcode((0xCB,0x23),8,2,"SLA E") { ctx => ctx.E = ctx.sla(ctx.E) })
  addOp(Opcode((0xCB,0x24),8,2,"SLA H") { ctx => ctx.H = ctx.sla(ctx.H) })
  addOp(Opcode((0xCB,0x25),8,2,"SLA L") { ctx => ctx.L = ctx.sla(ctx.L) })
  // *** SLA (HL)
  // **************
  addOp(Opcode((0xCB,0x26),15,2,"SLA (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sla(tmp))
  })
  // *** SLA (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x26),23,4,MNEMONIC_IXY_d("SLA (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sla(tmp))
  })
  // UNDOCUMENTED
  // SLL r
  addOp(Opcode((0xCB,0x30),8,2,"SLL B") { ctx => ctx.B = ctx.sla(ctx.B, 1) })
  addOp(Opcode((0xCB,0x31),8,2,"SLL C") { ctx => ctx.C = ctx.sla(ctx.C, 1) })
  addOp(Opcode((0xCB,0x32),8,2,"SLL D") { ctx => ctx.D = ctx.sla(ctx.D, 1) })
  addOp(Opcode((0xCB,0x33),8,2,"SLL E") { ctx => ctx.E = ctx.sla(ctx.E, 1) })
  addOp(Opcode((0xCB,0x34),8,2,"SLL H") { ctx => ctx.H = ctx.sla(ctx.H, 1) })
  addOp(Opcode((0xCB,0x35),8,2,"SLL L") { ctx => ctx.L = ctx.sla(ctx.L, 1) })
  // UNDOCUMENTED
  // SLL (HL)
  addOp(Opcode((0xCB,0x36),15,2,"SLL (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sla(tmp, 1))
  })
  // UNDOCUMENTED
  // SLL A
  addOp(Opcode((0xCB,0x37),8,2,"SLL A") { ctx => ctx.A = ctx.sla(ctx.A, 1) })
  // UNDOCUMENTED
  // *** SLL (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x36),23,4,MNEMONIC_IXY_d("SLL (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sla(tmp, 1))
  })
  // *** SRA r
  // **************
  addOp(Opcode((0xCB,0x2F),8,2,"SRA A") { ctx => ctx.A = ctx.sra(ctx.A) })
  addOp(Opcode((0xCB,0x28),8,2,"SRA B") { ctx => ctx.B = ctx.sra(ctx.B) })
  addOp(Opcode((0xCB,0x29),8,2,"SRA C") { ctx => ctx.C = ctx.sra(ctx.C) })
  addOp(Opcode((0xCB,0x2A),8,2,"SRA D") { ctx => ctx.D = ctx.sra(ctx.D) })
  addOp(Opcode((0xCB,0x2B),8,2,"SRA E") { ctx => ctx.E = ctx.sra(ctx.E) })
  addOp(Opcode((0xCB,0x2C),8,2,"SRA H") { ctx => ctx.H = ctx.sra(ctx.H) })
  addOp(Opcode((0xCB,0x2D),8,2,"SRA L") { ctx => ctx.L = ctx.sra(ctx.L) })
  // *** SRA (HL)
  // **************
  addOp(Opcode((0xCB,0x2E),15,2,"SRA (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sra(tmp))
  })
  // *** SRA (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x2E),23,4,MNEMONIC_IXY_d("SRA (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.sra(tmp))
  })
  // *** SRL r
  // **************
  addOp(Opcode((0xCB,0x3F),8,2,"SRL A") { ctx => ctx.A = ctx.srl(ctx.A) })
  addOp(Opcode((0xCB,0x38),8,2,"SRL B") { ctx => ctx.B = ctx.srl(ctx.B) })
  addOp(Opcode((0xCB,0x39),8,2,"SRL C") { ctx => ctx.C = ctx.srl(ctx.C) })
  addOp(Opcode((0xCB,0x3A),8,2,"SRL D") { ctx => ctx.D = ctx.srl(ctx.D) })
  addOp(Opcode((0xCB,0x3B),8,2,"SRL E") { ctx => ctx.E = ctx.srl(ctx.E) })
  addOp(Opcode((0xCB,0x3C),8,2,"SRL H") { ctx => ctx.H = ctx.srl(ctx.H) })
  addOp(Opcode((0xCB,0x3D),8,2,"SRL L") { ctx => ctx.L = ctx.srl(ctx.L) })
  // *** SRL (HL)
  // **************
  addOp(Opcode((0xCB,0x3E),15,2,"SRL (HL)") { ctx =>
    val adr = ctx.HL
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.srl(tmp))
  })
  // *** SRL (IX+d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x3E),23,4,MNEMONIC_IXY_d("SRL (IX%s)")) { ctx =>
    val adr = ctx.INDEX_+(ctx.byte(2))
    val tmp = ctx.read(adr)
    ctx.io.internalOperation(1,adr)
    ctx.write(adr,ctx.srl(tmp))
  })
  // *** RLCA
  // **************
  addOp(Opcode(0x07,4,1,"RLCA") { ctx => ctx.rlca() })
  // *** RRCA
  // **************
  addOp(Opcode(0x0F,4,1,"RRCA") { ctx => ctx.rrca() })
  // *** RLA
  // **************
  addOp(Opcode(0x17,4,1,"RLA") { ctx => ctx.rla() })
  // *** RRA
  // **************
  addOp(Opcode(0x1F,4,1,"RRA") { ctx => ctx.rra() })
  // *** RLD
  // **************
  addOp(Opcode((0xED,0x6F),18,2,"RLD") { ctx => ctx.rld })
  // *** RRD
  // **************
  addOp(Opcode((0xED,0x67),18,2,"RRD") { ctx => ctx.rrd })
  // ==================================== Bit manipulation ===================================================
  // *** BIT b,r
  // **************
  addOp(Opcode((0xCB,0x47),8,2,"BIT 0,A") { ctx => ctx.bit(0, ctx.A) })
  addOp(Opcode((0xCB,0x40),8,2,"BIT 0,B") { ctx => ctx.bit(0, ctx.B) })
  addOp(Opcode((0xCB,0x41),8,2,"BIT 0,C") { ctx => ctx.bit(0, ctx.C) })
  addOp(Opcode((0xCB,0x42),8,2,"BIT 0,D") { ctx => ctx.bit(0, ctx.D) })
  addOp(Opcode((0xCB,0x43),8,2,"BIT 0,E") { ctx => ctx.bit(0, ctx.E) })
  addOp(Opcode((0xCB,0x44),8,2,"BIT 0,H") { ctx => ctx.bit(0, ctx.H) })
  addOp(Opcode((0xCB,0x45),8,2,"BIT 0,L") { ctx => ctx.bit(0, ctx.L) })
  addOp(Opcode((0xCB,0x4F),8,2,"BIT 1,A") { ctx => ctx.bit(1, ctx.A) })
  addOp(Opcode((0xCB,0x48),8,2,"BIT 1,B") { ctx => ctx.bit(1, ctx.B) })
  addOp(Opcode((0xCB,0x49),8,2,"BIT 1,C") { ctx => ctx.bit(1, ctx.C) })
  addOp(Opcode((0xCB,0x4A),8,2,"BIT 1,D") { ctx => ctx.bit(1, ctx.D) })
  addOp(Opcode((0xCB,0x4B),8,2,"BIT 1,E") { ctx => ctx.bit(1, ctx.E) })
  addOp(Opcode((0xCB,0x4C),8,2,"BIT 1,H") { ctx => ctx.bit(1, ctx.H) })
  addOp(Opcode((0xCB,0x4D),8,2,"BIT 1,L") { ctx => ctx.bit(1, ctx.L) })
  addOp(Opcode((0xCB,0x57),8,2,"BIT 2,A") { ctx => ctx.bit(2, ctx.A) })
  addOp(Opcode((0xCB,0x50),8,2,"BIT 2,B") { ctx => ctx.bit(2, ctx.B) })
  addOp(Opcode((0xCB,0x51),8,2,"BIT 2,C") { ctx => ctx.bit(2, ctx.C) })
  addOp(Opcode((0xCB,0x52),8,2,"BIT 2,D") { ctx => ctx.bit(2, ctx.D) })
  addOp(Opcode((0xCB,0x53),8,2,"BIT 2,E") { ctx => ctx.bit(2, ctx.E) })
  addOp(Opcode((0xCB,0x54),8,2,"BIT 2,H") { ctx => ctx.bit(2, ctx.H) })
  addOp(Opcode((0xCB,0x55),8,2,"BIT 2,L") { ctx => ctx.bit(2, ctx.L) })
  addOp(Opcode((0xCB,0x5F),8,2,"BIT 3,A") { ctx => ctx.bit(3, ctx.A) })
  addOp(Opcode((0xCB,0x58),8,2,"BIT 3,B") { ctx => ctx.bit(3, ctx.B) })
  addOp(Opcode((0xCB,0x59),8,2,"BIT 3,C") { ctx => ctx.bit(3, ctx.C) })
  addOp(Opcode((0xCB,0x5A),8,2,"BIT 3,D") { ctx => ctx.bit(3, ctx.D) })
  addOp(Opcode((0xCB,0x5B),8,2,"BIT 3,E") { ctx => ctx.bit(3, ctx.E) })
  addOp(Opcode((0xCB,0x5C),8,2,"BIT 3,H") { ctx => ctx.bit(3, ctx.H) })
  addOp(Opcode((0xCB,0x5D),8,2,"BIT 3,L") { ctx => ctx.bit(3, ctx.L) })
  addOp(Opcode((0xCB,0x67),8,2,"BIT 4,A") { ctx => ctx.bit(4, ctx.A) })
  addOp(Opcode((0xCB,0x60),8,2,"BIT 4,B") { ctx => ctx.bit(4, ctx.B) })
  addOp(Opcode((0xCB,0x61),8,2,"BIT 4,C") { ctx => ctx.bit(4, ctx.C) })
  addOp(Opcode((0xCB,0x62),8,2,"BIT 4,D") { ctx => ctx.bit(4, ctx.D) })
  addOp(Opcode((0xCB,0x63),8,2,"BIT 4,E") { ctx => ctx.bit(4, ctx.E) })
  addOp(Opcode((0xCB,0x64),8,2,"BIT 4,H") { ctx => ctx.bit(4, ctx.H) })
  addOp(Opcode((0xCB,0x65),8,2,"BIT 4,L") { ctx => ctx.bit(4, ctx.L) })
  addOp(Opcode((0xCB,0x6F),8,2,"BIT 5,A") { ctx => ctx.bit(5, ctx.A) })
  addOp(Opcode((0xCB,0x68),8,2,"BIT 5,B") { ctx => ctx.bit(5, ctx.B) })
  addOp(Opcode((0xCB,0x69),8,2,"BIT 5,C") { ctx => ctx.bit(5, ctx.C) })
  addOp(Opcode((0xCB,0x6A),8,2,"BIT 5,D") { ctx => ctx.bit(5, ctx.D) })
  addOp(Opcode((0xCB,0x6B),8,2,"BIT 5,E") { ctx => ctx.bit(5, ctx.E) })
  addOp(Opcode((0xCB,0x6C),8,2,"BIT 5,H") { ctx => ctx.bit(5, ctx.H) })
  addOp(Opcode((0xCB,0x6D),8,2,"BIT 5,L") { ctx => ctx.bit(5, ctx.L) })
  addOp(Opcode((0xCB,0x77),8,2,"BIT 6,A") { ctx => ctx.bit(6, ctx.A) })
  addOp(Opcode((0xCB,0x70),8,2,"BIT 6,B") { ctx => ctx.bit(6, ctx.B) })
  addOp(Opcode((0xCB,0x71),8,2,"BIT 6,C") { ctx => ctx.bit(6, ctx.C) })
  addOp(Opcode((0xCB,0x72),8,2,"BIT 6,D") { ctx => ctx.bit(6, ctx.D) })
  addOp(Opcode((0xCB,0x73),8,2,"BIT 6,E") { ctx => ctx.bit(6, ctx.E) })
  addOp(Opcode((0xCB,0x74),8,2,"BIT 6,H") { ctx => ctx.bit(6, ctx.H) })
  addOp(Opcode((0xCB,0x75),8,2,"BIT 6,L") { ctx => ctx.bit(6, ctx.L) })
  addOp(Opcode((0xCB,0x7F),8,2,"BIT 7,A") { ctx => ctx.bit(7, ctx.A) })
  addOp(Opcode((0xCB,0x78),8,2,"BIT 7,B") { ctx => ctx.bit(7, ctx.B) })
  addOp(Opcode((0xCB,0x79),8,2,"BIT 7,C") { ctx => ctx.bit(7, ctx.C) })
  addOp(Opcode((0xCB,0x7A),8,2,"BIT 7,D") { ctx => ctx.bit(7, ctx.D) })
  addOp(Opcode((0xCB,0x7B),8,2,"BIT 7,E") { ctx => ctx.bit(7, ctx.E) })
  addOp(Opcode((0xCB,0x7C),8,2,"BIT 7,H") { ctx => ctx.bit(7, ctx.H) })
  addOp(Opcode((0xCB,0x7D),8,2,"BIT 7,L") { ctx => ctx.bit(7, ctx.L) })
  // *** BIT b,(HL)
  // **************
  addOp(Opcode((0xCB,0x46),12,2,"BIT 0,(HL)") { ctx => ctx.bit(0, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x4E),12,2,"BIT 1,(HL)") { ctx => ctx.bit(1, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x56),12,2,"BIT 2,(HL)") { ctx => ctx.bit(2, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x5E),12,2,"BIT 3,(HL)") { ctx => ctx.bit(3, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x66),12,2,"BIT 4,(HL)") { ctx => ctx.bit(4, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x6E),12,2,"BIT 5,(HL)") { ctx => ctx.bit(5, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x76),12,2,"BIT 6,(HL)") { ctx => ctx.bit(6, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  addOp(Opcode((0xCB,0x7E),12,2,"BIT 7,(HL)") { ctx => ctx.bit(7, ctx.read(ctx.HL), 2) ; ctx.io.internalOperation(1,ctx.HL) })
  // *** BIT b,(IX + d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x46),20,4,MNEMONIC_IXY_d("BIT 0,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(0, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x4E),20,4,MNEMONIC_IXY_d("BIT 1,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(1, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x56),20,4,MNEMONIC_IXY_d("BIT 2,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(2, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x5E),20,4,MNEMONIC_IXY_d("BIT 3,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(3, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x66),20,4,MNEMONIC_IXY_d("BIT 4,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(4, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x6E),20,4,MNEMONIC_IXY_d("BIT 5,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(5, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x76),20,4,MNEMONIC_IXY_d("BIT 6,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(6, ctx.read(addr,1), 1)
  })
  addOp(Opcode((0xDD,0xCB,0x7E),20,4,MNEMONIC_IXY_d("BIT 7,(IX%s)")) { ctx =>
    val addr = ctx.INDEX_+(ctx.byte(2))
    ctx.bit(7, ctx.read(addr,1), 1)
  })
  // *** RES b,r
  // **************
  addOp(Opcode((0xCB,0x87),8,2,"RES 0,A") { ctx => ctx.A = ctx.res(0,ctx.A) })
  addOp(Opcode((0xCB,0x80),8,2,"RES 0,B") { ctx => ctx.B = ctx.res(0,ctx.B) })
  addOp(Opcode((0xCB,0x81),8,2,"RES 0,C") { ctx => ctx.C = ctx.res(0,ctx.C) })
  addOp(Opcode((0xCB,0x82),8,2,"RES 0,D") { ctx => ctx.D = ctx.res(0,ctx.D) })
  addOp(Opcode((0xCB,0x83),8,2,"RES 0,E") { ctx => ctx.E = ctx.res(0,ctx.E) })
  addOp(Opcode((0xCB,0x84),8,2,"RES 0,H") { ctx => ctx.H = ctx.res(0,ctx.H) })
  addOp(Opcode((0xCB,0x85),8,2,"RES 0,L") { ctx => ctx.L = ctx.res(0,ctx.L) })
  addOp(Opcode((0xCB,0x8F),8,2,"RES 1,A") { ctx => ctx.A = ctx.res(1,ctx.A) })
  addOp(Opcode((0xCB,0x88),8,2,"RES 1,B") { ctx => ctx.B = ctx.res(1,ctx.B) })
  addOp(Opcode((0xCB,0x89),8,2,"RES 1,C") { ctx => ctx.C = ctx.res(1,ctx.C) })
  addOp(Opcode((0xCB,0x8A),8,2,"RES 1,D") { ctx => ctx.D = ctx.res(1,ctx.D) })
  addOp(Opcode((0xCB,0x8B),8,2,"RES 1,E") { ctx => ctx.E = ctx.res(1,ctx.E) })
  addOp(Opcode((0xCB,0x8C),8,2,"RES 1,H") { ctx => ctx.H = ctx.res(1,ctx.H) })
  addOp(Opcode((0xCB,0x8D),8,2,"RES 1,L") { ctx => ctx.L = ctx.res(1,ctx.L) })
  addOp(Opcode((0xCB,0x97),8,2,"RES 2,A") { ctx => ctx.A = ctx.res(2,ctx.A) })
  addOp(Opcode((0xCB,0x90),8,2,"RES 2,B") { ctx => ctx.B = ctx.res(2,ctx.B) })
  addOp(Opcode((0xCB,0x91),8,2,"RES 2,C") { ctx => ctx.C = ctx.res(2,ctx.C) })
  addOp(Opcode((0xCB,0x92),8,2,"RES 2,D") { ctx => ctx.D = ctx.res(2,ctx.D) })
  addOp(Opcode((0xCB,0x93),8,2,"RES 2,E") { ctx => ctx.E = ctx.res(2,ctx.E) })
  addOp(Opcode((0xCB,0x94),8,2,"RES 2,H") { ctx => ctx.H = ctx.res(2,ctx.H) })
  addOp(Opcode((0xCB,0x95),8,2,"RES 2,L") { ctx => ctx.L = ctx.res(2,ctx.L) })
  addOp(Opcode((0xCB,0x9F),8,2,"RES 3,A") { ctx => ctx.A = ctx.res(3,ctx.A) })
  addOp(Opcode((0xCB,0x98),8,2,"RES 3,B") { ctx => ctx.B = ctx.res(3,ctx.B) })
  addOp(Opcode((0xCB,0x99),8,2,"RES 3,C") { ctx => ctx.C = ctx.res(3,ctx.C) })
  addOp(Opcode((0xCB,0x9A),8,2,"RES 3,D") { ctx => ctx.D = ctx.res(3,ctx.D) })
  addOp(Opcode((0xCB,0x9B),8,2,"RES 3,E") { ctx => ctx.E = ctx.res(3,ctx.E) })
  addOp(Opcode((0xCB,0x9C),8,2,"RES 3,H") { ctx => ctx.H = ctx.res(3,ctx.H) })
  addOp(Opcode((0xCB,0x9D),8,2,"RES 3,L") { ctx => ctx.L = ctx.res(3,ctx.L) })
  addOp(Opcode((0xCB,0xA7),8,2,"RES 4,A") { ctx => ctx.A = ctx.res(4,ctx.A) })
  addOp(Opcode((0xCB,0xA0),8,2,"RES 4,B") { ctx => ctx.B = ctx.res(4,ctx.B) })
  addOp(Opcode((0xCB,0xA1),8,2,"RES 4,C") { ctx => ctx.C = ctx.res(4,ctx.C) })
  addOp(Opcode((0xCB,0xA2),8,2,"RES 4,D") { ctx => ctx.D = ctx.res(4,ctx.D) })
  addOp(Opcode((0xCB,0xA3),8,2,"RES 4,E") { ctx => ctx.E = ctx.res(4,ctx.E) })
  addOp(Opcode((0xCB,0xA4),8,2,"RES 4,H") { ctx => ctx.H = ctx.res(4,ctx.H) })
  addOp(Opcode((0xCB,0xA5),8,2,"RES 4,L") { ctx => ctx.L = ctx.res(4,ctx.L) })
  addOp(Opcode((0xCB,0xAF),8,2,"RES 5,A") { ctx => ctx.A = ctx.res(5,ctx.A) })
  addOp(Opcode((0xCB,0xA8),8,2,"RES 5,B") { ctx => ctx.B = ctx.res(5,ctx.B) })
  addOp(Opcode((0xCB,0xA9),8,2,"RES 5,C") { ctx => ctx.C = ctx.res(5,ctx.C) })
  addOp(Opcode((0xCB,0xAA),8,2,"RES 5,D") { ctx => ctx.D = ctx.res(5,ctx.D) })
  addOp(Opcode((0xCB,0xAB),8,2,"RES 5,E") { ctx => ctx.E = ctx.res(5,ctx.E) })
  addOp(Opcode((0xCB,0xAC),8,2,"RES 5,H") { ctx => ctx.H = ctx.res(5,ctx.H) })
  addOp(Opcode((0xCB,0xAD),8,2,"RES 5,L") { ctx => ctx.L = ctx.res(5,ctx.L) })
  addOp(Opcode((0xCB,0xB7),8,2,"RES 6,A") { ctx => ctx.A = ctx.res(6,ctx.A) })
  addOp(Opcode((0xCB,0xB0),8,2,"RES 6,B") { ctx => ctx.B = ctx.res(6,ctx.B) })
  addOp(Opcode((0xCB,0xB1),8,2,"RES 6,C") { ctx => ctx.C = ctx.res(6,ctx.C) })
  addOp(Opcode((0xCB,0xB2),8,2,"RES 6,D") { ctx => ctx.D = ctx.res(6,ctx.D) })
  addOp(Opcode((0xCB,0xB3),8,2,"RES 6,E") { ctx => ctx.E = ctx.res(6,ctx.E) })
  addOp(Opcode((0xCB,0xB4),8,2,"RES 6,H") { ctx => ctx.H = ctx.res(6,ctx.H) })
  addOp(Opcode((0xCB,0xB5),8,2,"RES 6,L") { ctx => ctx.L = ctx.res(6,ctx.L) })
  addOp(Opcode((0xCB,0xBF),8,2,"RES 7,A") { ctx => ctx.A = ctx.res(7,ctx.A) })
  addOp(Opcode((0xCB,0xB8),8,2,"RES 7,B") { ctx => ctx.B = ctx.res(7,ctx.B) })
  addOp(Opcode((0xCB,0xB9),8,2,"RES 7,C") { ctx => ctx.C = ctx.res(7,ctx.C) })
  addOp(Opcode((0xCB,0xBA),8,2,"RES 7,D") { ctx => ctx.D = ctx.res(7,ctx.D) })
  addOp(Opcode((0xCB,0xBB),8,2,"RES 7,E") { ctx => ctx.E = ctx.res(7,ctx.E) })
  addOp(Opcode((0xCB,0xBC),8,2,"RES 7,H") { ctx => ctx.H = ctx.res(7,ctx.H) })
  addOp(Opcode((0xCB,0xBD),8,2,"RES 7,L") { ctx => ctx.L = ctx.res(7,ctx.L) })
  // *** RES b,(HL)
  // **************
  addOp(Opcode((0xCB,0x86),15,2,"RES 0,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(0,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0x8E),15,2,"RES 1,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(1,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0x96),15,2,"RES 2,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(2,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0x9E),15,2,"RES 3,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(3,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0xA6),15,2,"RES 4,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(4,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0xAE),15,2,"RES 5,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(5,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0xB6),15,2,"RES 6,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(6,ctx.read(ctx.HL,1))) })
  addOp(Opcode((0xCB,0xBE),15,2,"RES 7,(HL)") { ctx => ctx.write(ctx.HL,ctx.res(7,ctx.read(ctx.HL,1))) })
  // *** RES b,(IX + d)
  // **************
  addOp(Opcode((0xDD,0xCB,0x86),23,4,MNEMONIC_IXY_d("RES 0,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(0,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0x8E),23,4,MNEMONIC_IXY_d("RES 1,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(1,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0x96),23,4,MNEMONIC_IXY_d("RES 2,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(2,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0x9E),23,4,MNEMONIC_IXY_d("RES 3,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(3,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xA6),23,4,MNEMONIC_IXY_d("RES 4,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(4,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xAE),23,4,MNEMONIC_IXY_d("RES 5,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(5,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xB6),23,4,MNEMONIC_IXY_d("RES 6,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(6,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xBE),23,4,MNEMONIC_IXY_d("RES 7,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.res(7,ctx.read(adr,1))) })
  // *** SET b,r
  // **************
  addOp(Opcode((0xCB,0xC7),8,2,"SET 0,A") { ctx => ctx.A = ctx.set(0,ctx.A) })
  addOp(Opcode((0xCB,0xC0),8,2,"SET 0,B") { ctx => ctx.B = ctx.set(0,ctx.B) })
  addOp(Opcode((0xCB,0xC1),8,2,"SET 0,C") { ctx => ctx.C = ctx.set(0,ctx.C) })
  addOp(Opcode((0xCB,0xC2),8,2,"SET 0,D") { ctx => ctx.D = ctx.set(0,ctx.D) })
  addOp(Opcode((0xCB,0xC3),8,2,"SET 0,E") { ctx => ctx.E = ctx.set(0,ctx.E) })
  addOp(Opcode((0xCB,0xC4),8,2,"SET 0,H") { ctx => ctx.H = ctx.set(0,ctx.H) })
  addOp(Opcode((0xCB,0xC5),8,2,"SET 0,L") { ctx => ctx.L = ctx.set(0,ctx.L) })
  addOp(Opcode((0xCB,0xCF),8,2,"SET 1,A") { ctx => ctx.A = ctx.set(1,ctx.A) })
  addOp(Opcode((0xCB,0xC8),8,2,"SET 1,B") { ctx => ctx.B = ctx.set(1,ctx.B) })
  addOp(Opcode((0xCB,0xC9),8,2,"SET 1,C") { ctx => ctx.C = ctx.set(1,ctx.C) })
  addOp(Opcode((0xCB,0xCA),8,2,"SET 1,D") { ctx => ctx.D = ctx.set(1,ctx.D) })
  addOp(Opcode((0xCB,0xCB),8,2,"SET 1,E") { ctx => ctx.E = ctx.set(1,ctx.E) })
  addOp(Opcode((0xCB,0xCC),8,2,"SET 1,H") { ctx => ctx.H = ctx.set(1,ctx.H) })
  addOp(Opcode((0xCB,0xCD),8,2,"SET 1,L") { ctx => ctx.L = ctx.set(1,ctx.L) })
  addOp(Opcode((0xCB,0xD7),8,2,"SET 2,A") { ctx => ctx.A = ctx.set(2,ctx.A) })
  addOp(Opcode((0xCB,0xD0),8,2,"SET 2,B") { ctx => ctx.B = ctx.set(2,ctx.B) })
  addOp(Opcode((0xCB,0xD1),8,2,"SET 2,C") { ctx => ctx.C = ctx.set(2,ctx.C) })
  addOp(Opcode((0xCB,0xD2),8,2,"SET 2,D") { ctx => ctx.D = ctx.set(2,ctx.D) })
  addOp(Opcode((0xCB,0xD3),8,2,"SET 2,E") { ctx => ctx.E = ctx.set(2,ctx.E) })
  addOp(Opcode((0xCB,0xD4),8,2,"SET 2,H") { ctx => ctx.H = ctx.set(2,ctx.H) })
  addOp(Opcode((0xCB,0xD5),8,2,"SET 2,L") { ctx => ctx.L = ctx.set(2,ctx.L) })
  addOp(Opcode((0xCB,0xDF),8,2,"SET 3,A") { ctx => ctx.A = ctx.set(3,ctx.A) })
  addOp(Opcode((0xCB,0xD8),8,2,"SET 3,B") { ctx => ctx.B = ctx.set(3,ctx.B) })
  addOp(Opcode((0xCB,0xD9),8,2,"SET 3,C") { ctx => ctx.C = ctx.set(3,ctx.C) })
  addOp(Opcode((0xCB,0xDA),8,2,"SET 3,D") { ctx => ctx.D = ctx.set(3,ctx.D) })
  addOp(Opcode((0xCB,0xDB),8,2,"SET 3,E") { ctx => ctx.E = ctx.set(3,ctx.E) })
  addOp(Opcode((0xCB,0xDC),8,2,"SET 3,H") { ctx => ctx.H = ctx.set(3,ctx.H) })
  addOp(Opcode((0xCB,0xDD),8,2,"SET 3,L") { ctx => ctx.L = ctx.set(3,ctx.L) })
  addOp(Opcode((0xCB,0xE7),8,2,"SET 4,A") { ctx => ctx.A = ctx.set(4,ctx.A) })
  addOp(Opcode((0xCB,0xE0),8,2,"SET 4,B") { ctx => ctx.B = ctx.set(4,ctx.B) })
  addOp(Opcode((0xCB,0xE1),8,2,"SET 4,C") { ctx => ctx.C = ctx.set(4,ctx.C) })
  addOp(Opcode((0xCB,0xE2),8,2,"SET 4,D") { ctx => ctx.D = ctx.set(4,ctx.D) })
  addOp(Opcode((0xCB,0xE3),8,2,"SET 4,E") { ctx => ctx.E = ctx.set(4,ctx.E) })
  addOp(Opcode((0xCB,0xE4),8,2,"SET 4,H") { ctx => ctx.H = ctx.set(4,ctx.H) })
  addOp(Opcode((0xCB,0xE5),8,2,"SET 4,L") { ctx => ctx.L = ctx.set(4,ctx.L) })
  addOp(Opcode((0xCB,0xEF),8,2,"SET 5,A") { ctx => ctx.A = ctx.set(5,ctx.A) })
  addOp(Opcode((0xCB,0xE8),8,2,"SET 5,B") { ctx => ctx.B = ctx.set(5,ctx.B) })
  addOp(Opcode((0xCB,0xE9),8,2,"SET 5,C") { ctx => ctx.C = ctx.set(5,ctx.C) })
  addOp(Opcode((0xCB,0xEA),8,2,"SET 5,D") { ctx => ctx.D = ctx.set(5,ctx.D) })
  addOp(Opcode((0xCB,0xEB),8,2,"SET 5,E") { ctx => ctx.E = ctx.set(5,ctx.E) })
  addOp(Opcode((0xCB,0xEC),8,2,"SET 5,H") { ctx => ctx.H = ctx.set(5,ctx.H) })
  addOp(Opcode((0xCB,0xED),8,2,"SET 5,L") { ctx => ctx.L = ctx.set(5,ctx.L) })
  addOp(Opcode((0xCB,0xF7),8,2,"SET 6,A") { ctx => ctx.A = ctx.set(6,ctx.A) })
  addOp(Opcode((0xCB,0xF0),8,2,"SET 6,B") { ctx => ctx.B = ctx.set(6,ctx.B) })
  addOp(Opcode((0xCB,0xF1),8,2,"SET 6,C") { ctx => ctx.C = ctx.set(6,ctx.C) })
  addOp(Opcode((0xCB,0xF2),8,2,"SET 6,D") { ctx => ctx.D = ctx.set(6,ctx.D) })
  addOp(Opcode((0xCB,0xF3),8,2,"SET 6,E") { ctx => ctx.E = ctx.set(6,ctx.E) })
  addOp(Opcode((0xCB,0xF4),8,2,"SET 6,H") { ctx => ctx.H = ctx.set(6,ctx.H) })
  addOp(Opcode((0xCB,0xF5),8,2,"SET 6,L") { ctx => ctx.L = ctx.set(6,ctx.L) })
  addOp(Opcode((0xCB,0xFF),8,2,"SET 7,A") { ctx => ctx.A = ctx.set(7,ctx.A) })
  addOp(Opcode((0xCB,0xF8),8,2,"SET 7,B") { ctx => ctx.B = ctx.set(7,ctx.B) })
  addOp(Opcode((0xCB,0xF9),8,2,"SET 7,C") { ctx => ctx.C = ctx.set(7,ctx.C) })
  addOp(Opcode((0xCB,0xFA),8,2,"SET 7,D") { ctx => ctx.D = ctx.set(7,ctx.D) })
  addOp(Opcode((0xCB,0xFB),8,2,"SET 7,E") { ctx => ctx.E = ctx.set(7,ctx.E) })
  addOp(Opcode((0xCB,0xFC),8,2,"SET 7,H") { ctx => ctx.H = ctx.set(7,ctx.H) })
  addOp(Opcode((0xCB,0xFD),8,2,"SET 7,L") { ctx => ctx.L = ctx.set(7,ctx.L) })
  // *** SET b,(HL)
  // **************
  addOp(Opcode((0xCB,0xC6),15,2,"SET 0,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(0,tmp))
  })
  addOp(Opcode((0xCB,0xCE),15,2,"SET 1,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(1,tmp))
  })
  addOp(Opcode((0xCB,0xD6),15,2,"SET 2,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(2,tmp))
  })
  addOp(Opcode((0xCB,0xDE),15,2,"SET 3,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(3,tmp))
  })
  addOp(Opcode((0xCB,0xE6),15,2,"SET 4,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(4,tmp))
  })
  addOp(Opcode((0xCB,0xEE),15,2,"SET 5,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(5,tmp))
  })
  addOp(Opcode((0xCB,0xF6),15,2,"SET 6,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(6,tmp))
  })
  addOp(Opcode((0xCB,0xFE),15,2,"SET 7,(HL)") { ctx =>
    val tmp = ctx.read(ctx.HL)
    ctx.io.internalOperation(1,ctx.HL)
    ctx.write(ctx.HL,ctx.set(7,tmp))
  })
  // *** SET b,(IX + d)
  // **************
  addOp(Opcode((0xDD,0xCB,0xC6),23,4,MNEMONIC_IXY_d("SET 0,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(0,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xCE),23,4,MNEMONIC_IXY_d("SET 1,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(1,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xD6),23,4,MNEMONIC_IXY_d("SET 2,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(2,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xDE),23,4,MNEMONIC_IXY_d("SET 3,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(3,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xE6),23,4,MNEMONIC_IXY_d("SET 4,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(4,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xEE),23,4,MNEMONIC_IXY_d("SET 5,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(5,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xF6),23,4,MNEMONIC_IXY_d("SET 6,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(6,ctx.read(adr,1))) })
  addOp(Opcode((0xDD,0xCB,0xFE),23,4,MNEMONIC_IXY_d("SET 7,(IX%s)")) { ctx => val adr = ctx.INDEX_+(ctx.byte(2)) ; ctx.write(adr,ctx.set(7,ctx.read(adr,1))) })
  // ========================================= Jump Call and Return Group ====================================
  // *** JP nn
  // **************
  addOp(Opcode(0xC3,10,3,MNEMONIC_nn("JP %s"),modifyPC = true) { ctx =>
    val addr = ctx.word(1)
    ctx.PC = addr
    ctx.memptr = addr
  })
  // *** JP cc,nn
  // **************
  addOp(Opcode(0xDA,10,3,MNEMONIC_nn("JP C,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.carry > 0) })
  addOp(Opcode(0xD2,10,3,MNEMONIC_nn("JP NC,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.carry == 0) })
  addOp(Opcode(0xCA,10,3,MNEMONIC_nn("JP Z,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.zero > 0) })
  addOp(Opcode(0xC2,10,3,MNEMONIC_nn("JP NZ,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.zero == 0) })
  addOp(Opcode(0xE2,10,3,MNEMONIC_nn("JP PO,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.parity == 0) })
  addOp(Opcode(0xEA,10,3,MNEMONIC_nn("JP PE,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.parity > 0) })
  addOp(Opcode(0xF2,10,3,MNEMONIC_nn("JP P,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.sign == 0) })
  addOp(Opcode(0xFA,10,3,MNEMONIC_nn("JP M,%s"),modifyPC = true) { ctx => ctx.jp_cond_nn(ctx.sign > 0) })
  // *** JR e
  // **************
  @inline private def MNEMONIC_jr(pattern:String) = (m:Memory,PC:Int) => pattern.format(hex4(PC + 2 + m.read(PC + 1).asInstanceOf[Byte]))
  addOp(Opcode(0x18,12,2,MNEMONIC_jr("JR %s"),modifyPC = true) { ctx => ctx.jre_e })
  // *** JR cc,e
  // **************
  addOp(Opcode(0x38,12,2,MNEMONIC_jr("JR C,%s"),modifyPC = true) { ctx => ctx.jr_cond_e(ctx.carry > 0) })
  addOp(Opcode(0x30,12,2,MNEMONIC_jr("JR NC,%s"),modifyPC = true) { ctx => ctx.jr_cond_e(ctx.carry == 0) })
  addOp(Opcode(0x28,12,2,MNEMONIC_jr("JR Z,%s"),modifyPC = true) { ctx => ctx.jr_cond_e(ctx.zero > 0) })
  addOp(Opcode(0x20,12,2,MNEMONIC_jr("JR NZ,%s"),modifyPC = true) { ctx => ctx.jr_cond_e(ctx.zero == 0) })
  // *** JP (HL)
  // **************
  addOp(Opcode(0xE9,4,1,"JP (HL)",modifyPC = true) { ctx => ctx.PC = ctx.HL })
  // *** JP (IX)
  // **************
  addOp(Opcode((0xDD,0xE9),8,1,"JP (IX)",modifyPC = true) { ctx => ctx.PC = ctx.INDEX })
  // *** CALL nn
  // **************
  addOp(Opcode(0xCD,17,3,MNEMONIC_nn("CALL %s"),modifyPC = true) { ctx =>
    val tmp = ctx.word(1)
    ctx.io.internalOperation(1, (ctx.PC + 1) & 0xFFFF)
    ctx.call(tmp)
  })
  // *** CALL cc,nn
  // **************
  addOp(Opcode(0xDC,10,3,MNEMONIC_nn("CALL C,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.carry > 0) })
  addOp(Opcode(0xD4,10,3,MNEMONIC_nn("CALL NC,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.carry == 0) })
  addOp(Opcode(0xCC,10,3,MNEMONIC_nn("CALL Z,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.zero > 0) })
  addOp(Opcode(0xC4,10,3,MNEMONIC_nn("CALL NZ,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.zero == 0) })
  addOp(Opcode(0xE4,10,3,MNEMONIC_nn("CALL PO,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.parity == 0) })
  addOp(Opcode(0xEC,10,3,MNEMONIC_nn("CALL PE,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.parity > 0) })
  addOp(Opcode(0xF4,10,3,MNEMONIC_nn("CALL P,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.sign == 0) })
  addOp(Opcode(0xFC,10,3,MNEMONIC_nn("CALL M,%s"),modifyPC = true) { ctx => ctx.call_cond_nn(ctx.sign > 0) })
  // *** DJNZ e
  // **************
  addOp(Opcode(0x10,8,2,MNEMONIC_jr("DJNZ %s"),modifyPC = true) { ctx => ctx.djnz })
  // *** RET
  // **************
  addOp(Opcode(0xC9,10,1,"RET",modifyPC = true) { ctx => val addr = ctx.pop ; ctx.PC = addr ; ctx.memptr = addr })
  // *** RET cc
  // **************
  addOp(Opcode(0xD8,5,1,"RET C",modifyPC = true) { ctx => ctx.ret_cond(ctx.carry > 0) })
  addOp(Opcode(0xD0,5,1,"RET NC",modifyPC = true) { ctx => ctx.ret_cond(ctx.carry == 0) })
  addOp(Opcode(0xC8,5,1,"RET Z",modifyPC = true) { ctx => ctx.ret_cond(ctx.zero > 0) })
  addOp(Opcode(0xC0,5,1,"RET NZ",modifyPC = true) { ctx => ctx.ret_cond(ctx.zero == 0) })
  addOp(Opcode(0xE0,5,1,"RET PO",modifyPC = true) { ctx => ctx.ret_cond(ctx.parity == 0) })
  addOp(Opcode(0xE8,5,1,"RET PE",modifyPC = true) { ctx => ctx.ret_cond(ctx.parity > 0) })
  addOp(Opcode(0xF0,5,1,"RET P",modifyPC = true) { ctx => ctx.ret_cond(ctx.sign == 0) })
  addOp(Opcode(0xF8,5,1,"RET M",modifyPC = true) { ctx => ctx.ret_cond(ctx.sign > 0) })
  // *** RETI
  // **************
  addOp(Opcode((0xED,0x4D),14,2,"RETI",modifyPC = true) { ctx => ctx.retni() })
  addOp(Opcode((0xED,0x45),14,2,"RETN",modifyPC = true,Array(0x55,0x65,0x75,0x5D,0x6D,0x7D)) { ctx => ctx.retni() })
  // *** RST p
  // **************
  addOp(Opcode(0xC7,11,1,"RST 0",modifyPC = true) { ctx => ctx.rst(0x00) })
  addOp(Opcode(0xCF,11,1,"RST 8",modifyPC = true) { ctx => ctx.rst(0x08) })
  addOp(Opcode(0xD7,11,1,"RST 10",modifyPC = true) { ctx => ctx.rst(0x10) })
  addOp(Opcode(0xDF,11,1,"RST 18",modifyPC = true) { ctx => ctx.rst(0x18) })
  addOp(Opcode(0xE7,11,1,"RST 20",modifyPC = true) { ctx => ctx.rst(0x20) })
  addOp(Opcode(0xEF,11,1,"RST 28",modifyPC = true) { ctx => ctx.rst(0x28) })
  addOp(Opcode(0xF7,11,1,"RST 30",modifyPC = true) { ctx => ctx.rst(0x30) })
  addOp(Opcode(0xFF,11,1,"RST 38",modifyPC = true) { ctx => ctx.rst(0x38) })
  // ====================================== Input Group ======================================================
  // *** IN A,n
  // **************
  addOp(Opcode(0xDB,11,2,MNEMONIC_n("IN A,%s")) { ctx => ctx.in_a_n })
  // *** IN r,(C)
  // **************
  addOp(Opcode((0xED,0x78),12,2,"IN A,(C)") { ctx => ctx.A = ctx.in_r_c() })
  addOp(Opcode((0xED,0x40),12,2,"IN B,(C)") { ctx => ctx.B = ctx.in_r_c() })
  addOp(Opcode((0xED,0x48),12,2,"IN C,(C)") { ctx => ctx.C = ctx.in_r_c() })
  addOp(Opcode((0xED,0x50),12,2,"IN D,(C)") { ctx => ctx.D = ctx.in_r_c() })
  addOp(Opcode((0xED,0x58),12,2,"IN E,(C)") { ctx => ctx.E = ctx.in_r_c() })
  addOp(Opcode((0xED,0x60),12,2,"IN H,(C)") { ctx => ctx.H = ctx.in_r_c() })
  addOp(Opcode((0xED,0x68),12,2,"IN L,(C)") { ctx => ctx.L = ctx.in_r_c() })
  // *** INI
  // **************
  /*
   *  mp := ((c)), (hl) := tmp, hl += 1,
      b -= 1 => flags, nf := tmp.7,
      tmp2 := tmp + [[c +/- 1] AND 0xff],
      pf := parity of [[tmp2 AND 0x07] XOR b],
      hf := cf := tmp2 > 255
   */
  addOp(Opcode((0xED,0xA2),16,2,"INI") { ctx => ctx.ini(inc = true) })
  // *** INIR
  // **************
  addOp(Opcode((0xED,0xB2),16,2,"INIR",modifyPC = true) { ctx =>
    val hl = ctx.HL
    ctx.ini(true)
    if (ctx.B == 0) ctx.incPC(2)//ctx.PC = (ctx.PC + 2) & 0xFFFF
    else {
      ctx.setAdditionalClockCycles(5)
      ctx.io.internalOperation(5,hl)
    }
  })
  // *** IND
  // **************
  addOp(Opcode((0xED,0xAA),16,2,"IND") { ctx => ctx.ini(inc = false) })
  // *** INDR
  // **************
  addOp(Opcode((0xED,0xBA),16,2,"INDR",modifyPC = true) { ctx =>
    val hl = ctx.HL
    ctx.ini(inc = false)
    if (ctx.B == 0) ctx.incPC(2)//ctx.PC = (ctx.PC + 2) & 0xFFFF
    else {
      ctx.setAdditionalClockCycles(5)
      ctx.io.internalOperation(5,hl)
    }
  })
  // ====================================== Output Group =====================================================
  // *** OUT (n),A
  // **************
  addOp(Opcode(0xD3,11,2,MNEMONIC_n("OUT (%s),A")) { ctx =>
    val port = ctx.byte(1)
    ctx.io.out(ctx.A,port,ctx.A)
    ctx.memptr = (port + 1) & 0xFF | ctx.A << 8
  })
  // *** OUT (C),r
  // **************
  addOp(Opcode((0xED,0x79),12,2,"OUT (C),A") { ctx => ctx.out_c_r(ctx.A) })
  addOp(Opcode((0xED,0x41),12,2,"OUT (C),B") { ctx => ctx.out_c_r(ctx.B) })
  addOp(Opcode((0xED,0x49),12,2,"OUT (C),C") { ctx => ctx.out_c_r(ctx.C) })
  addOp(Opcode((0xED,0x51),12,2,"OUT (C),D") { ctx => ctx.out_c_r(ctx.D) })
  addOp(Opcode((0xED,0x59),12,2,"OUT (C),E") { ctx => ctx.out_c_r(ctx.E) })
  addOp(Opcode((0xED,0x61),12,2,"OUT (C),H") { ctx => ctx.out_c_r(ctx.H) })
  addOp(Opcode((0xED,0x69),12,2,"OUT (C),L") { ctx => ctx.out_c_r(ctx.L) })
  // *** OUTI
  // **************
  addOp(Opcode((0xED,0xA3),16,2,"OUTI") { ctx => ctx.outi(inc = true) })
  // *** OTIR
  // **************
  addOp(Opcode((0xED,0xB3),16,2,"OTIR",modifyPC = true) { ctx =>
    ctx.outi(inc = true)
    if (ctx.B == 0) ctx.incPC(2)//ctx.PC = (ctx.PC + 2) & 0xFFFF
    else {
      ctx.setAdditionalClockCycles(5)
      ctx.io.internalOperation(5,ctx.BC)
    }
  })
  // *** OUTD
  // **************
  addOp(Opcode((0xED,0xAB),16,2,"OUTD") { ctx =>ctx.outi(inc = false) })
  // *** OTDR
  // **************
  addOp(Opcode((0xED,0xBB),16,2,"OTDR",modifyPC = true) { ctx =>
    ctx.outi(inc = false)
    if (ctx.B == 0) ctx.incPC(2)//ctx.PC = (ctx.PC + 2) & 0xFFFF
    else {
      ctx.setAdditionalClockCycles(5)
      ctx.io.internalOperation(5,ctx.BC)
    }
  })
  // UNDOCUMENTED
  // ** IN (C)
  // **************
  addOp(Opcode((0xED,0x70),12,2,"IN (C)") { ctx => ctx.in_r_c() })
  // UNDOCUMENTED
  // ** OUT (C),0
  // **************
  addOp(Opcode((0xED,0x71),12,2,"OUT (C),0") { ctx => ctx.out_c_r(0) })
  // =========================================================================================================
  // ====================================== Reflection =======================================================
  private def addOp(op:Opcode) : Unit = OPCODES += op

  private def initOpcodes  : Unit = {
    if (opcodes_1(0) != null) return

    for(o <- OPCODES) {
      o.opcodes match {
        case Array(op) =>
          if (opcodes_1(op) == null) opcodes_1(op) = o else { println(s"$op already set"); sys.exit(-1) }
        case Array(0xED,op) =>
          if (opcodes_ed(op) == null) opcodes_ed(op) = o else { println(s"0xED,$op already set"); sys.exit(-1) }
          if (o.copyopcodes != null)
            for(cop <- o.copyopcodes) { // copy the same Opcode to other hex
              if (opcodes_ed(cop) == null) opcodes_ed(cop) = o.copy(opcodes = Array(0xED,cop))(o.executeFunction) else { println(s"0xED,$op already set"); sys.exit(-1) }
            }
        case Array(0xCB,op) =>
          if (opcodes_cb(op) == null) opcodes_cb(op) = o else { println(s"0xCB,$op already set"); sys.exit(-1) }
        case Array(0xDD,0xCB,op) =>
          if (opcodes_ddcb(op) == null) opcodes_ddcb(op) = o else { println(s"0xDD,0xCB,_,$op already set"); sys.exit(-1) }
        case Array(0xDD,op) =>
          if (opcodes_dd(op) == null) opcodes_dd(op) = o else { println(s"0xDD,$op already set"); sys.exit(-1) }
        case x =>
          println(s"Fatal error: opcodes ${x.mkString(",")} unknown")
          sys.exit(-1)
      }
    }

    // FD copy for DD and DDCB
    for(o <- 0 to 1;
        i <- 0 to 0xFF) {
      val opcode = o match {
        case 0 => opcodes_dd(i)
        case 1 => opcodes_ddcb(i)
      }
      if (opcode != null) {
        val codes = Array.ofDim[Int](opcode.opcodes.length)
        System.arraycopy(opcode.opcodes,0,codes,0,codes.length)
        codes(0) = 0xFD
        val mnemonic = (m : Memory, v : Int) => {
          val s = opcode.getMnemonic(m,v)
          s.replaceAll("IX","IY")
        }
        o match {
          case 0 => opcodes_fd(i) = Opcode(codes,opcode.cycles,opcode.size,mnemonic,opcode.modifyPC)(new FD(opcode.executeFunction))
          case 1 => opcodes_fdcb(i) = Opcode(codes,opcode.cycles,opcode.size,mnemonic,opcode.modifyPC)(new FD(opcode.executeFunction))
        }
      }
    }

    // CB undocumented
    val regMnem = Array("B","C","D","E","H","L","_","A")
    for(o <- List(0xDD,0xFD)) {
      val opcodes = o match {
        case 0xDD => opcodes_ddcb
        case 0xFD => opcodes_fdcb
      }
      for (y <- 0 to 0xF) {
        for (c <- List(0x06,0x0E)) {
          val opcode = opcodes(y << 4 | c)
          val range = if (c == 0x06) (0 to 5) ++ (0x07 to 0x07) else (0x08 to 0x0D) ++ (0x0F to 0x0F)
          for(x <- range) {
            val code = y << 4 | x
            if (y >= 4 && y <= 7) { // BIT
              if (opcodes(code) == null) opcodes(code) = Opcode((o,0xCB,code),opcode.cycles,opcode.size,opcode.getMnemonic)(ctx => {
                opcode.executeFunction(ctx)
              })
              else { println(s"CB Undocumented ${o.toHexString} 0xCB ${code.toHexString} already set"); sys.exit(-1) }
            }
            else {
              val reg = x & 7
              val f = (ctx:Context) => {
                opcode.executeFunction(ctx)
                reg match {
                  case 0 => ctx.B = ctx.lastWrite
                  case 1 => ctx.C = ctx.lastWrite
                  case 2 => ctx.D = ctx.lastWrite
                  case 3 => ctx.E = ctx.lastWrite
                  case 4 => ctx.H = ctx.lastWrite
                  case 5 => ctx.L = ctx.lastWrite
                  case 7 => ctx.A = ctx.lastWrite
                }
              }
              val mnem = (m:Memory,s:Int) => s"${opcode.getMnemonic(m,s)},${regMnem(reg)}"
              if (opcodes(code) == null)
                opcodes(code) = Opcode((o,0xCB,code),opcode.cycles,opcode.size,mnem)(f)
              else { println(s"CB Undocumented ${o.toHexString} 0xCB ${code.toHexString} already set"); sys.exit(-1) }
            }
          }
        }
      }
    }

    OPCODES.clear()
  }
  // =========================================================================================================
}

/**
 * @author ealeame
 */
class Z80(mem:Memory,
          io_memory:Z80.IOMemory = null,
          trapListener : Z80.Context => Unit = null,
          undocHandler : Z80.Context => Int = null) extends Chip with TraceListener {
  val id: ID = ChipID.CPU
  override val componentID = "Z80"
  import Z80._
  val ctx = new Context(mem,io_memory)
  final val M1FETCH_PIN = 1
  final val REFRESH_PIN = 2
  final val DUMMY_READ_PIN = 4

  private[this] var irqLow,nmiLow,nmiOnNegativeEdge = false
  private[this] var im2LowByte = 0
  private[this] var M1Fetch,refresh,dummyRead = false
  private[this] var cpuWaitUntil = 0L
  private[this] var cpuRestCycles = 0.0
  private[this] var busREQ = false
  private[this] var tracing = false
  private[this] var stepCallBack : CpuStepInfo => Unit = _
  private[this] val syncObject = new Object
  private[this] var breakCallBack : CpuStepInfo => Unit = _
  private[this] var breakType : BreakType = _
  private[this] var lastPC = 0

  override def getProperties: Properties = {
    properties.setProperty("Context",ctx.toString)
    properties.setProperty("IRQ pending",irqLow.toString)
    properties.setProperty("NMI pending",nmiLow.toString)
    properties
  }

  // =================================== Tracing =============================================================
  def setCycleMode(cycleMode: Boolean): Unit = {}
  def setTraceOnFile(out:PrintWriter,enabled:Boolean) : Unit = {
    // TODO
  }
  def setTrace(traceOn:Boolean): Unit = tracing = traceOn
  def step(updateRegisters: CpuStepInfo => Unit) : Unit = {
    stepCallBack = updateRegisters
    syncObject.synchronized {
      syncObject.notify()
    }
  }
  def setBreakAt(breakType:BreakType,callback:CpuStepInfo => Unit) : Unit = {
    tracing = false
    breakCallBack = callback
    this.breakType = breakType match {
      case NoBreak => null
      case _ => breakType
    }
  }
  def jmpTo(pc:Int) : Unit = {
    ctx.PC = pc & 0xFFFF
  }
  def disassemble(mem:Memory,address:Int) : (String,Int) = {
    try {
      dummyRead = true
      val adr = Array(address)
      val opcode = fetch(adr)
      (opcode.disassemble(mem, adr(0)), opcode.size)
    }
    finally {
      dummyRead = false
    }
  }

  def getLastPC : Int = lastPC
  // =================================== Interrupt Handling ==================================================

  final def irq(low:Boolean,im2LowByte : Int = 0): Unit = {
    irqLow = low
    this.im2LowByte = im2LowByte
  }
  final def nmi(low:Boolean) : Unit = {
    if (!nmiLow && low) {
      nmiOnNegativeEdge = true
    }
    nmiLow = low
  }

  // ======================================== Bus Request (threee state) =====================================
  def requestBUS(request:Boolean) = busREQ = request
  // ======================================== Fetch & Execute ================================================

  def isM1Fetch : Boolean = M1Fetch
  def isRefresh : Boolean = refresh
  def isDummyRead : Boolean = dummyRead

  def pins : Int = (if (M1Fetch) M1FETCH_PIN else 0) | (if (refresh) REFRESH_PIN else 0) | (if (dummyRead) DUMMY_READ_PIN else 0)

  def init  : Unit = {
    Log.info("Z80 initializing opcodes...")
    Z80.initOpcodes
  }
  def reset  : Unit = {
    ctx.reset
    irqLow = false
    nmiLow = false
    nmiOnNegativeEdge = false
    M1Fetch = false
    refresh = false
    dummyRead = false
  }

  @inline private[this] def fetch(addr:Array[Int] = null) : Opcode = {
    M1Fetch = true
    try {
      val pc = if (addr == null) ctx.PC else addr(0)
      val op = mem.read(pc)
      if (addr == null) ctx.incR(1)
      refreshCycle
      // single opcode
      val opcode = opcodes_1(op)
      if (opcode != null) return opcode
      // extended
      val op1 = mem.read((pc + 1) & 0xFFFF)
      if (addr == null) ctx.incR(1)
      refreshCycle
      // ED
      if (op == 0xED) {
        val op2 = opcodes_ed(op1)
        if (op2 == null) {
          ctx.incPC()//ctx.PC = (ctx.PC + 1) & 0xFFFF
          ctx.setAdditionalClockCycles(4)
          return NOP
        }
        else return op2
      }
      // CB
      if (op == 0xCB) return opcodes_cb(op1)
      // DD
      if (op == 0xDD || op == 0xFD) {
        var opcodes_xxcb,opcodes_xx : Array[Opcode] = null
        if (op == 0xDD) {
          opcodes_xxcb = opcodes_ddcb
          opcodes_xx = opcodes_dd
        }
        else {
          opcodes_xxcb = opcodes_fdcb
          opcodes_xx = opcodes_fd
        }
        if (op1 == 0xCB) {
          val lastDummy = dummyRead
          dummyRead = true
          val op2 = mem.read((pc + 3) & 0xFFFF)
          dummyRead = lastDummy
          return opcodes_xxcb(op2)
        }
        else {
          opcodes_xx(op1) match {
            case null =>
              // skip prefix
              if (addr == null) {
                ctx.incPC()//ctx.PC = (ctx.PC + 1) & 0xFFFF
                ctx.setAdditionalClockCycles(4)
              }
              else addr(0) = (pc + 1) & 0xFFFF
              val c2 = opcodes_1(op1)
              if (c2 == null) return NOP // if op1 is DD or FD
              else return c2
            case xxcode => return xxcode
          }
        }
      }
      null
    }
    finally {
      M1Fetch = false
    }
  }

  @inline private def refreshCycle : Unit = {
    refresh = true
    val refreshAddress = ctx.I << 8 | ctx.R & 0x7F
    mem.read(refreshAddress)
    refresh = false
  }

  @inline private def interruptMode0Handling  : Unit = {
    // RST 38
    ctx.PC = 0x38
  }

  @inline private def interruptMode2Handling  : Unit = {
    val addr = ctx.I << 8 | im2LowByte
    ctx.PC = (mem.read(addr + 1) << 8) | mem.read(addr)
  }

  final def clock : Int = {
    if (breakType != null && breakType.isBreak(ctx.PC,false,false)) {
      tracing = true
      breakCallBack(CpuStepInfo(ctx.PC,ctx.toString))
    }

    if ((irqLow || nmiOnNegativeEdge) && !ctx.mustDelayInt) { // any interrupt pending ?
      ctx.lastQ = false
      if (nmiOnNegativeEdge) { // NMI
        if (ctx.halted) {
          ctx.halted = false
          ctx.incPC()//ctx.PC = (ctx.PC + 1) & 0xFFFF
        }
        ctx.io.internalOperation(5)
        ctx.push(ctx.PC)
        ctx.incR(1)
        refreshCycle
        if (breakType != null && breakType.isBreak(ctx.PC,false,true)) {
          tracing = true
          breakCallBack(CpuStepInfo(ctx.PC,ctx.toString))
          Log.debug("NMI Break")
        }
        nmiOnNegativeEdge = false

        ctx.IFF2 = ctx.IFF1
        ctx.IFF1 = 0
        ctx.PC = 0x0066
        return 11
      }
      else { // IRQ
        if (ctx.IFF1 == 1) {
          if (ctx.halted) {
            ctx.halted = false
            ctx.incPC()//ctx.PC = (ctx.PC + 1) & 0xFFFF
          }

          ctx.im match {
            case 0 => ctx.io.internalOperation(6)
            case 1|2 => ctx.io.internalOperation(7)
          }

          ctx.push(ctx.PC)
          ctx.incR(1)
          if (breakType != null && breakType.isBreak(ctx.PC,true,false)) {
            tracing = true
            breakCallBack(CpuStepInfo(ctx.PC,ctx.toString))
            Log.debug("IRQ Break")
          }

          ctx.IFF1 = 0
          ctx.IFF2 = 0
          ctx.im match {
            case 0 =>
              interruptMode0Handling
              return 12
            case 1 =>
              ctx.PC = 0x38
              return 13
            case 2 =>
              interruptMode2Handling
              return 19
          }
          return 0
        }
      }
    }

    if (trapListener != null) trapListener(ctx)

    ctx.setDelayInt(false)
    val opcode = fetch()
    if (opcode == null) {
      if (undocHandler == null) throw new IllegalArgumentException(s"Can't find opcode at ${hex4(ctx.PC)}: ${hex2(mem.read(ctx.PC))} ${hex2(mem.read(ctx.PC + 1))} ${hex2(mem.read(ctx.PC + 2))}")
      else {
        return undocHandler(ctx)
      }
    }
    // tracing
    if (tracing) {
      try {
        dummyRead = true
        Log.debug("[Z80] " + opcode.disassemble(mem, ctx.PC))
      }
      finally {
        dummyRead = false
      }
      stepCallBack(CpuStepInfo(ctx.PC,ctx.toString))
      syncObject.synchronized { syncObject.wait() }
    }
    // execute
    lastPC = ctx.PC
    ctx.copyQ
    opcode.executeFunction(ctx)

    val clocks = opcode.cycles + ctx.getAdditionalClockSycles

    if (!opcode.modifyPC) ctx.incPC(opcode.size)//ctx.PC = (ctx.PC + opcode.size) & 0xFFFF
    clocks
  }

  // ======================================== Clock ==========================================================
  final def clock(cycles:Long,scaleFactor:Double = 1) : Unit = {
    val canExecCPU = cycles > cpuWaitUntil && !busREQ
    if (canExecCPU) {
      val nextCPUCycles = cpuRestCycles + cycles + (clock - 1) / scaleFactor
      cpuWaitUntil = nextCPUCycles.toInt
      cpuRestCycles = nextCPUCycles - cpuWaitUntil
    }
  }

  // state
  override protected def saveState(out:ObjectOutputStream) : Unit = {
    out.writeBoolean(irqLow)
    out.writeBoolean(nmiLow)
    out.writeBoolean(nmiOnNegativeEdge)
    out.writeLong(cpuWaitUntil)
    ctx.saveState(out)
  }
  override protected def loadState(in:ObjectInputStream) : Unit = {
    irqLow = in.readBoolean
    nmiLow = in.readBoolean
    nmiOnNegativeEdge = in.readBoolean
    cpuWaitUntil = in.readLong
    ctx.loadState(in)
  }
  override protected def allowsStateRestoring : Boolean = true
}
