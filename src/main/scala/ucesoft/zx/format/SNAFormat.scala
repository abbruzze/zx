package ucesoft.zx.format

import ucesoft.zx.Model
import ucesoft.zx.spectrum.Spectrum

import java.io.ByteArrayOutputStream

class SNAFormat extends SnapshotFileFormat {
  override val extension: String = ".sna"

  override def apply(bs: Array[Int],spectrum:Spectrum): SnapshotResult = {
    bs.length match {
      case 49179 =>
        spectrum.mmu.modelChanged(Model._48K,true)

        import spectrum.z80.ctx._
        val iter = bs.iterator

        restoreZ80(iter, spectrum)

        var a = 0x4000
        while (iter.hasNext) {
          spectrum.mmu.load(a,iter.next())
          a += 1
        }
        // RETN
        PC = pop

        SnapshotResult(Model._48K,false,false)
      case 131103 =>
        spectrum.mmu.modelChanged(Model._128K,true)
        val port7FFD = bs(49181)
        val bankN = port7FFD & 7

        spectrum.mmu.set7FFDRegistry(port7FFD)

        val it = bs.iterator
        restoreZ80(it, spectrum)
        // bank 5, 2 and N
        for(i <- 0 until 0x4000) spectrum.mmu.loadBank(5,i,it.next())
        for(i <- 0 until 0x4000) spectrum.mmu.loadBank(2,i,it.next())
        for(i <- 0 until 0x4000) spectrum.mmu.loadBank(bankN,i,it.next())
        // PC
        spectrum.z80.ctx.PC = it.next() | it.next() << 8
        // 0x7FFD already read
        it.next()
        // TR-DOS not supported yet
        it.next()
        val remainingBanks = List(0,1,3,4,6,7).diff(bankN :: Nil)
        for(bank <- remainingBanks;
            i <- 0 until 0x4000) spectrum.mmu.loadBank(bank,i,it.next())

        SnapshotResult(Model._128K,false,false)
      case _ =>
        throw new IllegalArgumentException("SNA format not supported")
    }
  }

  private def restoreZ80(iter:Iterator[Int],spectrum:Spectrum) : Unit = {
    import spectrum.z80.ctx._
    I = iter.next()
    L1 = iter.next() ; H1 = iter.next()
    E1 = iter.next() ; D1 = iter.next()
    C1 = iter.next() ; B1 = iter.next()
    F1 = iter.next() ; A1 = iter.next()

    L = iter.next() ; H = iter.next()
    E = iter.next() ; D = iter.next()
    C = iter.next() ; B = iter.next()
    IY = iter.next() | iter.next() << 8
    IX = iter.next() | iter.next() << 8
    val _int = iter.next()
    IFF2 = (_int >> 2) & 1
    R = iter.next()
    F = iter.next() ; A = iter.next()
    SP = iter.next() | iter.next() << 8
    im = iter.next() & 3
    IFF1 = IFF2
    spectrum.ula.setBorderColor(iter.next() & 7)
  }

  private def backupZ80(spectrum:Spectrum,out:ByteArrayOutputStream) : Unit = {
    import spectrum.z80.ctx._
    val is48K = spectrum.model == Model._48K

    if (is48K) spectrum.z80.ctx.push(PC)

    out.write(I)
    out.write(L1) ; out.write(H1)
    out.write(E1) ; out.write(D1)
    out.write(C1) ; out.write(B1)
    out.write(F1) ; out.write(A1)

    out.write(L) ; out.write(H)
    out.write(E) ; out.write(D)
    out.write(C) ; out.write(B)

    out.write(IYL) ; out.write(IYH)
    out.write(IXL) ; out.write(IXH)

    out.write(IFF2 << 2)

    out.write(R)

    out.write(F) ; out.write(A)
    out.write(SP & 0xFF) ; out.write(SP >> 8)

    out.write(im)
    out.write(spectrum.ula.getBorderColor)

    if (is48K) spectrum.z80.ctx.pop
  }

  override def createSnapshot(spectrum:Spectrum) : Option[Array[Byte]] = {
    val out = new ByteArrayOutputStream()
    backupZ80(spectrum,out)

    if (spectrum.model == Model._48K) {
      for(a <- 0x4000 to 0xFFFF) out.write(spectrum.mmu.peek(a))
    }
    else {
      val _128reg = spectrum.mmu.get7FFDRegistry
      val bankN = _128reg & 7
      val banks = List(5,2,bankN) ++ List(0,1,3,4,6,7).diff(bankN :: Nil)
      for((b,i) <- banks.zipWithIndex) {
        for(a <- 0 to 0x3FFF) out.write(spectrum.mmu.peekBank(b,a))
        if (i == 2) {
          out.write(spectrum.z80.ctx.PC & 0xFF)
          out.write(spectrum.z80.ctx.PC >> 8)
          out.write(_128reg)
          out.write(0) // TR-DOS not supported
        }
      }
    }

    Some(out.toByteArray)
  }
}
