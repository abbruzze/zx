package ucesoft.zx.format

import ucesoft.zx.ChipID
import ucesoft.zx.spectrum.Spectrum

class SCR extends SnapshotFileFormat {
  override val needReset : Boolean = false
  override val extension: String = ".scr"

  override def apply(bs: Array[Int],spectrum:Spectrum): SnapshotResult = {
    if (bs.length != 6912) throw new IllegalArgumentException("Bad SCR size")
    for(a <- 0 until bs.length) spectrum.mmu.load(0x4000 + a,bs(a),ChipID.CPU)

    SnapshotResult(spectrum.model,spectrum.mmu.interfaceIEnabled,spectrum.mmu.isAYEnabled)
  }
}
