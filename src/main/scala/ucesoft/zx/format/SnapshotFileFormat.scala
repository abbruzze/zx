package ucesoft.zx.format

import ucesoft.zx.Model
import ucesoft.zx.spectrum.Spectrum

import java.io.File
import java.nio.file.Files

case class SnapshotResult(model:Model.Value,interfaceIOn:Boolean,ayEnabled:Boolean)

trait SnapshotFileFormat {
  val needReset : Boolean = true
  val extension : String
  def apply(bs:Array[Int],spectrum:Spectrum) : SnapshotResult
  def createSnapshot(spectrum:Spectrum) : Option[Array[Byte]] = None
}

object SnapshotFileFormat {
  private val formats : Array[SnapshotFileFormat] = Array(new Z80Format, new SNAFormat, new SCR)

  def getFormat(file:String) : Option[SnapshotFileFormat] = if (file != null) formats.find(f => file.toUpperCase().endsWith(f.extension.toUpperCase())) else None
  def validExt(file:String) : Boolean = getFormat(file).isDefined
  def extensions : List[String] = formats.map(e => s"*${e.extension}").toList
  def apply(file:String,spectrum:Spectrum) : Option[SnapshotResult] = {
    getFormat(file) match {
      case Some(ff) =>
        val buffer = Files.readAllBytes(new File(file).toPath) map { _.toInt & 0xFF }
        Some(ff(buffer,spectrum))
      case None =>
        None
    }
  }
}
