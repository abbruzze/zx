package ucesoft.zx.ui

import javax.swing.TransferHandler
import java.awt.datatransfer.DataFlavor
import java.io.File

class DNDHandler(handleDND:File => Unit) extends TransferHandler {
  private final val SUPPORTED_FORMATS = List(".SNA",".Z80",".SCR",".TAP",".TZX",".MDR",".WAV")
  override def canImport(support:TransferHandler.TransferSupport) : Boolean = support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)

  override def importData(support: TransferHandler.TransferSupport) : Boolean = {
    if (!canImport(support)) return false
    val t = support.getTransferable

    try {
      import scala.jdk.CollectionConverters._
      t.getTransferData(DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]].asScala.headOption match {
        case None =>
          false
        case Some(f) =>
          val name = f.getName.toUpperCase
          if (SUPPORTED_FORMATS.exists(name.endsWith)) {
            handleDND(f)
            true
          }
          else false
      }
    }
    catch {
      case t:Throwable =>
        t.printStackTrace()
        false
    }
  }
}
