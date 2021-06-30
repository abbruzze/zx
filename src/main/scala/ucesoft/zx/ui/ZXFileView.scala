package ucesoft.zx.ui

import ucesoft.zx.misc.ScaledImageIcon

import java.io.File
import javax.swing.filechooser.FileView

object ZXFileView extends FileView {
  private val icon = ScaledImageIcon("zx_logo.png")

  override def getIcon(f:File) = {
    val name = f.getName.toUpperCase
    if (name.endsWith(".SNA") ||
      name.endsWith(".Z80") ||
      name.endsWith(".TAP") ||
      name.endsWith(".MDR") ||
      name.endsWith(".SCR") ||
      name.endsWith(".TZX") ||
      name.endsWith(".WAV")) icon
    else null
  }
}
