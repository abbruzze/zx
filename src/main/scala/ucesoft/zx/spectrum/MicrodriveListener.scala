package ucesoft.zx.spectrum

import ucesoft.zx.format.MDR

trait MicrodriveListener {
  def mdrInserted(mdr:MDR,driveID:Int) : Unit = {}
  def selectedDrive(drive:Int) : Unit = {}
  def posChanged(pos:Int) : Unit = {}
  def mdrEjected(driveID:Int) : Unit = {}
}
