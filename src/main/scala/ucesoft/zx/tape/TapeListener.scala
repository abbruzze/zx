package ucesoft.zx.tape

import ucesoft.zx.format.TZX.TZXBlocks

trait TapeListener {
  def stateChanged(oldState:TapeState,newState:TapeState) : Unit = {}
  def blockChanged(block:TapeBlockInfo) : Unit = {}
  def endOfTape : Unit = {}
  def blockOffsetChanged(offset:Int,length:Int) : Unit = {}
  def tapeContentChanged(blocks:Option[TZXBlocks]) : Unit = {}
}
