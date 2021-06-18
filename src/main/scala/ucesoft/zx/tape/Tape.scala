package ucesoft.zx.tape

import ucesoft.zx.{ZXComponent, ZXComponentType}
import ucesoft.zx.ZXComponentType.Type
import ucesoft.zx.cpu.{Memory, Z80}
import ucesoft.zx.format.TZX.TZXBlocks
import ucesoft.zx.format.{TAP, TZX}

abstract class Tape extends ZXComponent {
  override val componentID: String = "Tape"
  override val componentType: Type = ZXComponentType.TAPE

  protected val tapeListeners = new collection.mutable.ListBuffer[TapeListener]
  protected var eot = false
  protected var state : TapeState = TAPE_STOP

  def isEmpty : Boolean
  def isEOT : Boolean = eot
  def isPlaying: Boolean = state == TAPE_PLAY
  def canRecord : Boolean = true
  def eject : Unit = {}

  def ear : Int
  def setEAR(value:Boolean) : Unit = {}

  def setTAP(tap:TAP.TAPBlocks) : Unit
  def setTZX(tzx:TZX.TZXBlocks) : Unit

  def createNewTape(path:String) : Unit

  def cycle : Unit
  def setState(state:TapeState) : Unit = {
    if (state != this.state) {
      val os = this.state
      this.state = state
      notifyStateChange(os, this.state)
    }
  }
  def getState : TapeState = state
  protected def setEOT(value:Boolean) : Unit = {
    eot = value
    if (eot) notifyEOT
  }
  def fastLoad(mem:Memory,z80:Z80) : Boolean
  def rewind(toIndex:Int = 0) : Unit

  def reset : Unit = {}

  def addTapeListener(tl:TapeListener) : Unit = tapeListeners += tl
  def removeTapeListener(tl:TapeListener) : Unit = tapeListeners -= tl

  protected def notifyStateChange(oldState:TapeState,newState:TapeState) : Unit = tapeListeners.foreach(_.stateChanged(oldState,newState))
  protected def notifyBlockChanged(block:TapeBlockInfo) : Unit = tapeListeners.foreach(_.blockChanged(block))
  protected def notifyBlockOffsetChanged(offset:Int,length:Int) : Unit = tapeListeners.foreach(_.blockOffsetChanged(offset,length))
  protected def notifyTapeContentChanged(blocks:Option[TZXBlocks]) : Unit = tapeListeners.foreach(_.tapeContentChanged(blocks))
  protected def notifyEOT : Unit = tapeListeners.foreach(_.endOfTape)
}

