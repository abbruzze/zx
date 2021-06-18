package ucesoft.zx.tape

import ucesoft.zx.Clock
import ucesoft.zx.cpu.{Memory, Z80}
import ucesoft.zx.format.{TAP, TZX}
import ucesoft.zx.format.TZX.{ID18_CSWRecording, ID30_Text}

import java.io.FileOutputStream
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Properties

class TZXTape extends Tape {
  private[this] final val EAR = 0x40
  private[this] var tzx : TZX.TZXBlocks = _
  private[this] var earValue = 0
  private[this] val ctx = new Ctx
  private[this] var pauseCycle = 0
  private[this] var preparePauseCycle = 0
  private[this] var stopTheTapeRequest = false
  private[this] var eotRequest = false
  private[this] var recordBlock = initRecordBlock(Clock.systemClock.getClockHz)

  override def getProperties: Properties = {
    properties.setProperty("State: ",state.toString)
    properties.setProperty("End of tape: ",eot.toString)
    properties.setProperty("Tape: ",Option(tzx).flatMap(_.fromFile).map(_.toString).getOrElse("Empty"))
    properties.setProperty("Current block: ",(ctx.index + 1).toString)
    properties
  }

  private def initRecordBlock(freq:Double) : ID18_CSWRecording = {
    val block = new ID18_CSWRecording(0,0)
    block.initForRecording(22050,(freq / 22050).toInt,1000)
    block
  }

  private class Ctx extends TZX.Context {
    override def index_=(i:Int) : Unit = {
      if (state == TAPE_PLAY) {
        pauseCycle = (tzx.blocks(index).getPauseBeforNextBlockInMillis * clockFreq / 1000).toInt
        if (pauseCycle > 0) preparePauseCycle = (clockFreq / 1000).toInt // 1ms
        if (i == tzx.blocks.length) {
          stopTheTape
          eotRequest = true
        }
        else notifyBlockChanged(tzx.blocks(i))
      }
      else if (tzx != null && tzx.blocks.length > 0) notifyBlockChanged(tzx.blocks(i))
      super.index_=(i)
    }

    override def nextIndex: Unit = {
      tzx.blocks(index).reset
      super.nextIndex
    }

    override def stopTheTape: Unit = {
      super.stopTheTape

      preparePauseCycle = (clockFreq / 1000).toInt // 1ms
      stopTheTapeRequest = true
    }

    override def notifyBlockOffsetChanged(offset:Int,length:Int) : Unit = TZXTape.this.notifyBlockOffsetChanged(offset,length)

    override def setPulse(value: Int): Unit = earValue = value
    override def pulse: Unit = earValue ^= EAR
    override def clockFreq: Double = Clock.systemClock.getClockHz
  }

  override def init : Unit = {
    Clock.systemClock.addChangeFrequencyListener(f => recordBlock = initRecordBlock(f))
  }

  override def canRecord : Boolean = tzx != null && tzx.fromFile.isDefined

  override def eject : Unit = {
    tzx = null
    setState(TAPE_STOP)
    notifyTapeContentChanged(None)
  }

  override def setTZX(tzx:TZX.TZXBlocks) : Unit = {
    this.tzx = tzx
    notifyTapeContentChanged(Some(tzx))
    rewind(0)
  }

  override def setTAP(tap: TAP.TAPBlocks): Unit = {
    tzx = TZX.TAP2TZX(tap,None)
    notifyTapeContentChanged(Some(tzx))
  }

  override def setEAR(value: Boolean): Unit = {
    if (state == TAPE_RECORD) {
      earValue = if (value) EAR else 0
    }
  }

  override def setState(state: TapeState): Unit = {
    state match {
      case TAPE_RECORD =>
        if (canRecord) recordBlock.reset
        else return
      case TAPE_STOP if this.state == TAPE_RECORD =>
        saveRecord
      case _ =>
    }

    super.setState(state)
  }

  private def saveRecord : Unit = {
    tzx.fromFile match {
      case Some(file) =>
        val out = new FileOutputStream(file,true)
        val text = new ID30_Text(0,0)
        text.setText(s"ZX Emulator ${LocalDateTime.now.format(DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm"))}")
        out.write(text.toBytes)
        out.write(recordBlock.toBytes)
        out.close
        recordBlock.reset
        TZX.readTZX(file.toString) match {
          case Some(t) =>
            tzx = t
          case None =>
        }
        notifyTapeContentChanged(Some(tzx))
      case None =>
    }
  }

  override def createNewTape(path:String) : Unit = {
    val out = new FileOutputStream(path)
    out.write("ZXTape!".getBytes)
    out.write(0x1A)
    out.write(1)
    out.write(20)
    out.close
  }

  override def rewind(toIndex:Int = 0): Unit = {
    val oldState = state
    setState(TAPE_STOP)
    setEOT(false)
    ctx.index = toIndex
    earValue = 0
    pauseCycle = 0
    preparePauseCycle = 0
    setState(oldState)
  }

  override def reset : Unit = {
    rewind(0)
    stopTheTapeRequest = false
    eotRequest = false
    if (tzx != null) tzx.blocks.foreach(_.reset)
    setState(TAPE_STOP)
  }

  override def isEmpty: Boolean = tzx != null
  override def ear: Int = earValue

  override def cycle : Unit = {
    state match {
      case TAPE_PLAY if !eot && tzx != null =>
        // check pause
        if (preparePauseCycle > 0) {
          preparePauseCycle -= 1
          if (preparePauseCycle == 0) {
            earValue = 0
            if (stopTheTapeRequest) {
              stopTheTapeRequest = false
              setState(TAPE_STOP)
              if (eotRequest) {
                setEOT(true)
                eotRequest = false
              }
            }
          }
          return
        }
        if (pauseCycle > 0) {
          pauseCycle -= 1
          return
        }
        tzx.blocks(ctx.index).cycle(ctx)
      case TAPE_RECORD =>
        recordBlock.cycleRec(earValue)
      case _ =>
    }
  }

  override def fastLoad(mem:Memory,z80:Z80) : Boolean = {
    if (tzx != null && ctx.index < tzx.blocks.length) {
      if (isEOT) {
        z80.ctx.setCarry(false)
        return false
      }
      tzx.blocks(ctx.index).fastLoad(ctx, mem, z80)
    }
    else false
  }
}
