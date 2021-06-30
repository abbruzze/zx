package ucesoft.zx.format

import ucesoft.zx.Clock
import ucesoft.zx.cpu.{Memory, Z80}
import ucesoft.zx.format.TAP.HeaderInfo
import ucesoft.zx.tape.TapeBlockInfo

import java.io.{ByteArrayOutputStream, File, InputStream, OutputStream}
import java.util.zip.{DeflaterOutputStream, InflaterInputStream}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TZX {
  case class TZXBlocks(blocks:Array[TapeBlock],fromFile:Option[File]) {
    override def toString: String = s"TZXTapeBlocks(${blocks.mkString(",")})"
  }

  def readTZX(file:String) : Option[TZXBlocks] = {
    val buffer = java.nio.file.Files.readAllBytes(new java.io.File(file).toPath).map(b => b.toInt & 0xFF)
    if (buffer.length < 10) return None
    val sb = new StringBuilder
    for(i <- 0 to 6) sb.append(buffer(i).toChar)
    if (sb.toString() != "ZXTape!") return None

    var pos = 0x0A
    var index = 1
    val blocks = new ListBuffer[TapeBlock]
    val props = new collection.mutable.HashMap[String,Any]
    props += "clock" -> Clock.systemClock.getClockHz

    var durationOffset = 0.0
    while (pos < buffer.length) {
      val id = buffer(pos)
      val block = createBlockForID(id,index,durationOffset.toInt)
      if (!block.parse(pos + 1,buffer,props)) return None
      durationOffset += block.getDurationInSeconds
      //println(s"[${block.durationOffsetInSeconds / 60}:${block.durationOffsetInSeconds % 60} sec=${block.getDurationInSeconds}]$block")
      pos = block.getParsePos
      blocks += block
      index += 1
    }
    Some(TZXBlocks(blocks.toArray,Some(new File(file))))
  }

  def TAP2TZX(tap:TAP.TAPBlocks,file:Option[File]) : TZXBlocks = {
    var durationOffset = 0.0
    val props = new collection.mutable.HashMap[String,Any]
    props += "clock" -> Clock.systemClock.getClockHz

    val blocks = for(b <- tap.blocks.zipWithIndex) yield {
      val buffer = Array.ofDim[Int](b._1.length)
      System.arraycopy(tap.data,b._1.startOffset,buffer,0,buffer.length)
      val block = new ID10_StandardSpeedData(b._2 + 1,durationOffset.toInt)
      block.setTAP(TAP.TAPBlocks(TAP.Block(0,buffer.length,b._1.headerInfo,0) :: Nil,buffer))

      block.calculateDuration(props)
      durationOffset += block.getDurationInSeconds
      //println(s"[${block.durationOffsetInSeconds / 60}:${block.durationOffsetInSeconds % 60} sec=${block.getDurationInSeconds}]$block")
      block
    }
    TZXBlocks(blocks.toArray,file)
  }

  def WAV2TZX(file:String) : Option[TZXBlocks] = {
    WAV.readWAV(file) match {
      case None =>
        None
      case Some(wav) =>
        val block = new WAVBlock(wav)
        Some(TZXBlocks(Array(block),Some(new File(file))))
    }
  }

  private def createBlockForID(id:Int,index:Int,durationOffsetInSeconds : Int) : TapeBlock = {
    id match {
      case 0x10 => new ID10_StandardSpeedData(index,durationOffsetInSeconds)
      case 0x11 => new ID11_TurboSpeedData(index,durationOffsetInSeconds)
      case 0x12 => new ID12_PureTone(index,durationOffsetInSeconds)
      case 0x13 => new ID13_PulseSequence(index,durationOffsetInSeconds)
      case 0x14 => new ID14_PureData(index,durationOffsetInSeconds)
      case 0x15 => new ID15_DirectRecording(index,durationOffsetInSeconds)
      case 0x18 => new ID18_CSWRecording(index,durationOffsetInSeconds)
      case 0x20 => new ID20_Pause(index,durationOffsetInSeconds)
      case 0x21 => new ID21_GroupStart(index,durationOffsetInSeconds)
      case 0x22 => new ID22_GroupEnd(index,durationOffsetInSeconds)
      case 0x23 => new ID23_JumpToBlock(index,durationOffsetInSeconds)
      case 0x24 => new ID24_LoopStart(index,durationOffsetInSeconds)
      case 0x25 => new ID25_LoopEnd(index,durationOffsetInSeconds)
      case 0x2A => new ID2A_StopTheTape48K(index,durationOffsetInSeconds)
      case 0x30 => new ID30_Text(index,durationOffsetInSeconds)
      case 0x32 => new ID32_ArchiveInfo(index,durationOffsetInSeconds)
      case 0x35 => new ID35_CustomInfo(index,durationOffsetInSeconds)
      case x =>
        throw new IllegalArgumentException(s"TZX Block ID ${x.toHexString} not supported")
    }
  }

  class Context {
    private var _index = 0
    def index : Int = _index
    def index_=(i:Int) : Unit = _index = i
    def nextIndex : Unit = index_=(_index + 1)

    def stopTheTape : Unit = {}
    def notifyBlockOffsetChanged(offset:Int,length:Int) : Unit = {}

    def pulse : Unit = {}
    def setPulse(value:Int) : Unit = {}
    def is48K : Boolean = true
    def clockFreq : Double = 1.0
  }

  abstract class TapeBlock(override val index:Int,override val durationOffsetInSeconds : Int) extends TapeBlockInfo {
    val ID : Int
    protected var byteCounter = 0
    protected var currentDataByte = 0
    protected var pulseCounter,pulseLengthCounter = 0
    protected val pulseUpDown = Array(0,0)
    protected var pulseUpDownIndex = 0
    protected var BIT1,BIT0 = 0
    protected var pauseBeforeNextBlockMillis = 0
    protected var lastByteUsedBits = 8
    protected var parsePos = 1
    protected var durationInCycles = 0L
    protected var durationInSeconds = 0

    override def toString : String = s"[$index]$blockType($blockInfo)${if (pauseBeforeNextBlockMillis > 0) s"<pause=$pauseBeforeNextBlockMillis>" else ""}"

    protected def readWord(buffer:Array[Int]) : Int = {
      val w = buffer(parsePos) | buffer(parsePos + 1) << 8
      parsePos += 2
      w
    }

    protected def readByte(buffer:Array[Int]) : Int = {
      val b = buffer(parsePos)
      parsePos += 1
      b
    }

    override def getDurationInCycles : Long = durationInCycles
    override def getDurationInSeconds: Int = durationInSeconds

    def getPauseBeforNextBlockInMillis : Int = pauseBeforeNextBlockMillis

    def cycle(ctx:Context) : Unit
    def calculateDuration(parseProps:collection.mutable.HashMap[String,Any]) : Unit = {
      calculateCycles
      durationInSeconds = math.round(durationInCycles / parseProps("clock").asInstanceOf[Double] + pauseBeforeNextBlockMillis / 1000).toInt
    }
    protected def calculateCycles : Unit = {}

    def reset : Unit = {
      pulseUpDownIndex = 0
    }

    def parse(pos:Int,buffer:Array[Int],parseProps:collection.mutable.HashMap[String,Any]) : Boolean = {
      try {
        parsePos = pos
        val result = parseBlock(buffer,parseProps:collection.mutable.HashMap[String,Any])
        if (result) calculateDuration(parseProps)
        result
      }
      catch {
        case e:Throwable =>
          println(s"Can't parse block $ID: " + e)
          false
      }
    }

    def getParsePos : Int = parsePos

    protected def parseBlock(buffer:Array[Int],parseProps:collection.mutable.HashMap[String,Any]) : Boolean

    protected def setNextBit : Unit = {
      val bit = (currentDataByte & 0x80) > 0
      currentDataByte <<= 1
      if (bit) {
        pulseUpDown(0) = BIT1
        pulseUpDown(1) = BIT1
      }
      else {
        pulseUpDown(0) = BIT0
        pulseUpDown(1) = BIT0
      }
      pulseCounter = pulseUpDown(0)
      pulseLengthCounter = 2
    }

    protected def produceSquareWave(ctx:Context) : Boolean = {
      //println(s"$earValue $pulseCounter/$pulseLengthCounter")
      pulseCounter -= 1
      if (pulseCounter == 0) {
        pulseUpDownIndex = (pulseUpDownIndex + 1) & 1
        pulseCounter = pulseUpDown(pulseUpDownIndex)
        pulseLengthCounter -= 1
        ctx.pulse
      }
      pulseLengthCounter == 0
    }

    def fastLoad(ctx:Context,mem:Memory,z80:Z80) : Boolean = false

    def toBytes : Array[Byte] = getBytes
    protected def getBytes : Array[Byte] = Array.ofDim[Byte](0)
  }

  private class WAVBlock(wav:WAV) extends TapeBlock(1,0) {
    override val ID: Int = 0x100
    override val blockType: String = "WAV"
    private final val CYCLES = (3500000.0 / wav.sampleRate).toInt
    private var cycles = 0

    durationInSeconds = wav.getDurationInSeconds

    override def reset: Unit = {
      wav.reset()
      cycles = 0
    }

    override def cycle(ctx: Context): Unit = {
      cycles += 1
      if (cycles > CYCLES) {
        cycles = 0
        wav.read match {
          case None =>
            ctx.stopTheTape
          case Some(v) =>
            ctx.setPulse(if (v) 0x40 else 0)
        }
      }
    }

    override protected def parseBlock(buffer: Array[Int], parseProps: mutable.HashMap[String, Any]): Boolean = true

    override def blockInfo: String = ""
  }

  private abstract class TAPBlock(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override def blockInfo: String = {
      val bytes = s"$tapBytes bytes"
      tap.blocks.head.headerInfo match {
        case Some(info@HeaderInfo(_, fileName)) =>
          s"${info.typeString} '$fileName'"
        case None =>
          bytes
      }
    }

    protected final val HEADER_MODE = 0
    protected final val SYNC_MODE = 1
    protected final val DATA_MODE = 2

    protected var tapBytes = 0
    protected var tap : TAP.TAPBlocks = null
    protected var currentBlock : TAP.Block = null
    protected var nextBlocks : List[TAP.Block] = Nil
    protected var tapBuffer : Array[Int] = null
    protected var mode = HEADER_MODE
    protected var blockOffset = 0

    protected var pilotHeaderDuration,pilotDataDuration,pilotLength = 0
    protected var syncUpLength,syncDownLength = 0

    protected def initBlock : Unit = {
      currentBlock.isHeader match {
        case true =>
          pulseLengthCounter = pilotHeaderDuration << 1
        case false =>
          pulseLengthCounter = pilotDataDuration << 1
      }
      pulseUpDown(0) = pilotLength
      pulseUpDown(1) = pilotLength
      pulseCounter = pulseUpDown(0)
      mode = HEADER_MODE
    }

    override def reset : Unit = {
      super.reset
      currentBlock = tap.blocks.head
      nextBlocks = tap.blocks.tail
      tapBuffer = tap.data
      initBlock
    }

    protected def setDataMode : Unit = {
      mode = DATA_MODE
      byteCounter = if (blockOffset == currentBlock.startOffset + currentBlock.length - 1) lastByteUsedBits else 8
      blockOffset = currentBlock.startOffset
      currentDataByte = tapBuffer(blockOffset)

      setNextBit
    }

    override def cycle(ctx:Context) : Unit = {
      if (produceSquareWave(ctx)) {
        mode match {
          case HEADER_MODE =>
            //println("HEADER_MODE")
            mode = SYNC_MODE
            pulseUpDown(0) = syncUpLength
            pulseUpDown(1) = syncDownLength
            pulseCounter = pulseUpDown(0)
            pulseLengthCounter = 2
          case SYNC_MODE =>
            //println("SYNC_MODE")
            setDataMode
          case DATA_MODE =>
            byteCounter -= 1
            if (byteCounter == 0) {
              blockOffset += 1
              ctx.notifyBlockOffsetChanged(blockOffset,currentBlock.length)
              if (blockOffset == currentBlock.startOffset + currentBlock.length) {
                if (nextBlocks.isEmpty) {
                  ctx.nextIndex
                }
                else {
                  currentBlock = nextBlocks.head
                  nextBlocks = nextBlocks.tail
                  initBlock
                }
              }
              else {
                byteCounter = if (blockOffset == currentBlock.startOffset + currentBlock.length - 1) lastByteUsedBits else 8
                currentDataByte = tapBuffer(blockOffset)
                setNextBit
              }
            }
            else setNextBit
        }
      }
    }

    override def fastLoad(ctx:Context,ram: Memory, z80: Z80): Boolean = {
      import z80.ctx._

      val length = currentBlock.length
      val flag = tapBuffer(currentBlock.startOffset) & 0xFF

      if (A != flag) {
        xor(flag)
        setCarry(false)
        ctx.nextIndex
        return true
      }
      var blockOffset = currentBlock.startOffset + 1

      var count = 0
      var address = IX
      val total = math.min(DE,length - 1)
      while(count < total && count < length - 1) {
        val data = tapBuffer(blockOffset)
        ram.load(address,data)
        xor(data)
        blockOffset += 1
        count += 1
        address = (address + 1) & 0xFFFF
      }
      if (count == total) {
        xor(tapBuffer(blockOffset))
        cp(1)
      }
      else if (count < total) F = 80

      IX = address
      DE = total - count
      ctx.notifyBlockOffsetChanged(currentBlock.length,currentBlock.length)
      ctx.nextIndex
      true
    }

    override protected def calculateCycles: Unit = {
      durationInCycles = 0
      for(b <- tap.blocks) {
        val headerDuration = if (b.isHeader) pilotHeaderDuration else pilotDataDuration
        durationInCycles += headerDuration * pilotLength + syncUpLength + syncDownLength
        for(i <- 0 until b.length) {
          var bits = if (i == b.length - 1) lastByteUsedBits else 8
          var data = tap.data(b.startOffset + i)
          while (bits > 0) {
            if ((data & 0x80) > 0) durationInCycles += 2 * BIT1 else durationInCycles += 2 * BIT0
            data <<= 1
            bits -= 1
          }
        }
      }
    }
  }

  private class ID10_StandardSpeedData(override val index : Int,override val durationOffsetInSeconds : Int) extends TAPBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Standard Speed Data"

    pilotHeaderDuration = 8063
    pilotDataDuration = 3223
    pilotLength = 2168
    syncUpLength = 667
    syncDownLength = 735
    BIT0 = 855
    BIT1 = 1710
    pauseBeforeNextBlockMillis = 1000

    override val ID = 0x10
    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      pauseBeforeNextBlockMillis = readWord(buffer)
      tapBytes = readWord(buffer)
      val ofs = parsePos
      parsePos += tapBytes
      TAP.readTAPBlock(buffer,ofs,tapBytes) match {
        case Some(tap) =>
          this.tap = tap
          reset
          true
        case None =>
          false
      }
    }

    def setTAP(tap:TAP.TAPBlocks) : Unit = {
      this.tap = tap
      reset
      tapBytes = tap.data.length
    }
  }
  private class ID11_TurboSpeedData(override val index : Int,override val durationOffsetInSeconds : Int) extends TAPBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Turbo Speed Data"

    override val ID: Int = 0x11
    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      pilotLength = readWord(buffer)
      syncUpLength = readWord(buffer)
      syncDownLength = readWord(buffer)
      BIT0 = readWord(buffer)
      BIT1 = readWord(buffer)
      val len = readWord(buffer)
      lastByteUsedBits = readByte(buffer)
      pauseBeforeNextBlockMillis = readWord(buffer)
      tapBytes = readWord(buffer) | readByte(buffer) << 16
      val ofs = parsePos
      parsePos += tapBytes
      TAP.readTAPBlock(buffer,ofs,tapBytes) match {
        case Some(tap) =>
          this.tap = tap
          if (tap.blocks.head.isHeader) pilotHeaderDuration = len else pilotDataDuration = len
          reset
          true
        case None =>
          false
      }
    }
  }

  private class ID12_PureTone(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Pure Tone"
    override def blockInfo: String = s"$pulseLength:$pulseSize"

    override val ID: Int = 0x12
    private[this] var pulseLength = 0
    private[this] var pulseSize = 0
    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      pulseLength = readWord(buffer)
      pulseSize = readWord(buffer)
      reset
      true
    }
    override def reset: Unit = {
      super.reset
      pulseCounter = pulseLength
      pulseLengthCounter = pulseSize// << 1
      pulseUpDown(0) = pulseCounter
      pulseUpDown(1) = pulseCounter
    }

    override def cycle(ctx: Context): Unit = {
      if (produceSquareWave(ctx)) {
        ctx.nextIndex
      }
    }
    override protected def calculateCycles: Unit = durationInCycles = pulseCounter * pulseLength
  }

  private class ID13_PulseSequence(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Pulse Sequence"
    override def blockInfo: String = s"$pulses pulses"
    override val ID: Int = 0x13

    private[this] var pulses = 0
    private[this] var lengths : Array[Int] = _
    private[this] var pulseIndex = 0

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      pulses = readByte(buffer)
      lengths = Array.ofDim[Int](pulses)
      for(i <- 0 until pulses) {
        lengths(i) = readWord(buffer)
      }
      reset
      true
    }
    override def reset: Unit = {
      super.reset
      pulseIndex = 0
      pulseCounter = lengths(0)
      pulseLengthCounter = 1
      pulseUpDown(0) = pulseCounter
      pulseUpDown(1) = pulseCounter
    }
    override def cycle(ctx: Context): Unit = {
      if (produceSquareWave(ctx)) {
        pulseIndex += 1
        if (pulseIndex == pulses) ctx.nextIndex
        else {
          pulseCounter = lengths(pulseIndex)
          pulseLengthCounter = 1
          pulseUpDown(0) = pulseCounter
          pulseUpDown(1) = pulseCounter
        }
      }
    }
    override protected def calculateCycles: Unit = durationInCycles = lengths.sum
  }

  private class ID14_PureData(override val index : Int,override val durationOffsetInSeconds : Int) extends TAPBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Pure Data"
    override val ID: Int = 0x14

    override def reset: Unit = {
      super.reset
      setDataMode
    }

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      syncUpLength = 0
      syncDownLength = 0
      pilotLength = 0
      BIT0 = readWord(buffer)
      BIT1 = readWord(buffer)
      lastByteUsedBits = readByte(buffer)
      pauseBeforeNextBlockMillis = readWord(buffer)
      tapBytes = readWord(buffer) | readByte(buffer) << 16
      val ofs = parsePos
      parsePos += tapBytes
      TAP.readTAPBlock(buffer,ofs,tapBytes,true) match {
        case Some(tap) =>
          this.tap = tap
          reset
          true
        case None =>
          false
      }
    }
    override def fastLoad(ctx:Context,ram: Memory, z80: Z80): Boolean = false
  }

  private class ID20_Pause(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Silence"
    override def blockInfo: String = s"$pauseBeforeNextBlockMillis millis"
    override val ID: Int = 0x20

    private[this] var stop = false


    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      pauseBeforeNextBlockMillis = readWord(buffer)
      stop = pauseBeforeNextBlockMillis == 0
      true
    }

    override def cycle(ctx: Context): Unit = {
      if (stop) ctx.stopTheTape
      ctx.nextIndex
    }
  }

  private class ID21_GroupStart(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Group start"
    override def blockInfo: String = s"$groupName"
    override val ID: Int = 0x21

    private[this] var groupName = ""

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      val nameLen = readByte(buffer)
      val sb = new StringBuilder
      for(_ <- 1 to nameLen) sb.append(readByte(buffer).toChar)
      groupName = sb.toString
      parseProps += "GroupIndex" -> index
      parseProps += "GroupName" -> groupName
      true
    }
    override def cycle(ctx: Context): Unit = {
      ctx.setPulse(0)
      ctx.nextIndex
    }
  }

  private class ID22_GroupEnd(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Group end"
    override def blockInfo: String = s"$groupName"
    override val ID: Int = 0x22

    private[this] var groupName = ""
    private[this] var groupIndex = 0

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      parseProps.get("GroupIndex") match {
        case Some(i) =>
          groupIndex = i.asInstanceOf[Int]
          groupName = parseProps("GroupName").asInstanceOf[String]
          parseProps -= "GroupIndex"
          parseProps -= "GroupName"
          true
        case None =>
          false
      }
    }
    override def cycle(ctx: Context): Unit = {
      ctx.nextIndex
    }
  }

  class ID30_Text(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Text"
    override def blockInfo: String = s"$text"
    override val ID: Int = 0x30

    private[this] var text = ""

    def setText(text:String) : Unit = this.text = text

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      val nameLen = readByte(buffer)
      val sb = new StringBuilder
      for(_ <- 1 to nameLen) sb.append(readByte(buffer).toChar)
      text = sb.toString
      true
    }
    override def cycle(ctx: Context): Unit = {
      ctx.nextIndex
    }

    override protected def getBytes: Array[Byte] = {
      val buffer = new collection.mutable.ArrayBuffer[Byte]
      buffer += ID.toByte
      buffer += text.length.toByte
      for(c <- text) buffer += c.toByte
      buffer.toArray
    }
  }

  private class ID32_ArchiveInfo(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Archive Info"
    override def blockInfo: String = ""
    override val ID: Int = 0x32

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      val len = readWord(buffer)
      parsePos += len
      true
    }

    override def cycle(ctx: Context): Unit = {
      ctx.nextIndex
    }
  }

  private class ID24_LoopStart(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Loop Start"
    override def blockInfo: String = s"$loopCounter"
    override val ID: Int = 0x24

    private[this] var loopCounter = 0

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      loopCounter = readWord(buffer)
      if (loopCounter == 0) loopCounter = 1
      parseProps += "LoopCounter" -> loopCounter
      parseProps += "LoopIndex" -> index
      true
    }
    override def cycle(ctx: Context): Unit = {
      ctx.nextIndex
    }
  }

  private class ID25_LoopEnd(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Loop End"
    override def blockInfo: String = s"$loopIndex"
    override val ID: Int = 0x25

    private[this] var loopCounter = 0
    private[this] var counter = 0
    private[this] var loopIndex = 0

    override def reset : Unit = {
      super.reset
      counter = loopCounter
    }

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      parseProps.get("LoopCounter") match {
        case Some(c) =>
          loopCounter = c.asInstanceOf[Int]
          loopIndex = parseProps("LoopIndex").asInstanceOf[Int]
          reset
          true
        case None =>
          false
      }
    }
    override def cycle(ctx: Context): Unit = {
      counter -= 1
      if (counter > 0) ctx.index = loopIndex
      else ctx.nextIndex
    }
  }

  private class ID2A_StopTheTape48K(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Stop the tape 48K"
    override def blockInfo: String = ""
    override val ID: Int = 0x2A

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      readWord(buffer)
      readWord(buffer)
      true
    }

    override def cycle(ctx: Context): Unit = {
      if (ctx.is48K) ctx.stopTheTape
      ctx.nextIndex
    }
  }

  class ID15_DirectRecording(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Direct Recording"
    override def blockInfo: String = s"$cyclesPerSample:${samples.length}"
    override val ID: Int = 0x15

    protected var cyclesPerSample = 0
    protected var samples : Array[Int] = _
    protected var cycles = 0
    protected var sampleIndex = 0
    protected var bitCounter = 0
    protected var sample = 0

    override def reset : Unit = {
      super.reset
      cycles = cyclesPerSample
      sampleIndex = 0
      bitCounter = if (samples.length == 1) lastByteUsedBits else 8
      sample = samples(0)
    }

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      cyclesPerSample = readWord(buffer)
      pauseBeforeNextBlockMillis = readWord(buffer)
      lastByteUsedBits = readByte(buffer)
      val size = readWord(buffer) | readByte(buffer) << 16
      samples = Array.ofDim[Int](size)
      for(i <- 0 until size) samples(i) = readByte(buffer)
      reset
      true
    }

    override def cycle(ctx: Context): Unit = {
      cycles -= 1
      if (cycles == 0) {
        cycles = cyclesPerSample
        cycleSample(ctx)
      }
    }

    override def fastLoad(ctx:Context,ram: Memory, z80: Z80): Boolean = false

    override protected def calculateCycles: Unit = durationInCycles = cyclesPerSample * samples.length * 8  // TODO: take into account last byte's bits

    protected def cycleSample(ctx: Context) : Unit = {
      ctx.setPulse(if ((sample & 0x80) > 0) 0x40 else 0)
      sample <<= 1
      bitCounter -= 1
      if (bitCounter == 0) {
        sampleIndex += 1
        if (sampleIndex == samples.length) ctx.nextIndex
        else {
          bitCounter = if (sampleIndex == samples.length - 1) lastByteUsedBits else 8
          sample = samples(sampleIndex)
        }
      }
    }
  }

  class ID18_CSWRecording(override val index : Int,override val durationOffsetInSeconds : Int) extends ID15_DirectRecording(index,durationOffsetInSeconds) {
    override val blockType: String = "CSW Recording"
    override val ID: Int = 0x18

    private[this] var lastEar = 0
    private[this] var recBuffer : collection.mutable.ListBuffer[Byte] = _
    private[this] var samplingRate = 0
    private[this] var gt0xff = 0

    override def reset: Unit = {
      super.reset
      lastEar = 0
      sample = samples(0)
      if (recBuffer != null) recBuffer.clear()
      gt0xff = 0
    }

    def initForRecording(samplingRate:Int,cyclesPerSample:Int,pauseBeforeNextBlockMillis:Int) : Unit = {
      this.samplingRate = samplingRate
      this.cyclesPerSample = cyclesPerSample
      this.pauseBeforeNextBlockMillis = pauseBeforeNextBlockMillis
      recBuffer = new collection.mutable.ListBuffer[Byte]
      samples = Array.ofDim[Int](1)
    }

    def cycleRec(ear:Int) : Unit = {
      cycles -= 1
      if (cycles == 0) {
        cycles = cyclesPerSample
        if (ear != lastEar) {
          if (sample > 0xFF) {
            recBuffer += 0.toByte
            recBuffer += (sample & 0xFF).toByte
            recBuffer += ((sample >> 8) & 0xFF).toByte
            recBuffer += ((sample >> 16) & 0xFF).toByte
            recBuffer += ((sample >> 24) & 0xFF).toByte
            gt0xff += 1
          }
          else recBuffer += sample.toByte
          sample = 0
          lastEar = ear
        }
        else sample += 1
      }
    }

    private def writeToBuf(out:OutputStream,value:Int,size:Int) : Unit = {
      var v = value
      for(_ <- 1 to size) {
        out.write(v & 0xFF)
        v >>= 8
      }
    }

    override protected def getBytes: Array[Byte] = {
      val buffer = recBuffer.toArray
      val out = new ByteArrayOutputStream()
      val ztmp = new ByteArrayOutputStream()
      val zout = new DeflaterOutputStream(ztmp)
      zout.write(buffer)
      zout.finish
      zout.close

      out.write(ID)
      writeToBuf(out,10 + ztmp.size(),4)
      writeToBuf(out,pauseBeforeNextBlockMillis,2)
      writeToBuf(out,samplingRate,3)
      writeToBuf(out,2,1)
      writeToBuf(out,buffer.length - gt0xff * 4,4)

      out.write(ztmp.toByteArray)
      out.toByteArray
    }

    override protected def parseBlock(buffer: Array[Int], parseProps: collection.mutable.HashMap[String, Any]): Boolean = {
      val oldPos = parsePos
      val len = readWord(buffer) | readWord(buffer) << 16
      val N = len - 10
      pauseBeforeNextBlockMillis = readWord(buffer)
      cyclesPerSample = (parseProps("clock").asInstanceOf[Double] / (readWord(buffer) | readByte(buffer) << 16)).toInt
      val ct = readByte(buffer)
      val size = readWord(buffer) | readWord(buffer) << 16
      val currentPos = parsePos
      parsePos = oldPos + len + 4
      var in : InputStream = new InputStream {
        var i = currentPos
        var count = N
        override def read(): Int = {
          if (count > 0) {
            count -= 1
            val b = buffer(i)
            i += 1
            b
          }
          else -1
        }
      }
      if (ct == 2) in = new InflaterInputStream(in)
      val buf = new collection.mutable.ArrayBuffer[Int]
      var r = in.read()
      while (r != -1) {
        if (r == 0) r = in.read | in.read << 8 | in.read << 16 | in.read << 24
        buf += r
        r = in.read()
      }
      in.close
      samples = buf.toArray
      reset
      samples.length == size
    }
    override protected def cycleSample(ctx: Context) : Unit = {
      sample -= 1
      if (sample == 0) {
        ctx.pulse
        sampleIndex += 1
        if (sampleIndex == samples.length) ctx.nextIndex
        else sample = samples(sampleIndex)
      }
    }

    override def fastLoad(ctx:Context,ram: Memory, z80: Z80): Boolean = false

    override protected def calculateCycles: Unit = durationInCycles = cyclesPerSample * samples.sum
  }

  class ID35_CustomInfo(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Custom info"
    override def blockInfo: String = ""
    override val ID: Int = 0x35

    private var idString = ""

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      val sb = new StringBuilder
      for(i <- 0 to 9) sb.append(readByte(buffer).toChar)
      idString = sb.toString
      val len = readWord(buffer) | readWord(buffer) << 16
      parsePos += len
      true
    }
    override def cycle(ctx: Context): Unit = {
      ctx.nextIndex
    }
  }

  class ID23_JumpToBlock(override val index : Int,override val durationOffsetInSeconds : Int) extends TapeBlock(index,durationOffsetInSeconds) {
    override val blockType: String = "Jump to block"
    override def blockInfo: String = s"$block"
    override val ID: Int = 0x23

    private var block = 0

    override protected def parseBlock(buffer: Array[Int],parseProps:collection.mutable.HashMap[String,Any]): Boolean = {
      block = readWord(buffer).toShort + index - 1
      true
    }
    override def cycle(ctx: Context): Unit = {
      ctx.index = block
    }
  }
}
