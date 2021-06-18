package ucesoft.zx.format

import ucesoft.zx.spectrum.MicrodriveListener

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MDR {
  var unformatted_sector_size = 254
  var writeChanges = true

  def readMDR(file:String) : Option[MDR] = {
    val f = new File(file)
    if (f.length() == 137923) {
      val mdr = new MDR(file)
      mdr.initTape(java.nio.file.Files.readAllBytes(f.toPath).map(_.toInt & 0xFF))
      Some(mdr)
    }
    else None
  }

  def createNewMDR(file:String) : MDR = new MDR(file,true)

  case class Entry(name:Option[String],headerPos:Int,recordPos:Int,sector:Int)
  case class Entries(cartName:String,entries:List[Entry])

  final val GAP_SIZE = 60
  final val SYNC_SIZE = 12
  final val RECORD_SIZE = 543
  final val RAW_RECORD_SIZE = 789
  final val TAPE_SIZE = RAW_RECORD_SIZE * 254
  final val GAP_MARK = 0x100
  final val GAP_VALUE = 0x5A
}

class MDR private(val file:String,create:Boolean = false) {
  import MDR._

  private[this] val tape = if (create) Array.fill[Int](RAW_RECORD_SIZE * unformatted_sector_size)(GAP_MARK | GAP_VALUE) else Array.fill[Int](TAPE_SIZE)(GAP_MARK | GAP_VALUE)
  private[this] var listener : MicrodriveListener = _

  def setMicrodriveListener(l:MicrodriveListener) : Unit = listener = l

  private[this] var tapePos = 0
  private[this] var lastRead = 0xFF
  private[this] var WP = false
  private[this] var name = ""
  private[this] var modified = false

  def eraseAll : Unit = util.Arrays.fill(tape,GAP_MARK | GAP_VALUE)

  def isModified : Boolean = modified

  def initTape(data:Array[Int]) : Unit = {
    WP = data(data.length - 1) != 0
    name = getNameAtOffset(4,data)

    var dataPtr = 0
    var tapePtr = 0

    def insertGap : Unit = {
      for(_ <- 1 to GAP_SIZE) {
        tape(tapePtr) = GAP_MARK | GAP_VALUE
        tapePtr += 1
      }
    }
    def insertPreamble : Unit = {
      for(p <- 1 to SYNC_SIZE) {
        if (p < 11) tape(tapePtr) = 0 else tape(tapePtr) = 0xFF
        tapePtr += 1
      }
    }

    while (dataPtr < data.length - 1) {
      insertGap
      insertPreamble
      System.arraycopy(data,dataPtr,tape,tapePtr,15)
      dataPtr += 15
      tapePtr += 15

      insertGap
      insertPreamble
      System.arraycopy(data,dataPtr,tape,tapePtr,528)
      dataPtr += 528
      tapePtr += 528
    }
  }

  def moveToNextGap : Unit = {
    val lastPos = tapePos
    incPos
    while (tapePos != lastPos && (tape(tapePos) & GAP_MARK) == 0) incPos
  }

  def getCartName : String = name

  def reset : Unit = {
    tapePos = 0
    if (listener != null) listener.posChanged(0)
  }

  private def getNameAtOffset(ofs:Int,data:Array[Int]) : String = {
    val sb = new StringBuilder
    for(o <- ofs until (ofs + 10)) sb.append(data(o % data.length).toChar)
    sb.toString
  }

  private def incPos : Unit = {
    lastRead = tape(tapePos) & 0xFF
    if (tapePos == tape.length - 1) tapePos = 0 else tapePos += 1

    listener.posChanged((tapePos.toDouble / tape.length * 100).toInt)
  }

  def read : Int = {
    incPos
    lastRead
  }

  def write(data:Int) : Unit = {
    tape(tapePos) = data
    modified = true
    incPos
  }

  def writeGap : Unit = {
    tape(tapePos) = GAP_MARK | GAP_VALUE
    incPos
  }

  def readStatus : Int = {
    var status = 0xF8

    if ((tape(tapePos) & GAP_MARK) > 0) status |= 0x04
    else if ((lastRead & tape(tapePos)) == 0) status |= 0x02
    if (!WP) status |= 1

    incPos
    status
  }

  def findEntries : Option[MDR.Entries] = {
    var ptr = 0
    var loopCount = 0
    var cartName : String = null
    val entries = new ListBuffer[Entry]
    val posSet = new mutable.HashSet[Int]
    val sectorSet = new mutable.HashSet[Int]

    def incPtr(how:Int = 1) : Unit = {
      var c = how
      while (c > 0) {
        if (ptr == tape.length - 1) {
          loopCount += 1
          ptr = 0
        }
        else ptr += 1
        c -= 1
      }
    }

    def checkSync : Boolean = {
      var c = 0
      while (c < SYNC_SIZE) {
        val cmp = if (c >= SYNC_SIZE - 2) 0xFF else 0x00
        if (tape(ptr) != cmp) return false
        incPtr()
        c += 1
      }
      true
    }

    while (loopCount < 2) {
      while (loopCount < 2 && (tape(ptr) & GAP_MARK) == 0) incPtr()
      while (loopCount < 2 && (tape(ptr) & GAP_MARK) > 0) incPtr()

      if (loopCount < 2) {
        if (checkSync) {
          val pos = ptr
          if (cartName == null) cartName = getNameAtOffset(ptr + 4, tape)

          while (loopCount < 2 && (tape(ptr) & GAP_MARK) == 0) incPtr()
          while (loopCount < 2 && (tape(ptr) & GAP_MARK) > 0) incPtr()

          if (loopCount < 2) {
            if (checkSync) {
              val pos2 = ptr
              if (!posSet.contains(pos) && (tape(pos) & 1) == 1) {
                val sector = tape(pos + 1)
                if (!sectorSet.contains(sector)) {
                  posSet += pos
                  sectorSet += sector
                  //println(s"Sector ${tape(pos + 1)}")
                  val entryName = getNameAtOffset(ptr + 4, tape)
                  val entryFC = entryName.charAt(0).toInt
                  if (entryFC >= 32 && entryFC <= 122) entries += Entry(Some(entryName), pos, pos2, sector)
                  else entries += Entry(None, pos, pos2, sector)
                }
              }
            }
          }
        }
      }
    }
    if (cartName == null) None
    else Some(Entries(cartName,entries.toList))
  }

  def save : Boolean = {
    if (!modified || !writeChanges) return true
    modified = false

    findEntries match {
      case Some(Entries(_,entries)) if entries.size == 254 =>
        try {
          val out = new BufferedOutputStream(new FileOutputStream(file))
          for(Entry(_,headerPos,recordPos,_) <- entries) {
            val pos = Array(headerPos,recordPos)
            for(s <- 0 to 1) {
              var total = if (s == 0) 15 else 528
              var ptr = pos(s)
              while (total > 0) {
                out.write(tape(ptr))
                if (ptr == tape.length - 1) ptr = 0 else ptr += 1
                total -= 1
              }
            }
          }
          if (create) out.write(0) // non write protected
          else out.write(if (WP) 1 else 0) // WP
          out.close
          true
        }
        catch {
          case t:Throwable =>
            println(s"Can't save MDR: $t")
            false
        }
      case Some(Entries(_,entries)) =>
        println(s"Can't save MDR: format not recognized (sectors = ${entries.size})")
        false
      case None =>
        println("Can't save MDR: format not recognized")
        false
    }
  }
}
