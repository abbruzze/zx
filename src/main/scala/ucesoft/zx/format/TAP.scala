package ucesoft.zx.format

import ucesoft.zx.tape.TapeBlockInfo

object TAP {
  def getHeaderInfo(startOffset:Int,buffer:Array[Int]) : HeaderInfo = {
    var i = startOffset + 2
    var j = 0
    val sb = new StringBuilder
    while (i < buffer.length && j < 10) {
      val c = buffer(i)
      sb.append(c.toChar)
      i += 1
      j += 1
    }
    HeaderInfo(buffer(startOffset + 1),sb.toString)
  }

  case class HeaderInfo(headerType:Int,fileName:String) {
    def typeString : String = {
      headerType match {
        case 0 => "Program"
        case 1 => "Number array"
        case 2 => "Character array"
        case 3 => "Code"
        case _ => "UNKNOWN"
      }
    }
    override def toString: String = {
      val typ = typeString
      if (headerType < 4) s"HeaderInfo($typ,'$fileName')" else s"HeaderInfo($typ/$headerType)"
    }
  }
  case class TAPBlocks(blocks:List[Block],data:Array[Int]) {
    override def toString: String = s"TAPBlocks($blocks,${data.length})"
  }
  case class Block(startOffset:Int,length:Int,headerInfo:Option[HeaderInfo],override val index : Int) extends TapeBlockInfo {
    override val durationOffsetInSeconds : Int = 0
    override val blockType: String = if (headerInfo.isDefined) "HEADER" else "DATA"
    override val blockInfo: String = headerInfo match {
      case Some(info) => info.fileName
      case None => ""
    }
    def isHeader : Boolean = headerInfo.isDefined
    def getDurationInCycles : Long = 0
    def getDurationInSeconds : Int = 0
    override def toString : String = s"${if (headerInfo.isDefined) "HEADER" else "DATA"}($startOffset,$length)${if (headerInfo.isDefined) s"[${headerInfo.get}]" else ""}"
  }

  def readTAP(buf:Array[Int],offset:Int,length:Int) : Option[TAPBlocks] = {
    val buffer = Array.ofDim[Int](length)
    System.arraycopy(buf,offset,buffer,0,length)
    try {
      var ptr = 0
      val blocks = new collection.mutable.ArrayBuffer[Block]
      while (ptr < buffer.length) {
        val length = buffer(ptr) | buffer(ptr + 1) << 8
        val flag = buffer(ptr + 2)
        blocks += Block(ptr + 2,length,if (flag < 128) Some(getHeaderInfo(ptr + 2,buffer)) else None,blocks.length)
        ptr += length + 2
      }
      Some(TAPBlocks(blocks.toList,buffer))
    }
    catch {
      case _:Throwable =>
        None
    }
  }

  def readTAPBlock(buf:Array[Int],offset:Int,length:Int,pureData : Boolean = false) : Option[TAPBlocks] = {
    val buffer = Array.ofDim[Int](length)
    System.arraycopy(buf,offset,buffer,0,length)
    try {
      val flag = buffer(0)
      Some(TAPBlocks(List(Block(0, length, if (!pureData && flag < 128 && length > 1) Some(getHeaderInfo(0, buffer)) else None,0)),buffer))
    }
    catch {
      case _:Throwable =>
        None
    }
  }

  def readTAP(file:String) : Option[TAPBlocks] = {
    val buffer = java.nio.file.Files.readAllBytes(new java.io.File(file).toPath).map(b => b.toInt & 0xFF)
    readTAP(buffer,0,buffer.length)
  }

  def main(args:Array[String]) : Unit = {
    for(l <- readTAP(args(0));b <- l.blocks) {
      b match {
        case Block(_,_,Some(hi),_) =>
          println(s"$b($hi)")
        case _ =>
          println(b)
      }
    }
  }
}
