package ucesoft.zx.ui

import ucesoft.zx.format.TZX.TZXBlocks

import javax.swing.table.AbstractTableModel

class TapeTableModel(private var tape:TZXBlocks) extends AbstractTableModel {
  private val COLUMNS = Array("","Index","Offset","Duration","Block Type","Block Info")
  private var currentBlock = 0

  def setCurrentBlockIndex(index:Int) : Unit = {
    currentBlock = index
    fireTableDataChanged()
  }

  def newTape(tape:TZXBlocks) : Unit = {
    this.tape = tape
    fireTableDataChanged()
  }

  def getCurrentBlockIndex : Int = currentBlock

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false

  override def getColumnName(column: Int): String = COLUMNS(column)

  override def getRowCount: Int = tape.blocks.length

  override def getColumnCount: Int = COLUMNS.length

  override def getValueAt(rowIndex: Int, columnIndex: Int): Any = {
    columnIndex match {
      case 0 => rowIndex == currentBlock
      case 1 => tape.blocks(rowIndex).index.toString
      case 2 => toMinSec(tape.blocks(rowIndex).durationOffsetInSeconds)
      case 3 => toMinSec(tape.blocks(rowIndex).getDurationInSeconds)
      case 4 => tape.blocks(rowIndex).blockType
      case 5 => tape.blocks(rowIndex).blockInfo
    }
  }

  private def toMinSec(seconds:Int) : String = {
    val min = seconds / 60
    val sec = seconds % 60
    s"${if (min < 10) "0" else ""}$min:${if (sec < 10) "0" else ""}$sec"
  }
}
