package ucesoft.zx.ui

import ucesoft.zx.misc.ScaledImageIcon

import java.awt.{Component, Font}
import javax.swing.{ImageIcon, JTable}
import javax.swing.table.DefaultTableCellRenderer

object TapeCellRenderer extends DefaultTableCellRenderer {
  val playIcon : ImageIcon = ScaledImageIcon("play.png",10,10)

  private var plainFont : Font = _
  private var boldFont : Font = _

  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
    super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column)
    val model = table.getModel.asInstanceOf[TapeTableModel]
    if (plainFont == null) {
      plainFont = getFont
      boldFont = getFont().deriveFont(Font.BOLD)
    }
    if (model.getCurrentBlockIndex == row) setFont(boldFont) else setFont(plainFont)
    if (column == 0) {
      setText("")
      if (value.asInstanceOf[Boolean]) setIcon(playIcon)
      else setIcon(null)
    }
    else {
      setIcon(null)
      setText(value.toString)
    }

    this
  }
}

object TapeTest extends App {
  import javax.swing._
  val clk = ucesoft.zx.Clock.setSystemClock()(_ => {})
  val f = new JFrame
  val tzx = ucesoft.zx.format.TZX.readTZX("""C:\Users\ealeame\Desktop\ZX\giochi\Legend Of Kage.tzx""").get
  val tm = new TapeTableModel(tzx)
  val t = new JTable(tm)
  t.getColumnModel().getColumn(0).setPreferredWidth(TapeCellRenderer.playIcon.getIconWidth)
  t.getColumnModel().getColumn(4).setPreferredWidth(160)
  t.getColumnModel().getColumn(5).setPreferredWidth(160)
  t.setDefaultRenderer(classOf[Any],TapeCellRenderer)
  f.getContentPane.add("Center",new JScrollPane(t))
  f.pack
  f.setVisible(true)
  tm.setCurrentBlockIndex(2)
}
