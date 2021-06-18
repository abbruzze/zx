package ucesoft.zx.ui

import ucesoft.zx.format.TZX
import ucesoft.zx.misc.ScaledImageIcon
import ucesoft.zx.tape.{TAPE_PLAY, TAPE_RECORD, TAPE_STOP, Tape, TapeBlockInfo, TapeListener, TapeState}

import java.awt.event.{MouseAdapter, MouseEvent, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, GridLayout, Rectangle}
import javax.swing.{ImageIcon, JCheckBoxMenuItem, JDialog, JFrame, JLabel, JMenuItem, JPanel, JPopupMenu, JProgressBar, JScrollPane, JTable, UIManager}

class TapeStatusPanel(tape:Tape,parent:JFrame) extends JPanel with TapeListener with SwingAware {
  //private val cassetteIcon = ScaledImageIcon("cassette.png")
  private val playIcon = ScaledImageIcon("play.png")
  private val stopIcon = ScaledImageIcon("stop.png")
  private val recIcon = ScaledImageIcon("record.png")
  private val rewindIcon = ScaledImageIcon("rewind.png")
  private val ejectIcon = ScaledImageIcon("eject.png")
  private val viewerIcon = ScaledImageIcon("browser.png")
  private val stateIcons : Map[TapeState, ImageIcon] = Map(TAPE_PLAY -> playIcon,TAPE_STOP -> stopIcon,TAPE_RECORD -> recIcon)
  private var blocks : Option[TZX.TZXBlocks] = None
  private val indexLabel = new JLabel("0")
  private val stateLabel = new JLabel(stopIcon)
  private val totalProgress = new JProgressBar()
  private val blockProgress = new JProgressBar()

  private val tableModel = new TapeTableModel(TZX.TZXBlocks(Array(),None))
  private val table = new JTable(tableModel)
  private val viewer = new JDialog(parent,"Tape Viewer",false)

  init

  private def init : Unit = {
    setLayout(new BorderLayout())
    val leftPane = new JPanel
    leftPane.add(new JLabel(ScaledImageIcon("cassette.png",20,20)))
    leftPane.add(stateLabel)
    leftPane.add(indexLabel)
    add("West",leftPane)
    val centerPane = new JPanel(new GridLayout(2,1,2,2))
    centerPane.add(totalProgress)
    centerPane.add(blockProgress)
    add("Center",centerPane)
    totalProgress.setString("empty")
    blockProgress.setString("")
    val dim = totalProgress.getPreferredSize
    totalProgress.setPreferredSize(new Dimension(80,dim.height))
    blockProgress.setPreferredSize(new Dimension(80,dim.height))
    totalProgress.setStringPainted(true)
    blockProgress.setStringPainted(true)

    val popup = new JPopupMenu
    val playItem = new JMenuItem("Play",playIcon)
    playItem.addActionListener(_ => tape.setState(TAPE_PLAY))
    val stopItem = new JMenuItem("Stop",stopIcon)
    stopItem.addActionListener(_ => tape.setState(TAPE_STOP))
    val recItem = new JMenuItem("Record",recIcon)
    recItem.addActionListener(_ => tape.setState(TAPE_RECORD))
    val rewindItem = new JMenuItem("Rewind",rewindIcon)
    rewindItem.addActionListener(_ => tape.rewind())
    val ejectItem = new JMenuItem("Eject",ejectIcon)
    ejectItem.addActionListener(_ => tape.eject)
    val viewerItem = new JCheckBoxMenuItem("Open viewer",viewerIcon)
    viewerItem.addActionListener(_ => {
      viewer.setVisible(viewerItem.isSelected)
    })
    popup.add(playItem)
    popup.add(stopItem)
    popup.add(recItem)
    popup.add(rewindItem)
    popup.add(ejectItem)
    popup.add(viewerItem)

    stateLabel.setToolTipText("Click to change state")
    stateLabel.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = popup.show(e.getComponent,e.getX,e.getY)
    })

    if (tape != null) tape.addTapeListener(this)

    table.setDefaultRenderer(classOf[Any],TapeCellRenderer)
    table.getColumnModel().getColumn(0).setPreferredWidth(TapeCellRenderer.playIcon.getIconWidth)
    table.getColumnModel().getColumn(4).setPreferredWidth(160)
    table.getColumnModel().getColumn(5).setPreferredWidth(160)
    table.getSelectionModel.addListSelectionListener(e => {
      if (e.getValueIsAdjusting) {
        if (tape.getState == TAPE_STOP) {
          val rowSelected = table.getSelectedRow
          if (rowSelected != -1) {
            tape.rewind(rowSelected)
            tableModel.setCurrentBlockIndex(rowSelected)
          }
        }
      }
    })

    val sp = new JScrollPane(table)
    viewer.getContentPane.add("Center",sp)
    viewer.pack
    viewer.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = {
        viewer.setVisible(false)
        viewerItem.setSelected(false)
      }
    })
  }

  override def stateChanged(oldState: TapeState, newState: TapeState): Unit = swing { stateLabel.setIcon(stateIcons(newState)) }

  override def blockChanged(block: TapeBlockInfo): Unit = swing {
    indexLabel.setText(s"${block.index}")
    totalProgress.setValue(block.durationOffsetInSeconds)
    blockProgress.setString(toMinSec(block.getDurationInSeconds))
    blockProgress.setValue(0)
    tableModel.setCurrentBlockIndex(block.index - 1)
    table.setRowSelectionInterval(block.index - 1,block.index - 1)
    table.scrollRectToVisible(new Rectangle(table.getCellRect(block.index - 1, 0, true)))
  }

  override def blockOffsetChanged(offset: Int, length: Int): Unit = swing { blockProgress.setValue(offset * 100 / length) }

  override def tapeContentChanged(blocks: Option[TZX.TZXBlocks]): Unit = swing {
    this.blocks = blocks
    indexLabel.setText("0")
    totalProgress.setValue(0)
    blockProgress.setValue(0)
    blocks match {
      case None =>
        viewer.setTitle(s"Tape Viewer")
        totalProgress.setString("empty")
        blockProgress.setString("")
        tableModel.newTape(TZX.TZXBlocks(Array(),None))
        setToolTipText("")
      case Some(bl@TZX.TZXBlocks(bs,file)) =>
        viewer.setTitle(s"Tape Viewer ${file.map(_.getName).getOrElse("")}")
        tableModel.newTape(bl)
        if (bs.length > 0) {
          val last = bs(bs.length - 1)
          val duration = last.durationOffsetInSeconds + last.getDurationInSeconds
          totalProgress.setString(toMinSec(duration))
          totalProgress.setMaximum(duration)
          blockProgress.setString(toMinSec(bs(0).getDurationInSeconds))
        }
        else {
          totalProgress.setString(toMinSec(0))
          totalProgress.setMaximum(0)
          blockProgress.setString(toMinSec(0))
        }
        setToolTipText(file.map(_.toString).getOrElse(""))
    }
  }

  override def endOfTape: Unit = {
    totalProgress.setValue(totalProgress.getMaximum)
  }

  private def toMinSec(seconds:Int) : String = {
    val min = seconds / 60
    val sec = seconds % 60
    s"${if (min < 10) "0" else ""}$min:${if (sec < 10) "0" else ""}$sec"
  }
}
