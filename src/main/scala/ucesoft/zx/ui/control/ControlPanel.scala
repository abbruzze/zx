package ucesoft.zx.ui.control

import ucesoft.zx.misc.Preferences
import ucesoft.zx.spectrum.Spectrum

import java.awt.{BorderLayout, CardLayout, Color, FlowLayout}
import javax.swing.event.{TreeExpansionEvent, TreeSelectionEvent, TreeSelectionListener, TreeWillExpandListener}
import javax.swing.tree.{DefaultMutableTreeNode, DefaultTreeCellRenderer, ExpandVetoException, TreeSelectionModel}
import javax.swing.{ImageIcon, JButton, JCheckBox, JDialog, JFrame, JLabel, JOptionPane, JPanel, JScrollPane, JTree}
import scala.annotation.tailrec

object ControlPanel {
  def getDialog(parent:JFrame,spectrum:Spectrum,videoControl:VideoControl) : JDialog = {
    val dialog = new JDialog(parent,"Control panel")

    dialog.getContentPane.add("Center",new ControlPanel(spectrum,videoControl))
    dialog.pack()
    dialog.setResizable(false)
    dialog.setLocationRelativeTo(parent)
    dialog
  }
}

class ControlPanel(spectrum:Spectrum,videoControl:VideoControl) extends JPanel with TreeSelectionListener {
  private val cardPanel = new JPanel(new CardLayout())
  private val root = new DefaultMutableTreeNode("Preferences")
  private val tree = new JTree(root)

  init

  @tailrec private def expandTree(tree:JTree) : Unit = {
    val rowCount = tree.getRowCount
    for(r <- 0 to rowCount) tree.expandRow(r)
    if (tree.getRowCount != rowCount) expandTree(tree)
  }

  private def init : Unit = {
    tree.addTreeWillExpandListener(new TreeWillExpandListener {
      override def treeWillExpand(event: TreeExpansionEvent): Unit = {}
      override def treeWillCollapse(event: TreeExpansionEvent): Unit = throw new ExpandVetoException(event)
    })
    tree.setShowsRootHandles(false)
    tree.getSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    tree.getSelectionModel.addTreeSelectionListener(this)
    val renderer = tree.getCellRenderer.asInstanceOf[DefaultTreeCellRenderer]
    renderer.setLeafIcon(null)
    renderer.setClosedIcon(null)
    renderer.setOpenIcon(null)
    // general
    val general = new DefaultMutableTreeNode("General")
    // general -> model
    general.add(new DefaultMutableTreeNode("Model"))
    root.add(general)
    // joystick
    val joy = new DefaultMutableTreeNode("Joystick")
    root.add(joy)
    // Video
    val video = new DefaultMutableTreeNode("Video")
    root.add(video)
    // Tape
    val tape = new DefaultMutableTreeNode("Tape")
    root.add(tape)
    // Audio
    val audio = new DefaultMutableTreeNode("Audio")
    root.add(audio)
    // Peripherals
    val peripherals = new DefaultMutableTreeNode("Peripherals")
    root.add(peripherals)
    // Mouse
    val mouse = new DefaultMutableTreeNode("Mouse")
    peripherals.add(mouse)
    // Interface I
    val ifI = new DefaultMutableTreeNode("Interface I")
    peripherals.add(ifI)

    // Memory
    val memory = new DefaultMutableTreeNode("Memory")
    root.add(memory)
    // ROMs
    val roms = new DefaultMutableTreeNode("System ROMs")
    memory.add(roms)
    // LEC
    val lec = new DefaultMutableTreeNode("LEC")
    memory.add(lec)

    val treeView = new JScrollPane(tree)

    expandTree(tree)

    setLayout(new BorderLayout())
    add("West",treeView)
    add("Center",cardPanel)

    val save = new JButton("Save configuration")
    save.addActionListener(_ => {
      spectrum.savePreferences
      JOptionPane.showMessageDialog(this,"Configuration saved", "Configuration",JOptionPane.INFORMATION_MESSAGE)
    })

    val buttonPanel = new JPanel(new BorderLayout())
    val dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(save)
    val autosave = new JCheckBox("Auto save preferences on exit")
    autosave.setSelected(spectrum.preferences.get[Boolean](Preferences.AUTOSAVE_PREFERENCES).map(_.value).getOrElse(false))
    autosave.addActionListener(_ => spectrum.preferences.update[Boolean](Preferences.AUTOSAVE_PREFERENCES,autosave.isSelected))
    dummyPanel.add(autosave)

    buttonPanel.add("North",dummyPanel)
    add("South",buttonPanel)

    val southPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    southPanel.setBackground(Color.black)
    southPanel.add(new JLabel(new ImageIcon(getClass.getResource("/resources/images/controlpanel_logo.png"))))
    buttonPanel.add("South",southPanel)

    cardPanel.add(new JoystickPanel(spectrum.preferences),"Joystick")
    cardPanel.add(new ModelPanel(spectrum.preferences),"Model")
    cardPanel.add(new VideoPanel(spectrum,videoControl),"Video")
    cardPanel.add(new TapePanel(spectrum),"Tape")
    cardPanel.add(new AudioPanel(spectrum),"Audio")
    cardPanel.add(new MousePanel(spectrum.preferences),"Mouse")
    cardPanel.add(new InterfaceIPanel(spectrum),"Interface I")
    cardPanel.add(new SystemROMsPanel(spectrum.preferences),"System ROMs")
    cardPanel.add(new LECPanel(spectrum.preferences),"LEC")

    cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,"Model")
  }

  override def valueChanged(e: TreeSelectionEvent): Unit = {
    val node = tree.getLastSelectedPathComponent.asInstanceOf[DefaultMutableTreeNode]
    if (node.isLeaf) {
      val card = node.getUserObject.toString
      cardPanel.getLayout.asInstanceOf[CardLayout].show(cardPanel,card)
    }
  }
}
