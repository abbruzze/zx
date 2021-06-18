package ucesoft.zx.ui

import ucesoft.zx.Version

import java.awt.{BorderLayout, Desktop, FlowLayout}
import javax.swing.event.HyperlinkEvent
import javax.swing.{ImageIcon, JButton, JDialog, JEditorPane, JFrame, JLabel, JPanel}

object AboutPanel {
  def showAboutDialog(parent:JFrame) : Unit = {
    val f = new JDialog(parent,"About",true)
    f.getContentPane.add("Center",new AboutPanel(() => f.dispose()))
    f.pack()
    val coord = parent.getLocationOnScreen
    val dim = parent.getSize()
    val fdim = f.getSize()
    f.setLocation(coord.x + (dim.width - fdim.width) / 2,coord.y + (dim.height - fdim.height) / 2)
    f.setVisible(true)
  }
}

class AboutPanel(closeAction:() => Unit) extends JPanel {
  init

  private def init : Unit = {
    setLayout(new BorderLayout())
    val center = new JPanel(new FlowLayout(FlowLayout.CENTER))
    center.add(new JLabel(new ImageIcon(getClass.getResource("/resources/images/zx_about.png"))))
    add("Center",center)
    val south = new JPanel(new BorderLayout())
    add("South",south)
    val editor = new JEditorPane()
    editor.setEditorKit(JEditorPane.createEditorKitForContentType("text/html"))
    editor.setEditable(false)
    editor.setText(s"""<html><center><h2>ZX emulator</h2>version ${Version.VERSION}<br>
                      |built on ${Version.BUILD_DATE}<br>
                      |java version ${scala.util.Properties.javaVersion}<br>
                      |scala version ${Version.SCALA_VERSION}<br>
                      |by Alessandro Abbruzzetti 2021<br>
                      |visit <a href=https://github.com/abbruzze/zx>https://github.com/abbruzze/zx</a>
                      |</html>""".stripMargin)

    south.add(editor)
    editor.addHyperlinkListener(e =>
      if(e.getEventType() == HyperlinkEvent.EventType.ACTIVATED && Desktop.isDesktopSupported()) Desktop.getDesktop().browse(e.getURL().toURI())
    )
    val close = new JButton("Close")
    close.addActionListener(_ => closeAction())
    val dummy = new JPanel(new FlowLayout(FlowLayout.CENTER))
    dummy.add(close)

    south.add("South",dummy)
  }
}
