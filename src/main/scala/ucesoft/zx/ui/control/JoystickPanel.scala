package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.joystick.AbstractUSBJoystick
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import javax.swing.{BorderFactory, ButtonGroup, JPanel, JRadioButton}

class JoystickPanel(pref:Preferences) extends JPanel {
  import Preferences._

  private val keyPanel = new JoystickKeysPanel(pref)

  init

  private def init : Unit = {
    setBorder(BorderFactory.createTitledBorder("Joystick settings"))
    val none = new JRadioButton("None")
    val kempstoneWithKeys = new JRadioButton("Kempstone on keyboards")
    val kempstoneWithUSB = new JRadioButton("Kempstone on USB")
    val group = new ButtonGroup
    group.add(none)
    group.add(kempstoneWithKeys)
    group.add(kempstoneWithUSB)

    val usbControllerName = AbstractUSBJoystick.getControllerName
    if (usbControllerName.isEmpty) kempstoneWithUSB.setEnabled(false)

    none.addActionListener(_ => {
      pref.update[String](JOY_TYPE,"none")
      keyPanel.setEnabled(false)
    })
    kempstoneWithKeys.addActionListener(_ => {
      pref.update[String](JOY_TYPE,"kempstone_keyboard")
      keyPanel.setEnabled(true)
    })
    kempstoneWithUSB.addActionListener(_ => {
      pref.update[String](JOY_TYPE,"kempstone_usb")
      keyPanel.setEnabled(false)
    })

    pref.get[String](JOY_TYPE).map(_.value) match {
      case None|Some("none") =>
        none.setSelected(true)
        keyPanel.setEnabled(false)
      case Some("kempstone_keyboard") =>
        kempstoneWithKeys.setSelected(true)
        keyPanel.setEnabled(true)
      case Some("kempstone_usb") =>
        kempstoneWithUSB.setSelected(true)
        keyPanel.setEnabled(false)
      case _ =>
    }

    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,5dlu").
      rows("10dlu,pref,10dlu,pref,20dlu,pref,10dlu").
      addStack(none,kempstoneWithKeys,kempstoneWithUSB).xy(2,2).
      add(s"USB controller: ${usbControllerName.getOrElse("not found")}").xy(2,4).
      add(keyPanel).xy(2,6).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
