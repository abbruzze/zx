package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JCheckBox, JPanel}

class MousePanel(pref:Preferences) extends JPanel {
  import Preferences._

  init

  private def init : Unit = {
    setBorder(BorderFactory.createTitledBorder("Mouse settings"))

    val mouse = new JCheckBox("Kempstone mouse enabled")

    mouse.addActionListener(_ => pref.update[Boolean](MOUSE_ENABLED,mouse.isSelected) )
    mouse.setSelected(pref.get[Boolean](MOUSE_ENABLED).map(_.value).getOrElse(false))

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu").
      add(mouse).xy(2,2).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
