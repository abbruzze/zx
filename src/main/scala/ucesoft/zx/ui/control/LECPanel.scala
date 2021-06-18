package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JCheckBox, JPanel}

class LECPanel(pref:Preferences) extends JPanel {
  import Preferences._

  init

  private def init : Unit = {
    setBorder(BorderFactory.createTitledBorder("LEC settings"))

    val lec = new JCheckBox("LEC enabled")

    lec.addActionListener(_ => pref.update[Boolean](LEC,lec.isSelected) )
    lec.setSelected(pref.get[Boolean](LEC).map(_.value).getOrElse(false))

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu").
      add(lec).xy(2,2).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
