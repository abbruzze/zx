package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import javax.swing.{BorderFactory, ButtonGroup, JPanel, JRadioButton}

class ModelPanel(pref:Preferences) extends JPanel with Preferences.PreferenceChangeListener {
  import Preferences._

  private val model16K = new JRadioButton("16K")
  private val model48K = new JRadioButton("48K")
  private val model128K = new JRadioButton("128K")
  private val model128KPlus2 = new JRadioButton("128K+2")
  private val model128KPlus2A = new JRadioButton("128K+2A")
  private val model128KPlus3 = new JRadioButton("128K+3")

  init

  private def init : Unit = {
    setBorder(BorderFactory.createTitledBorder("Model settings"))

    val group = new ButtonGroup
    group.add(model16K)
    group.add(model48K)
    group.add(model128K)
    group.add(model128KPlus2)
    group.add(model128KPlus2A)
    group.add(model128KPlus3)

    model16K.addActionListener(_ => pref.update[String](MODEL,"16k") )
    model48K.addActionListener(_ => pref.update[String](MODEL,"48k") )
    model128K.addActionListener(_ => pref.update[String](MODEL,"128k") )
    model128KPlus2.addActionListener(_ => pref.update[String](MODEL,"128k+2") )
    model128KPlus2A.addActionListener(_ => pref.update[String](MODEL,"128k+2A") )
    model128KPlus3.addActionListener(_ => pref.update[String](MODEL,"128k+3")
    )

    select

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu").
      addStack(model16K,model48K,model128K,model128KPlus2,model128KPlus2A,model128KPlus3).xy(2,2).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)

    pref.get[String](MODEL).foreach(_.addChangeListener(this))
  }

  private def select : Unit = {
    val radio = pref.get[String](MODEL).map(_.value).getOrElse("48k") match {
      case "16k" => model16K
      case "48k" => model48K
      case "128k" => model128K
      case "128k+2" => model128KPlus2
      case "128k+2A" => model128KPlus2A
      case "128k+3" => model128KPlus3
      case _ => model48K
    }

    radio.setSelected(true)
  }

  override def preferenceHasChanged(pref: Preference[_]): Unit =
    select
}
