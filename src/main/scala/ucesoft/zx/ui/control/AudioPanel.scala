package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences
import ucesoft.zx.spectrum.Spectrum
import ucesoft.zx.ui.VolumeSettingsPanel

import java.awt.BorderLayout
import javax.swing.{BorderFactory, ButtonGroup, JCheckBox, JPanel, JRadioButton}

class AudioPanel(spectrum:Spectrum) extends JPanel {
  import Preferences._

  init

  private def init : Unit = {
    val pref = spectrum.preferences
    setBorder(BorderFactory.createTitledBorder("Audio settings"))

    val ayOn48k = new JCheckBox("AY38912 enabled on 48k")
    val none = new JRadioButton("Stereo")
    val abc = new JRadioButton("Stereo ABC configuration")
    val acb = new JRadioButton("Stereo ACB configuration")
    val group = new ButtonGroup
    group.add(none)
    group.add(abc)
    group.add(acb)

    ayOn48k.addActionListener(_ => pref.update[Boolean](AUDIO_AY_ON48K_ENABLED,ayOn48k.isSelected) )
    none.addActionListener(_ => pref.update[String](AUDIO_AY_MODE,"none") )
    abc.addActionListener(_ => pref.update[String](AUDIO_AY_MODE,"abc") )
    acb.addActionListener(_ => pref.update[String](AUDIO_AY_MODE,"acb") )

    pref.get[String](AUDIO_AY_MODE).map(_.value).getOrElse("48k") match {
      case "none" => none.setSelected(true)
      case "abc" => abc.setSelected(true)
      case "acb" => acb.setSelected(true)
    }
    ayOn48k.setSelected(pref.get[Boolean](AUDIO_AY_ON48K_ENABLED).map(_.value).getOrElse(false))

    val volPanel = new VolumeSettingsPanel(spectrum.ay.driver,spectrum.speaker.driver)
    volPanel.setBorder(BorderFactory.createTitledBorder("Master volume"))
    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu,pref,10dlu,pref,10dlu").
      add(ayOn48k).xy(2,2).
      addStack(none,abc,acb).xy(2,4).
      add(volPanel).xy(2,6).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
