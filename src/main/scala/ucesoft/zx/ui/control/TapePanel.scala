package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences
import ucesoft.zx.spectrum.Spectrum

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JCheckBox, JPanel}

class TapePanel(spectrum:Spectrum) extends JPanel {
  import Preferences._

  init

  private def init : Unit = {
    val pref = spectrum.preferences

    setBorder(BorderFactory.createTitledBorder("Tape settings"))

    val autoPlay = new JCheckBox("Auto play enabled")
    val autoStop = new JCheckBox("Auto stop enabled")
    val fastLoad = new JCheckBox("Fast load enabled")
    val warpMode = new JCheckBox("Warp mode enabled while loading")

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu").
      add(autoPlay).xy(2,2).
      add(autoStop).xy(2,4).
      add(fastLoad).xy(2,6).
      add(warpMode).xy(2,8).
      build()

    autoPlay.addActionListener(_ => pref.update[Boolean](TAPE_AUTOPLAY_ENABLED,autoPlay.isSelected) )
    autoStop.addActionListener(_ => pref.update[Boolean](TAPE_AUTOSTOP_ENABLED,autoStop.isSelected) )
    fastLoad.addActionListener(_ => pref.update[Boolean](TAPE_FAST_LOADER_ENABLED,fastLoad.isSelected) )
    warpMode.addActionListener(_ => pref.update[Boolean](TAPE_WARPMODE_ENABLED,warpMode.isSelected) )

    autoPlay.setSelected(pref.get[Boolean](TAPE_AUTOPLAY_ENABLED).map(_.value).getOrElse(false))
    autoStop.setSelected(pref.get[Boolean](TAPE_AUTOSTOP_ENABLED).map(_.value).getOrElse(false))
    fastLoad.setSelected(pref.get[Boolean](TAPE_FAST_LOADER_ENABLED).map(_.value).getOrElse(false))
    warpMode.setSelected(pref.get[Boolean](TAPE_WARPMODE_ENABLED).map(_.value).getOrElse(false))

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
