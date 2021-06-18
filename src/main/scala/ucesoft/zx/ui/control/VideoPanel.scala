package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.gpu.ULAPlus
import ucesoft.zx.misc.Preferences
import ucesoft.zx.spectrum.Spectrum

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JButton, JCheckBox, JLabel, JPanel}

class VideoPanel(spectrum:Spectrum,videoControl: VideoControl) extends JPanel with Preferences.PreferenceChangeListener {
  import Preferences._
  private val ulaPlusActive = new JLabel("not active")
  private var snowEffect : JCheckBox = _

  init

  private def init : Unit = {
    val pref = spectrum.preferences

    setBorder(BorderFactory.createTitledBorder("Video settings"))

    val palScanLines = new JCheckBox("PAL scan lines enabled")
    val ulaPlus = new JCheckBox("ULA+ enabled")
    snowEffect = new JCheckBox("Snow effect enabled")
    val aspectButton = new JButton("4:3")
    val zoom1 = new JButton("Zoom x1")
    val zoom2 = new JButton("Zoom x2")
    val zoom3 = new JButton("Zoom x3")
    val fullScreen = new JButton("Full screen")

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,5dlu,right:pref,5dlu,left:pref,5dlu").
      rows("10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu").
      add(palScanLines).xy(2,2).
      add(ulaPlus).xy(2,4).
      add("ULA+ active:").xy(4,4).
      add(ulaPlusActive).xy(6,4).
      add(snowEffect).xy(2,6).
      addSeparator("Display adjustments").xyw(2,8,5).
      addBar(aspectButton,zoom1).xyw(2,10,4).
      addBar(zoom2,zoom3).xyw(2,12,4).
      addBar(fullScreen).xy(2,14).
      build()

    ULAPlus.activeListener = (active,enabled) => {
      ulaPlusActive.setText(if (active) "active" else "not active")
      ulaPlus.setSelected(enabled)
      pref.update[Boolean](ULAPLUS_ENABLED,enabled)
    }
    ulaPlus.addActionListener(_ => pref.update[Boolean](ULAPLUS_ENABLED,ulaPlus.isSelected) )
    snowEffect.addActionListener(_ => pref.update[Boolean](SNOW_EFFECT_ENABLED,snowEffect.isSelected) )
    palScanLines.addActionListener(_ => pref.update[Boolean](PAL_SCANLINES_ENABLED,palScanLines.isSelected) )
    aspectButton.addActionListener(_ => videoControl.aspect4_3 )
    zoom1.addActionListener(_ => videoControl.zoomDisplay(1) )
    zoom2.addActionListener(_ => videoControl.zoomDisplay(2) )
    zoom3.addActionListener(_ => videoControl.zoomDisplay(3) )
    fullScreen.addActionListener(_ => videoControl.fullScreen )

    palScanLines.setSelected(pref.get[Boolean](PAL_SCANLINES_ENABLED).map(_.value).getOrElse(false))
    ulaPlus.setSelected(pref.get[Boolean](ULAPLUS_ENABLED).map(_.value).getOrElse(false))
    snowEffect.setSelected(pref.get[Boolean](SNOW_EFFECT_ENABLED).map(_.value).getOrElse(true))

    pref.get[String](MODEL).foreach(_.addChangeListener(this))

    setLayout(new BorderLayout())
    add("Center",panel)
  }

  override def preferenceHasChanged(pref: Preference[_]): Unit = {
    pref.value match {
      case "128k+2A"|"128k+3" =>
        snowEffect.setEnabled(false)
        snowEffect.setSelected(false)
      case _ =>
        snowEffect.setEnabled(true)
    }
  }
}
