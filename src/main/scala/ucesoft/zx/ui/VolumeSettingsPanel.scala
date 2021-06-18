package ucesoft.zx.ui

import ucesoft.zx.audio.AudioDriverDevice

import javax.swing.{JCheckBox, JLabel, JPanel, JSlider}
import javax.swing.event.{ChangeEvent, ChangeListener}

class VolumeSettingsPanel(drivers:AudioDriverDevice*) extends JPanel with ChangeListener {
  private[this] val slider = new JSlider
  private[this] val mute = new JCheckBox

  mute.addChangeListener(this)
  slider.setValue(drivers(0).getMasterVolume)
  slider.addChangeListener(this)
  slider.setPaintLabels(true)
  slider.setPaintTicks(true)
  slider.setPaintTrack(true)
  slider.setMajorTickSpacing(20)
  slider.setMinorTickSpacing(2)

  add(new JLabel("Mute:"))
  add(mute)
  add(slider)

  def stateChanged(e:ChangeEvent) = if (!slider.getValueIsAdjusting) {
    if (e.getSource == slider) for(driver <- drivers) driver.setMasterVolume(slider.getValue)
    if (e.getSource == mute) for(driver <- drivers) driver.setMuted(mute.isSelected)
  }
}
