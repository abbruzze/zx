package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences
import ucesoft.zx.spectrum.Spectrum
import ucesoft.zx.ui.RS232StatusPanel

import java.awt.BorderLayout
import javax.swing.{BorderFactory, JCheckBox, JComponent, JPanel, JSpinner, SpinnerNumberModel}

class InterfaceIPanel(spectrum:Spectrum) extends JPanel {
  import Preferences._

  init

  private def enableComponent(comp:JComponent,enabled:Boolean) : Unit = {
    for(c <- comp.getComponents) {
      c match {
        case jc : JComponent =>
          jc.setEnabled(enabled)
          enableComponent(jc,enabled)
        case _ =>
      }
    }
  }

  private def init : Unit = {
    val pref = spectrum.preferences
    setBorder(BorderFactory.createTitledBorder("Interface I settings"))

    val rs232 = new RS232StatusPanel(spectrum.mmu.ifI.rs232)
    rs232.setBorder(BorderFactory.createTitledBorder("RS-232"))
    val ifI = new JCheckBox("Interface I enabled",pref.get[Boolean](IFI_ENABLED).map(_.value).getOrElse(false))
    enableComponent(rs232,ifI.isSelected)

    val writeChanges = new JCheckBox("Microdrive's cartridge modification will be flushed on disk")
    val model = new SpinnerNumberModel(pref.get[Int](MICRODRIVE_SECTORS).map(_.value).getOrElse(254),180,255,1)
    val mdSectors = new JSpinner(model)
    mdSectors.setEditor(new JSpinner.DefaultEditor(mdSectors))

    ifI.addActionListener(_ => {
      pref.update[Boolean](IFI_ENABLED,ifI.isSelected)
      rs232.setEnabled(ifI.isSelected)
      enableComponent(rs232,ifI.isSelected)
    } )
    mdSectors.addChangeListener(_ => pref.update[Int](MICRODRIVE_SECTORS,model.getValue.asInstanceOf[Int]) )
    writeChanges.addActionListener(_ => pref.update[Boolean](MICRODRIVE_WRITE_CHANGES,writeChanges.isSelected) )

    val rom = new ROMElement(this,pref,IFI_ROM_PREF)
    val panel = FormBuilder.create().
      columns("5dlu,right:pref:grow,5dlu,left:pref:grow,5dlu").
      rows("10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu,pref,10dlu").
      add(ifI).xyw(2,2,4).
      add(writeChanges).xyw(2,4,4).
      add("Microdrive's unformatted cartridge sectors").xy(2,6).
      add(mdSectors).xy(4,6).
      addSeparator("ROM").xyw(2,8,4).
      add(rom.getPanel).xyw(2,10,4).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)

    add("South",rs232)
  }
}
