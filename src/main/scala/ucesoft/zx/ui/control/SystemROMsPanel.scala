package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import javax.swing.JPanel

class SystemROMsPanel(pref:Preferences) extends JPanel {

  init

  private def init : Unit = {
    import Preferences._

    val panel = FormBuilder.create().
      columns("5dlu,right:pref,left:pref:grow,5dlu").
      rows("10dlu," + // 1
        "p," +                        // 2 sep
        "5dlu," +                     // 3 space
        "p," +                        // 4 48K r0
        "5dlu," +                     // 5 space
        "p," +                        // 6 sep
        "5dlu," +                     // 7 space
        "p," +                        // 8 128K r0
        "p," +                        // 9 128K r1
        "5dlu," +                     // 10 space
        "p," +                        // 11 sep
        "5dlu," +                     // 12 space
        "p," +                        // 13 128k+ r0
        "p," +                        // 14 128k+ r1
        "5dlu," +                     // 15 space
        "p," +                        // 16 sep
        "5dlu," +                     // 17 space
        "p," +                        // 18 128+2A r0
        "p," +                        // 19 128+2A r1
        "p," +                        // 20 128+2A r2
        "p," +                        // 21 128+2A r3
        "5dlu," +                     // 22 space
        "p," +                        // 23 sep
        "5dlu," +                     // 24 space
        "p," +                        // 25 128+3 r0
        "p," +                        // 26 128+3 r1
        "p," +                        // 27 128+3 r2
        "p," +                        // 28 128+3 r3
        "5dlu,"                       // 29 space
      ).
    // 48K
      addSeparator("Spectrum 48K").xyw(2,2,2).
      add("ROM0:").xy(2,4).
      add(new ROMElement(this,pref,_48K_ROM_PREF).getPanel).xy(3,4).
    // 128K
      addSeparator("Spectrum 128K").xyw(2,6,2).
      add("ROM0:").xy(2,8).
      add(new ROMElement(this,pref,_128K_0_ROM_PREF).getPanel).xy(3,8).
      add("ROM1:").xy(2,9).
      add(new ROMElement(this,pref,_128K_1_ROM_PREF).getPanel).xy(3,9).
    // 128K+
      addSeparator("Spectrum 128K+").xyw(2,11,2).
      add("ROM0:").xy(2,13).
      add(new ROMElement(this,pref,_128K_PLUS2_0_ROM_PREF).getPanel).xy(3,13).
      add("ROM1:").xy(2,14).
      add(new ROMElement(this,pref,_128K_PLUS2_1_ROM_PREF).getPanel).xy(3,14).
    // 128+2A
      addSeparator("Spectrum 128K+2A").xyw(2,16,2).
      add("ROM0:").xy(2,18).
      add(new ROMElement(this,pref,_128K_PLUS2A_0_ROM_PREF).getPanel).xy(3,18).
      add("ROM1:").xy(2,19).
      add(new ROMElement(this,pref,_128K_PLUS2A_1_ROM_PREF).getPanel).xy(3,19).
      add("ROM2:").xy(2,20).
      add(new ROMElement(this,pref,_128K_PLUS2A_2_ROM_PREF).getPanel).xy(3,20).
      add("ROM3:").xy(2,21).
      add(new ROMElement(this,pref,_128K_PLUS2A_3_ROM_PREF).getPanel).xy(3,21).
      // 128+3
      addSeparator("Spectrum 128K+3").xyw(2,23,2).
      add("ROM0:").xy(2,25).
      add(new ROMElement(this,pref,_128K_PLUS2A_0_ROM_PREF).getPanel).xy(3,25).
      add("ROM1:").xy(2,26).
      add(new ROMElement(this,pref,_128K_PLUS2A_1_ROM_PREF).getPanel).xy(3,26).
      add("ROM2:").xy(2,27).
      add(new ROMElement(this,pref,_128K_PLUS2A_2_ROM_PREF).getPanel).xy(3,27).
      add("ROM3:").xy(2,28).
      add(new ROMElement(this,pref,_128K_PLUS2A_3_ROM_PREF).getPanel).xy(3,28).
      build()

    setLayout(new BorderLayout())
    add("Center",panel)
  }
}
