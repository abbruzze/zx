package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{JButton, JCheckBox, JComponent, JFileChooser, JPanel, JTextField}

class ROMElement(parent:JComponent,pref:Preferences,key:String) {
  val default = new JCheckBox("Default")
  val file = new JTextField(30)
  val browse = new JButton("Browse")

  val conf = pref.get[String](key)
  val isDefault = conf.map(_.value).getOrElse("").isEmpty
  default.setSelected(isDefault)
  file.setEnabled(!isDefault)
  file.setText(conf.map(_.value).getOrElse(""))
  browse.setEnabled(!isDefault)

  default.addActionListener(_ => {
    val isDef = default.isSelected
    if (isDef) pref.update[String](key,"") else pref.update[String](key,file.getText)
    file.setEnabled(!isDef)
    browse.setEnabled(!isDef)
  })
  browse.addActionListener(_ => {
    val fc = new JFileChooser
    fc.setDialogTitle(s"ROM selection")
    fc.showOpenDialog(parent) match {
      case JFileChooser.APPROVE_OPTION =>
        file.setText(fc.getSelectedFile.toString)
    }
  })
  file.getDocument.addDocumentListener(new DocumentListener {
    override def removeUpdate(e: DocumentEvent): Unit = pref.update[String](key,file.getText)
    override def insertUpdate(e: DocumentEvent): Unit = pref.update[String](key,file.getText)
    override def changedUpdate(e: DocumentEvent): Unit = pref.update[String](key,file.getText)
  })

  def getPanel : JPanel = {
    FormBuilder.create().
      columns("5dlu,left:pref,5dlu,pref:grow,left:pref:grow,5dlu,pref:grow,5dlu").
      rows("pref").
      add(default).xy(2,1).
      add("Custom ROM:").xy(4,1).
      add(file).xy(5,1).
      add(browse).xy(7,1).
      build()
  }
}
