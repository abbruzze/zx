package ucesoft.zx.ui.control

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.misc.Preferences

import java.awt.BorderLayout
import java.awt.event.{KeyAdapter, KeyEvent}
import javax.swing.{BorderFactory, JButton, JLabel, JPanel}

class JoystickKeysPanel(pref:Preferences) extends JPanel {
  import Preferences._

  private val up = new JButton("Up")
  private val down = new JButton("Down")
  private val left = new JButton("Left")
  private val right = new JButton("Right")
  private val fire = new JButton("Fire")
  private val buttons = Array(up,down,left,right,fire)
  private val upLabel = new JLabel
  private val downLabel = new JLabel
  private val leftLabel = new JLabel
  private val rightLabel = new JLabel
  private val fireLabel = new JLabel

  init

  private def init : Unit = {
    setBorder(BorderFactory.createTitledBorder("Keys settings"))
    val panel = FormBuilder.create().
      columns("5dlu,fill:pref,2dlu,left:pref,5dlu").
      rows("10dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,5dlu,pref,10dlu").
      add(up).xy(2,2).
      add(upLabel).xy(4,2).
      add(down).xy(2,4).
      add(downLabel).xy(4,4).
      add(left).xy(2,6).
      add(leftLabel).xy(4,6).
      add(right).xy(2,8).
      add(rightLabel).xy(4,8).
      add(fire).xy(2,10).
      add(fireLabel).xy(4,10).
      build()

    setLabel(upLabel,pref.get[Int](JOY_UP_KEY).map(_.value))
    setLabel(downLabel,pref.get[Int](JOY_DOWN_KEY).map(_.value))
    setLabel(leftLabel,pref.get[Int](JOY_LEFT_KEY).map(_.value))
    setLabel(rightLabel,pref.get[Int](JOY_RIGHT_KEY).map(_.value))
    setLabel(fireLabel,pref.get[Int](JOY_FIRE_KEY).map(_.value))

    up.addActionListener(_ => listenTo(upLabel,JOY_UP_KEY))
    down.addActionListener(_ => listenTo(downLabel,JOY_DOWN_KEY))
    left.addActionListener(_ => listenTo(leftLabel,JOY_LEFT_KEY))
    right.addActionListener(_ => listenTo(rightLabel,JOY_RIGHT_KEY))
    fire.addActionListener(_ => listenTo(fireLabel,JOY_FIRE_KEY))

    setLayout(new BorderLayout())
    add("Center",panel)

    setFocusable(true)
  }

  override def setEnabled(enabled: Boolean): Unit = {
    for(b <- buttons) b.setEnabled(enabled)
  }

  private def listenTo(label:JLabel,key:String) : Unit = {
    label.setText("Press a key ...")
    requestFocus()
    for(b <- buttons) b.setEnabled(false)

    addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = {
        for(b <- buttons) b.setEnabled(true)
        removeKeyListener(this)
        setLabel(label,Some(e.getKeyCode))
        pref.update[Int](key,e.getKeyCode)
        println(pref.get[Int](key))
      }
    })
  }

  private def setLabel(label:JLabel,code:Option[Int]) : Unit = {
    code match {
      case Some(c) if c != -1 =>
        label.setText(KeyEvent.getKeyText(c))
      case _ =>
        label.setText("not configured")
    }
  }
}
