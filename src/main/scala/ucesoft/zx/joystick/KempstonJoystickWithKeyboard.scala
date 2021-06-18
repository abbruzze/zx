package ucesoft.zx.joystick

import java.awt.event.{KeyEvent, KeyListener}

class KempstonJoystickWithKeyboard extends KempstonJoystick with KeyListener {
  var upKeyCode = -1
  var downKeyCode = -1
  var leftKeyCode = -1
  var rightKeyCode = -1
  var fireKeyCode = -1

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {
    val k = e.getExtendedKeyCode
    if (k == rightKeyCode) set(RIGHT)
    else if (k == leftKeyCode) set(LEFT)
    else if (k == upKeyCode) set(UP)
    else if (k == downKeyCode) set(DOWN)
    else if (k == fireKeyCode) set(FIRE)
  }

  override def keyReleased(e: KeyEvent): Unit = {
    val k = e.getExtendedKeyCode
    if (k == rightKeyCode) clear(RIGHT)
    else if (k == leftKeyCode) clear(LEFT)
    else if (k == upKeyCode) clear(UP)
    else if (k == downKeyCode) clear(DOWN)
    else if (k == fireKeyCode) clear(FIRE)
  }
}
