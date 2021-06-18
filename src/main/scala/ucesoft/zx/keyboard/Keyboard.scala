package ucesoft.zx.keyboard

import ucesoft.zx.ZXComponent
import ucesoft.zx.ZXComponentType

import java.awt.event.{KeyEvent, KeyListener}

class Keyboard(var mapper:KeyboardMapper) extends ZXComponent with KeyListener {
  override val componentType: ZXComponentType.Type = ZXComponentType.KEYBOARD
  override val componentID: String = "Keyboard"

  private[this] val keyPressed = collection.mutable.Set.empty[Key.Value]

  override def init : Unit = {}
  override def reset : Unit = {
    keyPressed.clear()
  }

  override def keyTyped(e: KeyEvent): Unit = {}

  override def keyPressed(e: KeyEvent): Unit = {
    if (e.isAltDown) return

    val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode
    mapper.keyMap.get(key) match {
      case Some(l) =>
        for(k <- l) keyPressed += k
      case _ =>
    }
  }

  override def keyReleased(e: KeyEvent): Unit = {
    if (e.isAltDown) return

    val key = if (e.getKeyCode != KeyEvent.VK_UNDEFINED) e.getKeyCode else e.getExtendedKeyCode
    mapper.keyMap.get(key) match {
      case Some(l) =>
        for(k <- l) keyPressed -= k
      case _ =>
    }
  }

  def select(row:Int) : Int = {
    var keys = 0xFF
    for(rb <- 0 to 7) {
      val halfKeys = Key.HALF_ROWS(rb)
      for(cb <- 0 to 4) {
        val rowSel = (row & (1 << rb)) == 0
        if (rowSel && keyPressed.contains(halfKeys(cb))) keys &= ~(1 << cb)
      }
    }
    keys
  }
}
