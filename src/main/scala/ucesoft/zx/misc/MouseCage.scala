package ucesoft.zx.misc

import java.awt._
import java.awt.event._
import javax.swing.{JComponent, SwingUtilities}
import scala.util.Try

object MouseCage extends MouseAdapter {
  final val LEFT_BUTTON = 1
  final val RIGHT_BUTTON = 2
  final val MIDDLE_BUTTON = 4

  private val mousePointer = new Point
  private val robot : Robot = Try(new Robot).getOrElse(null)
  private var component : JComponent = _
  private var lastMoveTs = 0L
  private var lastButtonPressed = 0

  final def x: Int = if (robot != null) mousePointer.x else MouseInfo.getPointerInfo.getLocation.x
  final def y: Int = if (robot != null) mousePointer.y else MouseInfo.getPointerInfo.getLocation.y
  final def buttonPressed : Int = lastButtonPressed

  @inline private def robotMove(p:Point) : Unit = {
    if (robot != null) robot.mouseMove(p.x, p.y)
  }

  private def center : Point = {
    val loc = component.getLocationOnScreen
    new Point(loc.x + component.getWidth / 2,loc.y + component.getHeight / 2)
  }

  def enableMouseCageOn(c:JComponent) : Unit = {
    component = c
    val p = center
    mousePointer.x = p.x
    mousePointer.y = p.y
    val cursor = new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    component.setCursor(component.getToolkit.createCustomCursor(cursor,new Point(0, 0),"null"))
    component.addMouseListener(this)
    component.addMouseMotionListener(this)
    robotMove(p)
  }

  def disableMouseCage  : Unit = {
    if (component != null) {
      component.removeMouseListener(this)
      component.removeMouseMotionListener(this)
      component.setCursor(new java.awt.Cursor(java.awt.Cursor.DEFAULT_CURSOR))
    }
  }

  override def mouseExited(e:MouseEvent) = robotMove(center)

  override def mouseMoved(e:MouseEvent) : Unit = {
    val moveTs = System.currentTimeMillis()
    val deltaT = moveTs - lastMoveTs
    lastMoveTs = moveTs
    val factor = math.ceil(20.0 / deltaT)
    val compCenter = center
    mousePointer.x += ((e.getXOnScreen - compCenter.x) / factor).toInt
    mousePointer.y += ((e.getYOnScreen - compCenter.y) / factor).toInt
    robotMove(compCenter)
  }

  override def mouseDragged(e:MouseEvent) = mouseMoved(e)

  override def mousePressed(e: MouseEvent): Unit = {
    lastButtonPressed = 0
    if (SwingUtilities.isLeftMouseButton(e)) lastButtonPressed |= LEFT_BUTTON
    if (SwingUtilities.isRightMouseButton(e)) lastButtonPressed |= RIGHT_BUTTON
    if (SwingUtilities.isMiddleMouseButton(e)) lastButtonPressed |= MIDDLE_BUTTON
  }

  override def mouseReleased(e: MouseEvent): Unit = lastButtonPressed = 0
}
