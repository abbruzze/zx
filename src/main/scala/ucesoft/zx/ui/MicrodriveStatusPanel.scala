package ucesoft.zx.ui

import ucesoft.zx.format.MDR
import ucesoft.zx.misc.ScaledImageIcon
import ucesoft.zx.spectrum.MicrodriveListener

import java.awt.geom.Arc2D
import java.awt.{BasicStroke, BorderLayout, Color, GradientPaint, Graphics, Graphics2D, RenderingHints}
import javax.swing.{JLabel, JMenu, JMenuItem, JPanel, JPopupMenu}

class MicrodriveStatusPanel extends JPanel with MicrodriveListener with SwingAware {
  private val driveOffEmptyIcon = ScaledImageIcon("microdrive_off.png",50,27)
  private val driveOffCartIcon = ScaledImageIcon("microdrive_off_cart.png",50,27)
  private val driveOnIcon = ScaledImageIcon("microdrive_on.png",50,27)

  private var angle = 0
  private var driveSelected = 0
  private val icon = new JLabel(driveOffEmptyIcon)
  private val drivesWithCart = Array.ofDim[Boolean](8)
  private val drivesCartName = Array.ofDim[String](8)

  init

  private def init : Unit = {
    setLayout(new BorderLayout())
    add("Center",icon)
  }

  override def mdrInserted(mdr: MDR,driveID:Int): Unit = {
    icon.setIcon(driveOffCartIcon)
    drivesWithCart(driveID - 1) = true
    drivesCartName(driveID - 1) = mdr.getCartName

    repaint()
  }

  override def mdrEjected(driveID:Int): Unit = {
    setToolTipText("")
    drivesWithCart(driveID - 1) = false
    val allEmpty = drivesWithCart.forall(!_)
    if (allEmpty) icon.setIcon(driveOffEmptyIcon)

    repaint()
  }

  override def selectedDrive(drive: Int): Unit = {
    driveSelected = drive
    if (driveSelected > 0) {
      val cartName = drivesCartName(drive - 1)
      if (cartName != null) setToolTipText(cartName) else setToolTipText("")
      icon.setIcon(driveOnIcon)
    }
    else icon.setIcon(driveOffCartIcon)

    repaint()
  }

  override def posChanged(pos:Int) : Unit = {
    angle = (pos * 360.0 / 100.0).toInt

    repaint()
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)
    val radius = 6
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    if (driveSelected > 0) {
      g2.setColor(Color.WHITE)
      g2.drawString(driveSelected.toString, 5, 25)
    }
    val originX = 25
    val originY = 20

    val wideStroke = new BasicStroke(radius * 0.4f)
    g2.setColor(Color.BLUE)
    g2.setStroke(wideStroke)
    g2.setPaint(new GradientPaint(0,0,Color.BLUE,getSize().width.toFloat,getSize().height.toFloat,Color.RED))
    val arc = new Arc2D.Double()

    arc.setArcByCenter(originX,originY,radius,0,angle,Arc2D.OPEN)
    g2.draw(arc)
  }

}
