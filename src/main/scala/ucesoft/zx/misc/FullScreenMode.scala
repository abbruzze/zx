package ucesoft.zx.misc

import java.awt.{Color, Dimension, GraphicsEnvironment}
import java.awt.event.{KeyAdapter, KeyEvent, KeyListener, MouseListener}
import javax.imageio.ImageIO
import javax.swing.{JComponent, JFrame, JOptionPane}

object FullScreenMode {
  def goFullScreen(frame:JFrame,component:JComponent,width:Int,height:Int,mouseListener:MouseListener,keyListeners:KeyListener*) : Unit = {
    val env = GraphicsEnvironment.getLocalGraphicsEnvironment
    val device = env.getScreenDevices()(0)
    val conf = device.getDefaultConfiguration
    if (device.isFullScreenSupported) {
      val window = new JFrame(conf)
      window.getContentPane.setLayout(null)
      window.getContentPane.add(component)
      window.getContentPane.setBackground(Color.BLACK)
      frame.setVisible(false)
      window.setUndecorated(true)
      frame.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/images/zx_logo.png")))
      device.setFullScreenWindow(window)
      val size = conf.getBounds
      val windowWidthFactor = size.width / width.toDouble
      val windowHeightFactor = size.height / height.toDouble
      val factor = math.min(windowWidthFactor,windowHeightFactor)
      //println("Factor=" + windowWidthFactor + "," + windowHeightFactor)
      val originalSize = component.getSize()
      component.setSize(new Dimension((width * factor).toInt,(height * factor).toInt))
      val vicSize = component.getSize()
      val winSize = window.getSize()
      component.setLocation((winSize.width - vicSize.width) / 2,(winSize.height - vicSize.height) / 2)
      component.invalidate
      window.validate
      window.setVisible(true)
      window.toFront()
      window.addMouseListener(mouseListener)
      for(kl <- keyListeners) window.addKeyListener(kl)
      window.addKeyListener(new KeyAdapter {
        override def keyPressed(e:KeyEvent) : Unit = {
          e.getKeyCode match {
            case java.awt.event.KeyEvent.VK_ENTER if e.isAltDown =>
              window.dispose
              frame.setVisible(true)
              component.setSize(originalSize)
              frame.getContentPane.add("Center",component)
              frame.pack
            case _ =>
          }
        }
      })
    }
    else JOptionPane.showMessageDialog(frame,"Your display device does not support full screen mode","Full Screen Mode",JOptionPane.ERROR_MESSAGE)
  }
}