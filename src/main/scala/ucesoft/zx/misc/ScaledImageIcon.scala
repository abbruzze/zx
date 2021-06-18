package ucesoft.zx.misc

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.{ImageIcon, JLabel}

object ScaledImageIcon {
  def apply(res:String) : ImageIcon = apply(res,0,0)
  def apply(res:String,width:Int,height:Int) : ImageIcon = {
    val (w,h) = if (height == 0) {
      val f = new JLabel
      val h = f.getFontMetrics(f.getFont).getHeight
      (h,h)
    }
    else (width,height)
    val image = ImageIO.read(getClass.getResourceAsStream(s"/resources/images/$res"))
    val resizedImg = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val g2 = resizedImg.createGraphics

    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2.drawImage(image, 0, 0, w, h, null)
    g2.dispose()
    new ImageIcon(resizedImg)
  }
}
