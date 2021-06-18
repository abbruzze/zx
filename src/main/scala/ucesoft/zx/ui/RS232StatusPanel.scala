package ucesoft.zx.ui

import com.jgoodies.forms.builder.FormBuilder
import ucesoft.zx.rs232.{RS232, RS232Listener}

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, Graphics, Graphics2D}
import javax.swing.border.BevelBorder
import javax.swing.{BorderFactory, JButton, JComboBox, JComponent, JDialog, JFrame, JLabel, JPanel, JTextField, SwingConstants}

object RS232StatusPanel {
  def getDialog(rs232:RS232,parent:JFrame,closeAction: () => Unit) : JDialog = {
    val dialog = new JDialog(parent,"RS232 status panel")
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = closeAction()
    })
    dialog.getContentPane.add("Center",new RS232StatusPanel(rs232))
    dialog.pack()
    dialog
  }
}

class RS232StatusPanel(rs232:RS232) extends JPanel with RS232Listener with SwingAware {
  private[this] val FONT = new Font("Monospaced",Font.BOLD,10)

  private val addressTextField = new JTextField(20)
  private val portTextField = new JTextField(5)
  private val baudList = new JComboBox[String](Array("50","110","300","600","1200","2400","4800","9600","19200"))
  private val statusLabel = new JLabel("Not connected")
  private val bytesInLabel,bytesOutLabel,elapsedLabel = new JLabel
  private val connectButton = new JButton("Connect")
  private val disconnectButton = new JButton("Disconnect")
  private var bytesReceived, bytesTransmitted = 0
  private var timer : Thread = _

  private class SignalComponent extends JComponent {
    private[this] var value = 0

    setPreferredSize(new Dimension(10,10))

    override def paint(g:Graphics) : Unit = {
      val size = getSize()
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setColor(Color.BLACK)
      g2.drawRect(0,0,size.width - 1,size.height - 1)
      g2.setColor(if (value > 0) Color.GREEN else Color.DARK_GRAY)
      g2.fillRect(1,1,size.width - 1,size.height - 1)
    }

    def setValue(value:Int) : Unit = {
      this.value = value
      repaint()
    }
  }

  private class SignalPanel(signalName:String) extends JPanel {
    private[this] val signal = new SignalComponent
    private[this] val label = new JLabel(signalName + ":",SwingConstants.RIGHT)

    label.setFont(FONT)
    add(label)
    add(signal)

    def setValue(value:Int) = signal.setValue(value)
    def setColor(c:Color) = label.setForeground(c)
  }

  private[this] val rxSignal = new SignalPanel("Rx")
  private[this] val txSignal = new SignalPanel("Tx")

  init

  private def init : Unit = {
    rs232.setRS232Listener(this)
    val signalPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    signalPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED))
    signalPanel.add(rxSignal)
    signalPanel.add(txSignal)

    val panel = FormBuilder.create().
      columns("5dlu,left:pref,2dlu,left:pref,5dlu,left:pref,2dlu,left:pref,5dlu").
      rows("2dlu,pref,2dlu,pref,2dlu,pref,2dlu,pref,2dlu,pref,2dlu,pref,2dlu,pref,2dlu,pref,5dlu,pref,20dlu,pref,2dlu,pref,2dlu").
      addSeparator("Parameters").xyw(2,2,7).
      add("Address:").xy(2,4).
      add(addressTextField).xy(4,4).
      add("Port:").xy(6,4).
      add(portTextField).xy(8,4).
      add("Baud:").xy(2,6).
      add(baudList).xy(4,6).
      addSeparator("Status").xyw(2,8,7).
      add("Status:").xy(2,10).
      add(statusLabel).xyw(4,10,6).
      add("Bytes in:").xy(2,12).
      add(bytesInLabel).xy(4,12).
      add("Bytes out:").xy(2,14).
      add(bytesOutLabel).xy(4,14).
      add("Elapsed:").xy(2,16).
      add(elapsedLabel).xy(4,16).
      add(signalPanel).xyw(2,18,7).
      addSeparator("").xyw(2,20,7).
      addBar(connectButton,disconnectButton).xyw(4,22,4).
      build()
    setLayout(new BorderLayout())
    add("Center",panel)

    baudList.setSelectedItem("9600")
    disconnectButton.setEnabled(false)

    connectButton.addActionListener(_ => connect )
    portTextField.addActionListener( _ => connect )
    disconnectButton.addActionListener( _ => rs232.hangUp )

    baudList.addActionListener(_ => rs232.setBaud(baudList.getSelectedItem.toString.toInt))
  }

  private def connect : Unit = {
    val url = s"${addressTextField.getText}:${portTextField.getText}"
    rs232.connectTo(url)
    timer = new Thread("RS232Timer") {
      val ts = System.currentTimeMillis()

      override def run : Unit = {
        while (disconnectButton.isEnabled) {
          try { Thread.sleep(1000) } catch { case _ : Throwable => }
          val elapsed = (System.currentTimeMillis() - ts) / 1000
          val h = elapsed / 3600
          val mr = elapsed % 3600
          val m = mr / 60
          val s = mr % 60
          val format = "%02d:%02d:%02d".format(h,m,s)
          swing { elapsedLabel.setText(format) }
        }
      }
    }

    timer.start()
  }

  override def connected(address:String) : Unit = {
    bytesReceived = 0
    bytesTransmitted = 0
    disconnectButton.setEnabled(true)
    statusLabel.setText(s"Connected to $address")
  }
  override def disconnected : Unit = {
    disconnectButton.setEnabled(false)
    statusLabel.setText("Not connected")
  }
  override def rx(on:Boolean) : Unit = rxSignal.setValue(if (on) 1 else 0)
  override def tx(on:Boolean) : Unit = txSignal.setValue(if (!on) 1 else 0)
  override def byteReceived : Unit = {
    bytesReceived += 1
    bytesInLabel.setText(formatBytes(bytesReceived))
  }
  override def byteTransmitted : Unit = {
    bytesTransmitted += 1
    bytesOutLabel.setText(formatBytes(bytesTransmitted))
  }
  private def formatBytes(bytes:Int) : String = {
    if (bytes < 1024) s"$bytes bytes"
    else {
      val f = bytes / 1024.0
      "%.2f Kbytes".format(f)
    }
  }
}
