package ucesoft.zx.trace


import ucesoft.zx.{Clock, Log}
import ucesoft.zx.cpu.Memory
import ucesoft.zx.gpu.{Display, ULA}
import ucesoft.zx.misc.ScaledImageIcon

import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing._
import java.awt.{BorderLayout, FlowLayout}
import java.io.FileOutputStream
import java.io._
import javax.swing.JSpinner.DefaultEditor
import scala.collection.mutable.ListBuffer

object TraceDialog {
  def getTraceDialog(title:String,displayFrame: JFrame, mem: Memory,traceListener: TraceListener, display: Display,ula:ULA,closeAction: () => Unit): TraceDialog = {
    val dialog = new TraceDialog(title,displayFrame, mem, traceListener,Some(display),ula,closeAction)
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    dialog.pack
    dialog
  }
}

trait TracingListener {
  def stepInto(pc:Int) : Unit
}

class TraceDialog private (title:String,
                           displayFrame: JFrame,
                           var mem: Memory,
                           var traceListener: TraceListener,
                           display: Option[Display],
                           ula:ULA,
                           closeAction: () => Unit) extends JDialog(displayFrame, title) {
  private val notrace = new JButton(ScaledImageIcon("trace_enabled.png",32,32))
  private val rasterLineSpinner = new JSpinner(new SpinnerNumberModel(0,0,312,1))
  private val traceSR = new JLabel
  val logPanel = Log.getLogPanel
  private[this] var tracing = false
  private[this] var tracingFile : PrintWriter = _
  private[this] val tracingListeners = new ListBuffer[TracingListener]
  private[this] val breakLabel = new JLabel(s"Break type: $NoBreak")

  addWindowListener(new WindowAdapter {
    override def windowClosing(e:WindowEvent) : Unit = closeAction()
  })

  private def s2a(address: String) = address.trim()(0) match {
    case '$' => Integer.parseInt(address.substring(1), 16)
    case '%' => Integer.parseInt(address.substring(1), 2)
    case _ => address.toInt
  }

  def addListener(tl:TracingListener) : Unit = tracingListeners += tl
  def removeListener(tl:TracingListener) : Unit = tracingListeners -= tl

  def isTracing = tracing

  private def updateRegs(_regs:CpuStepInfo) : Unit = {
    val regs = s"${_regs.registers} rasterLine=${ula.getRasterLine} cycles=${Clock.systemClock.currentCycles - ula.getIRQCycle}"
    traceSR.setText(regs2HTML(regs))
    // notify listeners
    for(t <- tracingListeners) t.stepInto(_regs.pc)
  }

  private def regs2HTML(regs:String) : String = {
    val sb = new StringBuilder
    val cols = regs.split(" ")
    sb.append(
      """<html>
        |<table>
        |<tr>
        |""".stripMargin
    )
    val kv = cols map { _.split("=") }
    for(c <- kv) {
      sb.append(s"<th><b>${c(0)}</b></th>")
    }
    sb.append("</tr><tr>")
    for(c <- kv) {
      sb.append(s"<th>${c(1)}</th>")
    }
    sb.append("</tr></table></body></html>")
    sb.toString
  }

  def forceTracing(on:Boolean) : Unit = {
    tracing = on
    step.setEnabled(on)
    traceListener.step(updateRegs _)
    traceListener.setTrace(tracing)
    if (!tracing) {
      traceListener.step(updateRegs _)
      Log.setInfo
    }
    else Log.setDebug
  }

  private def checkAction(e : => Unit) : Unit = {
    try {
      e
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(this,t.toString,"Debug error",JOptionPane.ERROR_MESSAGE)
    }
  }

  private def info(s:String) : Unit = logPanel.writer.println(s)

  def setBrk(brk:BreakType) : Unit = {
    traceListener.setBreakAt(brk, (regs) => { updateRegs(regs) ; forceTracing(true) })
    Log.setInfo
    traceListener.step(updateRegs _)
    breakLabel.setText(s"Break type: $brk")
  }

  private def jmpTo : Unit = {
    Option(JOptionPane.showInputDialog(this, "Jump to address:")) match {
      case Some(address) =>
        traceListener.jmpTo(s2a(address))
      case _ =>
    }
  }
  private def stepInto : Unit = {
    if (tracing) Log.setDebug
    Log.setOutput(logPanel.writer)
    traceListener.step(updateRegs _)
  }
  private def brkMode : Unit = {
    Log.setOutput(logPanel.writer)
    Option(JOptionPane.showInputDialog(this, "Break type:")) match {
      case Some(breakType) =>
        setBrk(if (breakType.isEmpty) NoBreak else BreakType.makeBreak(breakType))
      case _ =>
    }
  }
  private def readValues : Unit = {
    Option(JOptionPane.showInputDialog(this, "Address to read:")) match {
      case Some(address) =>
        if (address.startsWith("find")) {
          val FINDRE = """find\s+(\d+)-(\d+)\s+'([^']+)'""".r
          address match {
            case FINDRE(from,to,what) =>
              for(a <- s2a(from) to s2a(to) - what.length) {
                var c = 0
                while (c < what.length && mem.peek(a + c) == what(c)) c += 1
                if (c == what.length) {
                  info("Found at address $" + a.toHexString)
                  return
                }
              }
              info(s"$what not found")
            case _ =>
              info("Bad find command")
          }
        }
        else
          if (address.contains(" ")) {
            val pars = address split " "
            var col = 0
            val sb = new StringBuilder
            val ascii = new StringBuilder
            var file : FileOutputStream = null
            if (pars.length == 3) file = new FileOutputStream(pars(2))
            for (a <- s2a(pars(0)) to s2a(pars(1))) {
              if (col == 0) sb.append("%04X: ".format(a))
              val c = mem.peek(a)
              if (file != null) file.write(c)
              if (c > 32) ascii.append(c.toChar) else ascii.append('.')
              sb.append("%02X ".format(c))
              col += 1
              if (col == 16) {
                col = 0
                info(s"$sb  $ascii")
                sb.clear()
                ascii.clear()
              }
            }
            if (file != null) file.close
            if (sb.length > 0) info(sb.toString)
          } else if (address.startsWith("w")) {
            val a = s2a(address.substring(1))
            val word = mem.peek(a + 1) * 256 | mem.peek(a)
            info(s"ReadWord(${address.substring(1)})=${Integer.toHexString(word)}")
          } else info(s"Read(${address})=${Integer.toHexString(mem.peek(s2a(address)))}")
      case _ =>
    }
  }
  private def writeValues : Unit = {
    Option(JOptionPane.showInputDialog(this, "Address to write:")) match {
      case Some(address) =>
        val addressValue = address split " "
        val fromA = s2a(addressValue(0))
        val toA = if (addressValue.length == 2) fromA else s2a(addressValue(1))
        val value = s2a(if (addressValue.length == 2) addressValue(1) else addressValue(2))
        for(a <- fromA to toA) {
          mem.load(a, value)
          info(s"Write(${a.toHexString})=${value.toHexString}")
        }
      case _ =>
    }
  }
  private def toggleTrace : Unit = {
    tracing = !tracing
    forceTracing(tracing)
  }
  private def disass : Unit = {
    Option(JOptionPane.showInputDialog(this, "Disassemble from address [to address]:")) match {
      case Some(address) =>
        val addresses = address split " " map s2a
        var a = addresses(0)
        val endAddress = if (addresses.length == 2) addresses(1) else a + 0x100
        while (a <= endAddress) {
          val (d,len) = traceListener.disassemble(mem, a)
          info(d)
          a += len
        }
      case _ =>
    }
  }
  
  private def showRaster(show:Boolean) : Unit = {
    display.get.setRasterLineAt(rasterLineSpinner.getValue.asInstanceOf[Int])
    display.get.setDrawRasterLine(show)
  }
  private def traceFile(button:JToggleButton) : Unit = {
    if (!button.isSelected) {
      tracingFile.close
      traceListener.setTraceOnFile(null,false)
    }
    else {
      val fc = new JFileChooser
      fc.showOpenDialog(displayFrame) match {
        case JFileChooser.APPROVE_OPTION =>
          tracingFile = new PrintWriter(new BufferedOutputStream(new FileOutputStream(fc.getSelectedFile)))
          traceListener.setTraceOnFile(tracingFile,true)
        case _ =>
          button.setSelected(false)
      }
    }
  }

  val tracePanel = new JPanel
  tracePanel.setLayout(new BorderLayout)

  val jmp = new JButton(ScaledImageIcon("trace_jump.png",32,32))
  jmp.setToolTipText("Jump to the given PC address")
  jmp.addActionListener( _ => checkAction(jmpTo) )
  val step = new JButton(ScaledImageIcon("trace_step.png",32,32))
  step.setToolTipText("Step into istruction or cycle")
  step.addActionListener( _ => checkAction(stepInto) )
  step.setEnabled(false)
  val brk = new JButton(ScaledImageIcon("trace_breakpoint.png",32,32))
  brk.setToolTipText("Set the breakpoint to the given address or 'irq'/'nmi'")
  brk.addActionListener( _ => checkAction(brkMode) )
  val read = new JButton(ScaledImageIcon("trace_read.png",32,32))
  read.setToolTipText("Read from a given address or address range <a1> <a2>")
  read.addActionListener( _ => checkAction(readValues) )
  val write = new JButton(ScaledImageIcon("trace_write.png",32,32))
  write.setToolTipText("Write to a given address <a> <v> or fill the given address range <a1> <a2> <v>")
  write.addActionListener( _ => checkAction(writeValues) )
  notrace.setToolTipText("Enable/Disable tracing")
  notrace.addActionListener( _ => toggleTrace )
  val disa = new JButton(ScaledImageIcon("trace_disa.png",32,32))
  disa.setToolTipText("Disassemble the given address range <a1> <a2>")
  disa.addActionListener( _ => checkAction(disass) )
  val clear = new JButton(ScaledImageIcon("trace_clear.png",32,32))
  clear.setToolTipText("Clear debug area")
  clear.addActionListener( _ => logPanel.clear )
  val buttonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  buttonPanel.add(jmp)
  buttonPanel.add(step)
  buttonPanel.add(brk)
  buttonPanel.add(read)
  buttonPanel.add(write)
  buttonPanel.add(notrace)
  buttonPanel.add(disa)
  buttonPanel.add(clear)
  buttonPanel.setBorder(BorderFactory.createTitledBorder("Actions"))
  logPanel.setBorder(BorderFactory.createTitledBorder("Messages"))
  
  val showRasterCB = new JCheckBox("Show raster", false)
  showRasterCB.addActionListener( _ => showRaster(showRasterCB.isSelected) )
  rasterLineSpinner.addChangeListener(_ => {
    val r = rasterLineSpinner.getValue.asInstanceOf[Int]
    display.get.setRasterLineAt(r)
  }
  )
  rasterLineSpinner.getEditor.asInstanceOf[DefaultEditor].getTextField.setEditable(true)
  rasterLineSpinner.setEditor(new JSpinner.NumberEditor(rasterLineSpinner, "###"))
  buttonPanel.add(showRasterCB)
  buttonPanel.add(rasterLineSpinner)
  

  val breakPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  breakPanel.setBorder(BorderFactory.createTitledBorder("Breaks"))
  val clearBreaks = new JButton("Clear breaks")
  breakPanel.add(clearBreaks)
  breakPanel.add(breakLabel)
  clearBreaks.addActionListener( _ => setBrk(NoBreak) )

  tracePanel.add("North", buttonPanel)
  val pcsrPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  pcsrPanel.setBorder(BorderFactory.createTitledBorder("Registers"))
  pcsrPanel.add(traceSR)
  //traceSR.setForeground(Color.BLUE)
  tracePanel.add("South", pcsrPanel)
  getContentPane.add("North", tracePanel)
  getContentPane.add("Center",logPanel)
  getContentPane.add("South",breakPanel)
}
