package ucesoft.zx.rs232

import ucesoft.zx.{Clock, ClockEvent, Log, ZXComponent, ZXComponentType}

import java.io.{InputStream, OutputStream}
import java.util.Properties

trait RS232Listener {
  def connected(address:String) : Unit = {}
  def disconnected : Unit = {}
  def rx(on:Boolean) : Unit = {}
  def tx(on:Boolean) : Unit = {}
  def byteReceived : Unit = {}
  def byteTransmitted : Unit = {}
}


class RS232 extends ZXComponent with ModemCommandListener {
  override val componentID: String = "RS232"
  override val componentType: ZXComponentType.Type = ZXComponentType.RS232

  protected var lastRxBit = false
  protected var lastTxBit = true
  protected var waitStartBit = true
  protected var waitStopBit = false
  protected val clk = Clock.systemClock
  protected var rxCount,txCount = 0
  protected var baudRate = 9600
  protected var BIT_T = getBITPeriodInCycles
  protected var sr = 0
  protected var txSending = false
  protected val modem = new Modem(this)
  protected var listener : RS232Listener = _
  protected var address = ""
  protected var _cts = false

  def setRS232Listener(l:RS232Listener) : Unit = listener = l

  protected def getBITPeriodInCycles : Int = 47 + 26 * (clk.getClockHz.toInt / (baudRate * 26) - 2)

  override def connectTo(address: String): Unit = {
    if (ConnectionManager.openConnection(address)) {
      this.address = address
      modem.setStreams(ConnectionManager.getInputStream,ConnectionManager.getOutputStream)
      if (listener != null) listener.connected(address)
    }
  }

  override def hangUp: Unit = {
    ConnectionManager.closeConnection
    modem.disconnect
    if (listener != null) listener.disconnected
    address = ""
  }

  override def reset: Unit = {
    lastRxBit = false
    lastTxBit = true
    waitStartBit = true
    waitStopBit = false
    rxCount = 0
    txCount = 0
    sr = 0
    txSending = false
  }

  override def init: Unit = {
    setBaud(9600)
    add(modem)
    clk.addChangeFrequencyListener(_ => setBaud(baudRate) )
  }

  def setBaud(baud:Int) : Unit = {
    Log.info(s"RS232: Set baud rate to $baud BIT period is $BIT_T cycles")
    baudRate = baud
    BIT_T = getBITPeriodInCycles
  }

  def getBaud : Int = baudRate

  def setStreams(in:InputStream,out:OutputStream) : Unit = modem.setStreams(in,out)

  def rxData(on:Boolean) : Unit = {
    if (on && waitStartBit) { // start bit
      waitStartBit = false
      waitStopBit = true
      rxCount = 0
      sr = 0
      clk.schedule(new ClockEvent("RX",clk.currentCycles + BIT_T,collectRxBits _))
    }
    if (listener != null) listener.rx(on)
    lastRxBit = on
  }

  protected def bit(b:Boolean) : Boolean = b

  protected def collectRxBits(cycles:Long) : Unit = {
    if (waitStopBit)  {
      if (!bit(lastRxBit)) waitStopBit = false
      clk.schedule(new ClockEvent("RX",cycles + BIT_T,collectRxBits _))
    }
    else {
      if (rxCount < 8) sr = (sr >> 1) | (if (bit(lastRxBit)) 0x80 else 0)
      if (rxCount == 10) {
        try {
          //println(sr.toChar + " " + sr)
          modem.outputStream.write(sr)
          if (listener != null) listener.byteTransmitted
        }
        catch {
          case _:Throwable =>
            hangUp
        }
        waitStartBit = true
      }
      else clk.schedule(new ClockEvent("RX",cycles + BIT_T,collectRxBits _))
      rxCount += 1
    }
  }

  protected def sendTxBits(cycles:Long) : Unit = {
    if (txCount < 8) {
      txCount += 1
      lastTxBit = (sr & 0x1) == 0x1
      sr >>= 1
      clk.schedule(new ClockEvent("TX",cycles + BIT_T,sendTxBits _))
    }
    else { // stop bits
      if (txCount < 10) {
        txCount += 1
        lastTxBit = true
        clk.schedule(new ClockEvent("TX",cycles + BIT_T,sendTxBits _))
      }
      else txSending = false
    }
    if (listener != null) listener.tx(lastTxBit)
  }

  def txData : Boolean = {
    try {
      if (modem.inputStream != null && modem.inputStream.available() > 0 && !txSending) {
        lastTxBit = false
        txSending = true
        sr = modem.inputStream.read()
        if (listener != null) listener.byteReceived
        txCount = 0
        clk.schedule(new ClockEvent("TX", clk.currentCycles + BIT_T, sendTxBits _))
      }
    }
    catch {
      case _:Throwable =>
        lastTxBit = true
        hangUp
    }

    lastTxBit
  }
  def dtr : Boolean = true
  def cts(on:Boolean) : Unit = {
    _cts = on
  }

  override def getProperties: Properties = {
    properties.setProperty("Address:",address)
    properties.setProperty("Baud:",baudRate.toString)
    properties
  }
}