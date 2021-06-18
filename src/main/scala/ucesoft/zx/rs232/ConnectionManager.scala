package ucesoft.zx.rs232

import ucesoft.zx.Log

import java.io.{InputStream, OutputStream}
import java.net.Socket

object ConnectionManager {
  private[this] var socket : Socket = _
  private[this] var in : InputStream = _
  private[this] var out : OutputStream = _

  def openConnection(address:String) : Boolean = {
    try {
      val (a,p) = address.split(":") match {
        case Array(a) =>
          (a.trim,80)
        case Array(a,p) =>
          (a.trim,p.trim.toInt)
        case _ =>
          return false
      }
      closeSocket
      socket = new Socket(a,p)
      in = socket.getInputStream
      out = socket.getOutputStream
      Log.info(s"Connected to $address")
      true
    }
    catch {
      case t:Throwable =>
        Log.info(s"Error while opening connection towards $address: $t")
        t.printStackTrace()
        false
    }
  }

  def closeConnection : Unit = {
    if (in != null) try { in.close() } catch { case _ : Throwable => }
    if (out != null) try { out.close() } catch { case _ : Throwable => }
    closeSocket
    socket = null
    in = null
    out = null
    Log.info(s"Connection closed")
  }

  private def closeSocket : Unit = if (socket != null) try { socket.close() } catch { case _ : Throwable => }

  def setStreams(in:InputStream,out:OutputStream) : Unit = {
    closeSocket
    this.in = in
    this.out = out
  }

  def getInputStream : InputStream = in
  def getOutputStream : OutputStream = out
}
