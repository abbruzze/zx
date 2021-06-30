package ucesoft.zx.format

import java.io.File
import javax.sound.sampled.{AudioFormat, AudioSystem}

object WAV {
  def readWAV(file:String) : Option[WAV] = {
    try {
      val ais = AudioSystem.getAudioInputStream(new File(file))
      val fmt = ais.getFormat
      if (fmt.getChannels != 1 || fmt.getSampleSizeInBits != 8) return None

      val buffer = Array.ofDim[Byte](2048)
      var min = 255
      var max = 0
      var stop = false
      var samples = 0
      while (!stop) {
        val read = ais.read(buffer)
        if (read == -1) stop = true
        else {
          samples += read
          var p = 0
          while (p < read) {
            val i = buffer(p).toInt & 0xFF
            if (i > max) max = i
            if (i < min) min = i
            p += 1
          }
        }
      }
      val pivot = (max + min) / 2
      ais.close()
      Some(new WAV(file,pivot,fmt.getSampleRate.toInt,samples))
    }
    catch {
      case _:Throwable =>
        None
    }
  }
}

class WAV(val file:String,pivot:Int,val sampleRate:Int,samples:Int) {
  private var ais = AudioSystem.getAudioInputStream(new File(file))

  def getDurationInSeconds : Int = samples / sampleRate

  def close() : Unit = ais.close()
  def reset() : Unit = {
    close()
    ais = AudioSystem.getAudioInputStream(new File(file))
  }

  def read : Option[Boolean] = {
    val r = ais.read()
    if (r == -1) None
    else Some(r < pivot)
  }
}
