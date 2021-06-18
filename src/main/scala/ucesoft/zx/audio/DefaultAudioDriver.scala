package ucesoft.zx.audio

import javax.sound.sampled.{SourceDataLine, _}

class DefaultAudioDriver(sampleRate:Int, bufferInMillis:Int, isStereo:Boolean = false) extends AudioDriverDevice with Runnable {
  private[this] val BUFFER_SIZE = (sampleRate.toDouble * bufferInMillis / 1000).toInt * (if (isStereo) 4 else 2)
  private[this] val dataLine = {
    val af = new AudioFormat(sampleRate.toFloat, 16,if (isStereo) 2 else 1, true, false)
    val dli = new DataLine.Info(classOf[SourceDataLine], af, BUFFER_SIZE)
    val dataLine = try {
      AudioSystem.getLine(dli).asInstanceOf[SourceDataLine] 
    }
    catch {
      case t:Throwable =>
        println("Warning: no audio available. Cause: " + t)
        null
    }
    
    if (dataLine != null) dataLine.open(dataLine.getFormat,BUFFER_SIZE)
    dataLine    
  }
  private[this] val volume : FloatControl = if (dataLine != null) dataLine.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl] else null
  private[this] var vol = 0
  private[this] val buffer,audioBuffer = Array.ofDim[Byte](BUFFER_SIZE)
  private[this] var audioBufferAvailable = false
  private[this] var pos = 0
  private[this] var muted = false
  private[this] var soundOn = true
  private[this] val audioThread = new Thread(this,"Audio")
  private[this] val sync = new Object

  setMasterVolume(100)
  if (dataLine != null) {
    dataLine.start()
    audioThread.setPriority(Thread.MAX_PRIORITY)
    audioThread.start()
  }
  
  def getMasterVolume = vol
  def setMasterVolume(v:Int) : Unit = {
    if (volume != null) {
      val max = volume.getMaximum
      val min = volume.getMinimum / 2f
      volume.setValue((v / 100.0f) * (max - min) + min)
      vol = v
    }
  }
  final def addSample(sample:Int) : Unit = {
    buffer(pos) = (sample & 0xff).toByte ; pos += 1
    buffer(pos) = (sample >> 8).toByte ; pos += 1
    if (pos == buffer.length) {      
      pos = 0

      if (dataLine != null) {
        sync.synchronized {
          audioBufferAvailable = true
          System.arraycopy(buffer,0,audioBuffer,0,buffer.length)
          sync.notify()
        }
      }
    }
  }

  final def reset : Unit = {
    pos = 0
    if (dataLine != null) dataLine.flush
    setSoundOn(true)
  }
  def discard : Unit = {
    if (dataLine != null) {
      dataLine.stop
      dataLine.flush
      dataLine.close()
    }
  }
  def setSoundOn(on:Boolean) : Unit = {
    soundOn = on
    updateLine
  }

  private def updateLine : Unit = {
    if (dataLine != null) {
      if (soundOn && !muted) dataLine.start
      else {
        dataLine.stop
        dataLine.flush
        pos = 0
      }
    }
  }

  override def setMuted(muted: Boolean): Unit = {
    this.muted = muted
    updateLine
  }

  override def isMuted: Boolean = muted

  override def isSoundOn: Boolean = soundOn

  override def run : Unit = {
    while (dataLine.isOpen) {
      sync.synchronized {
        while (!audioBufferAvailable) sync.wait()
        audioBufferAvailable = false
        dataLine.write(audioBuffer, 0, audioBuffer.length)
      }
    }
  }
}