package ucesoft.zx.audio

import ucesoft.zx.{Clock, ZXComponent, ZXComponentType}
import ucesoft.zx.ZXComponentType.Type

abstract class AudioDevice(val stereo:Boolean) extends ZXComponent {
  override val componentType: Type = ZXComponentType.AUDIO

  val sampleRate : Int
  val bufferInMillis : Int
  val driver : AudioDriverDevice = new DefaultAudioDriver(sampleRate,bufferInMillis,isStereo = stereo)

  protected var CPU_FREQ = 3500000
  protected var CLOCKS_PER_SAMPLE = CPU_FREQ / sampleRate
  protected var CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / sampleRate).toInt - CLOCKS_PER_SAMPLE * 1000
  private[this] var nextRest = 0
  private[this] var nextSample = 0
  protected var isOn : Boolean = true

  def setOn(value:Boolean) : Unit = isOn = value

  def setCPUFreq(freq:Int) : Unit = {
    CPU_FREQ = freq
    CLOCKS_PER_SAMPLE = CPU_FREQ / sampleRate
    CLOCKS_PER_SAMPLE_REST = ((CPU_FREQ * 1000L) / sampleRate).toInt - CLOCKS_PER_SAMPLE * 1000
    nextSample = 0
    nextRest = 0
  }

  override def init : Unit = {
    Clock.systemClock.addChangeFrequencyListener(f => setCPUFreq(f.toInt))
  }

  def clock(cycles:Long) : Unit = {
    tick(cycles)

    nextSample += 1
    if (nextSample == CLOCKS_PER_SAMPLE) {
      nextRest += CLOCKS_PER_SAMPLE_REST
      if (nextRest > 1000) {
        nextRest -= 1000
        nextSample = -1
      }
      else nextSample = 0

      sample(cycles)
    }
  }

  protected def tick(cycles:Long) : Unit = {}

  protected def sample(cycles:Long) : Unit
}
