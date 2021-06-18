package ucesoft.zx.audio

import java.util

object AY8912Mode extends Enumeration {
  val ACB,ABC,NONE = Value
}

object AY38912 {
  private val volumeRate = Array[Double](0.0, 0.0137, 0.0205, 0.0291, 0.0423, 0.0618, 0.0847, 0.1369, 0.1691, 0.2647, 0.3527, 0.4499, 0.5704, 0.6873, 0.8482, 1.0)
}

class AY38912(override val sampleRate:Int = 44100,override val bufferInMillis:Int = 100) extends AudioDevice(true) {
  override val componentID: String = "AY38912"
  import AY38912._

  abstract private class AYComponent {
    var amplitude = 0
    var counter = 0
    var pitch = 0
    var tone = false

    def cycle: Unit

    def reset(): Unit = {
      pitch = 1
      amplitude = 0
      counter = 0
      tone = false
    }

    def setPitchLo(value: Int): Unit = {
      pitch = value | (pitch & 0xF00)
    }

    def setPitchHi(value: Int): Unit = {
      pitch = value << 8 | (pitch & 0xFF)
    }
  }

  private class Channel extends AYComponent {
    var volume = 0
    var envelopeON = false
    var disableTone = false
    var disableNoise = false

    def setEnvVolume(value: Int): Unit = {
      val envelopeON = (value & 0x10) != 0x0
      this.envelopeON = envelopeON
      if (envelopeON) amplitude = volumeLevel(envelope.amplitude)
      else amplitude = volumeLevel(value & 0xF)
    }

    override def cycle: Unit = {
      counter += 1
      if (counter > pitch) {
        tone ^= true
        counter = 0
      }
    }

    def env(): Unit = {
      if (envelopeON) amplitude = volumeLevel(envelope.amplitude)
    }

    def osc: Unit = {
      if ((tone || disableTone) && (noise.tone || disableNoise)) volume += amplitude
    }

    override def reset(): Unit = {
      super.reset()
      volume = 0
      disableNoise = false
      disableTone = false
    }
  }

  var enabled = true
  private var lastClk = 0L
  private var audioMode = AY8912Mode.NONE
  private var cont = false
  private var hold = false
  private var alternate = false
  private var attack = false
  private val volumeLevel : Array[Int] = {
    val vl = Array.ofDim[Int](16)
    val maxAmplitude = 10922
    for (i <- 0 until vl.length) vl(i) = (maxAmplitude * volumeRate(i)).toInt
    vl
  }
  private var address = 0
  private var noisePeriod = 0
  private val channelA = new Channel
  private val channelB = new Channel
  private val channelC = new Channel
  private val noise = new AYComponent {
    private var rng = 1

    override def cycle: Unit = {
      counter += 1
      if (counter >= pitch) {
        counter = 0
        pitch = noisePeriod
        if (pitch == 0) pitch = 1
        pitch <<= 1
        if (((rng + 1) & 0x2) != 0x0) tone ^= true
        if ((rng & 0x1) != 0x0) rng ^= 0x24000
        rng >>>= 1
      }
    }
    override def reset(): Unit = {
      super.reset()
      rng = 1
    }
  }
  private val envelope = new AYComponent {
    override def cycle: Unit = {
      if (cont) {
        counter += 1
        if (counter >= pitch) {
          counter = 0
          if (attack) amplitude += 1 else amplitude -= 1
          if ((amplitude & 0x10) != 0x0) {
            if ((registers(13) & 0x8) == 0x0) {
              amplitude = 0
              cont = false
            }
            else {
              if (alternate) attack ^= true
              if (hold) {
                amplitude = if (attack) 15 else 0
                cont = false
              }
              else
                amplitude = if (attack) 0 else 15
            }
          }
          channelA.env()
          channelB.env()
          channelC.env()
        }
      }
    }
  }

  private val registers = Array.ofDim[Int](16)

  override def setCPUFreq(freq:Int) : Unit = super.setCPUFreq(freq >> 1)

  setCPUFreq(3500000)

  // ======================================================================

  def setAudioMode(mode:AY8912Mode.Value) : Unit = audioMode = mode

  override protected def tick(cycles:Long) : Unit = {
    if (enabled) {
      if ((cycles & 0x0F) == 0) {
        channelA.cycle
        channelB.cycle
        channelC.cycle
        noise.cycle
        envelope.cycle
      }
      channelA.osc
      channelB.osc
      channelC.osc
    }
  }

  override protected def sample(cycles:Long) : Unit = {
    val period = (cycles - lastClk).toInt
    lastClk = cycles
    var outL,outR = 0
    audioMode match {
      case AY8912Mode.NONE =>
        val value = channelA.volume + channelB.volume + channelC.volume
        outL = value
        outR = value
      case AY8912Mode.ABC =>
        outL = channelA.volume + channelB.volume
        outR = channelC.volume
      case AY8912Mode.ACB =>
        outL = channelA.volume + channelC.volume
        outR = channelB.volume + channelC.volume
    }

    channelA.volume = 0
    channelB.volume = 0
    channelC.volume = 0

    if (outL < 10) outL = 0
    if (outR < 10) outR = 0

    driver.addSample(outL / period)
    driver.addSample(outR / period)
  }

  def enable(enable: Boolean): Unit = {
    if (enable ^ enabled) {
      enabled = enable
    }
  }

  def setRegister(address: Int): Unit = {
    this.address = address & 0xF
  }

  def getData: Int = registers(address)

  def setData(_value: Int): Unit = {
    var value = _value
    address match {
      case 0 =>
        channelA.setPitchLo(value)
      case 1 =>
        value &= 0xF
        channelA.setPitchHi(value)
      case 2 =>
        channelB.setPitchLo(value)
      case 3 =>
        value &= 0xF
        channelB.setPitchHi(value)
      case 4 =>
        channelC.setPitchLo(value)
      case 5 =>
        value &= 0xF
        channelC.setPitchHi(value)
      case 6 =>
        value &= 0x1F
        noisePeriod = value
      case 7 =>
        channelA.disableTone = (value & 0x1) != 0x0
        channelB.disableTone = (value & 0x2) != 0x0
        channelC.disableTone = (value & 0x4) != 0x0
        channelA.disableNoise = (value & 0x8) != 0x0
        channelB.disableNoise = (value & 0x10) != 0x0
        channelC.disableNoise = (value & 0x20) != 0x0
      case 8 =>
        value &= 0x1F
        channelA.setEnvVolume(value)
      case 9 =>
        value &= 0x1F
        channelB.setEnvVolume(value)
      case 10 =>
        value &= 0x1F
        channelC.setEnvVolume(value)
      case 11 =>
        envelope.setPitchLo(value)
      case 12 =>
        envelope.setPitchHi(value)
      case 13 =>
        hold = (value & 0x1) != 0x0
        alternate = (value & 0x2) != 0x0
        attack = (value & 0x4) != 0x0
        envelope.amplitude = if (attack) 0 else 15
        envelope.counter = 0
        cont = true
        channelA.env()
        channelB.env()
        channelC.env()
        value &= 0xF
      case _ =>
    }
    registers(address) = value
  }

  def reset: Unit = {
    channelA.reset()
    channelB.reset()
    channelC.reset()
    noise.reset()
    envelope.reset()
    address = 0
    attack = false
    cont = false
    util.Arrays.fill(registers, 0)
    registers(7) = 0xFF
  }
}

