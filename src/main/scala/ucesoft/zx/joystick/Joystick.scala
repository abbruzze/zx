package ucesoft.zx.joystick

import ucesoft.zx.ZXComponent
import ucesoft.zx.ZXComponentType

import java.util.Properties

abstract class Joystick extends ZXComponent {
  override val componentType: ZXComponentType.Type = ZXComponentType.JOYSTICK
  override val componentID: String = "Joystick"

  private[joystick] def RIGHT : Int
  private[joystick] def LEFT : Int
  private[joystick] def DOWN : Int
  private[joystick] def UP : Int
  private[joystick] def FIRE : Int

  override def getProperties: Properties = {
    properties.setProperty("Data",data.toString)
    properties
  }

  protected var data = 0

  def port: Int

  override def init : Unit = {}
  override def reset : Unit = {}

  protected def set(bit:Int) : Unit = data |= bit
  protected def clear(bit:Int) : Unit = data &= ~bit

  def getData: Int = data & 0x1F
}

class JoystickBridge extends Joystick {
  var joy : Joystick = _

  private[joystick] def RIGHT : Int = joy.RIGHT
  private[joystick] def LEFT : Int = joy.LEFT
  private[joystick] def DOWN : Int = joy.DOWN
  private[joystick] def UP : Int = joy.UP
  private[joystick] def FIRE : Int = joy.FIRE

  override def getProperties: Properties = if (joy != null) joy.getProperties else properties

  override def init : Unit = {
    if (joy != null) joy.init
  }
  override def reset : Unit = {
    if (joy != null) joy.reset
  }

  override def port : Int = if (joy != null) joy.port else -1

  override def getData : Int = if (joy != null) joy.getData else 0
}
