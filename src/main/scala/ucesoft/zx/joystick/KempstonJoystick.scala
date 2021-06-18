package ucesoft.zx.joystick

abstract class KempstonJoystick extends Joystick {
  override final val RIGHT = 1
  override final val LEFT = 2
  override final val DOWN = 4
  override final val UP = 8
  override final val FIRE = 16

  override final val port = 0x1F
}
