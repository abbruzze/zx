package ucesoft.zx

abstract class Chip extends ZXComponent {
  val id: ChipID.ID   
  val componentType = ZXComponentType.CHIP
  lazy val componentID = componentType.toString
}

object ChipID extends Enumeration {
  type ID = Value
  val CPU = Value("CPU")
  val ULA = Value("ULA")
}