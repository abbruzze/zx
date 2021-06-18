package ucesoft.zx.tape

trait TapeBlockInfo {
  val index : Int
  val blockType : String
  val durationOffsetInSeconds : Int
  def blockInfo : String
  def getDurationInCycles : Long
  def getDurationInSeconds : Int
}
