package ucesoft.zx.cpu

trait BankedMemory extends Memory {
  def peekBank(bank:Int,address:Int) : Int
  def loadBank(bank:Int,address:Int,value:Int) : Unit
}
