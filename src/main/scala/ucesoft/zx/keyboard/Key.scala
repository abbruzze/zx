package ucesoft.zx.keyboard

object Key extends Enumeration {
  val _0,_1,_2,_3,_4,_5,_6,_7,_8,_9 = Value
  val Q,W,E,R,T,Y,U,I,O,P = Value
  val A,S,D,F,G,H,J,K,L,ENTER = Value
  val CAPS_SHIFT,Z,X,C,V,B,N,M,SYMBOL_SHIFT,SPACE = Value

  val HALF_ROWS : Map[Int,Array[Key.Value]] = Map(
    0 -> Array(CAPS_SHIFT,Z,X,C,V),
    1 -> Array(A,S,D,F,G),
    2 -> Array(Q,W,E,R,T),
    3 -> Array(_1,_2,_3,_4,_5),
    4 -> Array(_0,_9,_8,_7,_6),
    5 -> Array(P,O,I,U,Y),
    6 -> Array(ENTER,L,K,J,H),
    7 -> Array(SPACE,SYMBOL_SHIFT,M,N,B)
  )
}

trait KeyboardMapper {
  val keyMap : Map[Int,List[Key.Value]]
}

class DefaultKeyboardMapper extends KeyboardMapper {
  import Key._
  import java.awt.event.KeyEvent._

  val keyMap : Map[Int,List[Key.Value]] = Map(
    VK_0 -> (_0 :: Nil), VK_1 -> (_1::Nil), VK_2 -> (_2::Nil), VK_3 -> (_3::Nil), VK_4 -> (_4::Nil), VK_5 -> (_5::Nil), VK_6 -> (_6::Nil), VK_7 -> (_7::Nil), VK_8 -> (_8::Nil), VK_9 -> (_9::Nil),
    VK_Q -> (Q::Nil), VK_W -> (W::Nil), VK_E -> (E::Nil), VK_R -> (R::Nil), VK_T -> (T::Nil), VK_Y -> (Y::Nil), VK_U -> (U::Nil), VK_I -> (I::Nil), VK_O -> (O::Nil), VK_P -> (P::Nil),
    VK_A -> (A::Nil), VK_S -> (S::Nil), VK_D -> (D::Nil), VK_F -> (F::Nil), VK_G -> (G::Nil), VK_H -> (H::Nil), VK_J -> (J::Nil), VK_K -> (K::Nil), VK_L -> (L::Nil), VK_ENTER -> (ENTER::Nil),
    VK_CONTROL -> (CAPS_SHIFT::Nil), VK_Z -> (Z::Nil), VK_X -> (X::Nil), VK_C -> (C::Nil), VK_V -> (V::Nil), VK_B -> (B::Nil), VK_N -> (N::Nil), VK_M -> (M::Nil), VK_SHIFT -> (SYMBOL_SHIFT::Nil), VK_SPACE -> (SPACE::Nil),
    // SPECIAL
    VK_BACK_SPACE -> List(CAPS_SHIFT,_0), // backspace
    VK_COMMA -> List(SYMBOL_SHIFT,N),     // comma
    VK_PERIOD -> List(SYMBOL_SHIFT,M)    // period
  )
}

object DefaultKeyboardMapper extends DefaultKeyboardMapper
object _128KeyboardMapper extends DefaultKeyboardMapper {
  import Key._
  import java.awt.event.KeyEvent._

  override val keyMap = DefaultKeyboardMapper.keyMap ++ Map(
    VK_UP -> List(CAPS_SHIFT,_7),         // up
    VK_DOWN -> List(CAPS_SHIFT,_6),       // down
    VK_RIGHT -> List(CAPS_SHIFT,_8),      // right
    VK_LEFT -> List(CAPS_SHIFT,_5)        // left
  )
}