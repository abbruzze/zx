package ucesoft.zx.tape

sealed trait TapeState
case object TAPE_PLAY extends TapeState
case object TAPE_STOP extends TapeState
case object TAPE_RECORD extends TapeState
