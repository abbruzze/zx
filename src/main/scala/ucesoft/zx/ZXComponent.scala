package ucesoft.zx

import java.awt.Frame
import scala.collection.mutable.ListBuffer
import java.util.Properties
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import javax.swing.JOptionPane
import java.io.IOException
import scala.language.postfixOps

object ZXComponentType extends Enumeration {
  type Type = Value
  val CPU = Value
  val MEMORY = Value
  val CHIP = Value
  val VIDEO = Value
  val TAPE = Value
  val AUDIO = Value
  val JOYSTICK = Value
  val KEYBOARD = Value
  val MOTHERBOARD = Value
  val INTERFACE = Value
  val MICRODRIVE = Value
  val RS232 = Value
  val MODEM = Value
}

trait ZXComponent {
  val componentID : String
  val componentType : ZXComponentType.Type
  protected val properties = new Properties
  private[this] val _components = new ListBuffer[ZXComponent]

  def getProperties = properties

  final def add(c:ZXComponent) : Unit = {
    val alreadyAdded = _components exists { _.componentID == c.componentID }
    if (!alreadyAdded) _components += c
  }

  def reset : Unit
  def init : Unit
  def shutdown  : Unit = {}
  def hardReset : Unit = reset

  def modelChanged(model:Model.Value,resetRequest:Boolean) : Unit = {}

  final def change(oldComponent:ZXComponent, newComponent:ZXComponent) : Unit = {
    _components indexOf (oldComponent) match {
      case -1 => throw new IllegalArgumentException("Can't find component " + oldComponent)
      case i => _components(i) = newComponent
    }
  }

  final def remove(c:ZXComponent) : Unit = {
    _components -= c
  }

  def afterInitHook  : Unit = {}

  final def shutdownComponent  : Unit = {
    _components foreach { c =>
      c.shutdownComponent
    }
    shutdown
  }

  final def resetComponent : Unit = {
    Log.info(s"Resetting ${componentID}")
    _components foreach { c =>
      c.resetComponent
    }
    reset
  }
  final def hardResetComponent : Unit = {
    Log.info(s"Hard Resetting ${componentID}")
    _components foreach { c =>
      c.hardResetComponent
    }
    hardReset
  }
  def initComponent : Unit = {
    Log.info(s"Initializing ${componentID}")
    init
    _components foreach { c =>
      c.initComponent
    }
    afterInitHook
  }
  final def components = _components.toList
  final def printComponentsTree  : Unit = {
    def print(c:ZXComponent, ind:Int) : Unit = {
      println(("\t" * ind) + c.componentID + " - " + c.componentType)
      c.components foreach { c => print(c,ind + 1) }
    }
    print(this,0)
  }
  
  protected def saveState(out:ObjectOutputStream) : Unit = {}
  protected def loadState(in:ObjectInputStream) : Unit = {}
  protected def allowsStateRestoring : Boolean = true
  
  final def save(out:ObjectOutputStream) : Unit = {
    Log.info(s"Saving $componentID/$componentType's state ...")
    out.writeObject(componentID)
    saveState(out)
    for(c <- _components) c.save(out)    
  }
  final def load(in:ObjectInputStream) : Unit = {
    Log.info(s"Loading $componentID/$componentType's state ...")
    val id = in.readObject.asInstanceOf[String]
    if (id != componentID) componentIDMismatchHandling(id)
    loadState(in)
    for(c <- _components) c.load(in)
  }

  protected def componentIDMismatchHandling(id:String) : Unit = {
    throw new IOException(s"ID Mismatch: found $id, current $componentID")
  }

  final def allowsState : Boolean = {
    if (allowsStateRestoring) {
      _components forall { _.allowsStateRestoring }
    }
    else false
  }
  protected def saveClockEvents(out:ObjectOutputStream) : Unit = {
    val events = Clock.systemClock.getSubIdListFor(componentID)
    out.writeObject(events)
    for(e <- events) Log.info(s"Saving event ${e._1} for cycle ${e._2} for $componentID")
  }
  protected def loadClockEvents(in:ObjectInputStream)(f:Function2[Int,Long,ClockEvent]) : Unit = {
    val clk = Clock.systemClock
    val events = in.readObject.asInstanceOf[List[(Int,Long)]]
    for(e <- events) {
      Log.info(s"Loading event ${e._1} for cycle ${e._2} for $componentID")
      clk.schedule(f(e._1,e._2))
    }
  }
  protected def loadMemory[T](mem:Array[T],in:ObjectInputStream) : Unit = {
    val ram = in.readObject.asInstanceOf[Array[T]]
    if (ram.length != mem.length) throw new IOException(s"ROM/RAM length mismatch while loading $componentID.Expected ${mem.length}, found ${ram.length}")
    Array.copy(ram,0,mem,0,ram.length)
  }

  protected def getActiveFrame : Option[Frame] = {
    Frame.getFrames.filter(_.isActive).headOption match {
      case f@Some(_) =>
        f
      case None =>
        Frame.getFrames.filter(_.isVisible ).headOption
    }
  }

  def showError(title:String,error:String) : Unit = {
    if (isHeadless) throw new RuntimeException(s"[$title] $error")
    getActiveFrame match {
      case Some(activeWindow) =>
        JOptionPane.showMessageDialog(activeWindow,error,title,JOptionPane.ERROR_MESSAGE)
      case None =>
        println(s"[$title] $error")
    }

  }

  def isHeadless : Boolean = true
}