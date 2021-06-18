package ucesoft.zx.joystick

import net.java.games.input.{Component, Controller, ControllerEnvironment}
import ucesoft.zx.Log

object AbstractUSBJoystick {
  def getControllerName : Option[String] = {
    System.setProperty("jinput.loglevel","SEVERE")
    val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
    val controller = controllers find { c => c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK }
    controller map { _.getName }
  }
}

trait AbstractUSBJoystick extends Joystick {
  private[this] var controller : Option[Controller] = None
  private[this] var xAxisComponent : Component = _
  private[this] var yAxisComponent : Component = _
  private[this] var buttons : Array[Component] = _
  private[this] val dirThreshold = 0.5f

  def getController : Option[Controller] = controller

  override def init : Unit = {
    Log.info("Finding USB controllers ...")
    findController
  }

  def findController : Unit = {
    System.setProperty("jinput.loglevel","SEVERE")
    val controllers = ControllerEnvironment.getDefaultEnvironment.getControllers
    controller = controllers find { c => c.getType == Controller.Type.GAMEPAD || c.getType == Controller.Type.STICK }
    controller match {
      case None =>
      case Some(comp) =>
        Log.info(s"Found USB controller: ${comp.getName}")
        println(s"Found: ${comp.getName}")
        xAxisComponent = comp.getComponent(Component.Identifier.Axis.X)
        yAxisComponent = comp.getComponent(Component.Identifier.Axis.Y)
        buttons = comp.getComponents filter { c =>
          !c.isAnalog && !c.isRelative && c.getIdentifier().getClass().getName().toUpperCase().endsWith("BUTTON")
        }
        for(b <- buttons) {
          Log.info(s"Found USB button ${b.getName}")
        }
    }
  }

  override def getData: Int = {
    data = 0
    controller match {
      case None =>
        findController
      case Some(c) if c.poll =>
        // fire
        var i = 0
        while (i < buttons.length) {
          if (buttons(i).getPollData != 0.0) {
            set(FIRE)
            i = buttons.length
          }
          else i += 1
        }
        val x = xAxisComponent.getPollData
        val y = yAxisComponent.getPollData

        if (x < -dirThreshold) set(LEFT)
        else if (x > dirThreshold) set(RIGHT)

        if (y < -dirThreshold) set(UP)
        else if (y > dirThreshold) set(DOWN)
      case _ =>
        controller = None
    }
    super.getData
  }
}
