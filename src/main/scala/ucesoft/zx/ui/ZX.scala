package ucesoft.zx.ui

import com.formdev.flatlaf.FlatLightLaf
import ucesoft.zx.audio.{AY8912Mode}
import ucesoft.zx.format.{MDR, SnapshotFileFormat, TAP, TZX}
import ucesoft.zx.gpu.{Display}
import ucesoft.zx.joystick.{KempstonJoystickWithKeyboard, KempstonJoystickWithUSB}
import ucesoft.zx.misc.{FullScreenMode, MouseCage, Preferences, ScaledImageIcon}
import ucesoft.zx.{ClockEvent, Log, Model, Version}
import ucesoft.zx.spectrum.Spectrum
import ucesoft.zx.tape.{TAPE_PLAY, TAPE_STOP, TapeListener, TapeState}
import ucesoft.zx.trace.{InspectPanel, InspectPanelDialog, TraceDialog}
import ucesoft.zx.ui.control.{ControlPanel, VideoControl}

import java.awt.{Dimension, FlowLayout}
import java.awt.event.{WindowAdapter, WindowEvent}
import java.io.{File}
import javax.imageio.ImageIO
import javax.swing.border.BevelBorder
import javax.swing.{BorderFactory, ImageIcon, JCheckBoxMenuItem, JDialog, JFileChooser, JFrame, JMenu, JMenuBar, JMenuItem, JOptionPane, JPanel, KeyStroke, UIManager}

object ZX {
  def main(args: Array[String]): Unit = {
    val zx = new ZX
    zx.boot
    zx.configure(args)
    zx.run
  }
}

class ZX extends SwingAware with VideoControl {
  private[this] var systemInitialized = false
  private[this] val spectrum = new Spectrum(errorHandler _)
  private[this] var frame : JFrame = _
  private[this] var statusPanel : JPanel = _
  private[this] var headless = false
  private[this] var display : Display = _
  private[this] var traceDialog : TraceDialog = _
  private[this] var inspectDialog : InspectPanelDialog = _
  private[this] var lastDirectory : String = _
  private[this] var tapeWarpMode = true
  private[this] var controlPanelDialog : JDialog = _
  // ====================================================
  protected var warpModeItem : JCheckBoxMenuItem = _
  protected var traceItem : JCheckBoxMenuItem = _
  protected var inspectItem : JCheckBoxMenuItem = _
  protected var microdriveItem : JMenu = _
  protected var mouseEnabledItem : JCheckBoxMenuItem = _
  protected var microdriveEjectItems = Array.ofDim[JMenuItem](8)
  // ====================================================
  protected val kempJoyKeyb = new KempstonJoystickWithKeyboard
  protected val kempJoyUSB = new KempstonJoystickWithUSB

  def boot : Unit = {
    if (System.getProperty("swing.defaultlaf") == null) {
      FlatLightLaf.install()
      JFrame.setDefaultLookAndFeelDecorated(false)
      JDialog.setDefaultLookAndFeelDecorated(false)
      UIManager.setLookAndFeel("com.formdev.flatlaf.FlatDarculaLaf")
    }

    frame = new JFrame(s"ZX Emulator v${Version.VERSION} ")
    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e:WindowEvent) : Unit = shutdown
    })
    frame.setIconImage(ImageIO.read(getClass.getResourceAsStream("/resources/images/zx_logo.png")))

    frame.addKeyListener(spectrum.keyboard)

    // Status Panel
    statusPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))
    val tsp = new TapeStatusPanel(spectrum.tape,frame)
    val msp = new MicrodriveStatusPanel
    spectrum.mmu.ifI.addDriveListener(msp)
    tsp.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    msp.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    statusPanel.add(msp)
    statusPanel.add(tsp)
    frame.getContentPane.add("South",statusPanel)
    frame.setTransferHandler(new DNDHandler(handleDND _))

    // Display
    val displayDim = spectrum.ula.getDisplaySize
    display = new Display(displayDim.width,displayDim.height,frame.getTitle,frame,spectrum.clk)
    frame.getContentPane.add("Center",display)
    spectrum.ula.setDisplay(display)
    spectrum.add(display)

    // Trace
    traceDialog = TraceDialog.getTraceDialog("ZX trace",frame,spectrum.mmu,spectrum.z80,display,spectrum.ula,() => {
      traceItem.setSelected(false)
      traceDialog.setVisible(false)
    })

    Log.setOutput(traceDialog.logPanel.writer)

    // Frame
    buildMenu

    // Tape
    spectrum.tape.addTapeListener(new TapeListener {
      override def stateChanged(oldState: TapeState, newState: TapeState): Unit = {
        if (tapeWarpMode) {
          newState match {
            case TAPE_PLAY =>
              spectrum.setWarpMode(true)
              warpModeItem.setSelected(true)
            case TAPE_STOP =>
              spectrum.setWarpMode(false)
              warpModeItem.setSelected(false)
            case _ =>
          }
        }
      }
    })

    // Joystick
    frame.addKeyListener(kempJoyKeyb)

    // Display zoom
    zoomDisplay(2)
  }

  def configure(args: Array[String]) : Unit = {
    import Preferences._

    val pref = spectrum.preferences
    // Model
    pref.add(MODEL,"sets spectrum model","48k",Set("16k","48k","128k","128k+2","128k+2A","128k+3")) { model =>
      val newModel = Model.withName(model)
      spectrum.clk.pause
      modelChanged(newModel,true)
      if (systemInitialized) spectrum.clk.play
    }
    // Joystick
    pref.add(JOY_TYPE,"sets joystick type","none",Set("none","kempstone_keyboard","kempstone_usb")) { joy =>
      joy match {
        case "none" =>
          spectrum.joystick.joy = null
        case "kempstone_keyboard" =>
          spectrum.joystick.joy = kempJoyKeyb
        case "kempstone_usb" =>
          spectrum.joystick.joy = kempJoyUSB
      }
    }
    pref.add(JOY_UP_KEY,"sets joystick up keystroke",-1) { key => kempJoyKeyb.upKeyCode = key }
    pref.add(JOY_DOWN_KEY,"sets joystick up keystroke",-1) { key => kempJoyKeyb.downKeyCode = key }
    pref.add(JOY_LEFT_KEY,"sets joystick up keystroke",-1) { key => kempJoyKeyb.leftKeyCode = key }
    pref.add(JOY_RIGHT_KEY,"sets joystick up keystroke",-1) { key => kempJoyKeyb.rightKeyCode = key }
    pref.add(JOY_FIRE_KEY,"sets joystick up keystroke",-1) { key => kempJoyKeyb.fireKeyCode = key }
    // ULA
    pref.add(SNOW_EFFECT_ENABLED,"enables/disables snow effect",true) { enabled => spectrum.mmu.setSnowAllowed(enabled) }
    pref.add(ULAPLUS_ENABLED,"enables/disables ULA+",false) { enabled => spectrum.setULAPlus(enabled,false) }
    pref.add(PAL_SCANLINES_ENABLED,"enables/disables PAL scan lines",false) { enabled => spectrum.ula.setScanLineEffect(enabled) }
    pref.add(FULLS_SCREEN,"enables/disables full screen mode",false) { enabled => if (enabled) fullScreen }
    // TAPE
    pref.add(TAPE_AUTOPLAY_ENABLED,"enables/disables tape auto press-play",true) { enabled => spectrum.mmu.tapeAutoPlay = enabled }
    pref.add(TAPE_AUTOSTOP_ENABLED,"enables/disables tape auto press-stop",true) { enabled => spectrum.mmu.tapeAutostop = enabled }
    pref.add(TAPE_FAST_LOADER_ENABLED,"enables/disables tape fast loader",true) { enabled => spectrum.mmu.tapeFastLoadEnabled = enabled }
    pref.add(TAPE_WARPMODE_ENABLED,"enables/disables tape warp mode while loading",true) { enabled => tapeWarpMode = enabled }
    // AUDIO
    pref.add(AUDIO_AY_ON48K_ENABLED,"enables/disables AY38912 on 48k",true) { enabled => spectrum.mmu.setAYOn48KEnabled(enabled) }
    pref.add(AUDIO_AY_MODE,"sets AY38912 stereo mode: abc, acb, none","none",Set("abc","acb","none")) { mode => spectrum.ay.setAudioMode(AY8912Mode.withName(mode.toUpperCase())) }
    // MOUSE
    pref.add(MOUSE_ENABLED,"enables/disables Kempstone mouse",false) { enabled => spectrum.mmu.mouseActive = enabled }
    // IF I
    pref.add(IFI_ENABLED,"enables/disables Interface I",false) { enabled => spectrum.mmu.interfaceIEnabled = enabled }
    pref.add(MICRODRIVE_WRITE_CHANGES,"if not enabled changes on cartridge will not be write on disk",true) { enabled => MDR.writeChanges = enabled }
    pref.add(MICRODRIVE_SECTORS,"sets the number of sectors of an unformatted cartridge",254) { value => MDR.unformatted_sector_size = value }
    // VIDEO
    pref.add(ZOOM_FACTOR,"video zoom factor: 1,2 or 3",2,Set(1,2,3)) { zoomF => zoomDisplay(zoomF) }
    // ROMS
    pref.add(Preferences._48K_ROM_PREF,"48K ROM path","") { _ => }
    pref.add(Preferences._128K_0_ROM_PREF,"128K ROM0 path","") { _ => }
    pref.add(Preferences._128K_1_ROM_PREF,"128K ROM1 path","") { _ => }
    pref.add(Preferences._128K_PLUS2_0_ROM_PREF,"128K+ ROM0 path","") { _ => }
    pref.add(Preferences._128K_PLUS2_1_ROM_PREF,"128K+ ROM1 path","") { _ => }
    pref.add(Preferences._128K_PLUS2A_0_ROM_PREF,"128K+2A ROM0 path","") { _ => }
    pref.add(Preferences._128K_PLUS2A_1_ROM_PREF,"128K+2A ROM1 path","") { _ => }
    pref.add(Preferences._128K_PLUS2A_2_ROM_PREF,"128K+2A ROM2 path","") { _ => }
    pref.add(Preferences._128K_PLUS2A_3_ROM_PREF,"128K+2A ROM3 path","") { _ => }
    pref.add(Preferences._128K_PLUS3_0_ROM_PREF,"128K+3 ROM0 path","") { _ => }
    pref.add(Preferences._128K_PLUS3_1_ROM_PREF,"128K+3 ROM1 path","") { _ => }
    pref.add(Preferences._128K_PLUS3_2_ROM_PREF,"128K+3 ROM2 path","") { _ => }
    pref.add(Preferences._128K_PLUS3_3_ROM_PREF,"128K+3 ROM3 path","") { _ => }
    pref.add(Preferences.IFI_ROM_PREF,"Interface I ROM path","") { _ => }
    // General
    pref.add(Preferences.LEC,"enables/disables LEC memory expansion",false) { enabled => spectrum.mmu.setLecEnabled(enabled) }
    // LEC


    spectrum.preferences.parseAndLoad(args,spectrum.configuration)

    pref.parseAndLoad(args,spectrum.configuration)

    // Control panel
    controlPanelDialog = ControlPanel.getDialog(frame,spectrum,this)
  }

  private def modelChanged(model:Model.Value,resetRequest:Boolean) : Unit = {
    println(s"Model: $model")
    if (model != spectrum.model) {
      spectrum.modelChanged(model,resetRequest)
      val dim = spectrum.ula.getDisplaySize
      display.setNewResolution(dim.height,dim.width)
      spectrum.ula.setDisplay(display)
      spectrum.preferences.update[String](Preferences.MODEL,model.toString)
    }
  }

  def run : Unit = {
    Log.setInfo

    Log.info("Building the system ...")
    // Initialization
    spectrum.initComponent
    // Inspect
    inspectDialog = InspectPanel.getInspectDialog(frame,spectrum, () => {
      inspectItem.setSelected(false)
      inspectDialog.setVisible(false)
    })

    systemInitialized = true

    swing {
      val xy = spectrum.configuration.getProperty(Preferences.XY)
      if (xy == null) frame.setLocationByPlatform(true)
      else {
        try {
          val Array(x, y) = xy.split(",") map { _.toInt }
          frame.setLocation(x,y)
        }
        catch {
          case _ : Throwable =>
            frame.setLocationByPlatform(true)
        }

      }
      frame.setVisible(true)
      spectrum.clk.play
    }
  }

  override def fullScreen : Unit = {
    val clip = spectrum.ula.getClip
    val height = clip.y2 - clip.y1
    val width = clip.x2 - clip.x1
    FullScreenMode.goFullScreen(frame,display,width,height,null,spectrum.keyboard)
  }

  override def zoomDisplay(factor:Int) : Unit = {
    if (factor == 1) spectrum.clk.pause
    val clip = spectrum.ula.getClip
    val yDim = (clip.y2 - clip.y1) * factor
    val xDim = (clip.x2 - clip.x1) * factor
    val dim = new Dimension(xDim,yDim)
    display.setPreferredSize(dim)
    display.invalidate
    if (factor == 1) frame.setTitle("")
    frame.pack()
    if (factor == 1) {
      frame.setTitle(s"ZX Emulator v${Version.VERSION} ")
      if (systemInitialized) spectrum.clk.play
    }
  }

  override def aspect4_3 : Unit = {
    spectrum.clk.pause
    val oldDim = display.getSize()
    val dim = new Dimension((oldDim.height.toDouble * 4 / 3).toInt,oldDim.height)
    display.setPreferredSize(dim)
    display.invalidate
    frame.setTitle("")
    frame.pack()
    frame.setTitle(s"ZX Emulator v${Version.VERSION} ")
    if (systemInitialized) spectrum.clk.play
  }

  protected def shutdown : Unit = {
    // SAVE CONFIGURATION
    val pos = frame.getLocationOnScreen()
    spectrum.configuration.setProperty(Preferences.XY,s"${pos.x},${pos.y}")
    if (spectrum.preferences.get[Boolean](Preferences.AUTOSAVE_PREFERENCES).map(_.value).getOrElse(false)) spectrum.savePreferences
    else spectrum.saveConfiguration

    sys.exit(0)
  }

  protected def errorHandler(t:Throwable) : Unit = {
    Log.info("Fatal error occurred: " + spectrum.z80 + "-" + t)
    try Log.info(spectrum.z80.disassemble(spectrum.mmu,spectrum.z80.getLastPC)._1) catch { case _:Throwable => }
    t.printStackTrace(Log.getOut)
    t.printStackTrace
    if (headless) {
      println(s"Fatal error occurred on cycle ${spectrum.clk.currentCycles}: ${spectrum.z80}\n${spectrum.z80.disassemble(spectrum.mmu,spectrum.z80.getLastPC)._1}")
      t.printStackTrace
      sys.exit(1)
    } // exit if headless
    JOptionPane.showMessageDialog(frame,t.toString + " [PC=" + Integer.toHexString(spectrum.z80.getLastPC) + "]", "Fatal error",JOptionPane.ERROR_MESSAGE)
    reset(true)
  }

  protected def reset(play:Boolean,hard:Boolean = false) : Unit = {
    spectrum.clk.pause
    if (hard) spectrum.hardResetComponent else spectrum.resetComponent
    if (play) spectrum.clk.play
  }

  protected def buildMenu : Unit = {
    val menubar = new JMenuBar
    frame.setJMenuBar(menubar)

    val fileMenu = new JMenu("File")
    val traceMenu = new JMenu("Trace")
    val optionMenu = new JMenu("Options")
    val helpMenu = new JMenu("Help")

    menubar.add(fileMenu)
    menubar.add(traceMenu)
    menubar.add(optionMenu)
    menubar.add(helpMenu)

    buildFileMenu(fileMenu)
    buildOptionsMenu(optionMenu)
    buildTraceMenu(traceMenu)
    buildHelpMenu(helpMenu)
  }

  protected def buildFileMenu(fileMenu:JMenu) : Unit = {
    val openSnapItem = new JMenuItem("Load snapshot image ...")
    openSnapItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_L,java.awt.event.InputEvent.ALT_DOWN_MASK))
    openSnapItem.addActionListener(_ => loadSnapshot(None) )
    fileMenu.add(openSnapItem)

    val saveSnapItem = new JMenuItem("Save SNA image ...")
    saveSnapItem.addActionListener(_ => saveSnapshot(None) )
    fileMenu.add(saveSnapItem)

    fileMenu.addSeparator()

    val openTapeItem = new JMenuItem("Load tape image ...",ScaledImageIcon("cassette.png"))
    openTapeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_T,java.awt.event.InputEvent.ALT_DOWN_MASK))
    openTapeItem.addActionListener(_ => loadTape(None) )
    fileMenu.add(openTapeItem)

    val newTapeItem = new JMenuItem("New tape image ...")
    newTapeItem.addActionListener(_ => createNewTapeImage )
    fileMenu.add(newTapeItem)

    fileMenu.addSeparator()

    microdriveItem = new JMenu("Microdrive")
    fileMenu.add(microdriveItem)
    microdriveItem.setEnabled(true) // TODO only if IFI enabled
    val insertMDRItem = new JMenu("Insert cartridge")
    val createMDRItem = new JMenu("Create cartridge")
    val ejectMDRItem = new JMenu("Eject cartridge")
    microdriveItem.add(insertMDRItem)
    microdriveItem.add(ejectMDRItem)
    microdriveItem.add(createMDRItem)
    val insertIcon = ScaledImageIcon("microdrive.png")
    val ejectIcon = ScaledImageIcon("eject.png")
    for(d <- 1 to 8) {
      microdriveEjectItems(d - 1) = new JMenuItem(s"Eject cartridge from drive $d",ejectIcon)
      microdriveEjectItems(d - 1).setEnabled(false)
      ejectMDRItem.add(microdriveEjectItems(d - 1))
      microdriveEjectItems(d - 1).addActionListener(_ => {
        spectrum.mmu.ifI.drives(d - 1).ejectCart
        microdriveEjectItems(d - 1).setEnabled(false)
      } )
      val insert = new JMenuItem(s"Insert cartridge into drive $d",insertIcon)
      insertMDRItem.add(insert)
      insert.addActionListener(_ => loadMDR(None,d) )
      val insertNew = new JMenuItem(s"Create new cartridge into drive $d")
      createMDRItem.add(insertNew)
      insertNew.addActionListener(_ => loadMDR(None,d,true) )
    }

    fileMenu.addSeparator()

    val resetItem = new JMenuItem("Reset")
    resetItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_R,java.awt.event.InputEvent.ALT_DOWN_MASK))
    resetItem.addActionListener(_ => reset(true) )
    fileMenu.add(resetItem)
    val hardResetItem = new JMenuItem("Hard Reset")
    hardResetItem.addActionListener(_ => reset(true,true) )
    fileMenu.add(hardResetItem)

    fileMenu.addSeparator()

    val pauseItem = new JCheckBoxMenuItem("Pause")
    pauseItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_P,java.awt.event.InputEvent.ALT_DOWN_MASK))
    pauseItem.addActionListener(_ => pause(pauseItem.isSelected) )
    fileMenu.add(pauseItem)

    fileMenu.addSeparator()

    val prefItem = new JMenuItem("Preferences ...")
    prefItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C,java.awt.event.InputEvent.ALT_DOWN_MASK))
    prefItem.addActionListener(_ => controlPanelDialog.setVisible(true) )
    fileMenu.add(prefItem)

    val exitItem = new JMenuItem("Exit")
    exitItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X,java.awt.event.InputEvent.ALT_DOWN_MASK))
    exitItem.addActionListener(_ => shutdown )
    fileMenu.add(exitItem)
  }

  protected def buildOptionsMenu(optionMenu:JMenu) : Unit = {
    val fullScreenItem = new JMenuItem("Full screen ...")
    fullScreenItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F,java.awt.event.InputEvent.ALT_DOWN_MASK))
    fullScreenItem.addActionListener(_ => fullScreen )
    optionMenu.add(fullScreenItem)

    val zoom = new JMenu("Zoom")
    optionMenu.add(zoom)
    val zoom1 = new JMenuItem("Zoom x 1")
    zoom1.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_1,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom1.addActionListener(_ => zoomDisplay(1) )
    zoom.add(zoom1)
    val zoom2 = new JMenuItem("Zoom x 2")
    zoom2.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_2,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom2.addActionListener(_ => zoomDisplay(2) )
    zoom.add(zoom2)
    val zoom3 = new JMenuItem("Zoom x 3")
    zoom3.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_3,java.awt.event.InputEvent.ALT_DOWN_MASK))
    zoom3.addActionListener(_ => zoomDisplay(3) )
    zoom.add(zoom3)

    warpModeItem = new JCheckBoxMenuItem("Warp mode")
    warpModeItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W,java.awt.event.InputEvent.ALT_DOWN_MASK))
    warpModeItem.setSelected(spectrum.clk.maximumSpeed)
    warpModeItem.addActionListener(_ => spectrum.setWarpMode(warpModeItem.isSelected) )
    optionMenu.add(warpModeItem)

    mouseEnabledItem = new JCheckBoxMenuItem("Mouse Interface I enabled")
    mouseEnabledItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_M,java.awt.event.InputEvent.ALT_DOWN_MASK))
    mouseEnabledItem.addActionListener(_ => enableMouse(mouseEnabledItem.isSelected) )
    optionMenu.add(mouseEnabledItem)

  }

  protected def buildTraceMenu(traceMenu:JMenu) : Unit = {
    traceItem = new JCheckBoxMenuItem("Trace ...")
    traceItem.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_D,java.awt.event.InputEvent.ALT_DOWN_MASK))
    traceItem.addActionListener(_ => traceDialog.setVisible(traceItem.isSelected) )
    traceMenu.add(traceItem)

    inspectItem = new JCheckBoxMenuItem("Inspect ...")
    inspectItem.addActionListener(_ => inspectDialog.setVisible(inspectItem.isSelected) )
    traceMenu.add(inspectItem)
  }

  protected def buildHelpMenu(helpMenu:JMenu) : Unit = {
    val prefItem = new JMenuItem("Command options")
    helpMenu.add(prefItem)
    prefItem.addActionListener(_ => {
      val settingsPanel = new SettingsPanel(spectrum.preferences)
      JOptionPane.showMessageDialog(frame,settingsPanel,"Command options",JOptionPane.INFORMATION_MESSAGE,new ImageIcon(getClass.getResource("/resources/images/zx_about.png")))
    })
    val aboutItem = new JMenuItem("About")
    helpMenu.add(aboutItem)
    aboutItem.addActionListener(_ => AboutPanel.showAboutDialog(frame))
  }

  // ========================================================================
  protected def catchError(activity:String)(block: => Unit) : Unit = {
    try {
      block
    }
    catch {
      case t:Throwable =>
        JOptionPane.showMessageDialog(frame,s"$activity error: ${t.getMessage}", "Error",JOptionPane.ERROR_MESSAGE)
        t.printStackTrace()
    }
  }

  protected def handleDND(file:File) : Unit = {
    val name = file.getName.toUpperCase()

    if (name.endsWith(".TAP") || name.endsWith(".TZX")) {
      val clk = spectrum.clk
      clk.pause
      spectrum.resetComponent
      clk.schedule(new ClockEvent("TAPLoading",clk.currentCycles + clk.getClockHz.toLong * 2,_ => loadTape(Some(file.toString) )))
      clk.play
    }
    else if (name.endsWith(".MDR") && spectrum.mmu.interfaceIEnabled) loadMDR(Some(file.toString),1)
    else {
      SnapshotFileFormat.getFormat(file.toString) match {
        case Some(_) =>
          loadSnapshot(Some(file.toString))
        case None =>
      }
    }
  }

  protected def loadMDR(file: Option[String],driveID:Int,create:Boolean = false) : Unit = {
    val mdr = file match {
      case None =>
        val fc = new JFileChooser
        fc.setDialogTitle(if (!create) "Loading microdrive cartridge image" else "Creating new microdrive cartridge image")
        fc.setFileView(ZXFileView)
        if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
        fc.setFileFilter(new javax.swing.filechooser.FileFilter {
          def accept(f:File) = f.isDirectory || f.toString.toUpperCase().endsWith(".MDR")
          def getDescription = s"ZX microdrive cartridge image"
        })
        if (!create) {
          fc.showOpenDialog(frame) match {
            case JFileChooser.APPROVE_OPTION =>
              lastDirectory = fc.getSelectedFile.getParent
              Some(fc.getSelectedFile.toString)
            case _ =>
              None
          }
        }
        else {
          fc.showSaveDialog(frame) match {
            case JFileChooser.APPROVE_OPTION =>
              lastDirectory = fc.getSelectedFile.getParent
              Some(fc.getSelectedFile.toString)
            case _ =>
              None
          }
        }
      case f@Some(_) => f
    }
    mdr match {
      case Some(cart) =>
        spectrum.clk.pause

        if (!create) {
          MDR.readMDR(cart) match {
            case Some(result) =>
              spectrum.mmu.ifI.drives(driveID - 1).insertCart(result)
              microdriveEjectItems(driveID - 1).setEnabled(true)
            case None =>
          }
        }
        else {
          val crt = MDR.createNewMDR(cart)
          spectrum.mmu.ifI.drives(driveID - 1).insertCart(crt)
          microdriveEjectItems(driveID - 1).setEnabled(true)
        }
        spectrum.clk.play
      case None =>
    }
  }

  protected def createNewTapeImage : Unit = {
    catchError("Create new tape image") {
      val fc = new JFileChooser
      fc.setDialogTitle(s"Create tape image")
      fc.setFileView(ZXFileView)
      if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
      fc.setFileFilter(new javax.swing.filechooser.FileFilter {
        def accept(f:File) = f.isDirectory
        def getDescription = s"ZX tape image"
      })
      fc.showOpenDialog(frame) match {
        case JFileChooser.APPROVE_OPTION =>
          lastDirectory = fc.getSelectedFile.getParent
          var path = fc.getSelectedFile.toString
          if (!path.toUpperCase().endsWith(".TZX")) path += ".tzx"
          spectrum.tape.createNewTape(path)
          val tzx = TZX.readTZX(path).get
          spectrum.tape.setTZX(tzx)
        case _ =>
          None
      }
    }
  }

  protected def saveSnapshot(file:Option[String]) : Unit = {
    catchError("Saving snapshot") {
      try {
        spectrum.clk.pause
        val snap = file match {
          case None =>
            val fc = new JFileChooser
            fc.setDialogTitle(s"Saving snapshot image")
            fc.setFileView(ZXFileView)
            if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
            fc.setFileFilter(new javax.swing.filechooser.FileFilter {
              def accept(f: File) = f.isDirectory || SnapshotFileFormat.validExt(f.toString)

              def getDescription = s"ZX Snapshot image"
            })
            fc.showSaveDialog(frame) match {
              case JFileChooser.APPROVE_OPTION =>
                lastDirectory = fc.getSelectedFile.getParent
                Some(fc.getSelectedFile.toString)
              case _ =>
                None
            }
          case f@Some(_) => f
        }
        snap match {
          case Some(f) =>
            SnapshotFileFormat.getFormat(".sna").foreach { s =>
              s.createSnapshot(spectrum) match {
                case Some(buffer) =>
                  val fn = if (f.toUpperCase.endsWith(".SNA")) f else s"$f.sna"
                  java.nio.file.Files.write(new File(fn).toPath, buffer)
                case None =>
              }
            }
          case None =>
        }
      }
      finally {
        spectrum.clk.play
      }
    }
  }

  protected def loadSnapshot(file:Option[String]) : Unit = {
    catchError("Loading snapshot") {
      val snap = file match {
        case None =>
          val fc = new JFileChooser
          fc.setDialogTitle(s"Loading snapshot image")
          fc.setFileView(ZXFileView)
          if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
          fc.setFileFilter(new javax.swing.filechooser.FileFilter {
            def accept(f:File) = f.isDirectory || SnapshotFileFormat.validExt(f.toString)
            def getDescription = s"ZX Snapshot image: ${SnapshotFileFormat.extensions.mkString(",")}"
          })
          fc.showOpenDialog(frame) match {
            case JFileChooser.APPROVE_OPTION =>
              lastDirectory = fc.getSelectedFile.getParent
              Some(fc.getSelectedFile.toString)
            case _ =>
              None
          }
        case f@Some(_) => f
      }
      snap match {
        case Some(snapshot) =>
          SnapshotFileFormat.getFormat(snapshot) match {
            case Some(snap) =>
              if (snap.needReset) {
                spectrum.clk.pause
                spectrum.resetComponent
              }
              SnapshotFileFormat(snapshot,spectrum) match {
                case Some(result) =>
                  modelChanged(result.model,false)
                case None =>
              }
              if (systemInitialized) spectrum.clk.play
            case None =>
          }
        case None =>
      }
    }
  }

  protected def loadTape(file:Option[String]) : Unit = {
    catchError("Loading tape") {
      val tape = file match {
        case None =>
          val fc = new JFileChooser
          fc.setDialogTitle(s"Loading tape image")
          fc.setFileView(ZXFileView)
          if (lastDirectory != null) fc.setCurrentDirectory(new File(lastDirectory))
          fc.setFileFilter(new javax.swing.filechooser.FileFilter {
            def accept(f:File) = f.isDirectory || f.getName.toUpperCase.endsWith(".TAP") || f.getName.toUpperCase.endsWith(".TZX")
            def getDescription = s"ZX tape image"
          })
          fc.showOpenDialog(frame) match {
            case JFileChooser.APPROVE_OPTION =>
              lastDirectory = fc.getSelectedFile.getParent
              Some(fc.getSelectedFile.toString)
            case _ =>
              None
          }
        case f@Some(_) => f
      }
      tape match {
        case Some(tape) =>
          val blocks = if (tape.toUpperCase.endsWith(".TAP")) {
            TAP.readTAP(tape) match {
              case Some(tp) =>
                TZX.TAP2TZX(tp,Some(new File(tape)))
              case None =>
                throw new IllegalArgumentException("Can't read tap format")
            }
          }
          else {
            TZX.readTZX(tape) match {
              case Some(tp) => tp
              case None =>
                throw new IllegalArgumentException("Can't read tzx format")
            }
          }
          spectrum.tape.setTZX(blocks)
          if (spectrum.mmu.tapeAutoPlay) spectrum.insertKeyCodes(0xEF,0x22,0x22,0xD)
        case None =>
      }
    }
  }

  private def pause(on:Boolean) : Unit = {
    if (on) {
      spectrum.clk.pause
      display.setPaused
    }
    else spectrum.clk.play
  }

  private def enableMouse(enabled: Boolean) : Unit = {
    if (enabled) MouseCage.enableMouseCageOn(display) else MouseCage.disableMouseCage
    spectrum.mmu.mouseEnabled = enabled
  }
}
