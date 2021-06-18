package ucesoft.zx.mmu

import ucesoft.zx.misc.Preferences

import java.io.File
import java.nio.file.Files

object ROMs {
  final val _48K = getClass.getResourceAsStream("/resources/roms/spectrum48k.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_0 = getClass.getResourceAsStream("/resources/roms/spectrum128k_0.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_1 = getClass.getResourceAsStream("/resources/roms/spectrum128k_1.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2_0 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2_0.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2_1 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2_1.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2A_0 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2A_0.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2A_1 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2A_1.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2A_2 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2A_2.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS2A_3 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus2A_3.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS3_0 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus3_0.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS3_1 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus3_1.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS3_2 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus3_2.rom").readAllBytes() map { _.toInt & 0xFF }
  final val _128K_PLUS3_3 = getClass.getResourceAsStream("/resources/roms/spectrum128k_plus3_3.rom").readAllBytes() map { _.toInt & 0xFF }
  final val IFI = getClass.getResourceAsStream("/resources/roms/if1v2.rom").readAllBytes() map { _.toInt & 0xFF }

  def get48KROM(implicit pref:Preferences) : Array[Int] = {
    pref.get[String](Preferences._48K_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _48K
    }
  }

  def get128KROMs(implicit pref:Preferences) : Array[Array[Int]] = {
    val rom_0 = pref.get[String](Preferences._128K_0_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_0
    }
    val rom_1 = pref.get[String](Preferences._128K_1_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_1
    }
    Array(rom_0,rom_1)
  }

  def get128KPlus2ROMs(implicit pref:Preferences) : Array[Array[Int]] = {
    val rom_0 = pref.get[String](Preferences._128K_PLUS2_0_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2_0
    }
    val rom_1 = pref.get[String](Preferences._128K_PLUS2_1_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2_1
    }
    Array(rom_0,rom_1)
  }

  def get128KPlus2AROMs(implicit pref:Preferences) : Array[Array[Int]] = {
    val rom_0 = pref.get[String](Preferences._128K_PLUS2A_0_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2A_0
    }
    val rom_1 = pref.get[String](Preferences._128K_PLUS2A_1_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2A_1
    }
    val rom_2 = pref.get[String](Preferences._128K_PLUS2A_2_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2A_2
    }
    val rom_3 = pref.get[String](Preferences._128K_PLUS2A_3_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS2A_3
    }
    Array(rom_0,rom_1,rom_2,rom_3)
  }

  def get128KPlus3ROMs(implicit pref:Preferences) : Array[Array[Int]] = {
    val rom_0 = pref.get[String](Preferences._128K_PLUS3_0_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS3_0
    }
    val rom_1 = pref.get[String](Preferences._128K_PLUS3_1_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS3_1
    }
    val rom_2 = pref.get[String](Preferences._128K_PLUS3_2_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS3_2
    }
    val rom_3 = pref.get[String](Preferences._128K_PLUS3_3_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        _128K_PLUS3_3
    }
    Array(rom_0,rom_1,rom_2,rom_3)
  }

  def getIFIROM(implicit pref:Preferences) : Array[Int] = {
    pref.get[String](Preferences.IFI_ROM_PREF) match {
      case Some(path) if path.value != "" =>
        Files.readAllBytes(new File(path.value).toPath) map { _.toInt & 0xFF }
      case _ =>
        IFI
    }
  }
}
