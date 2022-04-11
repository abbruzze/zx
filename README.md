[![Build Status](https://app.travis-ci.com/abbruzze/zx.svg?branch=main)](https://app.travis-ci.com/abbruzze/zx)
[![Release](https://img.shields.io/github/v/release/abbruzze/zx)](https://github.com/abbruzze/zx/releases)
[![Language](https://img.shields.io/github/languages/top/abbruzze/zx)]()
[![Downloads](https://img.shields.io/github/downloads/abbruzze/zx/total)](https://github.com/abbruzze/zx/releases/latest)

<p align="center">
  <img src="https://github.com/abbruzze/zx/blob/main/images/zx_big_logo.png">
</p>

ZX Spectrum computers Scala emulator ver 1.0
========

![](https://github.com/abbruzze/zx/blob/main/images/_48k.PNG)![](https://github.com/abbruzze/zx/blob/main/images/_128k.PNG)
![](https://github.com/abbruzze/zx/blob/main/images/arkanoid.PNG)![](https://github.com/abbruzze/zx/blob/main/images/rtype.PNG)

### Emulator spec.
- Z80 accurate emulation (memptr included)
- Contended memory
- Models:
   - Spectrum 16K
   - Spectrum 48K
   - Spectrum 128K
   - Spectrum 128K+
   - Spectrum 128K+2A
   - Spectrum 128K+3
 - ULA
   - Snow effect
   - ULA+ expansion
 - Audio
   - AY 38912 can be configured on 48K
 - Tape
   - TAP and TZX format supported, read and write mode
   - Fast mode
   - Accelerated auto mode
   - Auto play / auto stop
 - Joystick
   - Kempston with keyboard or with USB real joystick
 - Mouse
   - Kempstone
 - Interface I
   - Microdrives
     - MDR format supported, read and write mode
   - RS-232
     - Built-in Internet emulation
 - Memory Expansions
   - LEC
 - Snapshot format
   - Z80, SNA, SCR

### Installation
Go to https://github.com/abbruzze/zx/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (14 or above) in the path and launch in the bin directory:
- On Windows: **zx.bat**
- On Linux: **zx.sh**

### What's new in 1.0 (June 18th 2021)
First release
