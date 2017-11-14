[![GitHub (pre-)release](https://img.shields.io/github/release/msollami/mutils/all.svg)](https://github.com/msollami/mutils/releases)
[![Github All Releases](https://img.shields.io/github/downloads/msollami/mutils/total.svg)](https://github.com/msollami/mutils/releases)


# mutils

My personal suite of *Mathematica* utilities. This package was primarily created for my own needs, if however you find it useful, please drop me an email.

* The paclet building scripts are from szhorvat's excellent [MaTeX package](https://github.com/szhorvat/MaTeX).
* I'll have a [blog post](http://mikesollami.com) out soon for a detailed introduction to mutils and up-to-date troubleshooting information out with the initial release.

## Installation

 - [Download the latest release](https://github.com/msollami/mutils/releases), distributed as a `.paclet` file, and install it using the `PacletInstall` function in Mathematica.  For example, assuming that the file `mutils-0.0.1.paclet` was downloaded into the directory `~/Downloads`, evaluate

	```	mathematica
	Needs["PacletManager`"]
	PacletInstall["~/Downloads/mutils-0.0.1.paclet"]
	```        

- If Paclets bother you, just load the package code without the documentation:

	``` mathematica
	Import @ "https://raw.githubusercontent.com/msollami/mutils/master/mutils/mutils.m"
	```

## Releases

#### Version 0.0.1

 - Initial pre-release
	 - 1 Documented Symbol(s): **iMap**

## Feedback

* Send feedback or bug reports to `msollami` at `gmail.com` or [open an issue in the tracker](https://github.com/msollami/mutils/issues).











