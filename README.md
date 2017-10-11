[![GitHub (pre-)release](https://img.shields.io/github/release/msollami/mutils/all.svg)](https://github.com/szhorvat/MaTeX/releases)
[![Github All Releases](https://img.shields.io/github/downloads/msollami/mutils/total.svg)](https://github.com/msollami/mutils/releases)


# mutils

My personal suite of *Mathematica* utilities.

I'll have a blog post for a detailed introduction to mutils and up-to-date troubleshooting information out with the initial release.

## Installation

 - [Download the latest release](https://github.com/msollami/mutils/releases), distributed as a `.paclet` file, and install it using the `PacletInstall` function in Mathematica.  For example, assuming that the file `mutils-0.0.1.pacle` was downloaded into the directory `~/Downloads`, evaluate

	```	mathematica
	Needs["PacletManager`"]
	PacletInstall["~/Downloads/mutils-0.0.1.paclet"]
	```        

- Another way to just load the package without the documentation:

	``` mathematica
	Import @ "https://raw.githubusercontent.com/msollami/mutils/master/mutils.m"
	```

## Releases

#### Version 0.0.1

 - Initial pre-release
	 - 1 Documented Symbol(s): **iMap**

## Feedback

The paclet building scripts are from [szhorvat's MaTeX](https://github.com/szhorvat/MaTeX).

This package was primarily created for my own needs.  However, if you find it useful, feel free to drop me an email.

Send feedback or bug reports to `msollami` at `gmail.com` or [open an issue in the tracker](https://github.com/msollami/mutils/issues).











