[![GitHub (pre-)release](https://img.shields.io/github/release/msollami/mutils/all.svg)](https://github.com/msollami/mutils/releases)
[![Github All Releases](https://img.shields.io/github/downloads/msollami/mutils/total.svg)](https://github.com/msollami/mutils/releases)


# mutils

My personal suite of *Mathematica* utilities. This package was primarily created for my own needs, if however you find it useful, please drop me an email.

* The paclet building scripts are from [szhorvat](http://szhorvat.net/pelican)'s excellent [MaTeX package](https://github.com/szhorvat/MaTeX).
* I'll have a [blog post](http://mikesollami.com) out soon for a detailed introduction to mutils and up-to-date troubleshooting information out with the initial release.

## Installation

1. [Download the latest release](https://github.com/msollami/mutils/releases), distributed as a `.paclet` file,
2. Install it using the `PacletInstall` function in Mathematica:

	```	mathematica
	Needs["PacletManager`"]
	PacletInstall["~/Downloads/mutils-1.0.0.paclet"]
	```        

- Beware loading the package file directly as things may break if installed this way:

	``` mathematica
	Import @ "https://raw.githubusercontent.com/msollami/mutils/master/mutils/mutils.m"
	```

## Testing

Tests are stored in `Tests/tests.wlt`

- To run the package's tests, run `Tests/test_summary.nb`
- To add new tests to the package, add them as testing cells in `Tests/test.nb` and then export to `Tests/tests.wlt` file.

## Feedback

* Send feedback or bug reports to `msollami` at `gmail.com` or [open an issue in the tracker](https://github.com/msollami/mutils/issues).











