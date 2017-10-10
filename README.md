# mutils

My personal suite of *Mathematica* utilities.

## Installation

Either clone the repo and `Get[]` the package, or just do this:

```
Import@"https://raw.githubusercontent.com/msollami/mutils/master/mutils.m"
```


## Example Usage

There are a ton of useful things in here, someday I will document them!

```
Cases[Names["mutils`*"], s_ /; s === Capitalize[s] && StringFreeQ[s, "$"]]

{"AbsoluteImageDimensions", "Backup", "Bag", "BagPart", \
"CaptionAbove", "ClearDock", "DecryptPath", \
"DownloadImageFileToDisk", "DownloadImageUrlsToDisk", "DropColumn", \
"DynamicUtilitiesOff", "DynamicUtilitiesOn", "EditFile", \
"EncryptPath", "ExportImages", "FinderOpen", "GitStatus", \
"GrangerCausalityTest", "HumanSize", "HumanTime", "InitEdit", \
"InitOpen", "InitPrint", "InitRedirect", "InitWhere", "ListView", \
"LocalNames", "NotebookBackup", "OpenTool", "ParentDir", "PDFtoPNG", \
"PrintMessage", "RandomData", "RemoveDynamic", "Rhymes", \
"RhymesWithXSynonymWithY", "SaveIOCellGroups", "SetDingbat", \
"SetDock", "ShowBarcode", "ShowInFinder", "Size", "Stuff", \
"SubstringQ", "Synonyms", "SystemInfo", "TakeColumn", "This", \
"ToAssociation", "ToList", "ToSequence", "ToThumbnails", "Train", \
"TrainTestSplit", "Warn", "Zoom"}
```