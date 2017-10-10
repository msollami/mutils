(* ::Package:: *)

(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*Begin[]*)


(* Should be in init.m *)
CurrentValue[$FrontEndSession, InitializationCellWarning] = False;


BeginPackage["mutils`", {"GeneralUtilities`", "DatabaseLink`"}];
(* Customizations *)


(* ::Subsection:: *)
(*Paths*)


$textEditorPath = "/Applications/Sublime Text.app/Contents/MacOS/Sublime Text";


(* ::Subsection:: *)
(*Aliases*)


Bag = Internal`Bag;
Stuff = Internal`StuffBag;
BagPart = Internal`BagPart;


(* ::Subsection:: *)
(*Net Aliases*)


(* Neural Nets *)
<<NeuralNetworks`;
netLayers[net_] := NeuralNetworks`PackageScope`GetLayers[net]
netLength[net_] := NeuralNetworks`PackageScope`GetLayers[net] // Length
netPlot[net_] := NeuralNetworks`LayerDepedencyGraph @ net
netVertices[net_] := Keys@Normal[net]["Vertices"]


(* ::Subsection:: *)
(*Shortcuts*)


This::usage = "This[] is a shortcut for evaluation notebook";
Backup::usage = "Backup[] creates a file.bak of the notebook in its directory.";
EditFile::usage = "Opens file in with app specified by $textEditorPath.";
ShowInFinder::usage = "Shows file in finder.";
ParentDir::usage = "Returns the parent directory of a file.";


(* ::Subsection:: *)
(*Initialization*)


InitEdit::usage="initOpen[] shows the file returned by InitWhere[] in the Finder.";
InitWhere::usage="initWhere[] returns the path of you init.m file for the evaluation kernel.";
InitPrint::usage="initPrint[] prints the contents of the file returned by InitWhere[].";
InitOpen::usage="initOpen[] shows the file returned by InitWhere[] in the Finder.";
InitRedirect::usage="initRedirect[newInitFilePath]";


(* ::Subsection:: *)
(*Dynamic*)


SystemInfo::usage = "SystemInfo[] launches a window to montior MMA's activity.";

iDo::usage = "iDo[expr] evaluate expr, but creates a button that allows the argument to be reevaluated at will. The output will be replaced in-place.";
iMap::usage = "iMap[func, list] shows progress of a very slow 'map' across a list until it completes. Pressing abort will not destroy already computed results, all remaining list elements will be $Aborted.";
iList::usage = "iList[list] shows a list with navigation buttons that presents only a partial span of the list at a time, the interpetation of the list is the full list.";
iScan::usage = "iScan[func, list] scan a function across a list, showing a temporary progress indicator and optionally a element-dependant label.";

RemoveDynamic::usage = "Remove occurences of dynamic totally.";
DynamicUtilitiesOn::usage = "Turn on the dynamic functionality in DynamicUtilities.";
DynamicUtilitiesOff::usage = "Turn off the dynamic functionality in DynamicUtilities. Calls will fallback to a sensible static output.";


(* ::Subsection:: *)
(*Git*)


GitStatus::usage="gitStatus[] returns info on the git repository in the current working Directory[].
GitStatus[dir] returns info on the git repository located in dir.";


(* ::Subsection:: *)
(*System Additions*)


NotebookBackup::usage="NotebookBackup[] creates a copy of the current notebook with vXXX appended to the filename.";

FinderOpen::usage="TODO";
ShowInFinder::usage = "TODO";
EditFile::usage = "TODO";

HumanTime::usage = "TODO";
HumanSize::usage = "TODO";
Size::usage = "Size[v] returns user friendly size information on a variable v.";

ListView::usage="TODO";

mmaFind::usage="mmaSearch[\"image_ext*\"] will find paths mma-related files matching e.g.
\"/Applications/Mathematica.app/Contents/SystemFiles/Links/LibraryLink/LibraryResources/Source/image_external.c\"";

LocalNames::usage="Prints a list of LocalSymbols";

Warn::usage="Issue a warning";

ToSequence::usage="Shortcut for Sequence @@ l";

ToAssociation::usage="ToAssociation[f_Function, keys_List] returns <| k->f[k], ...|>
ToAssociation[keys_List, values_List] returns <| k->v, ...|>
ToAssociation[{keys_List, values_List] returns <| k->f[k], ...|>";
toAssn::usage = "Shortcut for ToAssociation";

joinTo::usage = "JoinTo[var, list] sets var to Join[var, list]";
above::usage = "above[] returns the ToExpression of the contents in the cell above the execution cell";
aboveCell::usage = "aboveCell[] returns the fullform of the cell above the execution cell";
filterOpts::usage = "TODO";
deleteSpace::usage = "TODO";
deleteFailed::usage = "TODO";

EncryptPath::usage = "TODO";
DecryptPath::usage = "TODO";


(* ::Subsection:: *)
(*Imaging *)


ToThumbnails::usage = "ToThumbnails[dir1, dir2, s] converts all the images in dir1 to thumbnails of size s in dir2."

ShowBarcode::usage = "ShowBarcode[img, res] where res is the result of BarcodeRecognize[img]";
ExportImages::usage = "ExportImages[path, imgs] exports the images to the path or zip.";
DownloadImageUrlsToDisk::usage = "DownloadImageUrlsToDisk[urls, dir] saves all the images into dir.";
DownloadImageFileToDisk::usage = "DownloadImageFileToDisk[file, dir] saves all the urls in file into dir.";

AbsoluteImageDimensions::usage = "AbsoluteImageDimensions";

cssColor::usage = "TODO";
saveImages::usage = "TODO";
features::usage = "TODO";
showFeatures::usage = "TODO";
importImages::usage = "TODO";
size::usage = "TODO";
downloadTweet::usage = "TODO";
saveImageURL::usage = "saveImageURL[url, path] saves image at url to path, defaults to ~/Downloads";
exif::usage = "TODO";
exportToSVG::usage = "TODO";
exportToRotatingGif::usage = "TODO";
imageInfo::usage = "TODO";
randomImage::usage = "randomImage[] returns an random image from ExampleData[\"TestImage\"]
randomImage[w, h] returns a random image with those dimensions";
lena::usage = "TODO";
toURL::usage = "TODO";


(* ::Subsection:: *)
(*Terminal *)


ls::usage = "ls[dir] gives a list of filenames for files in the directory dir";
pwd::usage = "TODO";
shellCell::usage = "TODO";


(* ::Subsection:: *)
(*GUI*)


Zoom::usage = "SelectiveZoom[graphics] creates a pane wherein click and drag zooming is enabled.";
CaptionAbove::usage = "Execute CaptionAbove[cap] directly below the cell you want to caption with string cap.";

notify::usage = "notify[title, text] creates a growl notification";
onValueChange::usage = "don't use this";
assnBrowser::uages = "";
assnMenu::usage = "assnMenu[func, assn] gives a dynamic popup menu of assn's values and displays fun(chosen key).";


(* ::Subsection:: *)
(*Buttons*)


startStopButton::usage = "TODO";
PrintMessage::usage = "TODO";
qaButton::usage = "TODO";
backupButton::usage = "TODO";
todayButton::usage = "TODO";
quickDingbat::usage = "TODO";
noDingbat::usage = "TODO";
criticalDingbat::usage = "TODO";
getNextDingbat::usage = "TODO";
setDingbat::usage = "TODO";
buttonDock::usage = "TODO";
qaDock::usage = "TODO";
SetDock::usage = "TODO";
ClearDock::usage = "TODO";
SetDingbat::usage = "TODO";


(* ::Subsection:: *)
(*Strings*)


(* String Functions *)
decamelize::usage = "TODO";
parse::usage = "TODO";
characterCount::usage = "TODO";
deDotString::usage = "deDotString helps ";
deNullString::usage = "TODO";
stringHighlight::usage = "TODO";
SubstringQ::usage = "TODO";
mark::usage = "TODO";


(* ::Subsection:: *)
(*Dates & Times*)


(* Time & Date *)
formatSeconds::usage = "TODO";
dateConversionRules::usage = "TODO";
fromUnixDate::usage = "TODO";
dateQ::usage = "TODO";
formatIfDateQ::usage = "TODO";
formatDay::usage = "TODO";
datePlot::usage = "TODO";
formatDate::usage = "TODO";

timer::usage = "TODO";
taskTimer::usage = "TODO";
task::usage = "Task[t, m] creates a pallets for task for m min.";
countdown::usage = "countdown[x] creates a timer in the current notebook for x mins";
clock30::usage = "TODO";
clock40::usage = "TODO";


(* ::Subsection:: *)
(*Machine Learning*)


$MLModelTypes;
TrainTestSplit::usage="TrainTestSplit[data, testRatio] returns {trainingData, testingData}.";
Train::usage="Train[data, testRatio] returns a list of measurments for easch model type."



(* ::Subsection:: *)
(*List & Associations*)


ToList::usage="TODO";
DropColumn::usage="TODO";
TakeColumn::usage="TODO";

part::usage="Operator form for Part[], e.g. part[spec] @ list works"
raggedTranspose::usage="TODO";
dropFrom::usage="TODO";
nSubsets::usage="TODO";
safeTake::usage="TODO";


safeFirst::usage="TODO";
infoBox::usage="TODO";
reverseAssn::usage="TODO";


(* ::Subsection:: *)
(*Formatting & Stats*)


valueChart::usage="Todo";
valueTable::usage="TODO";


gridPartition::usage = "TODO";
fancy::usage = "TODO";
percent::usage = "TODO";
sentence::usage = "TODO";
prettyRule::usage = "format rules ";
intervalFormat::usage = "TODO";


GrangerCausalityTest::usage = "TODO";
table::usage = "TODO";
bars::usage = "TODO";
stats::usage = "TODO";
hist::usage = "TODO";
pie::usage = "TODO";
barStats::usage = "TODO";
histStats::usage = "TODO";
statGrid::usage = "TODO";


(* ::Subsection:: *)
(*Fake Data*)


RandomData::usage = "TODO";


(* ::Subsection:: *)
(*Writing*)


Synonyms::usage = "TODO";
Rhymes::usage = "TODO";
RhymesWithXSynonymWithY::usage = "TODO";


(* ::Section:: *)
(*Definitions*)


Begin["Private`"];


(* ::Subsection:: *)
(*Initialization*)


Backup[f_] := RunProcess[{"cp", f, f<>".bak"}];
EditFile[f_] := StartProcess[{$textEditorPath, f}];
parentDir[f_]:= FileNameSplit/*Most/*FileNameJoin @ f;
ShowInFinder[f_] :=  RunProcess[{"open", parentDir[f]}];
ShowInFinder[d_?DirectoryQ] :=  RunProcess[{"open", d}];
InitWhere[] := FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}];
InitPrint[] := FilePrint @ InitWhere[];
InitEdit[] := EditFile[InitWhere[]];
InitOpen[] := ShowInFinder[parentDir @ InitWhere[]];
InitRedirect[d_?DirectoryQ] := (
	backup[InitWhere[]];
	Export[InitWhere[],StringTemplate["Get[\"`1`\"]"][d], "Text"];
)


(* ::Subsection:: *)
(*Dynamic*)


Clear[SystemInfo];
SystemInfo[] := Dynamic @ Panel@Style[ TextGrid[{{"Memory:", HumanSize @ MemoryInUse[]}}, Frame->All], 20]
SystemInfo["Palette"] := CreatePalette[
	Dynamic @ Panel@Style[ TextGrid[{{"mem", HumanSize @ MemoryInUse[]}}, Frame->All], 20],
	WindowTitle -> "MMA Activity Monitor",
	WindowFloating->True
]


RemoveDynamic[dynamic_] := dynamic //. {Verbatim[Dynamic][x_,___] :> x, Verbatim[Refresh][x_,_] :> x, Verbatim[DynamicModule][args_,body_,___] :> Module[args,body]};

predictTime[begin_, fraction_] :=
	With[{elapsed = AbsoluteTime[] - begin},
		If[fraction == 0 || elapsed < 5, "Unknown",
			HumanTime[elapsed / fraction - elapsed]
		]];

iMap[func_, list_] :=
	DynamicModule[{len=Length[list], fullsize = 0, aborted = False, lastresult, begintime = AbsoluteTime[], n = 0},
		Monitor[Table[If[TrueQ[aborted], $Aborted, lastresult = func[list[[n]]]; fullsize += ByteCount[lastresult]; lastresult], {n, Range[len]}],
			Refresh[
			Grid[List @ {
				ProgressIndicator[n/len],
				StringForm["item: ``/``", n, len],
				"size: " <> HumanSize[fullsize],
				"time: " <> predictTime[begintime, n/len],
				Button["Abort", aborted = True]},
			BaseStyle -> FontFamily -> "Arial", Spacings -> 4, Frame -> All, FrameStyle -> {LightGray, Thick}],
			UpdateInterval -> 0.5, TrackedSymbols -> {}]]][[2]];


SetAttributes[niceButton, HoldAll];
SetAttributes[niceButton2, HoldAll];

ClearAll[iList];

Options[iList] = {"Position" -> Center, "Width" -> 20, "Initial" -> Identity, "Final" -> Identity, "Count" -> True, Appearance -> "Hyperlink"};

iList[list_, OptionsPattern[]] :=
	OptionValue["Final"][OptionValue["Initial"] /@ list]

iList[list_, OptionsPattern[]] :=
	Interpretation[
	DynamicModule[{pos = 1, len = Length[list], max, align, wrapper, count, appear, usearrow, mapper},
		max = OptionValue["Width"];
		wrapper = OptionValue["Final"];
		mapper = OptionValue["Initial"];
		appear = Switch[OptionValue["Appearance"], "Hyperlink", niceButton2, "Button", niceButton, True, niceButton];
		usearrow = OptionValue["Appearance"] != "Hyperlink";
		align = OptionValue["Position"];
		count = TrueQ[OptionValue["Count"]];

		If[max >= len, wrapper[mapper /@ list]];
   		Dynamic[Refresh[wrapper @
   			With[{
   				lbutton = If[pos > 1, {
   					appear[
   						If[count,
   							PrintF[If[usearrow, "\[LeftArrow] %", "prev\[NonBreakingSpace](total\[NonBreakingSpace]%\[NonBreakingSpace]items)"], pos - 1],
   							If[usearrow, "\[LongLeftArrow]", "prev"]],
   						pos = Max[1, pos - max]
   					]},
   				{}],

		 		rbutton = If[pos < len - max, {
					appear[
		 				If[count,
		 					PrintF[If[usearrow, "% \[RightArrow]", "next\[NonBreakingSpace](total\[NonBreakingSpace]%\[NonBreakingSpace]items)"], len - max - pos],
		 					If[usearrow, "\[LongRightArrow]", "next"]],
		 				pos = Min[pos + max, len - max]
		 			]},
		 		{}]
		 		},
		 			With[{
	   					left =  Switch[align, All | Left,  Join[lbutton, rbutton], Center, lbutton, _, {}],
	   					right = Switch[align, All | Right, Join[lbutton, rbutton], Center, rbutton, _, {}]
   					}, Join[
						left,
				 		mapper /@ Take[list, {pos, Min[pos + max,len]}],
				 		right
				 	]
	   			]
	   		], TrackedSymbols :> {pos}]
		]
	], list]

ClearAll[iScan];

ratiostring[num_?NumberQ, max_?NumberQ] :=
	With[{len = Length[IntegerDigits[max]]},
		StringJoin[ToString /@ IntegerDigits[num, 10, len]] <> "/" <> ToString[max]
	];

ratiostring[_, _] := "Unknown";

iScan[func_, list_, title_:"", labelfn_:None, persist_:False] := DynamicModule[
	{index, label, cell, aborted, len, finished, timetaken},
	If[list === {}, Return[{}]];
	finished = False;
	label = "";
	index = -1;
	len = Length[list];
	aborted = False;
	timetaken = 0;
	cell = If[!persist, PrintTemporary, Print] @
			Grid[{{
				ProgressIndicator[Dynamic[If[index <= 0 && len === 0, 1.0, index / Max[len-1,1]], TrackedSymbols :> {finished, index}], ImageSize -> Small],
				Style[title, 13, Background -> LightOrange],
				Dynamic["(" <> ratiostring[index, len] <> ", " <> ToString[Round[timetaken,.01]] <> "s)", TrackedSymbols :> {finished, index}],
				Dynamic[If[ValueQ[label], label, ""], TrackedSymbols :> {label}]
			}}, ColumnSpacings -> 1, RowAlignments -> Center];

	Scan[
		Function[
			index++;
			If[labelfn =!= None, label = labelfn[#]];
			If[aborted, Abort[]];
			timetaken = First @ AbsoluteTiming[func[#]];
		],
		list
	];
	index = len;
	finished = True;
	FinishDynamic[];
	Pause[0.2];
	If[!persist, NotebookDelete[cell]];
]

iScan[func_, list_, title_:"", labelfn_:None, persist_:False] :=
	Module[{index = 0, len, step},
		len = Length[list];
		step = Max[Ceiling[len / 10], 1];
		If[title =!= "", Print[title]];
		Scan[
			Function[
				index++;
				If[labelfn =!= None,
					Print["Processing ", ratiostring[index, len], ": ", labelfn[#]],
					If[Mod[index-1, step] == 0 || index == len, Print["Processing ", ratiostring[index, len]]]
				];
				Print["\ttook ", Round[First @ AbsoluteTiming[func[#];], 0.01], " seconds"];
			],
			list
		];
		Print["Finished processing"];
	]

SetAttributes[iDo, HoldFirst];
iDo[func_] :=
	DynamicModule[{value = func},
		Column[{
			Button["Evaluate", value = func, Appearance -> "Palette"],
			Spacer[5],
			Dynamic[value]
		}]
	];



(* ::Subsection:: *)
(*Git*)


gitStatus[] := Module[{dir, status},
	dir = Directory[];
	Return @ gitStatus[dir]
]

gitStatus[d_ /; DirectoryQ[d]] := Module[
	{status, res},

	SetDirectory[d];
	status = RunProcess[{"/usr/local/bin/git","status"}];
	If[FailedQ[status], Return @ Style["git status check failed", Black]];

	If[status["ExitCode"] != 0,
		Return @ Style["Not a git repo", Gray]
	];
	ResetDirectory[];

	Which[
		StringContainsQ[status["StandardOutput"], "untracked files present"],
			res = Tooltip[Style["\[FilledCircle]", Red],"Untracked files present"],
		StringContainsQ[status["StandardOutput"], "Changes not staged for commit"],
			res =  Tooltip[Style["\[FilledCircle]", Orange],"Unstaged changes"],
		StringContainsQ[status["StandardOutput"], "Your branch is ahead of 'origin/master' by"],
			res = Tooltip[Style["\[FilledCircle]", Blue],"Unpushed commits"],
		StringContainsQ[status["StandardOutput"], "nothing to commit, working directory clean"],
			res = Tooltip[Style["\[FilledCircle]", Hue[0.39,0.86,0.87]],"working directory clean"],
		True,
			res = Style["unknown", Black]
	];

	Return @ res
]



(* ::Subsection:: *)
(*Web Services*)


$PastebinKey="b832b42b8c99b928d8844701db755c47"
pastebinUpload[data_, title_:"MMA example data"] := Module[{url},
	url = Import["http://pastebin.com/api/api_post.php",
		"RequestMethod"->"POST",
		"RequestParameters"->{
			"api_dev_key"->$PastebinKey,
			"api_option"->"paste",
			"api_paste_private"->"0",
			"api_paste_code"->ToString[data],
			"api_paste_name" -> title}];
	Return @ Hyperlink[url]
]


googleSearch[search_String] := SystemOpen["https://www.google.com/#q="<>URLEncode[search]]


(* ::Subsection:: *)
(*System Additions*)


FinderOpen[f_] := Module[{dir = StringReplace[f, "~" -> $HomeDirectory]},
	If[! DirectoryQ @ dir, dir = DirectoryName @ dir]; 
	If[! DirectoryQ @ dir, Beep[];Warn["No dir found:", dir]];
	StartProcess[{"open", dir}]
]


ListView[x_] := Module[{},
	Button[
		MapIndexed[
			StatusArea[
				Framed[#,
					Background -> Dynamic[If[CurrentValue["MouseOver"], $P=#2;
						Hue[Length[#2]/10],White]],
					FrameStyle->If[MatchQ[#2,{___,0}], None, Directive[Thin, Gray]],
				FrameMargins -> 10,
				BaseStyle -> If[#=!=List && MatchQ[#2,{___,0}],
								{Larger, FontFamily->"Avenir"},
								{}],
				RoundingRadius->5],
			#2]&,
		x, {0,Infinity}, Heads->True], Print[$P],
	Appearance->None]
]


mmaFind[fileName_]:= FileNames[fileName,{$InstallationDirectory},Infinity];


LocalNames[] := (SetDirectory[StringTake[$LocalSymbolBase,8;;]]; FileNames[])


Warn[str_, vals___] := Echo[vals, str]


ToSequence[l_List] := Sequence @@ l


 (*!
  \function toAssn

  \calltable
		toAssn[l_] '' Given an list of pairs, convert to an Association

  \beginmoreinfo
  This code was taken from RecognizerTestingTools.m.
  \end
  *)

ToAssociation[f_Function, k_List] := Association[Rule @@@ Transpose[{k, f/@k}]];
ToAssociation[k_List, v_List] /; Length[k]==Length[v] := Association[Rule @@@ Transpose[{k,v}]];
ToAssociation[list : {k_List, v_List}] := Association[Rule @@@ Transpose[list]];
ToAssociation[list:{{_, _} ..}] := Association[Rule @@@ list];
ToAssociation[list:{Rule__}] := Association[list];
toAssn = ToAssociation;

(*
 toAssn[f_Function, list_List] := Association[(#->f[#])& /@ list]
toAssn[header_List, list_List] := (Association@Thread@(header->#))&/@list /;
(Length[header] == Length[list[[1]]])



  toAssn[list1_, list2_] := Association@Thread[list1->list2] /; Dimensions[list1]==Dimensions[list2]
    toAssn[list_] := toAssn[list[[1]], Rest[list]]
    toAssn[k_, f_, list_] := Association[(k[#]->f[#])& /@ list]
*)


above[] := With[{pc=NotebookRead[PreviousCell[]]}, ToExpression[pc[[1]]]]
aboveCell[] := With[{pc=NotebookRead[PreviousCell[]]}, FullForm[pc]]

Size[img_Image]:=ImageDimensions[img];
Size[l_List] /; Depth[l] == 2 := Length[l];
Size[l_List] /; Depth[l] > 2 := Dimensions[l];
Size[a_Association] := Length[a];
Size[s_String] := StringLength[s];


SetAttributes[joinTo, HoldFirst]
joinTo[a_, b_List] := (ReleaseHold[a = Join[a, b]]);

filterOpts[symb_, opts___] := Sequence@@FilterRules[{opts}, Options@symb];


deleteSpace[s_String] := StringReplace[s," "->""]


safeFirst[list_] := Null /; !ListQ[list];
safeFirst[{}] := Null;
safeFirst[list_List] := list[[1]];


deleteFailed[e_] := DeleteCases[e,$Failed,{0,\[Infinity]}];


setVertexCoordinates[g_Graph, a_Association] := SetProperty[g, VertexCoordinates -> Values[a]];
vertexCoordinates[g_Graph] := toAssn[PropertyValue[{g, #}, VertexCoordinates] &, VertexList[g]]


EncryptPath[path_, pass_, encryptedFileName_:"~/enc.txt"] := Module[
	{tmp = "~/enc_temp.zip"},
	CreateArchive[path, tmp];
	enc = Encrypt[pass, ToExpression @ Import[tmp, "Byte"]];
	DeleteFile[tmp];
	Export[encryptedFileName, Compress @ ToString @ InputForm @ enc]
]

DecryptPath[path_, pass_, decryptedFileName_:"~/decrypted.zip"] := Module[{enc},
	enc = ToExpression @ Uncompress @ Import[path];
	Export[decryptedFileName, Decrypt[pass, enc], "Byte"]
]


(* ::Subsection:: *)
(*Imaging*)


cssColor[col_] :=
 StringJoin["#",
  IntegerString[Round[255 (List @@ ColorConvert[col, "RGB"])], 16, 2]]


ToThumbnails[fromDir_, thumbDir_, size_] := Module[{files},
  files = FileNames["*", fromDir];
  Echo[Length@files];
  ParallelMap[
   i \[Function]
    Export[thumbDir <> FileNameTake[i], Thumbnail[Import[i], size]],
   files]
  ]


AbsoluteImageDimensions[img_]:=Module[{m=CurrentValue[Magnification]},
	Cases[{ToBoxes@img},GraphicsBox[___,OrderlessPatternSequence[Verbatim[Rule][ImageSizeRaw,{w0_,h0_}],Verbatim[Rule][PlotRange,_], Verbatim[Rule][ImageSize,{w_,_}]]]:>{m {w,w*h0/w0},100 (m w/w0)^2},\[Infinity]]
]


ShowBarcode[img_Image] := ShowBarcode[img, BarcodeRecognize[img, {"BoundingBox", "Data", "Format"}]]
ShowBarcode[img_Image, res_] := Show[
	SetAlphaChannel[#[[1]], 0.4],
		Graphics[{
			EdgeForm[{Thick, Purple}], FaceForm[], Rectangle @@ #[[2]],
			Inset[Style[#[[3]] <> "\n" <> If[ListQ[#[[4]]], First[#[[4]]], #[[4]]], Bold, Purple, FontSize -> 16], RegionCentroid[Rectangle @@ #[[2]]]]}], ImageSize -> 300] &[Prepend[res, img]]

Clear[ExportImages];
ExportImages[path_, iimgs_List, filenames_:None] := Module[
	{fn, imgs},
	imgs = DeleteCases[iimgs, x_ /; !ImageQ[x]];
	fn = If[filenames===None, fn=Table[ToString[i]<>".jpg",{i,Length[imgs]}],filenames];
	If[StringTake[path,-4;;]==".zip",
		Export[path, Thread[fn->imgs], "Rules"]
	,
		If[!DirectoryQ[path], CreateDirectory[path]];
		SetDirectory[path];
		Scan[Export@@#&, Thread[{fn,imgs}]]
	]
];


Clear[imageUrlBaseName];
imageUrlBaseName[u_] :=
 With[{s =
    SafeLast@
     StringCases[u,
      "/" ~~ b :
         Shortest[(w : (WordCharacter ..) ~~ "_t"|"_hr" ...) ~~
           "." ~~ ("jpg" | "png" | "jpeg" | "gif")] :> b]},

  If[StringTake[s, -6 ;; -5] == "_t", StringDrop[s, -6 ;; -5], s]]

DownloadImageFileToDisk[file_, targetDirectory_, kernels_:1] := Module[
	{urls = Import[file, "List"]},
	DownloadImageUrlsToDisk[urls, targetDirectory, kernels]
]


DownloadImageUrlsToDisk[urls_List, kernels_Integer:8] := Module[
	{dir, task, remaining, progressQueue, urlFunction, startDownload},

    dir = "~/Downloads/"<>StringReplace["~/Downloads/"<>DateString[]," "|":"->"_"];
	CreateDirectory[dir];

	remaining = urls;
	progressQueue = Association[];

	urlFunction[task_, "progress", {dlnow_, dltotal_, _, _}] := (progressQueue[task] =
   dlnow/dltotal);

	urlFunction[task_, "data", _] := (progressQueue[task] = 1; startDownload[]);

	urlFunction[task_, "error", e_] := (
		Print["Error downloading ", First[task], ": ", e];
		urlFunction[task, "data", {}]
	);

    step2[] := Once[
        << MXNetLink`;
        fns=FileNames["*",dir];
        Global`$imgs = MXNetLink`FastImageImport/@fns;
    ]

	startDownload[] := (If[Length[remaining] == 0, step2[];Return[]];
		Module[{u = First[remaining], suffix, fname},
			remaining = Rest[remaining];
			(* suffix = SafeFirst@StringCases[u,".png"|".jpg"|".gif"|".jpeg"] /. {} -> ".jpg";
			If[MissingQ[suffix], suffix = ".jpg"]; *)
			(*fname = SafeLast@StringCases[u, WordCharacter ... ~~ "."~~ ("jpg"|"png"|"jpeg"|"gif")];*)
			(* fname = CreateUUID["image-"]<>suffix; *)
			fname = FileNameTake[u];
			URLSaveAsynchronous[u, FileNameJoin[{dir,fname}],
			urlFunction, "Progress" -> True]]);

	Do[startDownload[], {kernels}];
	(*Dynamic[Length[remaining]]*)
	Dynamic[ProgressIndicator[Total[Values[progressQueue]]/Length[urls]]]

]



DownloadImageUrlsToDisk[urls_List, idir_String:None, kernels_Integer:16] := Module[
	{dir=idir, task, remaining, progressQueue, urlFunction, startDownload},
	If[dir == None,
		dir = "~/Downloads/"<>StringReplace["~/Downloads/"<>DateString[]," "|":"->"_"];
		CreateDirectory[dir];
	];

	remaining = urls;
	progressQueue = Association[];

	urlFunction[task_, "progress", {dlnow_, dltotal_, _, _}] := (progressQueue[task] =
   dlnow/dltotal);

	urlFunction[task_, "data", _] := (progressQueue[task] = 1; startDownload[]);

	urlFunction[task_, "error", e_] := (
		Print["Error downloading ", First[task], ": ", e];
		urlFunction[task, "data", {}]
	);

	startDownload[] := (If[Length[remaining] == 0, Return[]];
		Module[{u = First[remaining], suffix, fname},
			remaining = Rest[remaining];
			(* suffix = SafeFirst@StringCases[u,".png"|".jpg"|".gif"|".jpeg"] /. {} -> ".jpg";
			If[MissingQ[suffix], suffix = ".jpg"]; *)
			(*fname = SafeLast@StringCases[u, WordCharacter ... ~~ "."~~ ("jpg"|"png"|"jpeg"|"gif")];*)
			(* fname = CreateUUID["image-"]<>suffix; *)
			fname = FileNameTake[u];
			URLSaveAsynchronous[u, FileNameJoin[{dir,fname}],
			urlFunction, "Progress" -> True]]);

	Do[startDownload[], {kernels}];

	Dynamic[Length[remaining]]
	(*Dynamic[ProgressIndicator[Total[Values[progressQueue]]/Length[urls]]]*)
]


saveImages[urls_List] := Module[{m, dir},
    m = Max[ToExpression /@ StringCases[FileNames["/Users/mike/Downloads/temp-*"], ("temp-" ~~ (ns :DigitCharacter ..)) :> ns]] + 1;
	dir = CreateDirectory["/Users/mike/Downloads/temp-"<>ToString[m]];
	saveImageURL[#, dir] & /@ urls
]


Options[features] = Options[ImageKeypoints];
features[img_Image,opts:OptionsPattern[]] := Module[{key},
	key = ImageKeypoints[img, {"Position","Descriptor"}, opts, MaxFeatures -> 100];
	Transpose @ key
]


Options[showFeatures] = Options[ImageKeypoints];
showFeatures[img_,opts:OptionsPattern[]] := Module[{points},
  points = ImageKeypoints[img, {"Position", "Scale", "Orientation", "ContrastSign"}, opts, MaxFeatures -> 100];
  Return @ Show[img, Graphics[Table[{{If[p[[4]] == 1, Yellow, Red], Circle[p[[1]], p[[2]]*2.5], Line[{p[[1]], p[[1]] + p[[2]]*2.5*{Cos[p[[3]]], Sin[p[[3]]]}}]}}, {p, points}]]]
  ]


lena[] := ExampleData[{"TestImage","Lena"}];

saveImageURL[url_, path_:"~/Downloads/"] := Module[{img, imgName},
	imgName = FileBaseName[url]<>"."<>FileExtension[url];
	Return @ URLSave[url, FileNameJoin[{path, imgName}]]
]

saveImageURL[url_, path_:"~/Downloads/", basename_] := Module[{img, imgName},
	imgName = ToString[basename]<>"."<>FileExtension[url];
	Return @ URLSave[url, FileNameJoin[{path, imgName}]]
]


(*Table[saveImageURL[u,"/Users/mike/Downloads/recent"], {u,urls}]*)

importImages[dir_String] := Module[{imageNames,imgs},
	SetDirectory[dir];
	imageNames = FileNames[{"*.jpg","*.png","*.gif"}];
	imgs = Import /@ imageNames;
	ResetDirectory[];
	Return @ imgs
]/; DirectoryQ[dir]

importImages[urls_List] := Module[{imageNames,imgs},
	Return @ Table[Import[u], {u,urls}]
]


downloadTweet[id_] := Module[
	{tweetid = ToString@id, s=ServiceConnect["Twitter"], tweet, link, manylinks,anotherlink,xmldata,finallink},
	Check[
		tweet = ServiceExecute[s,"GetTweet","TweetID"->tweetid],
		Return @ $Failed
	];
	link = Last @ Flatten @ StringCases[StringSplit[tweet], "http"~~__];
	manylinks = Import[link,"Hyperlinks"];
	anotherlink = First @ Flatten @ StringCases[manylinks, __~~"photo"~~__];
	xmldata = Import[anotherlink, "XMLObject"];
	finallink = First @ Cases[xmldata,
		XMLElement["meta",{"property"->"og:image","content"->address_},{}] :> address, -1];
	Return @ Import[finallink]
]


toURL = toUrl;
toUrl[image_] := With[{c = CloudExport[image, "PNG",Permissions->"Public"]}, First @ c];





Clear[imageInfo,exif];
exif[img_]:= Where[e=("Exif"/.(MetaInformation/.Options[img,MetaInformation])), If[e==="Exif",None,OpenerView["Click for data",infoBox["Exif", Association@@e]]]]

imageInfo[img_Image]:=infoBox["Image Properties",Association["Size"->ImageDimensions[img],"Colorspace"->ImageColorSpace[img],
"Channels"->ImageChannels[img],"Aspect"->ImageAspectRatio[img],"Interleaving"->(Interleaving/.Options[img,Interleaving]),
"DominantColors"->Row[DominantColors[img], "|"],"Histogram"->ImageHistogram[img,Appearance->"Transparent"]]]


(* ::Subsection:: *)
(*Terminal*)


This[]:=EvaluationNotebook[];
pwd[]:=Directory[];

Options[ls] = {"IncludeHidden"->False, "FullPaths"->True};
ls[idir_:None, opts:OptionsPattern[]]:= Quiet@Module[{files, dir=idir},
	If[dir === None && NotebookDirectory[] == $Failed,
		Warn["Notebook not saved"]; Return[None]
	];
	dir = If[dir === None, NotebookDirectory[], idir];
	SetDirectory[dir];
	files = FileNames[];
	If[!OptionValue["IncludeHidden"],
		files = DeleteCases[files, x_ /; StringMatchQ[x, "."~~___]]
	];
	ResetDirectory[];
	Return[files]
];


 outlinedExport[name_, gr_, opts : OptionsPattern[]] :=
 Export[name,
        First@ImportString[ExportString[gr, "PDF"], "PDF",
                           "TextMode" -> "Outlines"], opts];


evaluatableCell[label_String,evaluationFunction_]:=(CellPrint[TextCell["","Program",Evaluatable->True,CellEvaluationFunction->evaluationFunction,CellFrameLabels->{{None,label},{None,None}},CellGroupingRules->"InputGrouping"]];SelectionMove[EvaluationNotebook[],All,EvaluationCell];NotebookDelete[];SelectionMove[EvaluationNotebook[],Next,CellContents])


Options[runTempFile]={FilePattern->"_",FileExtension->Automatic};
runTempFile[command_,text_,OptionsPattern[]]:=Module[{stream,file,result},stream=OpenWrite[];Export[stream,text,"Text"];file=Close[stream];OptionValue[FileExtension]/.ext:Except[Automatic]:>(file=RenameFile[file,file~~"."~~ext]);result=Import[StringReplace[command,OptionValue[FilePattern]->file],"Text"];DeleteFile[file];result]


shellEvaluate[cmd_]/;$OperatingSystem=="Windows":=runTempFile["!_ 2>&1","@echo off\n"~~cmd,FileExtension->"cmd"]


shellEvaluate[cmd_]:=Import["!"~~cmd~~" 2>&1","Text"]


shellCell[]:=evaluatableCell["Shell",shellEvaluate[#]&]


exportToRotatingGif[graphics_Graphics3D, frames_:50] := Module[{t},
	Table[Export[StringJoin["source",ToString[i],".jpg"],Graphics3D[Rotate[graphics[[1]],2\[Pi]*(i/frames),{0,0,1}],Boxed->False,ViewAngle->18\[Degree],AspectRatio->1],ImageSize->500],{i,1,frames}];
	t = Table[Import[StringJoin["source",ToString[i],".jpg"]],{i,1,frames}];
	Export[StringJoin["anim (",ToString[frames]," frames).gif"],t,"DisplayDurations"->0.08]
]


(* ::Subsection:: *)
(*Gui*)


Zoom[g_Graphics] := Module[
	{graph = First[g],
	options = DeleteCases[Options[g], PlotRange -> _],
	plotrange = PlotRange /. Options[g, PlotRange],
	rect = {Thick, Dashing[Small],
		Line[{#1, {First[#2], Last[#1]}, #2, {First[#1], Last[#2]}, #1}]} &},
	DynamicModule[{drag = False, first, second, range = plotrange},
	Panel@EventHandler[
		Dynamic @ Graphics[
			If[drag, {graph, rect[first, second]}, graph],
			PlotRange -> range,
			Sequence @@ options
		],
		{{"MouseDown", 1} :> (first = MousePosition["Graphics"]),
		{"MouseDragged",1} :> (drag = True; second = MousePosition["Graphics"]), {"MouseUp", 1} :>
			If[drag, drag = False;
				range = Transpose@{first, second}, range = plotrange]}]]];

mc[l_, pt_, m_, r_] :=
	l /. {x_?((Head[#] =!= List) &), y_?((Head[#] =!= List) &)} :>
		({x, y} + m/2 ({x, y} - pt) 2^(-Norm[{x, y} - pt]/r));

MagnifyingGlass[g_, None, m_, r_] := g;

MagnifyingGlass[g_, pt_, m_, r_] := g /. {
	GraphicsComplex[l_, c__] :> GraphicsComplex[mc[l, pt, m, r], c],
	Line[l_] :> Line[mc[l, pt, m, r]],
	Polygon[l_] :> Polygon[mc[l, pt, m, r]],
	Point[l_] :> Point[mc[l, pt, m, r]],
	Text[e_, l_, rest_] :> Text[e, mc[l, pt, m, r], rest],
	Rectangle[p1_, p2_, rest___] :> Rectangle[mc[p1, pt, m, r], mc[p2, pt, m, r], rest],
	Circle[l_, rest___] :> Circle[mc[l, pt, m, r], rest],
	Disk[l_, rest___] :> Disk[mc[l, pt, m, r], rest]
};


CaptionAbove[captionString_]:=Module[{cellFrameLabelFunc, alignFraction, lbl},
	cellFrameLabelFunc[lbl_,align_: 0,color_: White] := PanelBox[lbl, ImageSize->{Scaled[1],Automatic},Alignment->{align,Center},Appearance->"Frameless",Background->color];
	alignFraction=-1;
	lbl=Style[captionString,Editable->True,10,Italic,FontFamily->"Constantia"]//ToBoxes//StyleBox[#,FontFamily->"Menlo",FontSize->14,FontColor->GrayLevel[0]]&//cellFrameLabelFunc[#,alignFraction]&;
	SelectionMove[EvaluationNotebook[],Previous,Cell,2];
	SetOptions[NotebookSelection[],CellFrameLabels->{{None,None},{lbl,None}}]
]


assnBrowser[assn_] := With[{bn=assn},DynamicModule[{input="", res={}, y="tench"},
	res = Sort@Cases[Values@bn, b_String /; StringMatchQ[b, ___ ~~ y ~~ ___, IgnoreCase->True]];
	Panel@Column[{
		InputField[Dynamic[y], String, ContinuousAction->True],
		Dynamic[
			res = Sort@Cases[Values@bn, b_String /; StringMatchQ[b, ___ ~~ y ~~ ___, IgnoreCase->True]];
			Panel @ Pane[
				Multicolumn[Style[Framed[#,RoundingRadius->5],LineBreakWithin->False]&/@res,{14,Automatic}], ImageSize->{560,400}
				,Scrollbars->{True,False}, AppearanceElements->All]
		], Dynamic[StringTemplate["`1` of `2` tags"][Length@res, Length@bn], TrackedSymbols:>{res}]
	}]
]]


notify[txt_: "ping", subtext_: ""] :=
    Module[{process, cmd}, process = StartProcess["/bin/zsh"];
           cmd = "growlnotify -m '" <> subtext <> "'" <>
           " -a \"Mathematica\"" <> " -n \"" <> txt <> "\"";
           WriteLine[process, cmd];
           Pause[.1];
           KillProcess[process]
]


onValueChange[a_,f_] := Module[{},
	a/:Set[a,x_]:=((OwnValues[a]={HoldPattern[a]:>x}; f[x]);a)
]


assnMenu[slowF_, a_Association, title_: "Choose"] :=
 DynamicModule[{done = False, lastSelection = ""},
  Grid[{{
     ActionMenu[title,
      KeyValueMap[#2 :> (lastSelection = #2; done = "working";
          slowF[#1]; done = True) &, a], Method -> "Queued"],
     Dynamic@Style[lastSelection, FontFamily -> "Arial Black", FontSize -> 14]},
    {Dynamic@Switch[done, False, "",
       True, "done",
       "working", ProgressIndicator[Appearance -> "Percolate"]],
     SpanFromLeft}}]]

(*
(*Old*)
assnMenu[f_,a_Association, title_:"Choose"] := Module[
	{am,b,r,g},
	blup=Style[Pane[Magnify[ProgressIndicator[Appearance->"Percolate"],2]
		,Alignment->Center],Background->Transparent];
	r=blup;
	g:=(r=blup;r=f[#])&;
	onValueChange[b, g];
	am = Table[With[{i=i}, a[i]:>(b=i)], {i, Keys@a}];
	b = First @ Keys[a];
	Panel @ Grid[
		{{ActionMenu[title, am, Method->"Queued"],
			Dynamic[Style[a[b],FontFamily->"Arial Black",FontSize->14],UpdateInterval->1]},
		{Dynamic[r, TrackedSymbols:>{r}],SpanFromLeft}},
		Alignment->{Center,Center}
	]
	(*DynamicEvaluationTimeout\[Rule]10*)
]*)


$playView=ToExpression@GraphicsBox[
    TagBox[{
      {GrayLevel[0.9], RectangleBox[{5, -158}, {29, -135}]},
      {RGBColor[0.26,0.68,0.46],
       PolygonBox[NCache[{{13, -153}, {13, -140}, {21, Rational[-293, 2]}, {13, -153}}, {{13, -153}, {13, -140}, {21, -146.5}, {13, -153}}]]},
      {GrayLevel[0.5],
       StyleBox[LineBox[{{5, -158}, {5, -135}, {29, -135}, {29, -158}, {5, -158}}],
        Antialiasing->False]}},
     Null],
    ImageSize->{29.4921875, Automatic}];

$stopView=ToExpression@GraphicsBox[
     TagBox[{
       {GrayLevel[0.9], RectangleBox[{34, -158}, {58, -135}]},
       {RGBColor[0.81,0.33,0.26], RectangleBox[{42, -150}, {50, -143}]},
       {GrayLevel[0.5],
        StyleBox[LineBox[{{34, -158}, {34, -135}, {58, -135}, {58, -158}, {34, -158}}],
         Antialiasing->False]}},
      Null]];

Attributes[cycleState]={HoldFirst};
cycleState[s_/;s>0,len_:2] := s = If[s>=len,1,s+1]

SetAttributes[startStopButton,HoldAllComplete];
startStopButton[startAction_, stopAction_] := DynamicModule[
	{views={$playView, $stopView},state=1},
	Button[Dynamic[views[[state]]], Switch[state,1,startAction,2,stopAction];state=cycleState[state];,Appearance->"Frameless"]
]


PrintMessage[expr__] := NotebookWrite[MessagesNotebook[],Cell[RawBoxes@ToBoxes[expr,StandardForm],"Output"]]


(*http://mathematica.stackexchange.com/questions/20496/how-can-i-change-the-color-of-the-bar-in-a-progressindicator/20498#20498*)
(*toRed[{c_,c_,c_}]:={c,c,c}
toRed[col:{r_,g_,b_}] := 256 List@@Blend[{Blue,GrayLevel[Mean@col/256]}]
myPIList=(Rasterize[ProgressIndicator@#]/.col:{r_?NumberQ,g_?NumberQ,b_?NumberQ}:>toRed@col)&/@Range[0,1,0.01];
myProgressIndicator[x_,r:{a_,b_}:{0,1},opts:OptionsPattern[]]:=myPIList[[Round@Rescale[If[NumericQ@x,x,0],r,{1,101}]]]*)


noDingbat = Cell[BoxData[StyleBox["",FontFamily->"Avenir",FontSize->12,FontColor->Hue[0.66,0.81,0.83]]],"Text"];
quickDingbat = Cell[BoxData[StyleBox["EASY",FontFamily->"Avenir",FontSize->12,FontColor->Hue[0.57,1,1]]],"Text"];
criticalDingbat = Cell[BoxData[StyleBox["MUST",FontFamily->"Avenir",FontSize->12,FontColor->Red]],"Text"];


getNextDingbat[CellDingbat] = quickDingbat;
getNextDingbat[d_] := Which[d==quickDingbat,
	Return @ criticalDingbat, d==criticalDingbat, Return @ noDingbat, d==noDingbat, Return @ quickDingbat
]


setDingbat[icell_, db_]:=Module[{cell=icell},
	cell=DeleteCases[cell,Verbatim[Rule][CellDingbat,Blank[]],{0,Infinity}];
	cell=Insert[cell,Rule[CellDingbat,db],-1];
	Return @ cell
]


(* ::Subsection:: *)
(*Custom Buttons*)


niceButton[label_, action_] := Deploy @ Button[label, action, Appearance -> "Palette", BaseStyle -> 10, ImageSize -> {Automatic, Automatic}];
niceButton2[label_, action_] := Deploy @ Button[label, action, Appearance -> "None", BaseStyle -> {Underlined, Blue, 10}, ImageSize -> {Automatic, Automatic}];

(* Usage: SetDingbat[PreviousCell[],$QAButton]*)
Clear@qaButton;
qaButton[] := 
	Button[Style[Text@"\!\(\*AdjustmentBox[\(+\),\nBoxBaselineShift->-0.15827792812378325`,\nBoxMargins->{{0., 0.}, {0.15827792812378325`, -0.15827792812378325`}}]\)",20, FontColor->Blue], 
	SelectionMove[EvaluationCell[], All, Cell];
	SelectionMove[EvaluationNotebook[], After, Cell];
	NotebookWrite[EvaluationNotebook[], {Cell[CellGroupData[{
        Cell["", "ItemNumbered", "WholeCellGroupOpener"->True],
        Cell["", "Program"]
    }]]}]; SelectionMove[EvaluationNotebook[],Previous,CellContents,2],
    Appearance->"Palette", ImageSize->{28,28},ImageMargins->3, Background-> LightBlue,
    Alignment->{Center,Center}, Evaluator->Automatic, Method->"Preemptive"]
    
todayButton[] := Button["Today", NotebookWrite[EvaluationNotebook[], {Cell[CellGroupData[{Cell[DateString[{"DayName", " - ", "MonthNameShort", " ",
        "DayShort"}], "Section", "WholeCellGroupOpener" -> True],
        CellGroupData[{Cell["", "Item"]}]}]]}]; SelectionMove[EvaluationNotebook[],Previous,CellContents,1]];

Clear@NotebookBackup;
NotebookBackup[nb_:None] := Module[{path=nb, dir, base, ext, ds, newPath},
	If[path==None, path = NotebookFileName[]];
	dir = FileNameJoin[{DirectoryName@path,"backups"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	base = FileBaseName@path;
	ext= FileExtension@path;
	ds = DateString[{"(","Year",".","MonthShort",".","DayShort"," at ","Hour12Short",".","MinuteShort",".", "SecondShort"," ","AMPMLowerCase", ")"}];
	newPath = FileNameJoin[{dir,base<>" "<>ds<>"."<>ext}];
	(*Warn["Saved a copy this notebook as ", Hyperlink @ newPath];*)
	CopyFile[path, newPath];
];
Clear@StartPeriodicBackup;
StartPeriodicBackup[interval_:60] := Module[{task, n=NotebookFileName@EvaluationNotebook[]},
	task = CreateScheduledTask[If[MemberQ[Quiet[NotebookFileName /@ Notebooks[]], n], NotebookBackup[n], 
		RemoveScheduledTask[$BackupTasks[n]]], interval];
	If[!ValueQ @ $BackupTasks, $BackupTasks=<||>];
	$BackupTasks[n] = task;
	StartScheduledTask @ task
]
Clear@StopPeriodicBackup
StopPeriodicBackup[]:=If[ValueQ@$BackupTasks, RemoveScheduledTask/@$BackupTasks; $BackupTasks=<||>];
        
backupButton[nb_:None] := Button["Backup", NotebookBackup[nb]]




buttonDock[buttons_] := SetOptions[EvaluationNotebook[], DockedCells -> Cell[RawBoxes[MakeBoxes @ buttons]]];
buttonDock[buttons_List] := SetOptions[EvaluationNotebook[], DockedCells -> Cell[RawBoxes[MakeBoxes @ Row[buttons]]]];
ClearDock[] := SetOptions[EvaluationNotebook[], "DockedCells"->None];

SetDock["Backup"] := buttonDock[backupButton[]]

Clear@SetDingbat;
SetDingbat[cell_, ding_] := SetOptions[cell, CellDingbat -> ToBoxes[ding]]



(* ::Subsection:: *)
(*Strings*)


decamelize = StringUtilities`Private`CamelBreak;


SetAttributes[dropFrom,{HoldFirst}];
SubstringQ[str_, sub___] := StringFreeQ[str,sub];


(*mark[s_, where_] := With[{n = ToString@s},
                     Row@{StringDrop[n, -where], Style[StringTake[n, -where], Red]}];
mark[s_, w_] := With[{n = ToString@s},
                  Row@MapAt[Style[#, Bold, Red] &, Characters[n], {w}]];*)
mark[number_, spec : {{_, _} ..}] := With[{n = Characters@ToString@number},
   Fold[
    Function[{x, y}, MapAt[Style[#, y[[ 1]]] &, x, y[[ 2]]]],
    n,
    spec]] // Row
stringHighlight[string_, cases_] := Module[{pos, agg, res},
   pos = StringPosition[string, cases];
   agg = {Switch[
		EchoAs[#[[1, 2]]][#[[1, 2]]], 1, Blue, 2, Brown, _, Red],
		#[[;;, {1}]]
         } & /@ GatherBy[Tally@Flatten[Range @@@ pos], Last];
   mark[string, Echo@agg]
 ]


parse[x_String] := ImportString[x,"Lines"];



deDotString[str_String] := Last[StringSplit[str,"."]];
deDotString[other_] := other;
Attributes[deDotString] = {Listable};



SetAttributes[characterCount, HoldAllComplete];
characterCount[expr_] := (Print[StringLength@deleteSpace@ToString@InputForm@HoldForm[expr]-10];expr)


(* ::Subsection:: *)
(*Dates & Times*)


dateQ[d_List] := Length[d]==6;
dateQ[d_] := False;
formatDate[date_] := DateString[date,{"Month","/","YearShort"," ","Hour24",":","Minute"}];
formatIfDateQ[d_] := If[dateQ[d], formatDate[d], d];
formatDay[date_] := DateString[date,{"Month","/","YearShort"}];


Clear[datePlot];
datePlot[input_] := DateListPlot[Thread@{input,0},AspectRatio->1/12,Joined->False]
datePlots[input_] := DateListPlot[Thread@{#,0}&/@input,AspectRatio->1/12,Joined->False]


Options[formatSeconds] = {"FixedWidth"->True};
formatSeconds[secconds_, opts:OptionsPattern[]] := Module[
	{timeUnits = UnitConvert[Quantity[secconds, "Seconds"], MixedRadix["Hours", "Minutes", "Seconds"]],
	hours,mins,secs},

	hours = timeUnits[[1,1]];
	mins = timeUnits[[1,2]];
	secs = Round@timeUnits[[1,3]];

	Return @ iFormatSeconds[hours, mins, secs]
]

Options[iFormatSeconds] = Options[formatSeconds];
iFormatSeconds[hours_, mins_, secs_, OptionsPattern[]] := Module[
	{hf,mf,sf},

	hf = IntegerString[hours, 10, 2];
	mf = IntegerString[mins, 10, 2];
	sf = IntegerString[secs, 10, 2];

	Return @ Style[StringRiffle[{hf, mf, sf},":"], FontFamily->"Digital-7", FontSize->24]
]


Options[timer] = {"Autostart"->True, "HideButton"->True, "ProgressBar"->False,
	"Sparkline"->False};

timer[{title_String, mins_Integer}, opts:OptionsPattern[]] := DynamicModule[
	{max=mins*60, task=mins*60, lt, widgets = {}, grid={}},

	AppendTo[widgets,
		Item[Style[title, FontFamily->"Constantia", FontSlant->Italic,
			FontWeight->Bold, FontSize->16], Alignment->Center]
	];

	If[OptionValue["Sparkline"],
		AppendTo[widgets,
			Item[Magnify[Dynamic[AngularGauge[task, {0,max}, ImageSize->40,
				PlotTheme->"Marketing",ScaleOrigin->{6Pi/5,-Pi/5}]], 1.4],
			Alignment->Center]
		]
	];

	If[!OptionValue["HideButton"],
		AppendTo[widgets, Item[startStopButton[StartScheduledTask[lt],
			StopScheduledTask[lt]],Alignment->Center]]];

	AppendTo[widgets, Dynamic[If[task==0,RemoveScheduledTask[lt]];
		Style[formatSeconds[task], FontFamily -> "Gridnik", FontSize->17],
		TrackedSymbols:>{task}, Initialization:>(lt=CreateScheduledTask[task--,1];
		If[OptionValue["Autostart"], StartScheduledTask[lt]];)
	]];

	grid = {widgets};
	If[OptionValue["ProgressBar"],
		AppendTo[grid, {Dynamic[ProgressIndicator[task, {0,max},
			ImageSize -> 240, Appearance->"Percolate"]], SpanFromLeft}
		]
	];

	Framed[
		Grid[grid,
			Alignment -> {Center, Bottom},
			(*ImageSize \[Rule] {500}, *)
			BaselinePosition -> Center],
		FrameStyle -> GrayLevel[0.64],
		RoundingRadius -> 5]
]


clock40[] := Dynamic[{clock`d, clock`h, clock`m, clock`s} =
  List @@ DateDifference[Now,
     DateObject[{2015, 5, 18}], {"Day", "Hour", "Minute",
      "Second"}][[1]];
	Grid[Transpose@{{"d", clock`d}, {"h", clock`h}, {"m", clock`m}, {"s",
      Round@clock`s}}, Alignment -> Left], UpdateInterval -> 1];

clock30[] := Dynamic[{d,h,m,s}=List@@DateDifference[Now,DateObject[{2015,6,18}],{"Day","Hour","Minute","Second"}][[1]];
Grid[Transpose @ {{"d",d},{"h",h},{"m",m},{"s",Round@s}},Alignment->Left],UpdateInterval->1]


taskTimer[tasks_]:= Panel@Column[timer@@@tasks,Dividers->{None,All},Spacings->0.5,Background->LightBlue]
dailyTasks[] := taskTimer[$dailyTasks];


(* ::Subsection:: *)
(*Machine Learning*)


TrainTestSplit[data_,testRatio_:0.2]:=Module[{r,testSize,trainSize},
	r=RandomSample@data;
	testSize = Round[testRatio*Length@data];
	trainSize = Length@data - testSize;
	Return @ {r[[;;trainSize]],r[[trainSize+1;;]]}
]


$MLModelTypes = {"LogisticRegression","Markov","RandomForest","SupportVectorMachine","NearestNeighbors","NeuralNetwork","NaiveBayes"};

Train[data:{Rule..}, testRatio_:0.2] := Module[{c,train,test},
	{train,test}=TrainTestSplit[data];
	c = Classify[train, Method->#,PerformanceGoal->"Quality"]& /@ $MLModelTypes;
	Return @ {c, ClassifierMeasurements[#, test]& /@ c}
]



(* ::Subsection:: *)
(*List & Associations*)


part[spec__][data_] := data[[spec]];


ToList = Developer`ToList;


MapOrApply[f_, m_] := If[ListQ[m], f/@m, f@m]


DropColumn[mat_, n_] := Module[
	{toDrop, toKeep, numberOfColumns, normalized, fixNegs},
	numberOfColumns = Length[mat[[1]]];
	fixNegs = If[# < 0, numberOfColumns + # + 1, #]&;
	toDrop = If[ListQ @ n, fixNegs /@ n, {fixNegs @ n}];
	toKeep = Complement[Range[numberOfColumns], toDrop];
	Return @ mat[[All, toKeep]]
]


reverseAssn[a_Association] := (Normal/*Map[Reverse]/*ToAssociation)@a


raggedTranspose[mat_Association] := TableForm[Values[mat]~Flatten~{2}]
safeTake[list_List,n_]:=If[n>Length@list,list,Take[list,n]];


nSubsets[l_,n_]:= Module[
	{r = RandomSample@l,c},
	If[Length[l] < n, Return@$Failed];
	c = ToList /@ Take[r, n];
	r = Drop[r, n];
	Fold[Insert[#1, #2, {RandomInteger[{1, n}], 1}] &, c, r]
]


SetAttributes[dropFrom,{HoldFirst}];
dropFrom[x_, args___] := (x = Drop[x, args]);


infoBox[assn_Association] := infoBox["Association Details",assn];
infoBox[title_,assn_]:=
Where[tit=StringTemplate["\<\"`1`\"\>"][title],
gboxes={StyleBox[StringTemplate["\<\"`1`\"\>"][ToString[#[[1]]]],
               StripOnInput->False,
               LineOpacity->0.8,
               FrontFaceOpacity->0.8,
               BackFaceOpacity->0.8,
               Opacity->0.8,
               FontWeight->"SemiBold",
               FontOpacity->0.8], ToBoxes[#[[2]]](*StringTemplate["\<\"`1`\"\>"][ToString[#[[2]]]]*)}&/@Normal[assn],
CellPrint@Cell[BoxData[
 InterpretationBox[
  TagBox[
   StyleBox[
    FrameBox[GridBox[{
       {
        ItemBox[
         FrameBox[
          StyleBox[tit, "SuggestionsBarText",
           StripOnInput->False,
           FontSize->Larger],
          FrameMargins->{{10, 0}, {-5, 5}},
          FrameStyle->None,
          StripOnInput->False],
         Alignment->{Left, Bottom},
         Background->RGBColor[0.93119, 0.93119, 0.93119],
         Frame->{{False, False}, {True, False}},
         FrameStyle->Opacity[0.1],
         ItemSize->{Automatic, 1},
         StripOnInput->False]},
       {
        ItemBox[
         FrameBox[
          TagBox[GridBox[gboxes,
            AutoDelete->False,
            BaseStyle->{
             FontWeight -> "Light", FontFamily -> ".Helvetica Neue DeskInterface", NumberMarks ->
              False},
            GridBoxAlignment->{
             "Columns" -> {Right, {Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}},
              "RowsIndexed" -> {}},
            GridBoxDividers->{"Columns" -> {False, {
                 Opacity[0.15]}, False}},
            GridBoxItemSize->{
             "Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
            GridBoxSpacings->{"Columns" -> {
                Offset[0.27999999999999997`],
                Offset[2.0999999999999996`], {
                 Offset[1.75]},
                Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
                Offset[0.2], {
                 Offset[0.8]},
                Offset[0.2]}, "RowsIndexed" -> {}}],
           "Grid"],
          FrameMargins->{{10, 10}, {10, 5}},
          FrameStyle->None,
          StripOnInput->False],
         Alignment->Left,
         BaseStyle->{Deployed -> False},
         StripOnInput->False]}
      },
      DefaultBaseStyle->"Column",
      GridBoxAlignment->{
       "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
      GridBoxDividers->{
       "Columns" -> {{False}}, "ColumnsIndexed" -> {}, "Rows" -> {{False}}, "RowsIndexed" -> {}},
      GridBoxItemSize->{
       "Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
      GridBoxSpacings->{"Columns" -> {
          Offset[0.27999999999999997`], {
           Offset[0.5599999999999999]},
          Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
          Offset[0.2],
          Offset[1.2], {
           Offset[0.4]},
          Offset[0.2]}, "RowsIndexed" -> {}}],
     Background->RGBColor[0.9802, 0.9802, 0.9802],
     FrameMargins->{{0, 0}, {0, 0}},
     FrameStyle->GrayLevel[0.85],
     RoundingRadius->5,
     StripOnInput->False],
    StripOnInput->False,
    LineBreakWithin->False],
   Deploy,
   DefaultBaseStyle->"Deploy"],
  assn]], "Output"]]


(* ::Subsection:: *)
(*Formatting & Stats*)


valueChart[data_,labels_]:=Module[{},
	Return @ BarChart[Labeled[#,percent[#],After]& /@ data,
	ChartLabels -> decamelize/@labels,
	ChartElementFunction -> "GlassRectangle",ChartStyle->"Pastel",BarSpacing->0,BarOrigin->Left]
]

Options[valueTable]={"Headers"->Automatic};
valueTable[data_, labels_]:= TextGrid[Prepend[data,labels],Background->{None,{Lighter[Yellow,.9],{White,Lighter[Blend[{Blue,Green}],.9]}}},Dividers->{{Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Darker[Gray,.6],{False},Darker[Gray,.6]}},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}]
valueTable[data_, toplabels_, leftlabels_]:= TextGrid[PrependColumn[Prepend[data,toplabels],Prepend[leftlabels,""]],Background->{{Lighter[Yellow,.9], {None}},{Lighter[Yellow,.9],{White}}},Dividers->{{Darker[Gray,.6],Darker[Gray,.6],{Lighter[Gray,.5]},Darker[Gray,.6]},{Darker[Gray,.6],Darker[Gray,.6],{Lighter[Gray,.2]},Darker[Gray,.6]}},Frame->Darker[Gray,.6],ItemStyle->14,Spacings->{Automatic,.8}]

HumanSize::usage = "HumanSize[]";
HumanSize[size_] := Module[{sz, unit},
	Which[
		size > 1000000000, sz = size/1000000000.0; unit = "G",
		size > 1000000, sz = size/1000000.0; unit = "M",
		size > 1000, sz = size/1000.0; unit = "k",
		True, sz = size; unit = ""];
	ToString[NumberForm[sz, {3,1}]] <> unit];

HumanTime::usage = "HumanTime[seconds] formats a time quantity into human readable form.";
HumanTime[seconds_] := Module[{sz, unit, dec = True},
	Which[
		seconds > 60 * 60 * 24, sz = seconds / 60.0 / 60 / 24; unit = "d",
		seconds > 60 * 60, sz = seconds / 60.0 / 60 ; unit = "h",
		seconds >= 60, sz = seconds / 60.0 ; unit = "m",
		60.0 > seconds >= 10.0, sz = sz = seconds; unit = "s"; dec = False,
		10.0 > seconds >= 1.0, sz = seconds; unit = "s",
		1.0 > seconds >= 0.001, sz = seconds * 1000; unit = "ms"; dec = False,
		True, Return["0s"]];
	ToString[If[dec, NumberForm[sz, {2,1}], Round[sz]]] <> unit];




gridPartition[list_,n_:5] := Partition[list,n,n,1,{}]
fancy[s_String]:=Style[s, FontFamily -> "Avenir Next", FontSize->24]


CoolColor[z_] := RGBColor[z, 1-z, 1];

$plotSize = 500;

$mag=1.2;

intervalFormat[interval_]:=Row[{"[",interval[[1]],", ",interval[[2]],")"}];

deNullString[list_] := DeleteCases[list, x_/;MatchQ[x,""|Null]]
sentence[list_] := Magnify[Text@Row[list],$mag]
percent[x_] := Row[{IntegerPart[x*100],"%"}];

prettyRule = Row[{Keys[#][[1]], "  ", Style[Row[{"\[LeftAngleBracket]",Values[#][[1]],"\[RightAngleBracket]"}],Gray]}]&;
dateConversionRules = {SQLDateTime[x_List] :> DateList[x]};
fromUnixDate = DateList@DatePlus["1970", {# / 1000000 // N, "Second"}]&;


ClearAll[table];
Options[table] = Join[Options@Grid, {"Header" -> None, "DateStrings" -> True, "Magnify" -> 1}];

table[data_List, opts:OptionsPattern[]] := Module[
	{results = data},
	If[OptionValue["DateStrings"], results = Map[formatIfDateQ, results, {2}]];
	If[ListQ@OptionValue["Header"], PrependTo[results, OptionValue["Header"]]];
	Grid[results,
		filterOpts[Grid,opts],
		ItemStyle -> If[ListQ@OptionValue["Header"], If[Length@data == Length@OptionValue["Header"], Reverse, Identity][{ Directive[FontSize->10],1->Directive[Bold]}],Directive[FontSize->10]],
		Dividers -> GrayLevel[0.1,0.1],
		Background -> If[ListQ@OptionValue["Header"], If[Length@data == Length@OptionValue["Header"], Reverse, Identity][{None,1->LightGray}],None],
		Frame -> True,
		FrameStyle -> Thickness[2],
		Alignment->Left
	]//Magnify[#,OptionValue["Magnify"]]&
]

table[data_Association, opts:OptionsPattern[]] := Module[
	{d, l, v, toSubscript, results = List@@@Normal[data]},
	If[ListQ@OptionValue["Header"], results =  Transpose@{OptionValue["Header"],Values[data]}];
	If[OptionValue["DateStrings"], results = Map[formatIfDateQ, results, {2}]];
	Grid[
		results,
		filterOpts[Grid,opts],
		ItemStyle -> {1->Directive[Bold], Directive[FontSize->10]},
		Dividers -> GrayLevel[0.1,0.1],
		Background -> {1->LightGray,None},
		Frame -> True,
		FrameStyle -> Thickness[2],
		Alignment->Left
	]//Magnify[#,OptionValue["Magnify"]]&
] /; Depth@Values[data] == 2;

table[data_Association, opts:OptionsPattern[]] := Module[
	{d, l, v, toSubscript},
	toSubscript = li \[Function] MapIndexed[Subscript[#1,First[#2]]&, li];
	d = toSubscript /@ data;
	d = Flatten /@ Thread[{ToUpperCase/@Keys[data], Values@d}];
	l = MaximalBy[d,Length][[1]]//Length;
	Grid[Transpose[PadRight[#, l, ""]& /@ d],
		filterOpts[Grid,opts],
		ItemStyle -> {Directive[FontSize->10], 1->Directive[Bold]},
		Dividers -> GrayLevel[0.1,0.1],
		Background -> {None,1->LightGray},
		Frame -> True,
		FrameStyle->Thickness[2],
		Alignment->Left
	]//Magnify[#,OptionValue["Magnify"]]&
]


getLags[data_, i_] := Drop[data, -i];
GrangerCausalityTest[dat_, lag_: 3] := Module[
  {xx, x, y, laggeddata, le, res1, res2, grangerstat},
  le = Select[dat, (And @@ (NumericQ /@ #)) &];
  xx = Flatten[Most /@ le]; y = Last /@ le;
  laggeddata = Transpose[
    Map[Take[#, -Length[y] + lag] &,
     Join[Table[getLags[y, i], {i, lag}],
      Table[getLags[xx, i], {i, lag}], {y}]]
    ];
  res1 = Total[
    LinearModelFit[laggeddata,
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}],
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}]][
     "FitResiduals"]^2];
  res2 = Total[
    LinearModelFit[laggeddata,
      Table[Subscript[x, i], {i, (Length[First[laggeddata]] - 1)/2}],
      Table[Subscript[x, i], {i, Length[First[laggeddata]] - 1}]][
     "FitResiduals"]^2];
  grangerstat = ((res2 - res1)/
      lag)/(res1/(Length[laggeddata] - 2*lag - 1));
  1 - CDF[FRatioDistribution[lag, Length[laggeddata] - 2*lag - 1],
    grangerstat]
  ];




Clear[stats];
Options[stats] = {};
stats[input_List,opts:OptionsPattern[]] := Module[{},
		Grid[
			{
				{Text@Style["Max",Bold], MaximalBy[input, Identity][[1]]},
				{Text@Style["Min",Bold], MinimalBy[input, Identity][[1]]},
				{Text@Style["Mean",Bold], Mean[input]//N},
				{Text@Style["Median",Bold], Median[input]},
				{Text@Style["Commonest",Bold], Row[Commonest[input],", "]},
				{Text@Style["\!\(\*SuperscriptBox[\(\[Sigma]\), \(2\)]\)",Bold], N[Variance[input],3]},
				{Text@Style["Deviation (std)",Bold], N[StandardDeviation[input],3]},
				{Text@Style["Deviation (mean)",Bold], N[MeanDeviation[input],3]}
			},
			Spacings->{2,1}, Frame->LightGray,
			Dividers->All, Alignment->Left,
			Background -> {1->LightGray,None}
		]
		//Magnify[#,0.9]&
]

stats[input_Association,opts:OptionsPattern[]] := Module[{},
		Grid[
			{
				{Text@Style["Max",Bold], prettyRule @ MaximalBy[input, Identity]},
				{Text@Style["Min",Bold], prettyRule @ MinimalBy[input, Identity]},
				{Text@Style["Mean",Bold], Mean[input]//N},
				{Text@Style["Median",Bold], Median[input]},
				{Text@Style["Commonest",Bold],Row[Commonest[Flatten@Values@input],", "]},
				{Text@Style["Variance \!\(\*SuperscriptBox[\(\[Sigma]\), \(2\)]\)",Bold], N[Variance[input],3]},
				{Text@Style["Deviation (std)",Bold], N[StandardDeviation[input],3]},
				{Text@Style["Deviation (mean)",Bold], N[MeanDeviation[Values@input],3]}
			},
			Spacings->{2,1}, Frame->LightGray,
			Dividers->All, Alignment->Left,
			Background -> {1->LightGray,None}
		]
		//Magnify[#,0.9]&
]


pie[input_] := PieChart[input, ChartStyle->"DarkRainbow",
	ChartLabels->Placed[Keys[input],"RadialCallout"], LabelingFunction->"RadialCenter"]


Clear[barStats];
barStats[input_,opts:OptionsPattern[]] :=
	Grid[{{
		bars[input,filterOpts[bars,opts]],
		stats[input,filterOpts[stats,opts]]
	}}]


Clear[histStats];
histStats[input_, opts:OptionsPattern[]] :=
	Grid[{{
		hist[input, filterOpts[hist,opts]],
		stats[input, filterOpts[stats,opts]]
	}}]


Clear[statGrid];
statGrid[input_, opts:OptionsPattern[]] :=
	Grid[{{
		bars[input,filterOpts[bars,opts]],
		hist[input,filterOpts[hist,opts]],
		stats[input,filterOpts[stats,opts]]
	}}]


Clear[bars];

Options[bars] = Join[Options[BarChart],{"Legend"->False}];

bars[input_List, opts:OptionsPattern[]] :=
	BarChart[input, filterOpts[BarChart, opts],
		LabelingFunction -> If[Length[input]>10,None,(Placed[#,Above]&)],
		BarSpacing -> Small,
		PlotLabel -> Style["Barchart of Counts",Bold],
		ImageSize -> $plotSize
]

bars[input_Association, opts:OptionsPattern[]] := Module[{
		groupsQ, customLabelQ, datesQ, tooBigToLabelQ,
		chartLabels, labelingFunction, colorFunction
	},

	chartLabels = labelingFunction = colorFunction = Automatic;
	datesQ = dateQ @ First @ Keys @ input;
	groupsQ = ListQ @ OptionValue[opts, ChartLegends];
	tooBigToLabelQ = Length[input] > 37;
	chartLabels = If[tooBigToLabelQ, None,
		(Rotate[#,-45\[Degree]]& /@ If[!datesQ, deDotString@Keys[input], Keys[input]])];

	If[!tooBigToLabelQ,
		labelingFunction = Placed[#, Above]&;
	];
	colorFunction = CoolColor;

	Legended[
		BarChart[input, filterOpts[BarChart, opts],
			ChartLabels -> chartLabels,
			LabelingFunction -> labelingFunction,
			ColorFunction -> colorFunction
			BarSpacing -> Small,
			PlotLabel -> Style["Barchart of Counts",Bold],
			AspectRatio->0.5,
			ImageSize -> $plotSize
		],
		If[OptionValue["Legend"],
			Placed[BarLegend[{CoolColor@Rescale[#,{Min[input],Max[input]}]&,{Min[input],Max[input]}},
			11,LegendLayout->"Row"],Below],Null
		]
	]
]


(* ::Text:: *)
(*Histogram*)


Options[hist]=Options[Histogram];
hist[data_, opts:OptionsPattern[]] := Histogram[data, opts,
	ChartBaseStyle->EdgeForm[Dotted],
	LabelingFunction->LabelingFunction->If[Length[data]>10,None,(Placed[#,Above]&)],
	PlotLabel->Style["Histogram",Bold],
	AspectRatio->0.5,
	ImageSize->$plotSize
];


(* ::Subsubsection:: *)
(*Fake Data*)


(*RandomString[] := *)
RandomData[] := Association@Table[RandomString[CharacterRange["a", "z"], 10] -> RandomInteger[1000,RandomInteger[100]], {10}]
RandomData[_Integer] := Association@Table[RandomString[CharacterRange["a", "z"], 10] -> RandomInteger[1000], {10}]
RandomDate[] := With[{{min, max} = AbsoluteTime/@{{2014,1,1,0,0,0}, {2014,12,31,11,59,59}}}, RandomReal[{min,max}]];
RandomTimeSeries[] := Association@Table[RandomString[CharacterRange["a", "z"], 10] -> RandomInteger[1000],{100}]


randomImage[] := ExampleData @ RandomChoice[ExampleData["TestImage"]];
randomImage[x_ 100,y_:100] := RandomImage[1,{x,y},ColorSpace->"RGB"];
randomImage[str_String] := With[{result = Import["https://ajax.googleapis.com/ajax/services/search/images?v=1.0&q="<>URLEncode[str], "JSON"]},
	Import["url" /. ("results" /. ("responseData" /. result))[[1]]]];
randomImage[str_String, n_Integer] := randomImage[str, 1, n];
randomImage[str_String, i_Integer, j_Integer] := Module[{urls},
	urls=Import["https://ajax.googleapis.com/ajax/services/search/images?v=1.0&start="<>ToString[i]<>"&rsz="<>ToString[j]<>"&q="<>str, "JSON"];
	urls=Cases[urls,Verbatim[Rule]["tbUrl",x_]:>x,\[Infinity]];
	Return @ importImages[urls]
]


(* ::Subsection:: *)
(*Writing*)


Rhymes[r_] := Rhymes[r] = Module[{url, links, rhy},
	url=StringTemplate["http://www.rhymezone.com/r/rhyme.cgi?Word=`1`&typeofrhyme=perfect&org1=syl&org2=l&org3=y"][r];
	Check[links=Import[url,"Hyperlinks"],Return@$Failed];
	rhy=Flatten[StringCases[#,"http://www.rhymezone.com/r/d="~~w__:>StringReplace[w,"_"->" "]]&/@links];
	Return @ rhy
]

Synonyms[s_] := Synonyms[s] = Module[{url, links, syn},
	url=StringTemplate["http://www.rhymezone.com/r/rhyme.cgi?Word=`1`&typeofrhyme=syn&org1=syl&org2=l&org3=y"][s];
	Check[links=Import[url,"Hyperlinks"],Return@$Failed];
	syn=Flatten[StringCases[#,"http://www.rhymezone.com/r/d="~~w__:>StringReplace[w,"_"->" "]]&/@links];
	Return @ syn
]

RhymesWithXSynonymWithY[x_, y_] := Intersection[Rhymes@x, Synonyms@y]


(* ::Subsection:: *)
(*End[]*)


(* KFN *)
KFN[list_, k_Integer?Positive] := Module[{kTuples},
  kTuples = Subsets[RandomSample[list,20], {k}];
  MaximalBy[kTuples,
   Total[Flatten[Outer[EuclideanDistance[#1, #2] &, #, #, 1]]] &]
]


End[]
EndPackage[]
