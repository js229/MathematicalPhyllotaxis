(* ::Package:: *)

(* ::Input::Initialization:: *)
 github= PersistentSymbol["persistentGitHubPath","Local"];
(* Two different packages *)
latticePhyllotaxisPackagePath= FileNameJoin[github,"GeometricalPhyllotaxis\\Mathematica\\Packages"];
Get["LatticePhyllotaxis.wl",Path->latticePhyllotaxisPackagePath];
(* This is _not_ where LaTeX looks for the figures, which is MathematicalPhyllotaxis\\LaTeX\MAthematicaFigures probably. Moving them there  deliberately done manually *)
(* stylings *)
mathematicalPhyllotaxisPackagePath= FileNameJoin[github,"MathematicalPhyllotaxis\\Mathematica\\Packages"];
Get["TextbookStylings.m",Path->mathematicalPhyllotaxisPackagePath]


txbDraftFigurePath = FileNameJoin[
PersistentSymbol["persistentGDrive"],"Work\\Textbook\\Springer\\Working","Draft Figures"];
SetAttributes[txbExport,HoldFirst];
txbExport[fig_,size_:1] := Module[{figname,imageWidth,scaledFig},
	SetDirectory[txbDraftFigurePath];
	figname=SymbolName[Unevaluated[fig]];
	(*scaledFig = If[Head[fig]===Graphics,
		Show[fig,ImageSize->pageWidthPT * size],
		Print["fig is not graphics: ",Head[fig]];fig
	];*)
		(* mma inappropriately rasterizes when ImageSize is set *)
	Export[StringJoin[figname,".pdf"],fig,ImageResolution->300];
	ResetDirectory[];
	fig
];


