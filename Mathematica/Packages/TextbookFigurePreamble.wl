(* ::Package:: *)

(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
github = PersistentSymbol["persistentGitHubPath","Local"];


(* ::Input::Initialization:: *)
latticePhyllotaxisPackagePath= FileNameJoin[github,"GeometricalPhyllotaxis\\Mathematica\\Packages"];

Get["LatticePhyllotaxis.wl",Path->latticePhyllotaxisPackagePath];


txbDraftFigurePath = FileNameJoin[
PersistentSymbol["persistentGDrive"],"Work\\Textbook\\Springer\\Working","Draft Figures"];


mathematicalPhyllotaxisPackagePath= FileNameJoin[github,"MathematicalPhyllotaxis\\Mathematica\\Packages"];

Get["TextbookStylings.m",Path->mathematicalPhyllotaxisPackagePath];
