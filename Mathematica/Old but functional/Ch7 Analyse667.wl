(* ::Package:: *)

(* ::Input::Initialization:: *)
Get["LatticePhyllotaxis`",Path->PersistentSymbol["persistentGeometricalPhyllotaxis"]];



SetDirectory[NotebookDirectory[]];
Get["TextbookStylings.m"];



figureStyle = jStyle;
jParastichyColour[n_] := figureStyle["ParastichyColour"][n];
jFont[n_] := {FontFamily->figureStyle["FontFamily"],FontSize->n};



(* ::Input:: *)
(**)


i667= Import["PreMathematica Figures\\i667.jpg"];


(* ::Input::Initialization:: *)
p667 = ImageResize[i667,iWidth];
{r667,bb, rs} = Rasterize[Show[p667(*,Graphics[Disk[{0,0},100]]*)],{"Image","BoundingBox","RasterSize"}];
scaleFactor = rs[[1]]/bb[[1]];
rescale [{x_,y_}] := {x/scaleFactor,(rs[[2]]-y)/scaleFactor}




(* ::Input::Initialization:: *)
found = {{532,604},{512,558},{501,533},{502,487},{488,438},{496,379},{509,340},{527,281},{553,233},{566,174},{587,130},{617,69},{653,36},{941,514},{980,503},{1035,485},{1094,474},{1133,474},{1202,487},{1250,511},{1281,530},{1321,547},{1360,582},{1377,596},{946,470},{986,454},{1045,427},{1096,422},{1154,423},{1204,431},{1254,455},{1299,486},{1331,503},{1354,528},{977,409},{1040,392},{1100,383},{1148,376},{1208,374},{1244,401},{1295,419},{1352,459},{819,563},{856,563},{895,571},{907,526},{866,522},{825,521},{916,477},{867,469},{892,433},{934,431},{931,399},{974,361},{1023,339},{1087,327},{1131,313},{1188,317},{1238,327},{1295,355},{1331,375},{802,479},{838,438},{876,395},{911,362},{948,324},{1002,304},{1057,278},{1118,261},{1152,261},{1199,268},{1270,290},{1202,217},{1164,216},{763,476},{794,430},{819,392},{858,356},{895,324},{932,282},{985,249},{1035,225},{1087,208},{1139,195},{1136,151},{1086,148},{1045,164},{999,181},{949,215},{908,248},{866,287},{827,322},{790,368},{762,402},{738,451},{722,489},{707,528},{689,507},{696,470},{713,421},{736,374},{761,341},{787,298},{838,250},{876,210},{925,163},{967,135},{1015,117},{1051,106},{663,542},{656,493},{661,451},{680,402},{707,350},{727,317},{751,267},{787,224},{829,180},{866,144},{908,107},{950,85},{1005,67},{949,57},{587,551},{622,553},{628,529},{613,475},{620,421},{641,369},{665,329},{687,293},{711,237},{741,196},{776,154},{819,110},{857,92},{911,47},{585,511},{577,461},{579,402},{603,351},{625,307},{645,267},{673,213},{699,178},{724,129},{765,97},{801,63},{841,41},{569,607},{564,576},{543,528},{543,502},{533,441},{533,391},{547,346},{580,299},{601,244},{619,193},{648,151},{675,103},{716,65},{748,26},{441,1393},{512,1376},{567,1354},{619,1312},{665,1262},{705,1229},{741,1184},{769,1140},{793,1086},{810,1038},{812,985},{576,1421},{637,1390},{680,1347},{729,1299},{767,1256},{798,1205},{825,1158},{839,1117},{848,1059},{857,990},{656,1451},{698,1420},{746,1371},{789,1332},{825,1295},{855,1233},{878,1174},{889,1125},{896,1065},{897,1004},{888,949},{848,948},{860,898},{819,894},{812,844},{863,859},{906,906},{922,948},{948,971},{940,1004},{949,1067},{947,1122},{937,1182},{927,1227},{885,1288},{844,1339},{759,1438},{802,1395},{844,1456},{894,1403},{933,1342},{967,1293},{995,1237},{1006,1187},{1008,1134},{1006,1071},{1005,1014},{1046,1010},{1058,1074},{1060,1133},{1056,1186},{1051,1240},{1029,1288},{991,1353},{968,1394},{1091,1289},{1105,1229},{1113,1168},{1110,1121},{1114,1062},{1151,1032},{1162,1091},{1162,1152},{1157,1208},{1153,1257},{1207,1058},{1224,1118},{1204,1169},{1205,1227},{1237,1179},{1259,1130},{1299,1110},{1142,977},{1080,946},{1033,954},{978,962},{1204,998},{1181,955},{1162,916},{1116,928},{1081,893},{1048,899},{1003,907},{954,918},{916,867},{966,863},{1007,858},{1038,847},{1086,839},{1134,874},{1208,891},{1224,920},{1234,969},{1248,1019},{1258,1077},{1305,1057},{1306,997},{1353,1021},{1297,960},{1343,953},{1334,910},{1281,903},{1320,861},{1020,808},{989,816},{944,828},{901,829},{858,827},{1179,854},{1130,816},{1076,785},{1005,765},{986,779},{930,794},{886,798},{1247,859},{1213,824},{1162,789},{1116,765},{1050,742},{963,750},{918,758},{873,764},{846,764},{900,727},{877,702},{998,725},{1298,831},{1247,795},{1202,765},{1161,741},{1094,714},{1041,698},{988,690},{947,715},{1329,786},{1353,825},{1379,869},{1281,754},{1127,676},{1234,721},{1185,703},{1072,664},{1021,654},{925,683},{962,663},{861,633},{903,659},{924,628},{896,615},{875,605},{994,623},{1048,615},{1091,621},{1151,637},{1204,661},{1252,681},{1302,707},{1339,729},{1377,771},{1397,814},{963,593},{1018,580},{1070,573},{1123,581},{1171,598},{1223,616},{1278,640},{1312,660},{1345,685},{1395,704},{1394,659},{1353,630},{1323,609},{1280,585},{1232,566},{1193,552},{1130,534},{1076,531},{1032,532},{971,548},{376,998},{422,991},{467,967},{513,945},{92,989},{134,1009},{184,1029},{226,1037},{282,1044},{335,1042},{387,1042},{429,1032},{478,1008},{523,991},{561,961},{591,904},{614,847},{641,834},{632,889},{611,946},{579,985},{547,1021},{504,1048},{456,1080},{398,1089},{349,1097},{302,1096},{249,1087},{196,1082},{156,1071},{110,1062},{127,1109},{166,1126},{215,1137},{272,1155},{324,1159},{376,1156},{433,1147},{484,1124},{530,1090},{576,1053},{609,1022},{639,974},{656,935},{663,869},{694,858},{699,909},{693,967},{671,1017},{637,1058},{607,1095},{553,1143},{509,1176},{465,1203},{420,1215},{361,1218},{296,1215},{243,1216},{208,1204},{291,1284},{339,1279},{399,1275},{456,1264},{504,1242},{555,1207},{603,1164},{646,1121},{678,1084},{709,1043},{730,990},{740,944},{728,878},{743,843},{784,868},{769,904},{784,963},{771,1021},{748,1070},{722,1119},{685,1158},{651,1199},{611,1236},{560,1282},{496,1309},{440,1325},{400,1339},{330,1337},{425,91},{491,67},{550,70},{530,123},{514,173},{496,228},{474,282},{458,335},{450,390},{446,440},{459,487},{465,117},{460,178},{438,226},{413,273},{397,339},{391,387},{403,442},{424,493},{486,609},{445,582},{459,534},{412,134},{399,181},{372,238},{352,289},{349,346},{349,385},{357,446},{381,505},{425,553},{382,567},{339,514},{313,470},{303,421},{303,366},{304,308},{308,244},{316,204},{341,145},{400,613},{358,589},{313,550},{282,504},{263,459},{249,396},{249,329},{251,291},{257,215},{198,262},{198,312},{198,380},{205,421},{219,482},{239,528},{195,556},{166,506},{150,461},{152,404},{144,351},{154,281},{161,593},{126,537},{108,483},{103,427},{103,365},{136,640},{102,587},{78,531},{57,457},{42,500},{107,677},{73,622},{43,569},{21,612},{51,664},{88,718},{275,574},{248,601},{209,640},{181,671},{148,704},{318,607},{280,635},{262,660},{221,697},{183,739},{128,741},{49,741},{173,775},{101,784},{49,780},{146,811},{73,818},{124,844},{72,861},{113,894},{535,665},{474,660},{409,650},{371,632},{334,665},{298,690},{274,721},{246,760},{216,803},{192,826},{173,860},{394,678},{352,708},{319,733},{297,772},{274,810},{241,846},{212,883},{196,925},{446,690},{413,719},{369,750},{346,779},{322,817},{304,854},{279,894},{253,939},{470,717},{428,752},{399,790},{373,826},{342,864},{325,899},{300,951},{522,719},{483,754},{455,783},{417,831},{397,861},{357,912},{343,953},{471,820},{435,859},{413,897},{393,952},{444,944},{463,900},{488,839},{498,876},{522,900},{534,861},{522,792},{531,809},{557,818},{582,830},{586,871},{558,851},{551,917},{125,952},{237,982},{183,972},{266,989},{318,1002},{1094,1007},{150,916}};


foundPoints= rescale/@ found;
nodes667 = foundPoints;
q667 = Rasterize[Show[r667,Graphics[{PointSize[Large],Red,Point[foundPoints]}]]]
(* then open q667 and identify points by _index_ *)



(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
meshToGraph[mesh_] := Module[{edges,g,nxy,x,y},
edges= MeshCells[mesh,1]/. Line[{x_,y_}]->UndirectedEdge[x,y];
g= Graph[edges];
nxy = Map[First@MeshPrimitives[mesh,{0,#}]&,VertexList[g]];
g= Graph[g,VertexCoordinates->nxy];
g
];

cellData[nodes_] := Module[{linesOfNode},
vMesh = VoronoiMesh[nodes];
dmesh= DelaunayMesh[nodes];
polyCentroids= RegionCentroid /@ MeshPrimitives[vMesh,2];
voronoiPolygons= 
Map[<|"Polygon"->#[[1]],"Centroid"->#[[2]]|>&,Transpose[{MeshPrimitives[vMesh,2],polyCentroids}]];
nearestCell[node_] := Module[{res},
polys = Take[SortBy[voronoiPolygons,Norm[#Centroid- node]&],2];
res= Select[polys,RegionMember[#Polygon,node]&];
If[Length[res]>0,Return[First@res]];
res= Select[voronoiPolygons,RegionMember[#Polygon,node]&];
Return[First[res]]
];
voronoiCells = Map[<|"NodeXY"->#,"Polygon"->nearestCell[#]["Polygon"]|>&,nodes];
delaunayCells = Map[<|"Node"->#,"NodeXY"-> First@MeshPrimitives[dmesh,{0,#}]|>&,First/@MeshCells[dmesh,0]];
voronoiPolygon[delaunayNode_] := (First@SortBy[voronoiCells,Norm[#NodeXY- First@MeshPrimitives[dmesh,{0,delaunayNode}]]&])["Polygon"];
delaunayCells = Map[Append[#,"VoronoiPolygon"->voronoiPolygon[#Node]]&,delaunayCells];

g = meshToGraph[dmesh];

xyPair[node1_,node2_] := {AnnotationValue[{g,node1},VertexCoordinates],AnnotationValue[{g,node2},VertexCoordinates]};
lineAngle[Line[{xy1_,xy2_}]] := N@(ArcTan@@ (xy2-xy1));
lineLength[Line[{xy1_,xy2_}]] := N[Norm[xy2-xy1]];
normalisedVector[angle_] := {Cos[angle],Sin[angle]};


linesOfNode[node_] := Association@Map[(node \[UndirectedEdge] #) -> Line[xyPair[node,#]]&,AdjacencyList[g,node]];

delaunayCells= Map[Append[#,<|"Lines"-> linesOfNode[#Node]|>]&,delaunayCells];
cellLineAngles[cell_] := Map[lineAngle,cell["Lines"]];
cellLineSemiLengths[cell_] := SortBy[Map[lineLength[#]/2&,cell["Lines"]],N];
cellLineEndPoints[cell_] := Association@Map[#->
Line[{cell["NodeXY"],
cell["NodeXY"]+cell["LineSemiLength"][#]*normalisedVector[cell["LineAngles"][#]]}]&,Keys@cell["LineAngles"]];

delaunayCells= Map[Append[#,<|"LineAngles"-> cellLineAngles[#]|>]&,delaunayCells];
delaunayCells= Map[Append[#,<|"LineSemiLength"-> cellLineSemiLengths[#]|>]&,delaunayCells];
delaunayCells= Map[Append[#,<|"SemiLine"-> cellLineEndPoints[#]|>]&,delaunayCells];


delaunayCells= Association@Map[#Node->#&,delaunayCells];

centre= Mean@Map[#NodeXY&,delaunayCells];
delaunayCells= Map[Append[#,<|"Radius"-> N@Norm[#NodeXY-centre]|>]&,delaunayCells];

cellPhi[cell_] := N[ArcTan@@(cell["NodeXY"]-centre)];
delaunayCells= Map[Append[#,<|"Phi"-> cellPhi[#]|>]&,delaunayCells];


delaunayCells
];


(* ::Input::Initialization:: *)



ffs= Directive@{FaceForm[None],EdgeForm[Red]};





(* ::Input:: *)
(*cell667= cellData[Take[nodes667,All]];*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*maxRadius = Max[Map[#Radius&,cell667]];*)
(*centre = Mean[Map[#NodeXY&,cell667]];*)
(*outerCutoffCircle= Circle[centre,maxRadius * 1.01];*)
(*minRadius= Min[Map[#Radius&,cell667]];*)
(*innerCutoffCircle = Circle[centre,minRadius * 0.99];*)
(**)
(*emptyRegionQ[EmptyRegion[_]] := True;*)
(*emptyRegionQ[_] := False;*)
(**)
(*isOuterNode[cell_] := !emptyRegionQ[RegionIntersection[cell["VoronoiPolygon"],outerCutoffCircle]];*)
(*isInnerNode[cell_] := !emptyRegionQ[RegionIntersection[cell["VoronoiPolygon"],innerCutoffCircle]];*)
(*centralNodes= Keys@Select[cell667,!isOuterNode[#] && !isInnerNode[#] &];*)
(*outerNodes =Keys@Select[cell667,isOuterNode];*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
stronglyInterior[node_] := Module[{nbrs,r},
nbrs =  Last/@(Keys@cell667[node]["Lines"]);
r = cell667[node]["Radius"]- 5;
nbrs= Select[nbrs,cell667[#]["Radius"]<= r&];
nbrs
];




(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
mostOppositeEdge[cell_,edge_] := Module[{edgeAngle,angleDifference,mostOppositeLine},
edgeAngle = cell["LineAngles"][edge];
angleDifference[a1_,a2_] := Min[Abs/@{a1-a2,a1+\[Pi]-a2,a1-\[Pi]-a2}];
mostOppositeLine = First@Keys@Sort@Map[angleDifference[edgeAngle,#]&,KeyDrop[cell["LineAngles"],edge]];
KeyTake[cell["SemiLine"],{mostOppositeLine}]
];


(* ::Input::Initialization:: *)
shortestEdge[cell_]:= KeyTake[cell["Lines"],First[Keys[cell["LineSemiLength"]]]];


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
mostInterior[node_] := Module[{next},
next = Last/@(Keys@cell667[[node]]["Lines"]);
next = First@SortBy[next,cell667[#]["Radius"]&];
next
];

nextStraightest[nodeList_] := Module[{previous,this},
previous= nodeList[[-2]];this=nodeList[[-1]];
thisEdgeAngle = cell667[previous]["LineAngles"][previous \[UndirectedEdge] this];
angles = KeyDrop[cell667[this]["LineAngles"],this\[UndirectedEdge]previous];
straightest = Last@First@Keys@SortBy[angles,angleDifference[#,thisEdgeAngle]&];
straightestAngle= cell667[straightest]["LineAngles"][straightest \[UndirectedEdge] this];
thisAngleDifference = angleDifference[straightestAngle,thisEdgeAngle];
If[Abs[thisAngleDifference*(360)/(2\[Pi]) -180]> 45,straightest=Missing["Too bendy"]
,
If[cell667[straightest]["Radius"]>cell667[this]["Radius"],straightest=Missing["Reversing"]]
];
Append[nodeList,straightest]
];
followAlong[{node1_,node2_}] := Drop[#,-1]&@NestWhile[nextStraightest,{node1,node2},!MissingQ[Last[#]]&,1,20]

parastichyTrace[{node_,next_}] :=Module[{edges},
edges = 
 Map[Apply[UndirectedEdge,#]&,Partition[followAlong[{node,next}],2,1]];
Association@Map[#->cell667[First[#]]["Lines"][#]&,edges]
];

angleDifference[a1_,a2_] := Module[{a=Sort[{a1,a2}],res},
res= Mod[a[[2]]-a[[1]], 2\[Pi]];
res = Min[res,2\[Pi]-res];
res
];







(* ::Input:: *)
(*starts =Flatten[Map[Outer[List,{#},stronglyInterior[#]]&,outerNodes],2];*)
(*leftStarts = Join[Select[starts,cell667[#[[1]]]["Phi"]>cell667[#[[2]]]["Phi"]&],{{238,231},{389,383},{228,216},{227,217}}];*)
(*rightStarts =Select[starts,cell667[#[[1]]]["Phi"]<=cell667[#[[2]]]["Phi"]&];*)
(*leftTraces = Association@ Map[UndirectedEdge@@#->Values@parastichyTrace[#]&,leftStarts];*)


(* ::Input:: *)
(*right= {13\[UndirectedEdge]162,42\[UndirectedEdge]33,42\[UndirectedEdge]34,85\[UndirectedEdge]74,122\[UndirectedEdge]108,123\[UndirectedEdge]121,137\[UndirectedEdge]120,137\[UndirectedEdge]121,163\[UndirectedEdge]148,164\[UndirectedEdge]441,186\[UndirectedEdge]176,213\[UndirectedEdge]211,228\[UndirectedEdge]215,230\[UndirectedEdge]227,246\[UndirectedEdge]245,272\[UndirectedEdge]270,360\[UndirectedEdge]539,388\[UndirectedEdge]385,414\[UndirectedEdge]389,444\[UndirectedEdge]445,445\[UndirectedEdge]11,491\[UndirectedEdge]481,508\[UndirectedEdge]502,512\[UndirectedEdge]507,513\[UndirectedEdge]506,516\[UndirectedEdge]511,531\[UndirectedEdge]519,482\[UndirectedEdge]465,175\[UndirectedEdge]165,214\[UndirectedEdge]212,238\[UndirectedEdge]231};*)
(**)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
lineOfTrace[traceEdgeList_]:= Map[cell667[First[#]]["Lines"][#]&,traceEdgeList];



(* ::Input::Initialization:: *)
verticalTraceList = <|
12\[UndirectedEdge]160->{12,160,144},
13\[UndirectedEdge]161->{13,161,145,132},
23\[UndirectedEdge]348->{23,348,342},
33\[UndirectedEdge]21->{33,21,350,340},
34\[UndirectedEdge]22->{34,22,349,341},
42\[UndirectedEdge]32->{42,32,20,351,339},
60\[UndirectedEdge]40->{60,40,30},
61\[UndirectedEdge]41->{61,41,31,19},
72\[UndirectedEdge]59->{72,59,39,29,18},
73\[UndirectedEdge]71->{73,71,58,38},
74\[UndirectedEdge]70->{74,70,57,37,28,17},
85\[UndirectedEdge]84->{85,84,69,56,36},
86\[UndirectedEdge]83->{86,83,68,55},
109\[UndirectedEdge]87->{109,87,82},
122\[UndirectedEdge]108->{122,108},
123\[UndirectedEdge]121->{123,121},
137\[UndirectedEdge]120->{137,120,106},
148\[UndirectedEdge]135->{148,135,118,104,92},
149\[UndirectedEdge]136->{149,136,119,105},
163\[UndirectedEdge]147->{163,147,134,117,103},
162\[UndirectedEdge]146->{162,146,133,116},
165\[UndirectedEdge]439->{165,439,419,408},
164\[UndirectedEdge]440->{164,440,418,409},
175\[UndirectedEdge]166->{175,166,438,420,407},
185\[UndirectedEdge]176->{185,176,167,437,421,406},
186\[UndirectedEdge]177->{186,177,168,436,422,405},
211\[UndirectedEdge]187->{211,187,178,169,435,423,404},
213\[UndirectedEdge]212->{213,212,188,179,170,434},
214\[UndirectedEdge]210->{214,210,189},
227\[UndirectedEdge]217->{227,217,207,192},
228\[UndirectedEdge]216->{228,216,208,191,182},
229\[UndirectedEdge]215->{229,215,209,190,181},
230\[UndirectedEdge]226->{230,226,218},
239\[UndirectedEdge]231->{239,231,225,219},
243\[UndirectedEdge]238->{243,238,232,224},
244\[UndirectedEdge]242->{244,242,237,233,223},
245\[UndirectedEdge]241->{245,241,236,234},
246\[UndirectedEdge]269->{246,269,240,235,603},
270\[UndirectedEdge]268->{270,268,251,247,248},
272\[UndirectedEdge]271->{272,271},
274\[UndirectedEdge]273->{274,273,267,252},
275\[UndirectedEdge]276->{275,276,266},
312\[UndirectedEdge]277->{312,277,290},
334\[UndirectedEdge]310->{334,310},
335\[UndirectedEdge]311->{335,311,302},
345\[UndirectedEdge]333->{345,333,313},
346\[UndirectedEdge]344->{346,344,332,315},
347\[UndirectedEdge]343->{347,343,331},
360\[UndirectedEdge]598->{360,598,604,550,549,548,547,546,545,544,543,483},
386\[UndirectedEdge]361->{386,361,600,558,557,556,555,554,553,552,551,542,463},387\[UndirectedEdge]385->{387,385,362,599,566,565,564,563,562,561,560,559,541},
388\[UndirectedEdge]384->{388,384,363,601,573,572,571,570,569,568,567},
389\[UndirectedEdge]383->{389,383,364,602,580,579,578,577},
413\[UndirectedEdge]390->{413,390,382,365,356,584,583,582,581,591},
415\[UndirectedEdge]412->{415,412,391,381,366,357,585,586,588,590,596,594},
441\[UndirectedEdge]417->{441,417,410,393,379,368},
442\[UndirectedEdge]416->{442,416,411,392,380,367},
443\[UndirectedEdge]454->{443,454,447,9,157,141},
444\[UndirectedEdge]446->{444,446,10},
445\[UndirectedEdge]11->{445,11,159},
482\[UndirectedEdge]466->{482,466,456,449,7,155},
491\[UndirectedEdge]480->{491,480,468,458,451,5},
492\[UndirectedEdge]490->{492,490,479,469},
481\[UndirectedEdge]467->{481,467,457,450,6},
465\[UndirectedEdge]455->{465,455,448,8,156,140},
502\[UndirectedEdge]494->{502,494,488,477,471,460,452},
503\[UndirectedEdge]493->{503,493,489,478,470,459},
508\[UndirectedEdge]502->{508,502,493,490,480,467,456,448,9},
512\[UndirectedEdge]507->{512,507,501,494,489,479,468,457,449,8,157,142},
517\[UndirectedEdge]516->{517,516,511,506,500,495,488,478,469,458,450,7,156,141,129},
518\[UndirectedEdge]515->{518,515,510,505,499,496,487,477,470},
519\[UndirectedEdge]514->{519,514,509,504,498,497,486,476,471,460,452,5,154,139},
536\[UndirectedEdge]533->{536,533,530,524,523,522,521,520,485,475,472,461,573},
539\[UndirectedEdge]537->{539,537,535,532,529,528,527,526,525,484,474,473}
|>;

toPairs[list_] := Partition[list,2,1];
listToTrace[list_] := Map[UndirectedEdge @@@ toPairs[#]&,list];
verticalTraces= listToTrace[verticalTraceList];



(* ::Input::Initialization:: *)
g1:= Graphics[
{
{FaceForm[jStyle["CylinderColour"]],outerCutoffCircle/. Circle->Disk}
,{FaceForm[White],innerCutoffCircle /. Circle-> Disk}
,
Map[{
FaceForm[None],EdgeForm[White],#VoronoiPolygon}&,Values[KeyTake[cell667,centralNodes]]]
,{jParastichyColour[3],lineOfTrace/@Values[verticalTraces]/. Missing[__]->Nothing[]}
,{PointSize[Small],Map[Point[#["NodeXY"]]&,Values@cell667]}
}];
g2:= Graphics[
{
Map[{
FaceForm[None],EdgeForm[White],#VoronoiPolygon}&,Values[KeyTake[cell667,centralNodes]]]
}];
Ch7Sunflower667  :=GraphicsColumn[{Show[r667],Show[r667,g2],g1}]





(* ::Input:: *)
(*Ch7Sunflower667*)
(**)
