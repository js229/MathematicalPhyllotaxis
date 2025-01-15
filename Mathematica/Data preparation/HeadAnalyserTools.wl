(* ::Package:: *)

(* ::Section:: *)
(*Files and images*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(* mma bitmap image makes files too large for github...*)*)


(* ::Input::Initialization:: *)
replaceJPG[head_] := Module[{res=head,img},
img=head["Image"];
jpgString = ExportString[ img,"JPG"];
res["ImageString"]=jpgString;
res=KeyDrop[res,{"Image"}];
res
];



(* ::Section:: *)
(*Initial seed placements*)


(* ::Input:: *)
(*(* legacy code will need to be refactored *)*)


(* ::Input::Initialization:: *)
deleteLastPointFromThread[threadAssociation_] := Module[{res,currentThreadKey},
res=threadAssociation;
currentThreadKey=Last@Keys@threadAssociation;
If[Length[res[currentThreadKey]]>0,
res[currentThreadKey]=Drop[res[currentThreadKey],-1]];
res 
];


(* ::Input::Initialization:: *)
capturePoint[]:= Round/@MousePosition["Graphics"];


(* ::Input::Initialization:: *)
newThread[threadAssociation_] := Module[{threadID},
threadID=If[Length[threadAssociation]==0,1,Max[Keys[threadAssociation]]+1];
Append[threadAssociation,threadID->{}]
];


(* ::Input::Initialization:: *)
scol[threadAssociation_] := Module[{lastThread,lastPoint},
If[Length[threadAssociation]==0,lastThread={},lastThread=Last[threadAssociation]];
If[Length[lastThread]==0,lastPoint={},lastPoint=Last[lastThread]];
Show[Image[i667,ImageSize->Large],(* ie i667-> head["Image") *)
Graphics[
{Map[{Green,Line[#]}&,Values[threadAssociation]],
{Yellow,Point[lastThread]},
{Red,Point[lastPoint]}
}
]]
];
addPointToThread[threadAssociation_,point_] := Module[{res,currentThreadKey},
res=threadAssociation;
currentThreadKey=Last@Keys@threadAssociation;
res[currentThreadKey]=Append[res[currentThreadKey],point];
res 
];


(* ::Input::Initialization:: *)
fom[x_]:=If[Length[x]>0,First[x],Missing[x]];


(* ::Input:: *)
(*(*pointPicker[head667]*)*)


(* ::Section:: *)
(*Make graph from seed positions*)


(* ::Input::Initialization:: *)
graphFromMesh[mesh_]:=Module[{g,pts,edges,ptXY},
g=MeshConnectivityGraph[mesh,0];pts=Last/@VertexList[g];edges=Map[Last,EdgeList[g],{2}];ptXY=(#1->AnnotationValue[{g,{0,#1}},VertexCoordinates]&)/@pts;g=Graph[(Tooltip[#1,#1]&)/@pts,edges,VertexCoordinates->ptXY];
g
]


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Boundary finding*)


(* ::Input::Initialization:: *)
boundaryGraph[graph_] := Module[{leftMostNode,leftMostEdge,res,pathAsEdges},
leftMostNode = First@SortBy[VertexList[graph],AnnotationValue[{graph,#},VertexCoordinates][[1]]&];
leftMostEdge = fom@SortBy[centredVertexList[graph,leftMostNode],edgeClockTime[graph,#]&];
If[MissingQ[leftMostEdge],Return[Missing["No edges left"]]];
pathAsEdges=Catch[Nest[nextPath[graph,#]&,{leftMostEdge},1000]];
pathAsEdges=orderUndirectedEdges[pathAsEdges];
res=subGraphEdgesOnly[graph,pathAsEdges];
res=Graph[res,EdgeShapeFunction->{"Arrow"}];
res
];
centredVertexList[graph_,v_] := Module[{unsortedEdgeList,otherVertices},
unsortedEdgeList= EdgeList[graph,v\[UndirectedEdge]_];
otherVertices=Complement[Flatten[List@@@unsortedEdgeList],{v}];
Map[v\[UndirectedEdge]#&,otherVertices]
];
edgeClockTime[graph_,edge_] := Module[{v1,v2},
{v1,v2}=N@Map[AnnotationValue[{graph,#},VertexCoordinates]&,List@@edge];
12*Mod[\[Pi]/2-ArcTan@@(v2-v1),2\[Pi]]/(2\[Pi])
];
edgeClockTime[graph_,edge1_,edge2_] := Module[{t1,t2},
t1=edgeClockTime[graph,edge1];
t2=edgeClockTime[graph,edge2];
Mod[t2-t1-6,12] (* so time[ a\[UndirectedEdge]b, b\[UndirectedEdge]a] =0*)
];
edgeDeviation[graph_,edge1_,edge2_] := Module[{edgeTime},
edgeTime=edgeClockTime[graph,edge1,edge2];
Abs[edgeTime-6] 
];
nextPath[graph_,path_] := Module[{n},
n=nextEdgeByClockTime[graph,Last[path]];
If[MissingQ[n],(*Print["No nxt node"];*)Throw[path]];
If[MemberQ[path,n],Throw[path]];
Append[path,n]
];
nextEdgeByClockTime[graph_,v1_\[UndirectedEdge]v2_] :=Module[{es,nextv},
es=nextEdgesByClockTime[graph,v1\[UndirectedEdge]v2];
es=KeyDrop[es,v2 \[UndirectedEdge] v1] ;(* reverse edge*)
fom[Keys[es]]
];
nextEdgesByClockTime[graph_,v1_\[UndirectedEdge]v2_] := Sort@Association@
Map[#->edgeClockTime[graph,v1\[UndirectedEdge]v2,#]&,
centredVertexList[graph,v2]];

orderUndirectedEdges[chain_] := Module[{lastb,examinePair},
lastb=chain[[1,1]];
examinePair[a_\[UndirectedEdge]b_] := Module[{res},
res= If[a=!=lastb,b\[DirectedEdge]a,a\[DirectedEdge]b];
lastb=b;
res
];
Map[examinePair,chain]
];
subGraphEdgesOnly[graph_,sg_Graph]:=  subGraphEdgesOnly[graph,EdgeList[sg]];
subGraphEdgesOnly[graph_,edges_] := Module[{g,e},
g=EdgeDelete[graph,EdgeList[graph]];
g= EdgeAdd[g,edges];
g=Subgraph[g,edges];
g
];
stripBoundary[<|"Graph"->graph_,___,"Level"->level_,___|>] := Module[{bge,remainder,bv,bg},
bge=boundaryGraphEdges[graph];
If[MissingQ[bge],Return[bge]];
bv= VertexList[Graph[bge]];
remainder=Subgraph[graph,Complement[VertexList[graph],bv]];
bg= subGraphEdgesOnly[graph,bge];
<|"Graph"->remainder,"Level"->level+1,"Boundary"->bg,"BoundaryDirectedEdges"->bge|>
];
boundaryGraphEdges[graph_] := Module[{g,e},
g=boundaryGraph[graph];
If[MissingQ[g],Return[g]];
e=FindCycle[g];
fom@e
];
nodeBoundaryCoordinatesLookupLevel[graphBoundaryList_,level_] := Module[{g,v,np},
g=graphBoundaryList[[level]];
v=VertexList[g];
np=Length[v];
Association@Table[v[[nodeix]]-><|"Node"->v[[nodeix]],"Level"->level,"Y"->nodeix,"adjacentY"->{If[nodeix==1,np,nodeix-1],If[nodeix==np,1,nodeix+1]}
|>,{nodeix,np}]
];
makeNodeBoundaryCoordinatesLookup[graphBoundaryList_]:=
Join@@Map[nodeBoundaryCoordinatesLookupLevel[graphBoundaryList,#]&,Range[Length[graphBoundaryList]]];

makeBoundaryCoordinates[graph_] := Module[{stripResults,boundaryCoordinateLength,graphBoundaryList,boundaryCoordinatesLookup},
stripResults=DeleteMissing@NestWhileList[stripBoundary,<|"Graph"->graph,"Boundary"->_,"Level"->0|>,!MissingQ[#]&,1,20];
graphBoundaryList =Drop[#Boundary&/@stripResults,1];
boundaryCoordinatesLookup =makeNodeBoundaryCoordinatesLookup[graphBoundaryList];
boundaryCoordinatesLookup
];

addBoundaryCoordinatesToGraph[graphArg_] := Module[{graph=graphArg,bc},
bc=makeBoundaryCoordinates[graph];
Map[(AnnotationValue[{graph,#},"BoundaryCoordinates"]=bc[#])&,VertexList[graph]];
graph
];

stripOuterEdgesFromGraph[graphArg_] := Module[{graph=graphArg,bcLevel,outerEdges},
bcLevel[n_]:=AnnotationValue[{graph,n},"BoundaryCoordinates"]["Level"];
outerEdges=EdgeList[graph, _?(bcLevel[#]==1 &) \[UndirectedEdge] _?(bcLevel[#]==1 &)];
EdgeDelete[graph,outerEdges]
];


(* ::Input:: *)
(**)


(* ::Subsection:: *)
(*Thread finding*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
nodeOrdering[graph_,a_,b_]:= Module[{bc},
bc[node_] := AnnotationValue[{graph,node},"BoundaryCoordinates"];
If[MissingQ[bc[a]]|| MissingQ[bc[a]["Level"]],Return[False]];
If[MissingQ[bc[b]]|| MissingQ[bc[b]["Level"]],Return[True]];
If[bc[a]["Level"]<  bc[b]["Level"],Return[True]];
If[bc[b]["Y"]==1 && bc[a]["Y"]==bc[b]["adjacentY"][[1]],Return[True]];
Return[bc[a]["Y"]< bc[b]["Y"]];
];


sortEdge[graph_,a_\[UndirectedEdge]b_] := Module[{pair},
pair={a,b};
pair = Sort[pair,nodeOrdering[graph,#1,#2]&];
UndirectedEdge@@pair
];

makeSortedThreadEdges[graph_,thread_] := Module[{g,edges},
g=Subgraph[graph,thread["ThreadNodes"]];
edges = EdgeList[g];
If[nodeOrdering[graph,edges[[1,1]],edges[[-1,-1]]],
edges,Reverse/@(Reverse[edges])]
];

addThreadEdges[head_]:= Module[{res=head,threads},
threads=head["Threads"];
threads=Map[Append[#,"ThreadEdges"->makeSortedThreadEdges[head["Graph"],#]]&,threads];
res["Threads"]=threads;
res 
];

graphPrunedByThreadEdges[head_] := Module[{g,edgesInThreads},
edgesInThreads= Flatten@Map[#ThreadEdges&,head["Threads"]];
g=EdgeDelete[head["Graph"],edgesInThreads];
g
];

(*headEdgesThroughNodePair[head_,lastNode_,nearestNode_,pruneTailNodes_,maxDivergence_] := fom@Values@threadsThroughNode[head["Graph"],lastNode,nearestNode,pruneTailNodes,maxDivergence];
*)
anyHeadThreadsAtNode[head_,node_,maxThread_,maxDivergence_]  :=  Module[{g,threads},
If[MissingQ[node],Return[{}]];
threads = threadsSeededAt[head["Graph"],node,maxThread,maxDivergence];
threads
];

headThreadsAtNode[head_,node_,maxThread_,maxDivergence_]  :=  Module[{g,threads},
If[MissingQ[node],Return[{}]];
g=graphPrunedByThreadEdges[head];
threads = threadsSeededAt[g,node,maxThread,maxDivergence];
threads
];
threadsSeededAt[graph_,a_,maxThread_,maxDivergence_] := Module[{edgePairs},
edgePairs=edgeMatch[graph,a];
threads=Association@Map[#->extendThreadFromSeed[graph,maxThread,maxDivergence,#]&,edgePairs]
];


threadsThroughNode[graph_,selectedNode_,nextNode_,pruneTailNodes_,maxDivergence_] := Module[{threadPossibles},
If[MissingQ[selectedNode],Return[{}]];
threadPossibles = threadsSeededAt[graph,selectedNode,maxDivergence];
If[MissingQ[nextNode],Return[threadPossibles]];
threadPossibles=Select[threadPossibles,MemberQ[VertexList[#],nextNode]&];
threadPossibles=Map[Reverse@Drop[Reverse[#],UpTo[pruneTailNodes]]&,threadPossibles];
threadPossibles=Select[threadPossibles,Length[#]>0&];
threadPossibles
];



extendThreadFromSeed[graph_,maxThread_,maxDivergence_,seed_] :=Module[{thread},
thread = Catch[Nest[findNextEdgeInThread[graph,maxThread,maxDivergence,#,"Inwards"]&,seed,100]];
thread = Catch[Nest[findNextEdgeInThread[graph,maxThread,maxDivergence,#,"Outwards"]&,thread,100]];
thread 
];

findNextEdgeInThread[graph_,maxThread_,maxDivergence_,thread_,direction_] := Module[{res,lastEdge,lastNode,nextNodes,nextNode,nextBC,bottomLevel,nextLevel,lastLevel},
bottomLevel = Max@Map[#Level&,DeleteMissing[AnnotationValue[{graph,#},"BoundaryCoordinates"]]&/@VertexList[graph]];
lastEdge=If[direction=="Inwards",Last[thread],First[thread]];
lastNode=If[direction=="Inwards",Last[lastEdge],First[lastEdge]];
nextNodes= AdjacencyList[graph,lastNode];
res=Map[ <|"LastNode"->lastNode,"NextNode"->#,"NextEdge"-> If [direction=="Inwards",lastNode \[UndirectedEdge] #,#\[UndirectedEdge]lastNode]|>&,nextNodes];
res= Map[Append[#,"Deviation"->edgeDeviation[graph,lastEdge,#NextEdge]]&,res];
res = Select[res,#Deviation<maxDivergence&];
res = SortBy[res,#Deviation&];
res = fom@res;
If[MissingQ[res],Throw[thread]];
nextNode= fom@Complement[List@@res["NextEdge"],{lastNode}];
nextBC= AnnotationValue[{graph,nextNode},"BoundaryCoordinates"];
If[MissingQ[nextBC],Throw[thread]];
nextLevel=nextBC["Level"];
If[nextLevel ==1,Throw[thread]];
If[nextLevel ==bottomLevel,Throw[thread]];
lastLevel =  AnnotationValue[{graph,lastNode},"BoundaryCoordinates"]["Level"];
If[Length[thread]>3,
If[direction=="Inwards" && nextLevel < lastLevel,Throw[thread]];
If[direction!= "Inwards" && nextLevel > lastLevel,Throw[thread]];
];
res = If[direction=="Inwards",
Append[thread,res["NextEdge"]]
,
Prepend[thread,res["NextEdge"]]
];

If[Length[res]>= maxThread,Throw[thread]];
res
];

(* pairs of edges which can form threads *)
edgeMatch[graph_,node_] := Module[{ng,edges,refNode,level,nbrs,nodeTimes,edgePairs,bc},
edges=EdgeList[graph,_\[UndirectedEdge]node];
nbrs=Complement[VertexList[Graph[edges]],{node}];
bc= Association@Map[#-> AnnotationValue[{graph,#},"BoundaryCoordinates"]&,nbrs];
If[EvenQ[Length[nbrs]],
level=bc[node]["Level"];
refNode=First[nbrs];
nodeTimes =Association@Map[node \[UndirectedEdge]# -> edgeClockTime[graph,refNode \[UndirectedEdge] node,node \[UndirectedEdge]#]&,nbrs];
nodeTimes=Sort[nodeTimes];
edgePairs= Transpose[Partition[Keys[nodeTimes],Length[nbrs]/2]];
edgePairs = Map[sortEdgeListByLevel[graph,#]&,edgePairs]
(* by construction, these pairs all radiate from the node *)
,
(* odd # of edges *)
(*Print["Nonhex ", node, " ",Length[nbrs]];
*)edgePairs=edgeMatchByDeviation[graph,node]
];
edgePairs
];
edgeMatchByDeviation[graph_,node_]:= 
Module[{ng,edges,nbrs,edgePairs,maxPairs,leftOver,bc,edgePairDeviations},
bc[n_]:=  AnnotationValue[{graph,n},"BoundaryCoordinates"];
edges=EdgeList[graph,_\[UndirectedEdge]node];
nbrs=Complement[VertexList[Graph[edges]],{node}];
maxPairs=Floor[Length[nbrs]/2];
edgePairs= Subsets[edges,{2}];
edgePairDeviations = Association@Map[#->edgeDeviation[graph,#[[1]],#[[2]] ]&,edgePairs];
edgePairDeviations= Sort[edgePairDeviations];
edgePairs=Keys[edgePairDeviations];
edgePairs= removeDuplicateEdges[edgePairs];
edgePairs= Take[edgePairs,UpTo[maxPairs]];
If[edgePairDeviations[Last[edgePairs]]>2,Print["Large deviation ",edgePairs]];
If[OddQ[Length[nbrs]],
leftOver= List/@Complement[edges,Flatten[edgePairs]];
edgePairs=Join[edgePairs,leftOver]
];
edgePairs
];
removeDuplicateEdges [edgePairList_] := Module[{used,i,nextPair},
used = {};nextPair={};
For[i=1,i<=Length[edgePairList],i++,
nextPair= Append[nextPair,Complement[edgePairList[[i]],used]];
used=Join[used,edgePairList[[i]]];
];
Select[nextPair,Length[#]==2&]
];
sortEdgeListByLevel[graph_,{node_\[UndirectedEdge]a_,node_ \[UndirectedEdge]b_}] := Module[{bc},
bc[n_]:=  AnnotationValue[{graph,n},"BoundaryCoordinates"];
(
If[(bc[a]["Level"]< bc[b]["Level"] )||((bc[a]["Level"]== bc[b]["Level"]) && bc[a]["Y"]< bc[b]["Y"]),{a\[UndirectedEdge]node,node \[UndirectedEdge]b},{b\[UndirectedEdge]node,node \[UndirectedEdge]a }])
];


(* ::Section:: *)
(*Thread mangling*)


(* ::Input::Initialization:: *)

displayHeadFamilies[head_] := Row[Map[displayHeadFamily[head,#]&,familiesOfHead[head]]];
displayHeadFamily[head_,familyID_,imageSize_:150] := 
Module[{nodesInThreads,threadList,lineNodes,linesXY,pointsXY},
threadList=Select[head["Threads"],#Family==familyID&];
lineNodes=Association@Map[#ThreadID->#["ThreadNodes"]&,threadList];
nodesInThreads=Map[head["Seeds"],lineNodes,{2}];
linesXY=Map[Line,nodesInThreads];
pointsXY=Map[Point,nodesInThreads];
Show[
Image[head["Image"],ImageSize->imageSize],
Graphics[
{
{LightBlue,Values[linesXY],Values[pointsXY]}
}]
]
];

graphicsHeadLines[head_,familyID_,imageSize_:150] := 
Module[{nodesInThreads,threadList,lineNodes,linesXY,pointsXY},
threadList=Select[head["Threads"],#Family==familyID&];
lineNodes=Association@Map[#ThreadID->#["ThreadNodes"]&,threadList];
nodesInThreads=Map[head["Seeds"],lineNodes,{2}];
linesXY=Map[Line,nodesInThreads];
pointsXY=Map[Point,nodesInThreads];
Graphics[
{
{LightBlue,Values[linesXY],Values[pointsXY]}
}]
];


listCurrentPossibleEdgesThrough[head_,builtFromNode_,throughNode_] := Module[{g,threadPossibleEdges},
g = EdgeDelete[head["Graph"],Flatten[#ThreadEdges&/@head["Threads"]]];
threadPossibleEdges=Flatten@Values@threadsThroughNode[g,builtFromNode,throughNode];
threadPossibleEdges
];


displayHeadThreads[head_,displayThreadEdges_] := Module[{threadPossibles,threadPossibleEdges,g},
g = graphPrunedByThreadEdges[head];
Show[
Graphics[Rectangle[{0,0},ImageDimensions[head["Image"]]],ImageSize->300],
Image[head["Image"],ImageSize->300],
HighlightGraph[Graph[g,BaseStyle->White],displayThreadEdges]
]
];



(* ::Section:: *)
(*Thread moving*)


(* ::Input::Initialization:: *)
addFamily[head_] := Module[{res=head},res["FamilyCount"]=res["FamilyCount"]+1;res];
familiesOfHead[head_]:=Range[10];
nextFamilyID[head_,familyID_] := If[familyID>=head["FamilyCount"],1,familyID+1];

threadsByFamily[head_]:= GroupBy[head["Threads"],#Family&];
expandThread[thread_] := Map[Append[KeyDrop[thread,{"ThreadNodes"}],"Node"->#]&,thread["ThreadNodes"]]

moveThreadToNextFamily[head_,currentThreadID_] := Module[{res,currentThreadPosition,threads,thread,familyID},
threads=head["Threads"];
currentThreadPosition=fom@fom@Position[threads,<|___,"ThreadID"->currentThreadID,___|>];
thread=head["Threads"][[currentThreadPosition]];
familyID=thread["Family"];
familyID=nextFamilyID[head,familyID];
thread["Family"]=familyID;
threads[[currentThreadPosition]]=thread;
res=head;
res["Threads"]=threads;
res
];



addThreadToFamily[headArg_,currentFamily_,threadEdges_] := 
Module[{head=headArg,threadNodes,familyThreads,threadID},
threadNodes = VertexList[threadEdges];
familyThreads = Select[head["Threads"],#Family==currentFamily&];
threadID = If[Length[familyThreads]==0,1,Max[#ThreadID&/@familyThreads]+1];
head["Threads"]=Append[head["Threads"],
<|"Family"->currentFamily,"ThreadID"->threadID,"ThreadNodes"->threadNodes,"ThreadEdges"->threadEdges|>];
head
];

deleteFamily[headArg_,currentFamily_] := Module[{head=headArg},
head["Threads"]=Select[head["Threads"],#Family!=currentFamily&];
head
];


(* ::Input::Initialization:: *)
deleteMostRecentThread[headArg_] := Module[{head=headArg},
head["Threads"]=Drop[head["Threads"],-1];
head
];



(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
threadMover[headArg_] := DynamicModule[{
currentFamily=1,
currentThreadID=Missing[],
mode="ThreadSelect",
nearestNode=Missing[],
nodeLookup,
point,familyCount,
head=headArg},
nodeLookup[head_]:= First/@GroupBy[Flatten@Map[expandThread,head["Threads"]],(#Node&)];
familyCount=3;
Column@{
(*SetterBar[Dynamic[mode],{"ThreadSelect"->"Thread select"}],*)mode="ThreadSelect";
Button["Add family",
familyCount++;head["FamilyCount"]=familyCount],
Button["Move thread to next family",
head=moveThreadToNextFamily[head,currentThreadID]],
Button["Save",
saveHead[head]
],
Dynamic@SetterBar[Dynamic[currentFamily],Range[familyCount]],
(*Dynamic[StringJoin["Current thread ",ToString[Select[head["Threads"],#ThreadID==currentThreadID&]]]],
Dynamic[StringJoin["Threads by family ",ToString[Length/@threadsByFamily[head]]]],
*)

EventHandler[
Dynamic[
displayHead[head,currentFamily,nearestNode,currentThreadID] ],
"MouseDown":>(
point= MousePosition["Graphics"];
nearestNode=Last@Last@NearestMeshCells[{head["Mesh"],0},point];
(*If[mode=="ThreadSelect",
currentThreadID=nodeLookup[head][nearestNode]["ThreadID"]];
If[mode!="ThreadSelect",
currentThreadID=Missing[]];*)
currentThreadID=nodeLookup[head][nearestNode]["ThreadID"];
)],
Dynamic[displayHeadFamilies[head]]
}];


(* ::Section:: *)
(*Thread creation*)


(* ::Input::Initialization:: *)
threadCreator[headArg_] := DynamicModule[{
currentFamily=3,
nearestNode=Missing[],
point,familyCount=10,
maxThread=20,maxDivergence=1,
threadsThroughNode,
displayEdges={},
currentThread,
head=headArg},

threadsThroughNode[node_] := headThreadsAtNode[head,node,maxThread,maxDivergence];
currentThread[node_] :=First[threadsThroughNode[node]];

Column@{
Row[{"Max thread ", Dynamic[maxThread],Slider[Dynamic[maxThread],{1,30,1}]}], 
Row[{"Max divergence ", Dynamic[maxDivergence],Slider[Dynamic[maxDivergence],{1,3}]}], 
Row[{
Button[StringJoin["Adjust thread"],
displayEdges=currentThread[nearestNode];
],
Button[StringJoin["Save head",ToString@head["ID"]],
saveHead[head]
],
Button[StringJoin[ "Add to family "],
head=addThreadToFamily[head,currentFamily,displayEdges ]
],
Button[StringJoin[ "Delete most recent thread"],
head=deleteMostRecentThread[head ]
],
Button[StringJoin[ "Delete family "],
head=deleteFamily[head,currentFamily ]
]
}],
Row[{"Current family",Dynamic@SetterBar[Dynamic[currentFamily],Range[familyCount]]}],
Dynamic[{maxThread,displayEdges}],
EventHandler[
Dynamic[
displayHeadThreads[head,displayEdges]] ,
"MouseDown":>(
point= MousePosition["Graphics"];
nearestNode=Last@Last@NearestMeshCells[{head["Mesh"],0},point];
displayEdges=currentThread[nearestNode])],
Dynamic[displayHeadFamilies[head]]
}];


(* ::Section:: *)
(*Family pruning*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
headFamilyThreads[head_,familyID_]:= Select[head["Threads"],#Family==familyID&];
headFamilyEdges[head_,familyID_] := #ThreadEdges&/@headFamilyThreads[head,familyID];
threadText[head_,thread_]:= Text[thread["ThreadID"],AnnotationValue[{head["Graph"],First[thread["ThreadNodes"]]},VertexCoordinates],Background->LightBlue];


pruneThread[head_,state_] := Module[{res=head,threadtoPrune,nodeToPrune,wantedThreadNodes,orphanThreadNodes},
If[!KeyMemberQ[state,"SelectedEdge"],Print["Nothing selected"];Return[head]];
threadtoPrune=state["Edges"][[state["SelectedEdge"]]];
nodeToPrune=state["SelectedNode"];
nodePosition = Position[threadtoPrune["ThreadNodes"],nodeToPrune];
If[nodePosition=={},Print["Can't find ", nodeToPrune, " in ", threadtoPrune]];
nodePosition=First@First[nodePosition];
If[nodePosition==Length[threadtoPrune["ThreadNodes"]],Return[res]];
If[nodePosition==1,
wantedThreadNodes=Drop[threadtoPrune["ThreadNodes"],1];orphanThreadNodes=Missing[],
wantedThreadNodes=Take[threadtoPrune["ThreadNodes"],nodePosition];orphanThreadNodes=Drop[threadtoPrune["ThreadNodes"],nodePosition]
];

wantedThread=threadtoPrune;
wantedThread["ThreadNodes"]=wantedThreadNodes;wantedThread["ThreadEdges"]=UndirectedEdge@@@Partition[wantedThreadNodes,2,1];
res["Threads"]=Select[res["Threads"],!(#Family==threadtoPrune["Family"] && #ThreadID==threadtoPrune["ThreadID"])&];
res["Threads"]=Append[res["Threads"],wantedThread];

res
];



(* ::Input::Initialization:: *)
threadsGivenNode[head_,familyID_,node_] := Select[head["Threads"],MemberQ[#ThreadNodes,node]&& #Family==familyID&];

showHeadState[head_,state_] := Module[{familyID,selectedThread,node},
familyID=state["CurrentFamily"];
selectedThread=If[!KeyMemberQ[state,"SelectedEdge"]|| MissingQ[state["SelectedEdge"]],{},
state["Edges"][[state["SelectedEdge"]]]];
node=If[!KeyMemberQ[state,"SelectedNode"],{},state["SelectedNode"]];
Show[HighlightGraph[Graph[head["Graph"],VertexSize->Large],
{Flatten[headFamilyEdges[head,familyID]],
selectedThread["ThreadEdges"]
(*,state["SelectedNode"]*)
},ImageSize->400],Graphics[{
Map[threadText[head,#]&,headFamilyThreads[head,familyID]],
{PointSize[Large],Red,Point[AnnotationValue[{head["Graph"],node},VertexCoordinates]]}
}
]
]
];

onMouseDown[head_,state_] := Module[{point,res=state},
point= MousePosition["Graphics"];
nearestNode=If[point===None,Missing[],
Last@Last@NearestMeshCells[{head["Mesh"],0},point]];
res = Append[state,<|"SelectedNode"->nearestNode|>];
res= Append[res,<|"Edges"->threadsGivenNode[head,state["CurrentFamily"],nearestNode],"SelectedEdge"->1|>];

res
];
onFamilySet[head_,state_,currentFamily_] :=Module[{res=state},
res= Append[res,"CurrentFamily"->currentFamily];
res = KeyDrop[res,{"SelectedNode","Edges"}];
res
];
switchEdge[state_] := Module[{res=state},
If[!KeyMemberQ[state,"SelectedEdge"],Return[state]];
res["SelectedEdge"]=If[res["SelectedEdge"]==Length[res["Edges"]],1,res["SelectedEdge"]+1];
res
];
familyPruner[headArg_] := DynamicModule[{
head=headArg,
state=<|"CurrentFamily"->3,"SelectedNode"->1|>
},
Column@{
Row[{
Button[StringJoin["Save head",ToString@head["ID"]],
saveHead[head]
]
}],
Row[{"Current family",Dynamic@SetterBar[Dynamic[currentFamily,(currentFamily=#;state=onFamilySet[head,state,currentFamily])&],Range[familyCount]]}],
Row[{
Button["Switch thread",state=switchEdge[state]]
}],
Row[{
Button["Prune thread",
head=pruneThread[head,state];state=KeyDrop[state,{"SelectedEdge","SelectedNode"}];globalHead=head;globalState=state]
}],
(*Dynamic[state],
*)EventHandler[
Dynamic[
showHeadState[head,state]] ,
"MouseDown":> (globalState=state = onMouseDown[head,state] )]
}];



(* ::Input:: *)
(*familyPruner[head667]*)


(* ::Input:: *)
(*head667=loadHead[667];*)


(* ::Input:: *)
(*displayHeadFamilies[head667]*)


(* ::Input::Initialization:: *)
localEdges [head_,node_] := Map[<|"ThreadEdges"->#|>&,Values[anyHeadThreadsAtNode[head,node,20,1]]]
onAddingMouseDown[head_,state_] := Module[{point,res=state},
point= MousePosition["Graphics"];
nearestNode=If[point===None,Missing[],
Last@Last@NearestMeshCells[{head["Mesh"],0},point]];
res = Append[state,<|"SelectedNode"->nearestNode|>];
res= Append[res,<|"Edges"->localEdges[head,nearestNode],"SelectedEdge"->1|>];

res
];

newAddThreadToFamily[headArg_,state_] := Module[{head=headArg,familyID,threadEdges,threadID,familyThreads,threadNodes},
familyID=state["CurrentFamily"];
threadEdges=If[!KeyMemberQ[state,"SelectedEdge"]|| MissingQ[state["SelectedEdge"]],{},
state["Edges"][[state["SelectedEdge"]]]]["ThreadEdges"];
threadNodes = VertexList[threadEdges];
familyThreads = Select[head["Threads"],#Family==currentFamily&];
threadID = If[Length[familyThreads]==0,1,Max[#ThreadID&/@familyThreads]+1];
head["Threads"]=Append[head["Threads"],
<|"Family"->currentFamily,"ThreadID"->threadID,"ThreadNodes"->threadNodes,"ThreadEdges"->threadEdges|>];
head
];


familyAdder[headArg_] := DynamicModule[{
head=headArg,
state=<|"CurrentFamily"->3,"SelectedNode"->1|>
},
Column@{
Row[{
Button[StringJoin["Save head",ToString@head["ID"]],
saveHead[head]
]
}],
Row[{"Current family",Dynamic@SetterBar[Dynamic[currentFamily,(currentFamily=#;state=onFamilySet[head,state,currentFamily])&],Range[familyCount]]}],
Row[{
Button["Switch thread",state=switchEdge[state]]
}],
Button[StringJoin[ "Add to family "],
head=newAddThreadToFamily[head,state ];globalHead=head;
],
Row[{
Button["Prune thread",
head=pruneThread[head,state];state=KeyDrop[state,{"SelectedEdge","SelectedNode"}];globalHead=head;globalState=state]
}],
EventHandler[
Dynamic[
showHeadState[head,state]] ,
"MouseDown":> (globalState=state = onAddingMouseDown[head,state] )]
}];


