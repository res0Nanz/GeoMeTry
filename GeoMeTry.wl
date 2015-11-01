(* ::Package:: *)

BeginPackage["GeoMeTry`"];


GeoMeTry::usage = "Let's try~";
If[Not@ValueQ[JacobiMatrix::usage], JacobiMatrix::usage =
"JacobiMatrix[{f1, f2, ...}, {\!\(\*SubscriptBox[\(\[Xi]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Xi]\), \(2\)]\), ...}] returns the Jacobi matrix \
of the vactor function."];

If[Not@ValueQ[MinkowskiMetric::usage], MinkowskiMetric::usage =
"MinkowskiMetric[dim, sig : -1] gives the metric of dim dimensional \
Minkowski space with signatue sig."];

If[Not@ValueQ[Metric::usage], Metric::usage =
"Metric[trans, coor] gives the metric under coor coodinates."];

If[Not@ValueQ[ChristoffelSymbol::usage], ChristoffelSymbol::usage =
"ChristoffelSymbol[g, {\!\(\*SubscriptBox[\(\[Xi]\), \(1\)]\), \!\(\*SubscriptBox[\(\[Xi]\), \(2\)]\), ...}, \[Lambda], \[Mu], \[Nu]] gives Christoffel \
Symbol \!\(\*SuperscriptBox[SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\)], \(\[Lambda]\)]\), under \[Xi] space with metric g."];

If[Not@ValueQ[ChristoffelTable::usage], ChristoffelTable::usage =
"ChristoffelTable[g, {\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(x\), \(2\)]\), ...}]"];


If[Not@ValueQ[BackgroundMetric::usage], BackgroundMetric::usage =
"BackgroundMetric is an option for metric related functions to configure \
the origional metric of transformations. The default value is Euclidean."];
If[Not@ValueQ[Euclidean::usage], Euclidean::usage =
"If BackgroundMetric is set to Euclidean, functions will use identity \
matrix as origional metric for transformations."];
If[Not@ValueQ[MinkowskiPlus::usage], MinkowskiPlus::usage =
"If BackgroundMetric is set to MinkowskiPlus, functions will use Minkowski \
metric with signature (+, -, ...) as origional metric for transformations."];
If[Not@ValueQ[MinkowskiMinus::usage], MinkowskiMinus::usage =
"If BackgroundMetric is set to MinkowskiMinus, functions will use Minkowski \
metric with signature (-, +, ...) as origional metric for transformations."];

If[Not@ValueQ[Inverted::usage], Inverted::usage =
"Inverted is a boolean option for transformation releted functions to know \
which direction the transformation going. Its value represents the given \
symbolic variables should be treated as parameters whether before the \
transformation or not."];

Gmt::opts = "`1`";


Begin["`Private`"];


Options[GmtTransformOptions]={BackgroundMetric->Euclidean,Inverted->False};


GmtMetricTypes={Euclidean,MinkowskiPlus,MinkowskiMinus};


GmtGetMetric=Switch[#2,
	Euclidean,IdentityMatrix[#1],
	MinkowskiPlus,MinkowskiMetric[#1,+1],
	MinkowskiMinus,MinkowskiMetric[#1],
	MinkowskiMetric,Message[Gmt::opts,"'MinkowskiMetric' is a function name, you may use 'MinkowskiMinus' or 'MinkowskiPlus' instead."],
	_,#2]&;


JacobiMatrix=Simplify[Table[D[o,i],{o,#1},{i,#2}]]&;


MinkowskiMetric[n_,sig_:-1] :=
	DiagonalMatrix[{Sign[sig]}~Join~Table[-Sign[sig],{i,n-1}]];


Metric[trans_,coor_,OptionsPattern[GmtTransformOptions]]:=Simplify[Check[
	Transpose[#].GmtGetMetric[Length[coor],OptionValue[BackgroundMetric]].
	#&[If[OptionValue[Inverted],Inverse,#&][JacobiMatrix[trans,coor]]],
	Abort[]]];


ChristoffelSymbol[g_,x_List,u_Integer,m_Integer,n_Integer]/;
	MatrixQ[g]&&Dimensions[g]==Function[a,{a,a}][Dimensions[x][[1]]]:=
		Simplify[1/2 Inverse[g][[u]].(D[g[[m]],x[[n]]]+D[g[[n]],x[[m]]]-Grad[g[[m,n]],x])];
ChristoffelSymbol[trans_,coor_,u_,m_,n_,OptionsPattern[GmtTransformOptions]]/;AllTrue[VectorQ,{trans,coor}]&&Length[trans]==Length[coor]:=
	ChristoffelSymbol[Metric[trans,coor],coor,u,m,n];


ChristoffelTable[trans_,coor_,opts__:{}]:=
	Table[ChristoffelSymbol@@{trans,coor,u,m,n}~Join~Flatten[{opts}],{u,Length[coor]},{m,Length[coor]},{n,Length[coor]}];


End[];


EndPackage[];
