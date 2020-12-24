(* ::Package:: *)

(* Quadratic Programming Package *)
(* 2011-05-27 - RJF - Adapted from AMS-511 *)
(* 2014-12-19 - RJF - Improved usage message to alert users that 1/2 \[Times] quad is used internally. *)
(*                    Changed ToNminize to ToFindMinimum. *)

BeginPackage["QuadraticProgramming`"]

(* Define usage attributes for public functions *)

xToFindMinimum::usage = 
	"xToFindMinimum[{lin, quad}, cons, rhs, bounds]\n" <>
	"     The objective function minimized is \"0.5 x.quad.x + lin.x\".";

xQuadraticProgramming::usage = 
    "xQuadraticProgramming[{lin, quad}, cons, rhs, bound]\n" <>
	"     The objective function minimized is \"0.5 x.quad.x + lin.x\".";

xDisplayQP::usage = 
    "xDisplayQP[name, {rows, cols}, {lin, quad}, cons, rhs, bounds]\n <>";
	"     The objective function minimized is \"0.5 x.quad.x + lin.x\".";

(* Define all functions in the private context *)

Begin["`Private`"]

xToFindMinimum[{vnLin_, mnQuad_}, mnCons_, mnRhs_, mnBounds_] := Module[
      {eAllCons, vxAns,qBounds, vxBounds, qCons, vxCons, qLin, qQuad, eObjFunc,vxSol, vxVar},
	  qLin=Not[(vnLin=={})\[Or](And@@(#==0&/@vnLin))];
	  qQuad=Not[(mnQuad=={})\[Or](And @@ (#==0&/@Flatten[mnQuad]))];
      qCons=mnCons!={};
      qBounds=Not[(mnBounds=={})\[Or]((And@@(#==-\[Infinity] &/@(First/@ mnBounds)))\[And](And@@(#==\[Infinity]&/@(Last/@ mnBounds))))];
      vxVar=Table[Unique["x"],{If[qLin,Length[vnLin],Length[mnQuad]]}]; 
      eObjFunc=If[qQuad, 0.5(vxVar.mnQuad.vxVar), 0]+If[qLin, vnLin.vxVar, 0];
      vxCons = If[qCons,
          Table[
             Which[
                #3 == -1, #1.vxVar <= #2, 
                #3 == 0, #1.vxVar == #2,
                #3 == 1, #1.vxVar >= #2
            ] &[mnCons[[i, All]],mnRhs[[i, 1]], mnRhs[[i, 2]]],
            {i, 1, Length[mnRhs]}
         ],
         {}
      ];
      vxBounds = If[qBounds,
          Table[
             If[mnBounds[[i, 1]] ==   mnBounds[[i, 2]],
             vxVar[[i]]==mnBounds[[i, 1]],
             mnBounds[[i, 1]]<=vxVar[[i]]<= mnBounds[[i, 2]]
          ],
          {i, 1, Length[vxVar]}
          ],
          {}
     ];
     eAllCons=And@@Join[vxCons, vxBounds];
     {{eObjFunc,eAllCons}, vxVar}
];

xQuadraticProgramming[{vnLin_, mnQuad_}, mnCons_, mnRhs_, mnBounds_, opts:OptionsPattern[]] := Module[
      {eAllCons, vxAns,qBounds, vxBounds, qCons, vxCons, qLin, qQuad, eObjFunc, vxSol, vxVar, x},
	  qLin=Not[(vnLin=={})\[Or](And@@(#==0&/@vnLin))];
	  qQuad=Not[(mnQuad=={})\[Or](And @@ (#==0&/@Flatten[mnQuad]))];
      qCons=mnCons!={};
	  qBounds=Not[(mnBounds=={})\[Or]((And@@(#==-\[Infinity] &/@(First/@ mnBounds)))\[And](And@@(#==\[Infinity]&/@(Last/@ mnBounds))))];
      vxVar=Table[x[i],{i, 1, If[qLin,Length[vnLin],Length[mnQuad]]}]; 
	  eObjFunc=If[qQuad, 0.5(vxVar.mnQuad.vxVar), 0]+If[qLin, vnLin.vxVar, 0];
      vxCons = If[qCons,
          Table[
             Which[
                #3 == -1, #1.vxVar <= #2, 
                #3 == 0, #1.vxVar == #2,
                #3 == 1, #1.vxVar >= #2
            ] &[mnCons[[i, All]],mnRhs[[i, 1]], mnRhs[[i, 2]]],
            {i, 1, Length[mnRhs]}
         ],
         {}
      ];
      vxBounds = If[qBounds,
          Table[
             If[mnBounds[[i, 1]] ==   mnBounds[[i, 2]],
             vxVar[[i]]==mnBounds[[i, 1]],
             mnBounds[[i, 1]]<=vxVar[[i]]<= mnBounds[[i, 2]]
          ],
          {i, 1, Length[vxVar]}
          ],
          {}
     ];
     eAllCons=And@@Join[vxCons, vxBounds];
     vxAns=FindMinimum[{eObjFunc,eAllCons}, vxVar, Evaluate[FilterRules[{opts},Options[FindMinimum]]]];
     vxSol={vxAns[[1]],vxVar/.vxAns[[2]]};
     vxSol
];

xDisplayObjective[{vsRow_, vsCol_}, {vnLin_, mnQuad_}] := MatrixForm[
      {
        {MatrixForm[Transpose[{vnLin}], TableHeadings -> {vsCol, {"  "}}]},
        {MatrixForm[mnQuad, TableHeadings -> {vsCol, vsCol}]}
        },
      TableHeadings -> {{"Lin", "Quad"}, {" "}}
      ];
      
xDisplayConstraints[{vsRow_, vsCol_}, mnConstraint_, mnRhs_] := MatrixForm[
      Transpose[
        Join[Transpose[
            mnConstraint], {{"<=", "==", ">="}[[
                  Sign[#] + 2]] & /@ (Last /@ 
                  mnRhs)}, {First /@ mnRhs}]],
      TableHeadings -> {vsRow, Join[vsCol, {"  ", "RHS"}]}
      ];
      
xDisplayBounds[vsCol_, mnBounds_] := MatrixForm[
      mnBounds,
      TableHeadings -> {vsCol, {"LO", "UP"}}
      ];
      
xDisplayQP[sName_, {vsRow_, vsCol_}, {vnLin_, mnQuad_}, mnCons_, mnRhs_, 
      mnBounds_] :=
    MatrixForm[
      {
        {xDisplayObjective[{vsRow, vsCol}, {vnLin, mnQuad}]},
        {If[mnCons == {}, MatrixForm[{}], 
            xDisplayConstraints[{vsRow, vsCol}, mnCons, mnRhs]]},
        {xDisplayBounds[vsCol, mnBounds]}
        },
      TableHeadings -> {{"Objective", "Constraints", "Bounds"}, {sName}}
      ];

End[]

EndPackage[]



