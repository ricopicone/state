(* ::Package:: *)

(* ::Text:: *)
(*Compute the linear state model (a,b,c,d) and transfer functions, for a MIMO system.*)
(*Note: this cell is an initialization cell.*)


BeginPackage["State`"];

State::usage =
        "State[
	InVars,         (* input variable names.  e.g.  vS   *)
	StVarElEqns,    (* state equations for state variable.  e.g.  vM' == 1/M fM   *)
	OtherElEqns,    (* other elemental equations. 
                                          e.g. v1 \[Equal] Km o2, or tJm = Jm oJm'   *)
	Constraints,    (* constraint expressions.  e.g.  fM \[Rule] fD - f4   *)
	OutputVars      (* output variables.  e.g.  fM   *)
    ]
Computes the state equations, StEqn. 
Also, gives the a, b, c, d, e, f matricies and transfer functions, TfM, for a linear state model.
StateVers=1.3";

State[InVarsLo_,
	StVarElEqnsLo_,
	OtherElEqnsLo_,
	ConstraintsLo_,
	OutputVarsLo_] :=Module[{i, j, St2,E3, Co2,StateEquation,StateEqsFinal,OutputEqsFinal, 
StVarsLo,StVarsLoT,OtherPriVarsLo,OtherPriVarsLoT,OtherElEqnsLoT,SecVars,SecVarsT,OutputVarsLoT,
ConstraintsLoT,StVarElEqnsLoT,InVarsLoT,t,
nSt,nIn,nOut,aa,bb,cc,dd,ee,ff, bbp, ddp,TT},

(* Find lists of state, other primary,  secondary, input, and output variables *)
StVarsLo=Map[Part[#,1]&,Map[Part[#,1]&,StVarElEqnsLo]];
StVarsLoT=StVarsLo/.Map[#->Apply[#,{t}]&,StVarsLo];
OtherPriVarsLo=Map[Part[#,1]&,OtherElEqnsLo];
OtherPriVarsLoT=OtherPriVarsLo/.Map[#->Apply[#,{t}]&,OtherPriVarsLo];
SecVars=Map[Part[#,1]&,ConstraintsLo];
SecVarsT=SecVars/.Map[#->Apply[#,{t}]&,SecVars];
InVarsLoT=Map[Apply[#,{t}]&,InVarsLo];
OutputVarsLoT=Map[Apply[#,{t}]&,OutputVarsLo];

(* Transform input variables, state and other primary elemental equations, and constraints into functions of time *)
StVarElEqnsLoT=StVarElEqnsLo/.Map[#->Apply[#,{t}]&,Map[Part[#,1]&,StVarElEqnsLo]];
StVarElEqnsLoT=StVarElEqnsLoT/.Map[#->Apply[#,{t}]&,SecVars];
OtherPriVarsLoT=OtherPriVarsLo/.Map[#->Apply[#,{t}]&,{SecVars,OtherPriVarsLo}//Flatten];
OtherElEqnsLoT=OtherElEqnsLo/.Map[#->Apply[#,{t}]&,Map[Part[#,1]&,OtherElEqnsLo]];
OtherElEqnsLoT=OtherElEqnsLoT/.Map[#->Apply[#,{t}]&,SecVars];
OtherElEqnsLoT=Map[StringReplace[ToString[#,InputForm],"["<>ToString[t]<>"]]"->"]["<>ToString[t]<>"]"]&,OtherElEqnsLoT]//ToExpression;
ConstraintsLoT=ConstraintsLo/.Map[#->Apply[#,{t}]&,{StVarsLo,SecVars,OtherPriVarsLo,InVarsLo}//Flatten];

(*  Substitute cut-set and tie-set equations (along with any necessary derivatives) into all elemental equations. Solve for the Other primary variables,in terms of state variables  *)
St2=StVarElEqnsLoT/.Flatten[{ConstraintsLoT,D[ConstraintsLoT,t]}];
Co2=OtherElEqnsLoT/.Flatten[{ConstraintsLoT,D[ConstraintsLoT,t]}];
OtherPriVarsLoT=OtherPriVarsLoT;
E3=Solve[Flatten[{Co2,D[Co2,t]}],Flatten[{OtherPriVarsLoT,D[OtherPriVarsLoT,t]}]][[1]]//
Simplify;

(*  Eliminate the non-state variables in the state equations  *)

StateEquation=St2/.E3//
Solve[#,D[StVarsLoT,t]][[1]]& //
Simplify;
StateEqsFinal=D[StVarsLoT,t]/.StateEquation;
OutputEqsFinal=OutputVarsLoT/.ConstraintsLoT/.E3/.StateEquation//Simplify;

(*  Extract the state matricies: a, b, c, d, e, and f  *)

nSt=Length[StVarsLoT];
nIn=Length[InVarsLoT];
nOut=Length[OutputEqsFinal];
Clear[ aa,bb,cc,dd];
aa=Table[0,{nSt},{nSt}];
For[i=1,i<=nSt,i++,
For[j=1, j<=nSt,j++,
aa[[i,j]]=D[ StateEqsFinal[[i]],StVarsLoT[[j]] ];
];
];
bb=Table[0,{nSt},{nIn}];
For[i=1,i<=nSt,i++,
For[j=1, j<=nIn,j++,
bb[[i,j]]=D[ StateEqsFinal[[i]],InVarsLoT[[j]] ];
];
];
cc=Table[0,{nOut},{nSt}];
For[i=1,i<=nOut,i++,
For[j=1, j<=nSt,j++,
cc[[i,j]]=D[ OutputEqsFinal[[i]],StVarsLoT[[j]] ];
];
];
dd=Table[0,{nOut},{nIn}];
For[i=1,i<=nOut,i++,
For[j=1, j<=nIn,j++,
dd[[i,j]]=D[ OutputEqsFinal[[i]],InVarsLoT[[j]] ];
];
];
ee=Table[0,{nSt},{nIn}];
For[i=1,i<=nSt,i++,
For[j=1, j<=nIn,j++,
ee[[i,j]]=D[ StateEqsFinal[[i]],D[InVarsLoT,t][[j]] ];
];
];
ff=Table[0,{nOut},{nIn}];
For[i=1,i<=nOut,i++,
For[j=1, j<=nIn,j++,
ff[[i,j]]=D[ OutputEqsFinal[[i]],D[InVarsLoT,t][[j]] ];
];
];

(* Compute the transfer function matrix, 
	accounting for possible nonstandard state model *)
bbp=(aa.ee+bb);
ddp=(cc.ee+dd);
TT=cc.Inverse[s IdentityMatrix[nSt]-aa].bbp+ddp+ff s//Simplify;
a=aa;
b=bb;
c=cc;
d=dd;
e=ee;
f=ff;
TfM=TT;
StVars=StVarsLoT;
StEqn=StateEqsFinal;
StateVers=1.3;
{a,b,c,d,e,f,TfM,StEqn,StateVers}
(* 
a=OutputEqsFinal;
{a}*)
];
EndPackage[ ];


(* ::Output:: *)
(*"StateModel`Private`"*)


(* ::Output:: *)
(*"StateModel`Private`"*)
