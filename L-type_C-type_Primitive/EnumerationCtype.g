n:=4;
FileSave:=Concatenation("DATA_TEMP/ListCtypeAdj", String(n));
if IsExistingFilePlusTouch(FileSave)=false then
  ListTotal:=CTYP_EnumerateCtypes(n);
  SaveDataToFilePlusTouch(FileSave, ListTotal);
else
  ListTotal:=ReadAsFunction(FileSave)();
fi;
Print("We have ListTotal\n");



FileSave:=Concatenation("DATA_TEMP/Ctype_Complex", String(n));
if IsExistingFilePlusTouch(FileSave)=false then
  TheComplex:=CTYP_FullEnumerationOfComplex(n, ListTotal);
  SaveDataToFilePlusTouch(FileSave, TheComplex);
else
  TheComplex:=ReadAsFunction(FileSave)();
fi;
Print("We have TheComplex\n");
ListDimCtype:=List(TheComplex, Length);
Print("ListDimCtype=", ListDimCtype, "\n");
Print("TotDim=", Sum(ListDimCtype), "\n");



PrintListDim:=function()
  local TheDim, ListLine, eDim, eLine, ListHeader, eFileTex, nbCol;
  TheDim:=n*(n+1)/2;
  ListLine:=[];
  for eDim in [1..TheDim]
  do
    eLine:=[String(eDim), String(ListDimCtype[TheDim + 1 - eDim])];
    Add(ListLine, eLine);
  od;
  ListHeader:=["n", "nr. iso. edge"];
  eFileTex:="Tables/Table_NumberCtypeDomain_pre.tex";
  nbCol:=2;
  VOR_PrintListLine(ListLine, ListHeader, nbCol, eFileTex);
end;
PrintListDim();



FileSave:=Concatenation("DATA_TEMP/Ctype_ListSpanCone", String(n));
#RemoveFileIfExist(FileSave);
if IsExistingFilePlusTouch(FileSave)=false then
  TheBasis:=CTYP_GetBasis(n);
  nbCell:=Length(ListTotal);
  ListSpann:=[];
  for iCell in [1..nbCell]
  do
    Print("Spanning iCell=", iCell, " / ", nbCell, "\n");
    TheSpann:=CTYP_SpanCone(ListTotal[iCell].TheCtype, TheBasis, n);
    Add(ListSpann, TheSpann);
  od;
  SaveDataToFilePlusTouch(FileSave, ListSpann);
else
  ListSpann:=ReadAsFunction(FileSave)();
fi;
Print("We have ListSpann\n");


ArithmeticEquivalenceGramFamilies:=function(ListGram1, ListGram2)
    local SumGram1, SumGram2, test, ListGram1_map, ListGram1_mapRed, ListGram2_Red;
    SumGram1:=Sum(ListGram1);
    SumGram2:=Sum(ListGram2);
    test:=ArithmeticIsomorphismSemidefinite([SumGram1], [SumGram2]);
    if test=false then
        return false;
    fi;
    if test * SumGram1 * TransposedMat(test) <> SumGram2 then
        Error("Equivalence algorithm error");
    fi;
    ListGram1_map:=List(ListGram1, x->test*x*TransposedMat(test));
    ListGram1_mapRed:=List(ListGram1_map, RemoveFractionMatrix);
    ListGram2_Red:=List(ListGram2, RemoveFractionMatrix);
    if Set(ListGram1_mapRed) <> Set(ListGram2_Red) then
        return false;
    fi;
    return true;
end;


ProduceCppIneqFiles:=true;
if ProduceCppIneqFiles then
    TheBasis:=CTYP_GetBasis(n);
    nbCell:=Length(ListTotal);
    for iCell in [1..nbCell]
    do
        Print("Writing for iCell=", iCell, " / ", nbCell, "\n");
        RecIneq:=CTYP_GetInequalitiesOfCtype(ListTotal[iCell].TheCtype, TheBasis, n);
        TheFile:=Concatenation("TheCtype_", String(n), "_", String(iCell));
        SYMPOL_PrintMatrix(TheFile, RecIneq.ListInequalities);
    od;
fi;






#
# Related to Conway Sloane Conjecture.
#
ComputeIntersection:=true;
if ComputeIntersection then
  FileSave:=Concatenation("DATA_TEMP/Ctype_ListRelativeIntersections", String(n));
#  RemoveFileIfExist(FileSave);
  if IsExistingFilePlusTouch(FileSave)=false then
    TheSolution:=true;
    nbCell:=Length(ListTotal);
    for iCell in [1..nbCell]
    do
      for jCell in [iCell+1..nbCell]
      do
        Print("iCell=", iCell, " jCell=", jCell, "\n");
        EXTint:=ComputeIntersectionOfCones([ ListSpann[iCell].ListVect, ListSpann[jCell].ListVect ]);
        ListPos1:=List(EXTint, x->Position(ListSpann[iCell].ListVect, x));
        if Position(ListPos1, fail)<>fail then
            Error("Broken Conway conjecture 1\n");
        fi;
        ListPos2:=List(EXTint, x->Position(ListSpann[jCell].ListVect, x));
        if Position(ListPos2, fail)<>fail then
            Error("Broken Conway conjecture 2\n");
        fi;
        ListGram1:=ListSpann[iCell].ListGram{ListPos1};
        ListGram2:=ListSpann[jCell].ListGram{ListPos2};
        if ArithmeticEquivalenceGramFamilies(ListGram1, ListGram2)=false then
            Error("Broken Conway conjecture 3\n");
        fi;
      od;
    od;
    SaveDataToFilePlusTouch(FileSave, TheSolution);
  else
    TheSolution:=ReadAsFunction(FileSave)();
  fi;
  Print("We have TheSolution\n");
fi;








FileSave:=Concatenation("DATA_TEMP/Ctype_ListThetaMap", String(n));
#RemoveFileIfExist(FileSave);
if IsExistingFilePlusTouch(FileSave)=false then
  TheBasis:=CTYP_GetBasis(n);
  nbCell:=Length(ListTotal);
  ListThetaMap:=[];
  for iCell in [1..nbCell]
  do
    Print("Computing ThetaMap iCell=", iCell, " / ", nbCell, "\n");
    ThetaMap:=CTYP_ComputingThetaMaps(ListTotal[iCell].TheCtype, TheBasis, n);
    Add(ListThetaMap, ThetaMap);
  od;
  SaveDataToFilePlusTouch(FileSave, ListThetaMap);
else
  ListThetaMap:=ReadAsFunction(FileSave)();
fi;
Print("We have ListThetaMap\n");

FileSave:=Concatenation("DATA_TEMP/Ctype_TestConjectureZonotopalTheta", String(n));
RemoveFileIfExist(FileSave);
DoZonotopal:=true;
if DoZonotopal and IsExistingFilePlusTouch(FileSave)=false then
  ListConeTheta:=List(ListThetaMap, x->x.EXTray_SymMat);
  ListConeThetaRank:=List(ListConeTheta, x->RankMat(Sum(x)));
  #
  eFile:=Concatenation("DATA_TEMP/ListZonotopal", String(n));
  RecConeZonotopal:=ReadAsFunction(eFile)();
  FindEntry:=function(eCone, ListCone)
    local ListSum, eSum, idx, eIsom, eConeImg;
    ListSum:=List(ListCone, Sum);
    eSum:=Sum(eCone);
    for idx in [1..Length(ListSum)]
    do
      eIsom:=ArithmeticIsomorphismSemidefinite([eSum], [ListSum[idx]]);
      if eIsom<>false then
        eConeImg:=List(eCone, x -> eIsom*x*TransposedMat(eIsom));
        if Set(eConeImg)<>Set(ListCone[idx]) then
          Error("Found zonotopal zone that is not in the matroidal locus");
        fi;
        return idx;
      fi;
    od;
    return -1;
  end;
  IsMatroidalFace:=function(ListGram)
      local ListV;
      ListV:=List(ListGram, x->IsRankOneMatrix(x).V);
      return IsUnimodularVectorSystem(ListV);
  end;
  ListStatusUnimodularity:=List(ListConeTheta, IsMatroidalFace);
  ListConeMaximalIdx:=List(ListConeTheta, x->FindEntry(x, RecConeZonotopal.ListConeMaximal));
  eDiff:=Difference(Set(ListConeMaximalIdx), [-1]);
  if Position(ListStatusUnimodularity, false)<>fail then
    Error("The conjecture is broken 1");
  fi;
  if eDiff<>[1..Length(RecConeZonotopal.ListConeMaximal)] then
    Error("The conjecture is broken 2");
  fi;
fi;
