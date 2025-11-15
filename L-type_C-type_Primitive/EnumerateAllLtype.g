n:=5;
#
# Get the 1, 1, 3, 222 primitive Ltypes in dimension n
#
ListLtype:=VOR_LTYPE_GetPrimitiveLtype(n);
TheDim:=n*(n+1)/2;
Print("Data has been loaded\n");



CheckConjectureVolume:=true;
if CheckConjectureVolume then
    ListEntry:=[];
    ListEXTcan:=[];
    for iEnt in [1..Length(ListLtype)]
    do
        Print("iEnt=", iEnt, "\n");
        eEntry:=CTYP_GetCanonicalEntryForLowerBound(ListLtype[iEnt].Ltype);
        Add(ListEntry, eEntry);
        Add(ListEXTcan, eEntry[1]);
    od;
fi;






Compute_Ctype_Data:=function(ListConfiguration)
  local FileSave, ListCtype;
  FileSave:=Concatenation("DATA_TEMP/Ctype", String(n));
  if IsExistingFilePlusTouch(FileSave) then
    ListCtype:=ReadAsFunction(FileSave)();
  else
    ListCtype:=VOR_LTYPE_GetListCtypes(ListConfiguration);
    SaveDataToFilePlusTouch(FileSave, ListCtype);
  fi;
  return ListCtype;
end;

#
# Get the 2, 5, 52, 110244 Ltypes in dimension n
#
#ListConfiguration:=VOR_LTYPE_GetListConfiguration(ListLtype);

#
# Get all the Ltypes (not just the primitive)
#
optProg:="NAUTY";
#optProg:="ISOM";
#TotalInfo:=VOR_LTYPE_GetTotalInfo(ListConfiguration, optProg);

#
# Get the rigid lattices in dimension n (only D4 in dim 4), (7 lattices in dimension 5)
#
#ListRigidForm:=VOR_LTYPE_GetRigidFormsAndGeometry(ListConfiguration);

#
# Get the C-type 
#
DoCtype:=false;
if DoCtype then
  ListCtype:=Compute_Ctype_Data(ListConfiguration);
fi;


#
# Get the C-type invariant
#




#
# Get the Zone contracted tyes (82 in dimension 5)
#
DoZone:=false;
if DoZone then
  FileSave:=Concatenation("DATA_TEMP/ZoneContractions_", String(n));
#  RemoveFileIfExist(FileSave);
  if IsExistingFilePlusTouch(FileSave) then
    ListZoneCont:=ReadAsFunction(FileSave)();
  else
    ListZoneCont:=VOR_LTYPE_GetZoneContracted(n);
    SaveDataToFilePlusTouch(FileSave, ListZoneCont);
  fi;
fi;

DoTableGeneralizedZoneCont:=false;
if DoTableGeneralizedZoneCont then
  VOR_LTYPE_ComputeTableZoneContraction(n);
fi;


DoEulerPoincareChar:=false;
if DoEulerPoincareChar then
  VOR_LTYPE_GetEulerPoincareCharacteristic(n);
fi;



DoMaximalParallelotope:=false;
if DoMaximalParallelotope then
  ListCaseMaximal:=VOR_LTYPE_GetMaximalParallelotope(n);
fi;

DoBravaisGroup:=false;
if DoBravaisGroup then
  VOR_LTYPE_GetBravaisGroups(n);
fi;

DoTableBravais:=false;
if DoTableBravais then
  RecBravaisTable:=VOR_LTYPE_TablePossibleGroup(n);
fi;


DoEXT_FAC:=false;
if DoEXT_FAC then
  VOR_LTYPE_ComputeEXT_FAC(n);
fi;


DoInclusion:=false;
if DoInclusion then
  VOR_LTYPE_Inclusions(n, optProg);
fi;

DoZonotopal:=false;
if DoZonotopal then
  VOR_LTYPE_GetListZonotopal(n);
fi;





#
# Determine "principal primitive" ones
# (all Delaunay are of volume 1)
#
DoPrincipalPrimitive:=false;
if DoPrincipalPrimitive then
  eDim:=n*(n+1)/2;
  eFile:=Concatenation("DATA_TEMP/List", String(n), "_rnk", String(eDim));
  TheRead:=ReadAsFunction(eFile)();
  nbPrincipalPrimitive:=0;
  for eFamMat in TheRead.ListConfiguration
  do
    TheSumMat:=Sum(eFamMat);
    LFC:=DelaunayComputationStandardFunctions(TheSumMat);
    ListDet:=[];
    for eRec in LFC.GetDelaunayDescription()
    do
      EXT:=eRec.EXT;
      TheDet:=AbsInt(DeterminantMat(EXT));
      Add(ListDet, TheDet);
    od;
    if Set(ListDet)=[1] then
      nbPrincipalPrimitive:=nbPrincipalPrimitive+1;
    fi;
  od;
fi;


DoString:=false;
if DoString then
  VOR_LTYPE_ComputeCanonicalStrings(n);
fi;

DoSubordination:=false;
if DoSubordination then
  VOR_LTYPE_ComputeSubordinationInformation(n);
fi;


DoComputeSkeleton:=false;
if DoComputeSkeleton then
  RecOpt:=rec(SaveFaceLattice:=false, ExportAlexeyGarber:=true);
  VOR_LTYPE_ComputeFaceLatticeVoronoi(n, RecOpt);
fi;


DoComputeTwoDimComplexes:=false;
if DoComputeTwoDimComplexes then
  RequirePackage("simpcomp");
  VOR_LTYPE_ComputeTwoDimComplexes(n);
fi;




#DoExportAlexeyGarber:=true;
#if DoExportAlexeyGarber then
#  VOR_LTYPE_ExportFaceLatticeAlexeyGarber(n);
#fi;






DoTableNrExtRay:=false;
if DoTableNrExtRay then
  VOR_LTYPE_TableNrExtRay(n);
fi;


DoIdentifyRigidDim5:=false;
if DoIdentifyRigidDim5 then
  ListInfo:=[[40, 42], [42, 96], [48, 180], [50, 192], [50, 282], [54, 342], [54, 366]];
  U:=ReadAsFunction("ListRigidDim5")();
  ListPos:=[];
  ListSymb:=[];
  for eG in U
  do
    LFC:=DelaunayComputationStandardFunctions(eG);
    RecVor:=LFC.GetVoronoiVertices();
    nbFAC:=Length(RecVor.FAC);
    nbEXT:=Length(RecVor.EXT);
    eSymb:=[nbFAC, nbEXT];
    Add(ListSymb, eSymb);
    pos:=Position(ListInfo, eSymb);
    Add(ListPos, pos);
  od;
  SaveDataToFile("DATA_TEMP/IdentificationRigidLatt", ListPos);
fi;


