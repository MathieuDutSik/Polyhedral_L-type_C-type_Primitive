U:=ReadAsFunction("File5")();

ListLType:=U.ListDIM5.ListLtype;
n:=5;
TheDim:=n*(n+1)/2;

GRP:=Group([-IdentityMat(n)]);
TheBasis:=InvariantFormDutourVersion(GeneratorsOfGroup(GRP));
eCase:=rec(Basis:=TheBasis);

nbType:=Length(ListLType);
ListIrregInfo:=[];
ListListMat:=[];
ListSingularity:=[];
for iLType in [1..nbType]
do
  Print("iLType=", iLType, "/", nbType, "\n");
  ListOrbitDelaunay:=ListLType[iLType].Ltype;
  RecIneq:=WriteFaceInequalities(ListOrbitDelaunay, eCase);
  ListIneq:=RecIneq.ListInequalities;
  ListIneqRed:=List(ListIneq, x->x{[2..TheDim+1]});
  EXT:=DualDescription(ListIneqRed);
  ListMat:=[];
  for eEXT in EXT
  do
    eMat:=NullMat(n,n);
    for i in [1..TheDim]
    do
      eMat:=eMat+eEXT[i]*TheBasis[i];
    od;
    Add(ListMat, RemoveFractionMatrix(eMat));
  od;
  Add(ListListMat, ListMat);
  eIrregInfo:=GetIrregularityInfoGeneral(ListMat);
  if eIrregInfo.DeltaSimp>0 or eIrregInfo.TheIndex>1 then
    Add(ListSingularity, ListMat);
  fi;
  Add(ListIrregInfo, eIrregInfo);
od;

GetInvariant:=function(ListMat)
  local ListRnk, eMat, SHV, ListVect;
  ListRnk:=List(ListMat, RankMat);
  eMat:=Sum(ListMat);
  SHV:=ShortestVectorDutourVersion(eMat);
  ListVect:=List(ListMat, SymmetricMatrixToVector);
  return rec(CollRnk:=Collected(ListRnk),
             nbShort:=Length(SHV), eDet:=DeterminantMat(eMat), 
             LinInv:=LinPolytope_Invariant(ListVect));
end;


DoSelection:=function(ListSingularity)
  local n, ListMaximal, NewListSingularity, ListInvariant, FuncInsert, eSingularity, ListVect, ListSets, IsMaximal, eSet, ListMat, eIrregInfo;
  n:=Length(ListSingularity[1][1]);
  ListMaximal:=[];
  NewListSingularity:=[];
  ListInvariant:=[];
  FuncInsert:=function(eNewSing)
    local eNewMat, eSing, eMat, eIsom, iSing, eNewInv;
    eNewMat:=Sum(eNewSing);
    eNewInv:=GetInvariant(eNewSing);
    for iSing in [1..Length(NewListSingularity)]
    do
      if ListInvariant[iSing]=eNewInv then
        eSing:=NewListSingularity[iSing];
        eMat:=Sum(eSing);
        eIsom:=ArithmeticIsomorphism([eNewMat], [eMat]);
        if eIsom<>false then
          return;
        fi;
      fi;
    od;
    Add(NewListSingularity, eNewSing);
    Add(ListInvariant, eNewInv);
    Print("Now |NewListSingularity|=", Length(NewListSingularity), "\n");
  end;
  for eSingularity in ListSingularity
  do
    ListVect:=List(eSingularity, SymmetricMatrixToVector);
    ListSets:=DualDescriptionSets(ListVect);
    IsMaximal:=true;
    for eSet in ListSets
    do
      ListMat:=eSingularity{eSet};
      eMat:=Sum(ListMat);
      if RankMat(eMat)=n then
        eIrregInfo:=GetIrregularityInfoGeneral(ListMat);
        if eIrregInfo.DeltaSimp>0 or eIrregInfo.TheIndex>1 then
          FuncInsert(ListMat);
          IsMaximal:=false;
        fi;
      fi;
    od;
    if IsMaximal then
      Add(ListMaximal, eSingularity);
    fi;
  od;
  return rec(ListMaximal:=ListMaximal,
             ListSingularity:=NewListSingularity);
end;

TotalInfo:=[];
while(true)
do
  eRecEnum:=DoSelection(ListSingularity);
  Print("|ListMaximal|=", Length(eRecEnum.ListMaximal), " |ListSingularity|=", Length(eRecEnum.ListSingularity), "\n");
  Add(TotalInfo, eRecEnum);
  ListSingularity:=eRecEnum.ListSingularity;
  if Length(ListSingularity)=0 then
    break;
  fi;
od;

