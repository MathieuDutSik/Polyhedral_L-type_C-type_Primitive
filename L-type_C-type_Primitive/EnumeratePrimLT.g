AnLatticeStar_V2:=function(n)
  local DistMat, i, j;
  DistMat:=NullMat(n+1, n+1);
  for i in [1..n+1]
  do
    for j in [1..n+1]
    do
      if i<>j then
        DistMat[i][j]:=1;
      fi;
    od;
  od;
  return RemoveFractionMatrix(Inverse(DistanceMatrixToGramMatrix(DistMat)));
end;


AnLattice_V2:=function(n)
  local DistMat, i, j;
  DistMat:=NullMat(n+1, n+1);
  for i in [1..n+1]
  do
    for j in [1..n+1]
    do
      if i<>j then
        DistMat[i][j]:=1;
      fi;
    od;
  od;
  return RemoveFractionMatrix(DistanceMatrixToGramMatrix(DistMat));
end;


ListInDimensionN:=function(n)
  local PathSAVE, TheGroup, TheBasis, eCase, TheSuperMat;
  PathSAVE:=Concatenation("./ListLtype", String(n), "/");
  Exec("mkdir -p ", PathSAVE);
  TheSuperMat:=AnLatticeStar_V2(n);
  TheGroup:=Group([-IdentityMat(n)]);
  TheBasis:=InvariantFormDutourVersion(GeneratorsOfGroup(TheGroup));
  eCase:=rec(Basis:=TheBasis, IsBravaisSpace:=true, TheGroup:=TheGroup, SuperMat:=TheSuperMat);
  return EnumerationProcedureLtype(eCase);
end;

Antipodal:=function(EXT)
  local n, RetEXT, eVect, eVectRed, fVect;
  n:=Length(EXT[1])-1;
  RetEXT:=[];
  for eVect in EXT
  do
    eVectRed:=eVect{[2..n+1]};
    fVect:=Concatenation([1], -eVectRed);
    Add(RetEXT, fVect);
  od;
  return RetEXT;
end;


#ListFC:=ListInDimensionN(2);  # one Ltype
#ListLtypes:=ListFC.FuncGetAllLtypes();
#ListInvs:=List(ListLtypes, x->GetLtypeInvariants(x));
#DataSave:=rec(ListLtypes:=ListLtypes, ListInvs:=ListInvs);
#SaveDataToFile("File2", DataSave);



ListFC:=ListInDimensionN(3);  # one Ltype
ListLtypes:=ListFC.FuncGetAllLtypes();
ListInvs:=List(ListLtypes, x->GetLtypeInvariants(x));
DataSave:=rec(ListLtypes:=ListLtypes, ListInvs:=ListInvs);
SaveDataToFile("File3", DataSave);


#ListFC:=ListInDimensionN(4);  # one Ltype
#ListLtypes:=ListFC.FuncGetAllLtypes();
#ListInvs:=List(ListLtypes, x->GetLtypeInvariants(x));
#DataSave:=rec(ListLtypes:=ListLtypes, ListInvs:=ListInvs);
#SaveDataToFile("File4", DataSave);


#ListFC:=ListInDimensionN(5);  # one Ltype
#ListLtypes:=ListFC.FuncGetAllLtypes();
#ListInvs:=List(ListLtypes, x->GetLtypeInvariants(x));
#DataSave:=rec(ListLtypes:=ListLtypes, ListInvs:=ListInvs);
#SaveDataToFile("File5", DataSave);



#ListDIM4:=ListInDimensionN(4);  # three Ltypes
#ListINV4:=List(ListDIM4.ListLtype, x->PrintInvariantInformation(x));
#SaveDataToFile("File4", rec(ListDIM4:=ListDIM4, ListINV4:=ListINV4));


#ListDIM5:=ListInDimensionN(5);  # 222 Ltypes
#ListINV5:=List(ListDIM5.ListLtype, x->PrintInvariantInformation(x));
#SaveDataToFile("File5", rec(ListDIM5:=ListDIM5, ListINV5:=ListINV5));
