n:=5;
TheDim:=n*(n+1)/2;

eBasis:=CTYP_GetBasis(n);
eFileA:=Concatenation("DATA_TEMP/ListCtypeAdj", String(n));
ListCtypeA:=ReadAsFunction(eFileA)();

eFileB:=Concatenation("DATA_TEMP/Ctype", String(n));
ListCtypeB:=ReadAsFunction(eFileB)();


ListSumMatB:=List(ListCtypeB, x->x.eGram);

FuncGetIndex:=function(ListSumMat, eMat)
  local iMat, fMat, test;
  for iMat in [1..Length(ListSumMat)]
  do
    fMat:=ListSumMat[iMat];
    test:=ArithmeticIsomorphism([fMat], [eMat]);
    if test<>false then
      return rec(iMat:=iMat, test:=test);
    fi;
  od;
  return fail;
end;

eBasis:=CTYP_GetBasis(n);

ListSumMatA:=[];
for eCtype in ListCtypeA
do
  ListIneq:=List(eCtype.ListInequalities, x->x{[2..TheDim+1]});
  EXT:=DualDescription(ListIneq);
  ListMat:=[];
  for eEXT in EXT
  do
    eMat:=NullMat(n,n);
    for i in [1..TheDim]
    do
      eMat:=eMat + eEXT[i]*eBasis[i];
    od;
    eMatR:=RemoveFractionMatrix(eMat);
    Add(ListMat, eMatR);
  od;
  SumMat:=Sum(ListMat);
  Add(ListSumMatA, SumMat);
od;

ListPosAB:=List(ListSumMatA, x->FuncGetIndex(ListSumMatB, x));
ListPosBA:=List(ListSumMatB, x->FuncGetIndex(ListSumMatA, x));

