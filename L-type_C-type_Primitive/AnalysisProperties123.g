n:=5;

eFile:=Concatenation("File", String(n));
U:=ReadAsFunction(eFile)();

if n=5 then
  ListLType:=U.ListDIM5.ListLtype;
elif n=4 then
  Print(NullMat(5));
fi;

TheDim:=n*(n+1)/2;

ZerPoint:=Concatenation([1], ListWithIdenticalEntries(TheDim,0));


GRP:=Group([-IdentityMat(n)]);
TheBasis:=InvariantFormDutourVersion(GeneratorsOfGroup(GRP));
eCase:=rec(Basis:=TheBasis);

nbType:=Length(ListLType);
ListIrregInfo:=[];
ListListMat:=[];
ListSingularity:=[];

ListProperty1:=[];
ListProperty2:=[];
ListProperty3:=[];

for iLType in [1..nbType]
do
  Print("iLType=", iLType, "/", nbType, "\n");
  ListOrbitDelaunay:=ListLType[iLType].Ltype;
  RecIneq:=WriteFaceInequalities(ListOrbitDelaunay, eCase);
  ListIneq:=RecIneq.ListInequalities;
  ListIneqRed:=List(ListIneq, x->x{[2..TheDim+1]});
  EXT:=DualDescription(ListIneqRed);
  ListMat:=[];
  ListVect:=[];
  ListVertices:=[];
  for eEXT in EXT
  do
    eMat:=NullMat(n,n);
    for i in [1..TheDim]
    do
      eMat:=eMat+eEXT[i]*TheBasis[i];
    od;
    eMatRed:=RemoveFractionMatrix(eMat);
    eVect:=SymmetricMatrixToVector(eMatRed);
    fEXT:=Concatenation([1], eVect);
    Add(ListMat, eMatRed);
    Add(ListVect, eVect);
    Add(ListVertices, fEXT);
  od;
  TotalListVertices:=Concatenation(ListVertices, [ZerPoint]);
  #
  NSP:=NullspaceMat(TransposedMat(ListVertices));
  dim:=Length(NSP);
  if dim<>0 and dim<>1 then
    Print("Logical error");
    Print(NullMat(5));
  fi;
  if dim=1 then
    Property1:=true;
    eNSP:=NSP[1];
  else
    Property1:=false;
  fi;
  Add(ListProperty1, Property1);
  #
  FAC:=DualDescription(TotalListVertices);
  eRecSolve:=RunZsolve(FAC);
  IntegralVertices:=List(eRecSolve.TheINH, x->Concatenation([1], x));
  if IsSubset(Set(IntegralVertices), Set(TotalListVertices))=false then
    Print("Logical error 2");
    Print(NullMat(5));
  fi;
  Property2:=Length(IntegralVertices)=Length(TotalListVertices);
  if Property1 then
    TheDiff:=Difference(Set(IntegralVertices), Set(TotalListVertices));
    Property3:=First(TheDiff, x->eNSP*x<>0)=fail;
  else
    Property3:=false;
  fi;
  Add(ListProperty2, Property2);
  Add(ListProperty3, Property3);
  Add(ListListMat, ListMat);
od;
