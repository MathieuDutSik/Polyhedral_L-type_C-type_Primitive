n:=6;
GramMat:=LatticeAn(n);
eG:=Inverse(GramMat);

LFC:=DelaunayComputationStandardFunctions(eG);

GRP:=Group([-IdentityMat(n)]);
TheBasis:=InvariantFormDutourVersion(GeneratorsOfGroup(GRP));
eCase:=rec(Basis:=TheBasis);

ListVect:=[];
for i in [1..n]
do
  for j in [i+1..n+1]
  do
    eVect:=ListWithIdenticalEntries(n+1,0);
    eVect[i]:=1;
    eVect[j]:=-1;
    Add(ListVect, eVect);
  od;
od;

ListVectRed:=List(ListVect, x->x{[1..n]});
ListRankOne:=List(ListVectRed, x->TransposedMat([x])*[x]);

eSum:=NullMat(n,n);
for iRankOne in [1..Length(ListRankOne)]
do
  eSum:=eSum + iRankOne*ListRankOne[iRankOne];
od;

LFC:=DelaunayComputationStandardFunctions(eSum);

DelCO:=LFC.GetDelaunayDescription();
RecIneq:=WriteFaceInequalities(DelCO, eCase);
ListIneq:=RecIneq.ListInequalities;

