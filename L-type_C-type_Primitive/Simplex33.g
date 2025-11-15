GetRepartitioningPolytope:=function(TheRel)
  local eScal, TheProj, n, TheProjB, FirstVertex, i, ListDiff, TheBasis, EXTred, EXT;
  eScal:=TheRel*TheRel;
  n:=Length(TheRel);
  TheProj:=(TransposedMat([TheRel])*[TheRel])/eScal;
  TheProjB:=IdentityMat(n) - TheProj;

  FirstVertex:=TheProjB[1];
  ListDiff:=[];
  for i in [2..n]
  do
    Add(ListDiff, TheProjB[i] - TheProjB[1]);
  od;
  TheBasis:=GetZbasis(ListDiff);
  EXTred:=[];
  for i in [1..n]
  do
    Add(EXTred, SolutionMat(TheBasis, TheProjB[i] - TheProjB[1]));
  od;
  EXT:=List(EXTred, x->Concatenation([1],x));
  return rec(EXT:=EXT);

end;
