ListSearchCase:=[
rec(d:=5, dim:=4, nbExt:=8),
rec(d:=5, dim:=9, nbExt:=16),
rec(d:=5, dim:=10, nbExt:=21)];

DoSearch:=function(eCase)
  local TheFile, U, nbConf, ListIdx, ListConf, iConf, eConf;
  TheFile:=Concatenation("DATA_TEMP/List", String(eCase.d), "_rnk", String(eCase.dim));
  U:=ReadAsFunction(TheFile)();
  nbConf:=Length(U.ListConfiguration);
  ListIdx:=[];
  ListConf:=[];
  for iConf in [1..nbConf]
  do
    eConf:=U.ListConfiguration[iConf];
    if Length(eConf)=eCase.nbExt then
      Add(ListIdx, iConf);
      Add(ListConf, eConf);
    fi;
  od;
  return rec(ListIdx:=ListIdx, ListConf:=ListConf);
end;


for eCase in ListSearchCase
do
  eRec:=DoSearch(eCase);
  Print("d=", eCase.d, " dim=", eCase.dim, " nbExt=", eCase.nbExt, " ListIdx=", eRec.ListIdx, "\n");
  for eEnt in eRec.ListConf
  do
    TheSum:=Sum(eEnt);
    ListSymMat:=List(eEnt, SymmetricMatrixToVector);
    GRP:=ArithmeticAutomorphismGroup([TheSum]);
    Print("  |GRP|=", Order(GRP), "\n");
  od;
od;

