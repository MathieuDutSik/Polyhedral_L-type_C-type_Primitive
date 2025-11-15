DoGraphicTest:=false;
if DoGraphicTest then
  n:=7;
  ListInfo:=[];
  ListCand:=[];
  for eList in GetIsomorphismTypeGraph(n)
  do
    TheGra:=NullGraph(Group(()), n);
    for i in [1..n]
    do
      for eAdj in eList[i]
      do
        AddEdgeOrbit(TheGra, [i, eAdj]);
      od;
    od;
    if IsConnectedGraph(TheGra) then
      Add(ListCand, TheGra);
    fi;
  od;
  for TheGra in ListCand
  do
    eRecSyst:=Matroid_GraphicVectorSystem(TheGra);
    ListVect:=ColumnReduction(eRecSyst.TheSyst).EXT;
    eRecTest:=CTYP_TestUnimodularFamily(ListVect);
    eInfo:=rec(TheGra:=TheGra, ListVect:=ListVect, eRecTest:=eRecTest);
    Add(ListInfo, eInfo);
  od;
  ListTest:=List(ListInfo, x->x.eRecTest.IsCtype);
fi;


DoExceptionalTest:=false;
if DoExceptionalTest then
#  ListVect:=Matroid_Exceptional("R10");
  ListVect:=Matroid_Exceptional("R12");
  eRecTest:=CTYP_TestUnimodularFamily(ListVect);
fi;

DoCographicTest:=true;
if DoCographicTest then
  n:=6;
  ListCand:=[];
  for eList in GetIsomorphismTypeGraph(n)
  do
    TheGra:=NullGraph(Group(()), n);
    for i in [1..n]
    do
      for eAdj in eList[i]
      do
        AddEdgeOrbit(TheGra, [i, eAdj]);
      od;
    od;
    if IsConnectedGraph(TheGra) then
      eRecSyst:=Matroid_CographicVectorSystem(TheGra);
      if Length(eRecSyst.TheSyst)>0 then
        if Length(eRecSyst.TheSyst[1]) > 1 then
          Add(ListCand, TheGra);
        fi;
      fi;
    fi;
  od;
  ListInfo:=[];
  ListTest:=[];
  for TheGra in ListCand
  do
    eRecSyst:=Matroid_CographicVectorSystem(TheGra);
    ListVect:=ColumnReduction(eRecSyst.TheSyst).EXT;
    eRecTest:=CTYP_TestUnimodularFamily(ListVect);
    eInfo:=rec(TheGra:=TheGra, ListVect:=ListVect, eRecTest:=eRecTest);
    Add(ListInfo, eInfo);
    Add(ListTest, eRecTest.IsCtype);
  od;
fi;
