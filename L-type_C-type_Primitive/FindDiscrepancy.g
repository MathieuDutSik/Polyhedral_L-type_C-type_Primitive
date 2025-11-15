n := 5;
k := 1;
SumDefect:=0;
TotalNumber:=0;
TotalNumberSubordination:=0;
TotalSet:=Set([]);
while(true)
do
  eFile:=Concatenation("DATA_TEMP/SUB/Subordination", String(n), "_rnk", String(k));
  if IsExistingFile(eFile) then
    TheData:=ReadAsFunction(eFile)();
    NewData:=[];
    for eRec in TheData
    do
      NewRec:=List(eRec, Set);
      Add(NewData, NewRec);
    od;
    TheDefect:=Length(NewData) - Length(Set(NewData));
    TotalSet:=Union(TotalSet, Set(NewData));
#    Print("k=", k, " defect=", TheDefect, " nb=", Length(NewData), "\n");
    Print("k=", k, " nb=", Length(NewData), " nb(red.sub.)=", Length(Set(NewData)), "\n");
    SumDefect:=SumDefect + TheDefect;
    TotalNumber:=TotalNumber + Length(NewData);
    TotalNumberSubordination:=TotalNumberSubordination + Length(Set(NewData));
  else
    break;
  fi;
  k:=k+1;
od;
TotalDefect:=TotalNumber - Length(TotalSet);
Print("SumDefect=", SumDefect, "\n");
Print("TotalDefect=", TotalDefect, "\n");
Print("TotalNumber=", TotalNumber, "\n");
Print("TotalNumberSubordination=", TotalNumberSubordination, "\n");

SumTotal:=110244;
SumEngel:=103769;
DiffFinal:=SumTotal - SumEngel;
Print("DiffFinal=", DiffFinal, "\n");
