n:=5;
TheDim:=n*(n+1)/2;
ListCanon:=[];
for i in [1..TheDim]
do
  eFile:=Concatenation("DATA_TEMP/Canonical", String(n), "_rnk", String(i));
  if IsExistingFilePlusTouch(eFile) then
    LCanon:=ReadAsFunction(eFile)();
    Append(ListCanon, LCanon);
  fi;
od;
