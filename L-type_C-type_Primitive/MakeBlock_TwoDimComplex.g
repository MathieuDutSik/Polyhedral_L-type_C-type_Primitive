n:=5;

TotalListConf:=[];
TheDim:=n*(n+1)/2;
for rnk in [1..TheDim]
do
  eFile:=Concatenation("DATA_TEMP/List", String(n), "_rnk", String(rnk));
  eRecEnum:=ReadAsFunction(eFile)();
  Append(TotalListConf, eRecEnum.ListConfiguration);
od;

nbConf:=Length(TotalListConf);

sizBlock:=100;
nbBlock:=UpperInteger(nbConf / sizBlock);
for iBlock in [1..nbBlock]
do
  Print("iBlock=", iBlock, " / ", nbBlock, "\n");
  ePos1:=1 + (iBlock-1)*sizBlock;
  ePos2:=Minimum(nbConf, iBlock * sizBlock);
  PartList:=TotalListConf{[ePos1..ePos2]};
  eFileSave:=Concatenation("BLOCK_", String(iBlock));
  SaveDataToFile(eFileSave, PartList);
od;
