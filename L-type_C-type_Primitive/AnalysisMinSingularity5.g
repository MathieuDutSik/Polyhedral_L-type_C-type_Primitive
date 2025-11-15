ListMin:=ReadAsFunction("ListMinSingularity5")();

nbMin:=Length(ListMin);


ListSumMat:=[];
for iMin in [1..nbMin]
do
  eMin:=ListMin[iMin];
  nbMat:=Length(eMin);
  SumMat:=NullMat(5,5);
  for eMat in eMin
  do
    SumMat:=SumMat + eMat;
  od;
  Add(ListSumMat, SumMat);
  eFile:=Concatenation("Sing", String(iMin), ".tex");
  RemoveFileIfExist(eFile);
  output:=OutputTextFile(eFile, true);
  AppendTo(output, "\\begin{equation*}\n");
  for iMat in [1..nbMat]
  do
    LatexPrintMatrix(output, eMin[iMat]);
    if iMat < nbMat then
      AppendTo(output, ",\\mbox{~}");
    fi;
  od;
  AppendTo(output, "\\end{equation*}\n");
  CloseStream(output);

od;
