n := 6;
FileSave:=Concatenation("DATA_TEMP/ListCtypeAdj", String(n));
if IsExistingFilePlusTouch(FileSave)=false then
  ListTotal:=CTYP_EnumerateCtypes(n);
  SaveDataToFilePlusTouch(FileSave, ListTotal);
else
  ListTotal:=ReadAsFunction(FileSave)();
fi;
Print("We have ListTotal\n");
