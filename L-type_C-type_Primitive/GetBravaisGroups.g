LoadPackage("carat");
GetNumberGroup:=function(d)
  local ListSymb, ListGRP, eSymb, LGrp;
  ListSymb:=CaratCrystalFamilies[d];
  ListGRP:=[];
  for eSymb in ListSymb
  do
    LGrp:=BravaisGroupsCrystalFamily(eSymb);
    Append(ListGRP, LGrp);
  od;
  return ListGRP;
end;
