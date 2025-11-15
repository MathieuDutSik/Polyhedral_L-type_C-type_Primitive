eRec:=ReadAsFunction("DEBUG_ListMatrix")();

ListMatrix:=eRec.ListMatrix;
ListOrbitTotallyZoneCont:=eRec.ListOrbitTotallyZoneCont;

ListVector:=List(ListMatrix, SymmetricMatrixToVector);

ListRank:=List(ListMatrix, RankMat);

eNSP:=NullspaceMat(ListVector)[1];

EXT:=DirectPolytopizationEXT(ListVector);



