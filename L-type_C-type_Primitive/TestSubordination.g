eG:=LatticeAn(5);

LFC:=DelaunayComputationStandardFunctions(eG);
RecVor:=LFC.GetVoronoiVertices();

EXT:=RecVor.EXT;
FAC:=RecVor.FAC;

GroupFAC:=LinPolytope_Automorphism(FAC);
TheReply:=CreateK_skeletton(GroupFAC, FAC, EXT);


TheSub:=RecVor.GetSubordinationInformation();
