eG:=ClassicalSporadicLattices("E6");

LFC:=DelaunayComputationStandardFunctions(eG);

#TheList:=LFC.GetVoronoiVertices().GetFaceLattice();

k:=3;
ListSubDelaunay:=LFC.GetKdimensionalCells(k);



