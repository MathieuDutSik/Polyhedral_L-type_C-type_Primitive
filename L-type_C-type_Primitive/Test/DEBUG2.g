
EXT:=[
[1 , -1 , 1],
[1 , -1 ,-1],
[1 ,  1 , 1],
[1 ,  1 , 0],
[1 ,  1 ,-1]];

BoundingFac:=DualDescription(EXT);
GRPperm:=Group(());

EliminationByRedundancyEquivariant(EXT, BoundingFac, GRPperm);


