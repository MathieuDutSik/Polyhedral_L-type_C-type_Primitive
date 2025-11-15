eMat:=IdentityMat(3);
eSimp3:=List(Concatenation([[0,0,0]], eMat), x->Concatenation([1],x));

ePyramid:=[
[1,0,0,0],
[1,1,0,0],
[1,0,1,0],
[1,1,1,0],
[1,0,0,1]];

ePrism3:=[
[1,0,0,0],
[1,1,0,0],
[1,0,1,0],
[1,0,0,1],
[1,1,0,1],
[1,0,1,1]];

ListPoint:=Concatenation(IdentityMat(3), -IdentityMat(3));
ListDiff:=List(ListPoint, x->x - ListPoint[1]);
eBasis:=GetZbasis(ListDiff);
ListPointSol:=List(ListDiff, x->SolutionMat(eBasis, x));
eCrossPoly:=List(ListPointSol, x->Concatenation([1], x));

eCube:=List(BuildSet(3, [0,1]), x->Concatenation([1],x));

ListDel:=[eSimp3, ePyramid, ePrism3, eCrossPoly, eCube];
SaveDataToFile("ListDelaunay3", ListDel);
