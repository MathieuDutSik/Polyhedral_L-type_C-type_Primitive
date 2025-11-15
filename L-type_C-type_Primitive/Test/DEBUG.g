REC_redund:=ReadAsFunction("REC_redund")();
eSetSelect:=[ 2, 5, 11, 23, 45, 136, 178, 190, 193, 235, 247, 250, 262, 265, 268 ];

EXT:=REC_redund.EXT;
BoundingFac:=REC_redund.BoundingFac;
#BoundingFac:=[];
GRPperm:=REC_redund.GRPperm;
#GRPperm:=Group(());

#EXT_select:=EXT{eSetSelect};
#BoundingFac:=DualDescription(EXT_select);


TheFinal:=EliminationByRedundancyEquivariant(EXT, BoundingFac, GRPperm);
