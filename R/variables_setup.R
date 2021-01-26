
# Global DBs ----
dbListFull <- list("Groups" = c("Region", "Cell"),
                   
                   # Region Level Datasets
                   "Region Level" = c(
                     "Stanley Database" = "Stanley",
                     "Mt.Sinai ACC" = "MtSinaiACC",
                     "Mt.Sinai DLPFC" = "MtSinaiDLPFC",
                     "Mt.Sinai TPA" = "MtSinaiTPA",
                     "Mt.Sinai MTA" = "MtSinaiMTA",
                     "Gandal Microarray" = "gandalMicro",
                     "Gandal RNAseq" = "gandalRNAseq", 
                     "CommonMind DLPFC Re 1"="CommonMind_DLPFC",
                     "barnesAll",
                     "barnesFemale",
                     "barnesMale",
                     "Iwamoto_BA10_SCZ",
                     "Iwamoto_BA46_SCZ",
                     "MaycoxAll","MaycoxFemale", "MaycoxMale"
                   ),
                   
                   # Cell Level Datasets
                   "Cell Level" = c(
                     "Superficial_Neurons", "Deep_Neurons", 
                     "Superficial_Deep_Neurons",
                     "hiPSC_Neuron", 
                     "hiPSC_NPC1",
                     "Blood mRNA" = "BloodmRNA",
                     "DISC1 RNAseq" = "DISC1_RNA",
                     "DISC1 Proteomics" = "DISC1_Prot",
                     "Lewis_2015_L3",
                     "Lewis_2015_L5",
                     "Lewis_2017_L3",
                     "Lewis_2017_L5",
                     "PietersenAllParvalbumin","PietersenFemaleParvalbumin", "PietersenFemalePyramidal",
                     "PietersenMaleParvalbumin" , "PietersenMalePyramidal",     "PietersenAllPyramidal"
                   )
                   
)  

BSRegions <- list(
  "Amygdaloid Complex (AMY)" = "AMY", 
  "Dorsolateral Prefrontal Cortex (DFC)" = "DFC", 
  "Inferolateral Temporal Cortex (ITC)" = "ITC",
  "Superior Temporal Cortex (STC)" = "STC",
  "Ventrolateral Prefrontal Cortex (VFC)" = "VFC",
  "Posteroventral (inferior) Parietal Cortex (IPC)" = "IPC", 
  "Primary Visual Cortex (V1C)" = "V1C",
  "Hippocampus (HIP)" = "HIP",
  "Medial Prefrontal Cortex (MFC)" = "MFC", 
  "Primary Auditory Cortex (A1C)" = "A1C"
)

dbListFullDop <- list(
  "GNS PNU" = "DP_PNU", 
  "PD Lat" = "DP_Lat", 
  "PD Med" = "DP_Med",
  "PD Sup" = "DP_Sup",
  "MidBrain Cocaine" = "DP_coc"
)

dbListGTEx <- list(
  "Amygdala" = "Amygdala", 
  "Frontal Cortex" = "Frontal Cortex", 
  "Anterior Cingulate ortex" = "Anterior_cingulate_cortex",
  "Hypothalamus" = "Hypothalamus",
  "Caudate Basal Ganglia" = "Caudate Basal Ganglia",
  "Hippocampus" = "Hippocampus"
)


mddListFullDop <- list(
  "Gandal_MDD" = "Gandal_MDD",
  "DLPFC Females" = "DLPFC_FemalesMDD",
  "DLPFC Males" = "DLPFC_MalesMDD",
  "BA11 Females" = "BA11_FemalesMDD",
  "BA11 Males" = "BA11_MalesMDD",
  "BA25 Females" = "BA25_FemalesMDD",
  "BA25 Males" = "BA25_MalesMDD",
  "aINS Females" = "aINS_FemalesMDD",
  "aINS Males" = "aINS_MalesMDD",
  "vSUB Females" = "vSUB_FemalesMDD",
  "vSUB Males" = "vSUB_MalesMDD",
  "NAc Females" = "NAc_FemalesMDD",
  "NAc Males" = "NAc_MalesMDD",
  "Iwamoto_BA10_MDD",
  "Disc_MDD_Proteomics",
  "Benoit_Mouse_PFC",
  "Benoit_Mouse_NAC", 
  #"D3 Proteomics", 
  "D3 RNAseq",
  "Arion_LCM_MDD",
  "Chang_ACC_Female",
  "Chang_ACC_Male",
  "Chang_ACC1",
  "Chang_ACC2",
  "Chang_ACC3",
  "Chang_AMY1",
  "Chang_AMY2",
  "Chang_DLPFC_Female",
  "Chang_DLPFC_Male",
  "Chang_DLPFC",
  "Chang_OrbitalVentralPrefrontalCortex",
  "ALS D1", 
  "ALS D2"
  
)

bpdListFullDop <- list(
  "Iwamoto_BA10_BPD",
  "Iwamoto_BA46_BPD" 
)

AntipsychoticsListFullDop <- list(
  "CLZ_high_Colantuoni (4031)" = "CLZ_high_Colantuoni (4031)",
  "CLZ_medium_Colantuoni(4031)" = "CLZ_medium_Colantuoni(4031)",
  "CLZ-low-Colantuoni(4031)"  = "CLZ-low-Colantuoni(4031)",
  "HAL medium Colantuoni (4031)" = "HAL medium Colantuoni (4031)",
  "HAL High Colantuoni (4031)" = "HAL High Colantuoni (4031)",
  "HAL Low Colantuoni (4031)" = "HAL Low Colantuoni (4031)",
  
  "HAL_2h_25uM_glia (89873)" = "HAL_2h_25uM_glia (89873)",
  "QUE_1uM_4d GSE125325" = "QUE_1uM_4d GSE125325",
  "QUE-1uM_2d GSE125325" = "QUE-1uM_2d GSE125325",
  
  "HAL_3mgkg_30d (Kim 2018)_GSE677" = "HAL_3mgkg_30d (Kim 2018)_GSE677",
  "CLZ-1h (Korostynski 2013)_GSE48" = "CLZ-1h (Korostynski 2013)_GSE48",
  "CLZ-2h (Korostynski 2013)" = "CLZ-2h (Korostynski 2013)",
  
  "CL-4h (Korostynski 2013)" = "CL-4h (Korostynski 2013)",
  "CLZ-8h (Korostynski 2013)" = "CLZ-8h (Korostynski 2013)",
  "RIS-8h (Korostynski 2013)" = "RIS-8h (Korostynski 2013)",
  
  "RIS-4h (Korostynski 2013)" = "RIS-4h (Korostynski 2013)",
  "RIS-2h (Korostynski 2013)" = "RIS-2h (Korostynski 2013)",
  "RIS-1h (Korostysnki 2013)" = "RIS-1h (Korostysnki 2013)",
  
  "HAL_0.25mgkg_HIP_GSE66275" = "HAL_0.25mgkg_HIP_GSE66275",
  "HAL_0.25mgkg_fCTX_GSE66275" = "HAL_0.25mgkg_fCTX_GSE66275",
  "HAL_0.25mgkg_STR_GSE66275" = "HAL_0.25mgkg_STR_GSE66275",
  
  "RIS_5mgkg_21D_HIP_GSE66275" = "RIS_5mgkg_21D_HIP_GSE66275",
  "RIS_5mgkg_21D_STr_GSE66275" = "RIS_5mgkg_21D_STr_GSE66275",
  "RIS_5mgkg_21d_CTX_GSE66275" = "RIS_5mgkg_21d_CTX_GSE66275",
  
  "QUE_Str_100uM_GSE45229 " = "QUE_Str_100uM_GSE45229 ",      
  "QUE_Str_10uM_GSE45229" = "QUE_Str_10uM_GSE45229",
  "QUE_FrCTX_100uM_GSE45229" = "QUE_FrCTX_100uM_GSE45229",
  
  "QUE_FrCTX_10uM_GSE45229" = "QUE_FrCTX_10uM_GSE45229",
  "HAL_FrCTX_1uM_GSE45229" = "HAL_FrCTX_1uM_GSE45229",
  "HAL_FrCTX_0.3uM_GSE45229" = "HAL_FrCTX_0.3uM_GSE45229",
  
  "HAL_Str_1uM_GSE45229" = "HAL_Str_1uM_GSE45229",
  "HAL_Str_0.3uM_GSE45229" = "HAL_Str_0.3uM_GSE45229",
  "CLZ 3mgkg 1hr 48955" = "CLZ 3mgkg 1hr 48955"     ,    
  "RIS 0.5mgkg 1hr 48955" = "RIS 0.5mgkg 1hr 48955",
  "HAL 1mgkg 1hr 48955" = "HAL 1mgkg 1hr 48955",
  "CLZ 3mgkg 2hr 48955" = "CLZ 3mgkg 2hr 48955",
  
  "RIS 0.5mgkg 2hr 48955 " =  "RIS 0.5mgkg 2hr 48955 ",
  "HAL 1mgkg 2hr 48955" = "HAL 1mgkg 2hr 48955",
  "RIS_0.5mgkg_8hr_GSE48955" = "RIS_0.5mgkg_8hr_GSE48955",
  
  "HAL_1mgkg_8hr_GSE48955" = "HAL_1mgkg_8hr_GSE48955",
  "CLZ_3mgkg_8hr_GSE48955" = "CLZ_3mgkg_8hr_GSE48955",
  "RIS_0.5mgkg_4hr_GSE48955" = "RIS_0.5mgkg_4hr_GSE48955",
  
  "HAL_1mgkg_4hr_GSE48955" = "HAL_1mgkg_4hr_GSE48955",
  "CLZ_3mgkg_4hr_GSE48955" = "CLZ_3mgkg_4hr_GSE48955",
  "CLZ_12wk_12mgkg_GSE6467" = "CLZ_12wk_12mgkg_GSE6467"   , 
  "HAL_12wk_1.6mgkg_GSE6467" = "HAL_12wk_1.6mgkg_GSE6467",
  "HAL_4wk_1.6mgkg_GSE6511" = "HAL_4wk_1.6mgkg_GSE6511",
  "CLZ_4wk_1.6mgkg_GSE6511" = "CLZ_4wk_1.6mgkg_GSE6511"    ,
  "CLZ_hindbrain_B6_GSE33822" = "CLZ_hindbrain_B6_GSE33822",
  "CLZ_forebrain_B6_GSE33822" = "CLZ_forebrain_B6_GSE33822",
  "CLZ_wholebrain_B6_GSE33822" = "CLZ_wholebrain_B6_GSE33822",
  "LOX_110256" = "LOX_110256", "ZIP_110256" = "ZIP_110256", "LOX_119290" = "LOX_119290", "CLZ_93918" = "CLZ_93918", 
  "OLA-52615-cerebellum" = "OLA-52615-cerebellum" ,     "CLO 25mgkg (Iwata 2006)" ="CLO 25mgkg (Iwata 2006)",       
  "OLA 1.25mgkg (Iwata 2006)" ="OLA 1.25mgkg (Iwata 2006)", "QUE 18.75mgkg (Iwata 2006)" = "QUE 18.75mgkg (Iwata 2006)" ,   "THI 25 mgkg (Iwata 2006)" = "THI 25 mgkg (Iwata 2006)" ,    
  "CLO_0.1uM_6hr (Readhead 2018)" = "CLO_0.1uM_6hr (Readhead 2018)",  "CLO_0.03uM_6hr (Readhead 2018)" = "CLO_0.03uM_6hr (Readhead 2018)" ,"HAL_0.1uM_6hr (Readhead 2018)" = "HAL_0.1uM_6hr (Readhead 2018)" ,
  "Lox_1uM_6hr (Readhead 2018)" = "Lox_1uM_6hr (Readhead 2018)"  , "ARI-1uM-6h (Readhead 2018)" = "ARI-1uM-6h (Readhead 2018)"  ,   "QUE-10uM-6h (Readhead 2018)" = "QUE-10uM-6h (Readhead 2018)",   
  "QUE-0.03uM-6h (Readhead 2018)" = "QUE-0.03uM-6h (Readhead 2018)",  "RIS-0.03uM-6h (Readhead 2018)" = "RIS-0.03uM-6h (Readhead 2018)" , "ZIP-0.1uM-6h (Readhead 2018)" = "ZIP-0.1uM-6h (Readhead 2018)" , 
  "ZIP-0.03uM-6h (Readhead 2018)"= "ZIP-0.03uM-6h (Readhead 2018)" 
  
)


InsulinListFullDop <- list(
  "HFD_18wks (Anand, 2017)" ,       "HFD_15wks (Anand, 2017)" ,     
  "HFD_6day (Anand, 2017)"    ,    "HFD_10day (Anand, 2017)"  ,      
  "HFD_14day (Anand, 2017)"    ,     "HFD_3wks (Anand, 2017)"  ,       
  "HFD_6wks (Anand, 2017)"      ,    "HFD_9wks (Anand, 2017)"   ,      
  "HFD_12wks (Anand, 2017)"     ,    "CB1R (Bilkei et al.,2017)_2mon" ,
  "CB1R (Bilkei et al.,2017)_12mon", "NOS (Boone et al., 2017)"       ,
  "PI3K-LY29 (Chung et al., 2010)" , "MEK-U0126 (Chung et al., 2010)" ,
  "PI3K (Claeys et al., 2019)"     , "AICAR-Hippo-14day"              ,
  "AICAR-Hippo-7day"                ,"AICAR-Cortex-14day"             ,
  "AICAR-Cortex-7day"               ,"CB1R (Juknat et al., 2013)"     ,
  "HFD (Kruger 2012)"          ,     "MEK-U0126 (N.A.)"               ,
  "NOS (N.A)"                   ,    "SHSY5Y_A_1day"                  ,
  "SHSY5Y_A_2day"                ,   "SHSY5Y_A_3day"                  ,
  "SHSY5Y_A_6hr"                   ,
  "SHSY5Y_A_5day"                 ,  
  "SHSY5Y_E_1day"                  , "SHSY5Y_E_2day"                  ,
  "SHSY5Y_E_3day"            ,       "SHSY5Y_E_5day"                  ,
  "SHSY5Y_E_6hr"              ,      "HFD_1wk (Sergi 2018)"           ,
  "HFD_4wks (Sergi 2018)"      ,     "HFD_4wks (Vagena 2019)"         ,
  "HFD_8wks (Vagena 2019)", "GSE116813_THC", "GSE50873_AICAR_14D", "GSE50873_AICAR_7D",
  "GSE50873_AICAR_3D", "GSE100349_HFD", "GSE104709_HFD"
)

MCListFull <- list("GSE12214_1000ug-L", "GSE12214_100ug-L", "GSE29861_24hr_10uM", "GSE29861_24hr_1uM", 
                   "GSE29861_24hr_5uM", "GSE29861_4hr_100uM", "GSE29861_4hr_10uM", "GSE29861_4hr_50uM", 
                   "GSE59495_0.5hr_100ug-kg", "GSE59495_0.5hr_10ug-kg", "GSE59495_0.5hr_1ug-kg", 
                   "GSE59495_0.5hr_50ug-kg", "GSE59495_1hr_100ug-kg", "GSE59495_1hr_10ug-kg", 
                   "GSE59495_1hr_1ug-kg", "GSE59495_1hr_50ug-kg", "GSE59495_3hr_100ug-kg", 
                   "GSE59495_3hr_10ug-kg", "GSE59495_3hr_1ug-kg", "GSE59495_3hr_50ug-kg", 
                   "GSE59495_6hr_100ug-kg", "GSE59495_6hr_10ug-kg", "GSE59495_6hr_1ug-kg", 
                   "GSE59495_6hr_50ug-kg", "GSE59906_0.67days", "GSE59906_1day")

AgingListFull <- list("Aging_Mice_Hippocampus_PR",
                      "Aging_Mice_Cerebellum_PR",
                      "Aging_Mice_Cortex_PR")

CVListFull <- list(
  "GSE45042_MOCKvsEMC_24h",
  "GSE56192_CTLvsMERS_24h",
  "GSE56192_CTLvsSARS_24h",
  "GSE3326_MOCKvsSARS_36h"
)


AntidepressantsListFullDop <- list()

ADListFullDop <- list()

AddedListFullDop <- list()

AddedListFullDop_sc <- list()



BACellTypesList <- list(
  "Astro", "Endo", "Exc", "Inh", "Micro", "OPC", "Oligo"
)




# L1000 Genes List ----
L1000Genes <- c('AARS','ABCF1','ABL1','ACAA1','ACAT2','ACLY','ADAM10','ADH5','PARP1','ADRB2','AGL','AKT1','ALAS1','ALDOA','ALDOC','SLC25A4','ANXA7','APBB2','BIRC2','BIRC5','APOE','APP','FAS','RHOA','ARHGAP1','ASAH1','ATF1','ATP1B1','ALDH7A1','ATP6V0B','BAD','BAX','CCND1','BCL2','BDH1','BID','BLMH','BLVRA','BMP4','BNIP3','BNIP3L','BPHL','BRCA1','BTK','BUB1B','C5','DDR1','CALM3','CALU','CAPN1','CAST','CASP2','CASP3','CASP7','CASP10','CAT','CBLB','CBR1','CBR3','CCNA2','CCNB1','CCND3','CCNF','CCNH','SCARB1','CD40','CD44','CD58','ADGRE5','CDK1','CDC20','CDC25A','CDC25B','CDC42','CDH3','CDK2','CDK4','CDK6','CDK7','CDKN1A','CDKN1B','CDKN2A','CEBPA','CEBPD','CENPE','CETN3','CHEK1','CHN1','CIRBP','CLTB','CLTC','COL1A1','COL4A1','CREB1','CRK','CRKL','CRYZ','CSK','CSNK1A1','CSNK1E','CSNK2A2','CSRP1','CTNND1','CTSD','CTSL','CYB561','DAG1','DAXX','DCK','DCTD','DDB2','GADD45A','DDX10','DECR1','DFFA','DFFB','DLD','DNM1','DNMT1','DNMT3A','DPH2','DSG2','TSC22D3','DUSP3','DUSP4','DUSP6','TOR1A','E2F2','ECH1','EDN1','EGF','EGFR','EGR1','EIF4EBP1','EIF4G1','EIF5','ELAVL1','CTTN','EPB41L2','EPHA3','EPHB2','EPRS','NR2F6','ERBB2','ERBB3','ETFB','ETS1','ETV1','EXT1','EZH2','FAH','PTK2B','FAT1','FDFT1','FGFR2','FGFR4','FHL2','FKBP4','FOXO3','FOS','FPGS','FUT1','FYN','SLC37A4','GAA','GABPB1','GALE','GAPDH','GATA2','GATA3','GFPT1','GHR','GLI2','GLRX','GNA11','GNA15','GNAI1','GNAI2','GNAS','SFN','GPC1','GPER1','GRB7','GRB10','GRN','NR3C1','CXCL2','GSTM2','GSTZ1','MSH6','GTF2A2','GTF2E2','HSD17B10','HADH','HDAC2','HIF1A','HK1','HLA-DMA','HLA-DRA','HMGCR','HMGCS1','HMOX1','HOXA5','HOXA10','HPRT1','HES1','DNAJB2','HSPA1A','HSPA4'
                ,'HSPA8','HSPB1','HSPD1','DNAJB1','ICAM1','ICAM3','ID2','IDE','IFNAR1','IGF1R','IGF2R','IGFBP3','IGHMBP2','IKBKB','IL1B','IL4R','IL13RA1','ILK','INPP1','INSIG1','ITGAE','ITGB5','JUN','KCNK1','KIF5C','KIT','KTN1','LAMA3','STMN1','LBR','LGALS8','LIG1','LIPA','LOXL1','LRPAP1','LYN','SMAD3','MAN2B1','MAT2A','MBNL1','MCM3','ME2','MEF2C','MAP3K4','MEST','MIF','FOXO4','MMP1','MMP2','MNAT1','MSRA','MUC1','MYBL2','MYC','GADD45B','MYLK','MYO10','NCK1','NFATC3','NFATC4','NFE2L2','NFIL3','NFKB2','NFKBIA','NFKBIB','NFKBIE','NIT1','NMT1','NOS3','CNOT4','NOTCH1','PNP','NPC1','SLC11A2','NRAS','NUCB2','NUP88','NVL','ORC1','OXA1L','OXCT1','PAFAH1B1','PAFAH1B3','SERPINE1','PAK1','PCBD1','PCCB','PCK2','PCM1','PCMT1','PCNA','PDGFA','PFKL','PGAM1','PGM1','PHKA1','PHKB','PHKG2','PIK3C2B','PIK3C3','PIK3CA','PIN1','PLA2G4A','PLCB3','PLK1','PLP2','PLS1','PLSCR1','PMAIP1','PMM2','POLB','POLE2','POLR2I','POLR2K','PPARD','PPARG','PPIC','PPOX','PPP2R5A','PPP2R5E','PRCP','PRKACA','PRKCD','PRKCH','PRKCQ','MAPK9','MAPK13','MAP2K5','PRKX','PROS1','LGMN','HTRA1','PSMB8','PSMB10','PSMD2','PSMD4','PSMD9','PSMD10','PSME1','PSME2','PTGS2','PTK2','PTPN1','PTPN6','PTPN12','PTPRC','PTPRF','PTPRK','PXMP2','PXN','PYCR1','PYGL','RAB4A','RAB27A','RAC2','RAD9A','RAD51C','MOK','RALA','RALB','RALGDS','RAP1GAP','RASA1','RB1','KDM5A','RELB','RFC2','RFC5','RFNG','RFX5','RGS2','RHEB','RNH1','RPA1','RPA2','RPA3','MRPL12','RPN1','RPS5','RPS6','RPS6KA1','RSU1','RTN2','S100A4','S100A13','SATB1','SCP2','CCL2','SDHB','SGCB','SHB'
                ,'SHC1','SKIV2L','SKP1','SLC1A4','SMARCA4','SMARCC1','SMARCD2','SNAP25','SNCA','SOX2','SOX4','SPAG4','SPP1','SPR','SPTAN1','SRC','STAT1','STAT3','STAT5B','AURKA','STK10','STX1A','STX4','STXBP1','STXBP2','SUPV3L1','SUV39H1','SYK','SYPL1','TARBP1','TBP','TBX2','TBXA2R','TCEA2','VPS72','TCTA','DYNLT3','TERT','TESK1','TFAP2A','TFDP1','TGFB3','TGFBR2','TIAM1','TIMP2','TJP1','TLE1','TLR4','TSPAN6','TSPAN4','TOP2A','TP53','TP53BP1','TP53BP2','TPD52L2','TPM1','TSTA3','TXNRD1','UBE2A','UGDH','NR1H2','USP1','VDAC1','WFS1','WRB','XBP1','XPNPEP1','ZFP36','ZNF131','ZMYM2','PAX8','CXCR4','IFRD2','MAPKAPK3','USP7','REEP5','ST7','KAT6A','PDHX','FOSL1','HMGA2','NCOA3','NRIP1','SMC1A','LAGE3','AXIN1','CDC45','FZD1','FZD7','HIST2H2BE','PIP4K2B','NCK2','DYRK3','DUSP11','RAE1','PIK3R3','NIPSNAP1','IKBKAP','HAT1','MAPKAPK5','BHLHE40','MKNK1','CASK','AKR7A2','RUVBL1','PSMG1','BECN1','MBTPS1','EED','CTNNAL1','RNMT','PEX11A','CREG1','INPP4B','IQGAP1','SOCS2','CFLAR','CDK5R1','ST3GAL5','IER3','SQSTM1','SLC5A6','CPNE3','CCNA1','TIMELESS','P4HA2','PLOD3','NOL3','SLC25A14','MPZL1','MAP7','DNAJA3','USP14','MTA1','PDLIM1','SMC3','PRPF4','CCNB2','CCNE2','SYNGR3','LPAR2','ARHGEF2','ZW10','AURKB','VAPB','NOLC1','UBE2L6','MAPKAPK2','CYTH1','ITGB1BP1','BCL7B','COPB2','ADGRG1','TM9SF2','MAP4K4','HOMER2','SH3BP5','PIGB','PSMF1','SPTLC2','TBPL1','BAG3','POLR1C','SPAG7','FEZ2','IKBKE','MTFR1','HS2ST1','IPO13','VGLL4','NUP93','UBE3C','EDEM1','TRAM2','CEP57','KIAA0100','HERPUD1','KIAA0355','USP6NL','CCP110','MLEC','TATDN2','MRPL19','SCRN1','EFCAB14','KEAP1','MELK','PLEKHM1','C2CD5','KIAA0753','C2CD2L','TOMM70A','KIAA0196','KLHL21','ARNT2'
                ,'FAM20B','NCAPD2','PAN2','LPGAT1','KIF14','OXSR1','MVP','DMTF1','GNPDA1','HDAC6','PARP2','MAMLD1','DNAJB6','SMC4','ABCC5','ABCB6','DNM1L','TSPAN3','KIF20A','ARL4C','TRAP1','G3BP1','MBNL2','CEBPZ','SLC25A13','SORBS3','RBM6','TXNDC9','TRIM13','TRIB1','MFSD10','SLC35B1','TIMM17B','AKAP8','STUB1','NET1','SMNDC1','PAK4','TNIP1','IKZF1','TMEM5','HMG20B','MYL9','LYPLA1','PPIE','VAV3','LRRC41','CRTAP','VAT1','STK25','APPBP2','CHERP','HYOU1','RPP38','SLC35A1','DRAP1','PAICS','ST6GALNAC2','STAMBP','NPRL2','IGF2BP2','YKT6','CGRRF1','RRAGA','GNB5','EBP','CNPY3','YME1L1','TCFL5','KDM5B','POP4','ARPP19','ZNF274','MTHFD2','WASF3','UTP14A','FRS2','CLPX','PGRMC1','MALT1','CPSF4','BLCAP','TCERG1','RNPS1','TOMM34','PDIA5','MLLT11','EBNA1BP2','TMED10','ASCC3','SLC27A3','KIF2C','CCDC85B','TLK2','KDELR2','RAB31','B4GAT1','PAPD7','UBE2C','DUSP14','TOPBP1','PRSS23','PWP1','PKIG','CORO1A','LSM6','PSIP1','SLC2A6','NISCH','CHEK2','PRAF2','POLG2','CHP1','PNKP','ECD','DDX42','TWF2','CASC3','COG2','ATF5','MTF2','PUF60','RAB11FIP2','CLSTN1','FOXJ3','KIAA0907','EPN2','SACM1L','ATF6','RPIA','RAB21','SPEN','FBXO21','RBM34','WDTC1','XPO7','TBC1D9B','RRP1B','MYCBP2','CDK19','GPATCH8','MAST2','DCUN1D4','FCHO1','SNX13','ATP11B','JMJD6','RRS1','RRP12','SYNE2','PDS5A','CAMSAP2','ATMIN','TRIM2','KIAA1033','USP22','WDR7','JADE2','ARHGEF12','PPP1R13B','RRP8','NUDCD3','SIRT3','SLC35A3','ICMT','MACF1','SUZ12','KAT6B','NNT','ADAT1','TMEM50A','KLHDC2','ACOT9','SSBP2','NUP62','ARFIP2','LSM5','PLA2G15','TMEM2','ZNF318','FBXO7','SPDEF','BAMBI','BACE2','COG4','MPC2','CLIC4','C2CD2','TIPARP','TSKU','RNF167','LRP10','ZNF451','SENP6','RAI14','KIF1BP'
                ,'TES','PHGDH','MYCBP','CHIC2','TIMM9','AKAP8L','ATP2C1','TRAPPC3','ATP5S','TNFRSF21','SESN1','HTATSF1','TMEM97','BZW2','CHMP4A','GTPBP8','DNAJC15','PACSIN3','RBM15B','HOOK2','SNX11','TIMM22','NENF','UBQLN2','ERO1L','DNTTIP2','PIK3R4','HDGFRP3','COPS7A','NSDHL','HEBP1','MTERF3','AMDHD2','ISOC1','MRPS16','FIS1','GOLT1B','GLOD4','GMNN','LAP3','NOSIP','DERA','SCCPDH','MRPS2','VPS28','HSD17B11','NUSAP1','SCAND1','CD320','NGRN','SNX7','ATP6V1D','ZNF589','PRKAG2','UBE2J1','EVL','HACD3','UFM1','LSR','DHRS7','CAB39','ARID4B','NUDT9','CYCS','TERF2IP','GFOD1','KCTD5','TMCO1','DHX29','EXOSC4','DDIT4','PAF1','P4HTM','SLC35F2','ZNF586','FBXL12','TEX10','YTHDF1','TXNL4B','HERC6','PIH1D1','PPP2R3C','FKBP14','CDCA4','PLEKHJ1','HEATR1','ANO10','UBR7','FAIM','ADI1','ABCF3','ENOSF1','LRRC16A','ANKRD10','STAP2','IARS2','NUP133','CNDP2','FAM63A','KDM3A','PECR','EAPP','CISD1','ZNF395','KLHL9','NPDC1','TM9SF3','PAK6','DUSP22','ADCK3','CIAPIN1','PLSCR3','SCYL3','LYRM1','ZMIZ1','MCOLN1','THAP11','ABHD6','TRIB3','POLD4','SQRDL','ENOPH1','PRUNE','SNX6','FASTKD5','ELAC2','ABHD4','MCUR1','RBKS','ATG3','NARFL','ZDHHC6','ACBD3','CERK','NT5DC2','ACD','INTS3','TRAK2','METRN','ELOVL6','TMEM109','CCDC86','TRAPPC6A','CHAC1','MBOAT7','PRR15L','CRELD2','FSD1','TCTN1','CHMP6','NPEPL1','FAM57A','NUP85','TCEAL4','DHDDS','DENND2D','FBXO11','CCDC92','COASY','WDR61','TSEN2','PRR7','ITFG1','GDPD5','GRWD1','ARID5B','TUBB6','PSRC1','ADO','HIST1H2BK','MICALL1','UBE3B','HN1L','SLC25A46','COG7','MAPK1IP1L','TBC1D31','H2AFV','RPL39L','CANT1','WIPF2','TICAM1','TXLNA','SPRED2','EML3','TMEM110
                ','FAM69A')


# Uplaod Tab formats ----
pre_sc_table <- tibble(
  Gene_Symbol = character(),
  Log2FC = numeric(),
  P_Value = numeric(),
  DataSet = character(),
  Fold_Change = numeric(),
  Log2FCAbs = numeric(),
  ecdfPlot = numeric(),
  Dir = character(),
  Group = character(),
)

ex_table <- tibble(
  Gene_Symbol = c("Gene 1", "Gene 2", "Gene 3"),
  Log2FC = c(1.23, -0.47, 2.84),
  P_Value = c(0.09, 0.67, 0.94),
  DataSet = c("XXX")
)