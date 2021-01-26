library(tidyverse)
library(pool)
library(dbplyr)

theme_set(theme_bw())

go_ids <- readRDS("data/List_of_GOGenes.rds")
go_ids <- read_delim("data/GO_Biological_Process_2018.txt", delim = "\t", col_names = F)

go_ids %>% select(-X2) %>% pivot_longer(2:ncol(.)) %>% 
  select(-name) %>% filter(!is.na(value)) %>% 
  filter(X1 == "excitatory postsynaptic potential (GO:0060079)") %>% 
  pull(value) -> genes_of


my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)

poolClose(my_db)

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



c("Deep_Neurons","barnesAll",
"barnesFemale",
"barnesMale",
"Iwamoto_BA10_SCZ",
"Iwamoto_BA46_SCZ","MaycoxMale",
"hiPSC_Neuron", 
"hiPSC_NPC1",
"BloodmRNA",
"Lewis_2015_L3",
"Lewis_2015_L5",
"Lewis_2017_L3",
"Lewis_2017_L5",
"PietersenAllParvalbumin","PietersenFemaleParvalbumin", "PietersenFemalePyramidal",
"PietersenMaleParvalbumin" , "PietersenMalePyramidal",     "PietersenAllPyramidal"
) -> scz_ds


tbl(my_db, "lookup_new") %>% 
  #fullDataSet %>% 
  select(Gene_Symbol, Log2FC, Fold_Change, P_Value, ecdfPlot,DataSet, Group) %>%
  filter(Gene_Symbol %in% genes_of, DataSet %in% scz_ds) %>% collect() -> half_table


half_table %>% mutate(Ref = case_when(
  DataSet == "Deep_Neurons" ~ "Ref",
  DataSet %in% dbListFull$`Region Level` ~ "Region",
  DataSet %in% dbListFull$`Cell Level` ~ "Cell",
)) -> half_table


half_table %>% mutate(DataSet = factor(DataSet, levels = scz_ds)) -> half_table


half_table %>% 
  ggplot(aes(DataSet, Log2FC, fill = Ref) ) + geom_violin() +
  geom_boxplot(width = 0.3) + geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "regulation of postsynaptic membrane potential (GO:0060078)", x = "",
       subtitle = paste(genes_of, collapse = ",")
       ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 4),
        title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.title = element_blank()
        #legend.position = "none"
        ) + ylim(c(-0.8, 0.8))


half_table %>% group_by(DataSet) %>% summarise(Dir = Log2FC >= 0) %>% count(Dir) %>%
  mutate(n = ifelse(Dir, n, n * -1)) %>% 
  ggplot(aes(DataSet, n, fill = Dir)) + geom_bar(stat="identity", position="identity")



####@#

sel_all <- c("AMP_AD_MSSM_PHG_MALE",   "AMP_AD_MSSM_STG_MALE" ,  "AMP_AD_MSSM_STG_FEMALE")

tbl(my_db, "lookup_new") %>% 
  select(Gene_Symbol, Log2FC, Fold_Change, P_Value, ecdfPlot,DataSet, Group) %>%
  filter(DataSet %in% sel_all) %>% collect() -> half_table
