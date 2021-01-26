library(fgsea)
library(dbplyr)
library(pool)
library(tidyverse)
# gsea 


my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)

poolClose(my_db)

scz_datasets <- list("Groups" = c("Region", "Cell"),
                     
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


tbl(my_db, "lookup_new") %>% 
  select(Gene_Symbol, Log2FC,DataSet, Group) %>%
  filter(Group == "SCZ") %>% collect() -> scz_datasets

scz_datasets %>% select(-Group) %>% 
  distinct(DataSet, Gene_Symbol, .keep_all = T) %>% 
  arrange(DataSet,desc(Log2FC)) %>% 
  mutate(
         ranked = setNames(Log2FC, Gene_Symbol)
         ) -> ranked_ds


ranked_ds$ranked %>% head()

X <- split(ranked_ds$ranked, ranked_ds$DataSet)


library(fgsea)

#gsea_pathway <- gmtPathways("../BioPathNetApp/files/gsea_genesets/c5.bp.v7.1.symbols.gmt")
gsea_pathway <- gmtPathways("Human_GO_AllPathways_with_GO_iea_June_20_2019_symbol.gmt")


#names(gsea_pathway) <- names(gsea_pathway) %>% str_replace_all(., "_", " ") %>%  str_remove(.,"^GO ")

names(gsea_pathway) <- names(gsea_pathway) %>% str_extract(., "(^[\\D\\w]+%[\\D\\w]+)%") %>% 
  str_remove(., "%$")

names(gsea_pathway)[!is.na(names(gsea_pathway))]

yy <- X[1:2]

set.seed(123)
start1 <- Sys.time()
map(X, fgsea::fgsea, pathways =  gsea_pathway, nperm = 1000, minSize = 15,maxSize = 500 ) -> res
end1 <- Sys.time() 

end1 - start1


res %>% v


enframe(res) %>%
  unnest -> res_df

colnames(res_df)

library(tidyverse)
write_delim(select(res_df, -leadingEdge, -size, -nMoreExtreme),"gsea_table_scz_new_all_dbs.txt", delim = "\t")

theme_set(theme_bw())



#GO_ACTIVATION_OF_INNATE_IMMUNE_RESPONSE
#GO_VENTRAL_SPINAL_CORD_DEVELOPMENT

res_df_2 <- read_delim("gsea_table_scz.txt", delim = "\t")


tgt_path <- "CRANIAL NERVE MORPHOGENESIS"

red_res_df <- filter(res_df, pathway == tgt_path)

red_res_df %>% 
  mutate(Group = case_when(
  name %in% scz_datasets$`Region Level` ~ "Region",
  name %in% scz_datasets$`Cell Level` ~ "Cell",
)) %>% 
  ggplot(aes(reorder(name, ES), ES)) + geom_col(aes(fill = Group)) + 
  geom_point(data = subset(red_res_df, padj <= 0.25)) + coord_flip() +
  labs(title = tgt_path
       #subtitle = paste(gsea_pathway$GO_VENTRAL_SPINAL_CORD_DEVELOPMENT, collapse = ",")
  ) + 
  theme(
        title = element_text(size = 8),
        plot.subtitle = element_text(size = 2),
        legend.title = element_blank()
  )
  




res_df %>% mutate(Group = case_when(
  name %in% scz_datasets$`Region Level` ~ "Region",
  name %in% scz_datasets$`Cell Level` ~ "Cell",
)) %>% 
  group_by(pathway, Group) %>% 
  summarise(Up  = sum(ES > 0),
         Down = sum(ES < 0)) %>% View()


sup_neg <- read_delim("gsea_report_for_na_neg_1561394485305.txt", delim = "\t", col_names = T)

res_df %>% filter(name == "Superficial_Neurons") %>% View()
gsea_pathway$GO_PROTEASOMAL_UBIQUITIN_INDEPENDENT_PROTEIN_CATABOLIC_PROCESS -> genes_imm



# enframe(gsea_pathway) %>%
#   unnest -> gsea_pathway_df
# 
# gsea_pathway_df$name %>% unique()
# 
# write_delim(gsea_pathway_df, "gsea_go_pathways_7-1.txt", delim = "\t")


enframe(gsea_pathway) %>%
  unnest -> gsea_pathway_df

gsea_pathway_df %>% group_by(name) %>% summarise(genes = paste(value, collapse = ",")) -> gsea_pathway_df_2

gsea_pathway_df$name %>% unique()

write_delim(gsea_pathway_df_2, "gsea_go_pathways_alldbs.txt", delim = "\t")


