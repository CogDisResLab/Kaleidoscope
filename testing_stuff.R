library(dplyr)
library(tidyverse)
library(data.table)
library(gt)
fullDataSet <- readRDS("/Users/khaled/Desktop/Master_Classes/Summer_2018/Shiny_Apps/PsychoScope/PsychoScope/fullDataSet_Oct8.rds")
fullDataSet %>% 
  mutate(Group = case_when(
    DataSet %in% scz ~ "SCZ",
    DataSet %in% mddListFullDop %>% unlist() %>% as.vector() ~ "MDD",
    DataSet %in% dbListFullDop %>% unlist() %>% as.vector() ~ "Dop",
    DataSet %in% AntipsychoticsListFullDop %>% unlist() %>% as.vector() ~ "AP",
    DataSet %in% InsulinListFullDop %>% unlist() %>% as.vector() ~ "Ins"
  )) -> fullDataSet

fullDataSet %>% filter(!DataSet %in% c("D3 Proteomics")) -> fullDataSet

fullDataSet %>% filter(Group == "Dop") %>% View()

InsulinListFullDop %>% unlist() %>% as.vector() -> inslist
AntipsychoticsListFullDop %>% unlist() %>% as.vector() -> aplist

saveRDS(fullDataSet,"/Users/khaled/Desktop/Master_Classes/Summer_2018/Shiny_Apps/PsychoScope/PsychoScope/fullDataSet_Oct15.rds")  

fullDataSet <- readRDS("/Users/khaled/Desktop/Master_Classes/Summer_2018/Shiny_Apps/PsychoScope/PsychoScope/fullDataSet_Oct15.rds")

fullDataSet$Log2FCAbs <- abs(fullDataSet$Log2FC)
fullDataSet$Dir <- ifelse(fullDataSet$Log2FC>0,"Up", "Down")

gsub(" ", "", "Fos ")

fullDataSet$`Gene Symbol` <- gsub(" ", "", fullDataSet$`Gene Symbol`)
maxMod <- function(...){
  max(...,na.rm = T)
}
data.table::setorder(setDT(fullDataSet), DataSet,-Log2FCAbs)[, indx := seq_len(.N), DataSet][indx <= 1000] %>%  
  select(`Gene Symbol`, Group, Dir) %>% 
  add_count(`Gene Symbol`, name = "Hits") %>% add_count(`Gene Symbol`,Group, name ="HitsPerGroup") %>% 
  add_count(`Gene Symbol` , Dir,name = "All") %>% 
  add_count(`Gene Symbol`, Group , Dir, name = "byDir") %>% 
  distinct() -> tempDf

  tempDf %>% select(`Gene Symbol`, All, Dir) %>% 
  pivot_wider(names_from = Dir, values_from = c(All), values_fn = list(All = max)
              
              ) -> tempDfAll

  tempDf %>% select(-All) %>% 
  pivot_wider(names_from = Dir, values_from = c(byDir), values_fill = list(Up = 0, Down = 0)
              ) %>% 
  pivot_wider(names_from = c(Group), values_from = c(HitsPerGroup,Down, Up),names_sep = ">") %>% 
  left_join(tempDfAll, by = "Gene Symbol") %>% select(`Gene Symbol`, Hits ,Up, Down, everything()) %>% 
 
  arrange(desc(Hits)) %>% ungroup() %>% 
  slice(1:100) %>% 
  gt() %>% cols_split_delim(delim = ">") %>% 
  fmt_missing(columns = 1:ncol(.), missing_text = "-")
  
  xx %>% select(`Gene Symbol`, Hits, Up, Down) %>% 
    mutate(
      isUp = case_when(
        is.na(Up) ~ F,
        is.na(Down) ~ T,
        Up > Down ~ T,
        Up < Down ~ F
      )
    ) -> xxx 
  
  split(xxx$`Gene Symbol`,xxx$isUp ) -> xxx
  
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#F9E3D6"),
  #     cell_text(style = "italic")
  #   ),
  #   locations = cells_data( 
  #     columns = ends_with("AP")
  #     )
  #   ) %>% 
  # data_color(
  #   columns = starts_with("Up"),
  #   colors = scales::col_numeric(
  #     palette = c(
  #       "red", "orange", "yellow"),
  #     domain = NULL)
  # )
  
  
  View()
  
  spec <- fullDataSet %>% dplyr::rename(p = `P Value`) %>% 
    expand(DataSet, .value = c("Log2FC", "p")) %>% 
    mutate(.name = paste0(DataSet, ">", .value))

  fullDataSet %>% dplyr::rename(p = `P Value`) %>% distinct(`Gene Symbol`,DataSet, .keep_all=T) %>% 
    select(-`Fold Change`, -ecdfPlot) %>% 
    mutate_if(is.numeric, round, 3) %>% slice(1:100) %>% 
    pivot_wider_spec(spec = spec) %>% gt() %>% 
    cols_split_delim(delim = ">") %>% fmt_missing(columns = 1:ncol(.), missing_text = "-")
