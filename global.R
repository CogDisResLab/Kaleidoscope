# Libs ----------

library(shiny)
library(reactable)
library(tidyverse)
library(echarts4r)
library(shinyWidgets)
library(pool)
library(shinyjs)
library(shinyalert)
library(png)
library(dbplyr)
library(bsplus)
library(waiter)
library(httr)
#library(visNetwork)

#library(reactlog)

# ggplot theme
theme_set(theme_bw())

# Modules -----------------------------------------------------------------

source("modules/geneInputUI_mod.R")
source("modules/brainrnaseq_mod.R")
source("modules/string_mod.R")
source("modules/process_input.R")
source("modules/ilincs_mod.R")
source("modules/braincloud_mod.R")
source("modules/gwascatalog_mod.R")
source("modules/gtex_mod.R")
source("modules/brainatlas_mod.R")
source("modules/lookup_mod.R")
source("modules/enrichr_mod.R")
source("modules/info_mod.R")
source("modules/download_mod.R")

# Data ----

all_human_prop <- readRDS("modules/brainseq_avg_human_prop_fixed.RDS")
all_mice_prop <- readRDS("modules/brainseq_avg_mice_prop_fixed.RDS")

ba_all_human_avg <- readRDS("modules/brainatlas_avg_all_genes.RDS")

# DB connect ----

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase_new",
  host = "cdrlshinyapps-2.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)


onStop(function() {
  poolClose(my_db)
})

# Find datasets
tbl(my_db, "lookup_updated_meta") %>%
  collect() %>% 
  filter(!DataSet %in% c("DISC1_Prot", "Disc_MDD_Proteomics")) -> ks_datasets


#tell shiny to log all reactivity
#reactlog_enable()
# 
#shiny::reactlogShow()






