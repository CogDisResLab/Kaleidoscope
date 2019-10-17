# Loading Libraries ------
library(shiny)
library(shinyjs)
library(gplots)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(httr)
library(networkD3)
library(rjson)
library(rvest)
library(png)
library(tidyverse)
library(DT)
library(conflicted)
library(pheatmap)
library(stringr)
library(Hmisc)
library(corrplot)
library(gt)
library(data.table)

conflict_prefer("box", "shinydashboard")
conflict_prefer("filter", "dplyr")
conflict_prefer("show", "shinyjs")


# Functions -----

firstLetterCap <- function(x) {
    x <- gsub(" ", "", x)
    x <- tolower(x)
    x <- strsplit(x,"")
    x[[1]][1] <- toupper(x[[1]][1])
    paste(x[[1]],collapse = "")
}

gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

'%ni%' <- Negate('%in%')

# Global Varaibles ----
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
                       "CommonMind DLPFC Re 1"="CommonMind_DLPFC"
                   ),
                   
                   # Cell Level Datasets
                   "Cell Level" = c(
                       "Superficial_Neurons", "Deep_Neurons", 
                       "Superficial_Deep_Neurons",
                       "hiPSC_Neuron", 
                       "hiPSC_NPC1",
                       "Blood mRNA" = "BloodmRNA",
                       "DISC1 RNAseq" = "DISC1_RNA",
                       "DISC1 Proteomics" = "DISC1_Prot"
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
    "Disc_MDD_Proteomics",
    "Benoit_Mouse_PFC",
    "Benoit_Mouse_NAC", 
    #"D3 Proteomics", 
    "D3 RNAseq",
    "ALS D1", 
    "ALS D2"
    
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

AddedListFullDop <- list()


BACellTypesList <- list(
    "Astro", "Endo", "Exc", "Inh", "Micro", "OPC", "Oligo"
)
# UI ---- 
ui <- 
    
    dashboardPage(
        
        # Tabs Headers ----
        dashboardHeader(title = "Datasets", 
                        shiny::tags$li(a(href='http://www.utoledo.edu/med/depts/neurosciences/CDRL.html', target="_blank",
                                         img(src = 'Toledo.jpg',
                                             title = "UTlogo", height = "50px"),
                                         style = "padding-top:0px; padding-bottom:0px; padding-right:0px; padding-left:0px;"), class = "dropdown"))
        ,
        dashboardSidebar(
            sidebarMenu(
                id = "tabsList",
                menuItem("Info", tabName = "KalidInfo"),
                menuItem("Brain RNA-Seq", icon = icon("signal"),
                         startExpanded = F,
                         menuSubItem("One Target", tabName = "barresDB2"),
                         menuSubItem("Multiple Targets", tabName = "barresDB3")
                ),
                menuItem("BrainCloud", tabName = "brainCloud", icon = icon("cloud")),
                menuItem("GTEx", tabName = "GTEx", icon = icon("creative-commons-sampling")),
                menuItem("BrainAtlas", tabName = "brainAtlas", icon = icon("compass")),
                #menuItem("BrainSpan", tabName = "brainSpanTab", icon = icon("address-card")),
                menuItem("STRING", tabName = "stringTab", icon = icon("fullscreen", lib = "glyphicon")),
                menuItem("Lookup", tabName = "lookupTab", icon = icon("search")),
                #menuItem("Datasets", tabName = "DsInfo", icon = icon("wpexplorer")),
                menuItem("iLINCS?", tabName = "iLINCSTab", icon = icon("connectdevelop")),
                menuItem("GWAS Catalog", tabName = "GWAS", icon = icon("thumbtack"))
                
            )
        ),
        
        dashboardBody(
            tags$head(HTML(
                "<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-149864972-1'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-149864972-1');
</script>"
                
            )),
            useShinyjs(),
            tabItems(
                
                # STRING UI Tab ----
                tabItem(tabName = "stringTab",
                        fluidPage(
                            fluidRow(
                                column(6, 
                                       textInput("strNet", "Enter a target", placeholder = "", value = ""),
                                       sliderInput("slider1", label = h3("Score"), min = 0,
                                                   max = 999, value = 500, step = 10),
                                       numericInput("scoreInp","", min = 0, max = 999, value = 500),
                                       sliderInput("slider2", label = h3("Nodes"), min = 1,
                                                   max = 100, value = 25, step = 1),
                                       actionButton("getNet", "Get Network!"),tags$hr(),htmlOutput("err")
                                ),
                                
                                column(12, 
                                       plotOutput("myPlot", height = "700px"), 
                                       shinyjs::hidden(div(id = "stringInfoDIV",
                                                           img(src = "stringInfo.png", width = "100%", hight = "500px")
                                       )),
                                       tags$hr(),verbatimTextOutput("intctos"),hidden(actionButton("trnsNet", "Copy")), 
                                       tags$hr() ,
                                       box(
                                           title = "STRING Table", width = NULL, status = "primary",
                                           div(style = 'overflow-x: scroll', shiny::tableOutput('NetTable'))
                                       )
                                       #DTOutput("NetTable")
                                )
                            )
                        )
                ),
                
                # Lookup UI Tab ----
                tabItem(tabName = "lookupTab", 
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("inputChoice", label = "Input:", 
                                                 choices = list("Your own list" = "ownList", "Common hits" = "CommHits")),
                                    textInput("gene", "Enter target(s) separated by the selected separation character:",
                                              placeholder = "e.g. LDAH, DLG4", value = ""),
                                    
                                    radioButtons("sepChoice", label = "Select separation character:", 
                                                 choices = list("Comma" = ",", "Space" = " ", "Semicolon" = ";")),
                                    pickerInput(inputId="dbs",label="Schizophrenia",choices=dbListFull, options = list(`actions-box` = TRUE), 
                                                multiple = TRUE),
                                    pickerInput(inputId="dbs2",label="Dopamine",choices=dbListFullDop, options = list(`actions-box` = TRUE), 
                                                multiple = TRUE),
                                    pickerInput(inputId="dbs4",label="MDD",choices=mddListFullDop, options = list(`actions-box` = TRUE),
                                                multiple = TRUE),
                                    pickerInput(inputId="dbs5",label="Antipsychotics",choices=AntipsychoticsListFullDop, options = list(`actions-box` = TRUE),
                                                multiple = TRUE),
                                    pickerInput(inputId="dbs6",label="Insulin Signaling Inhibition",choices=InsulinListFullDop, options = list(`actions-box` = TRUE),
                                                multiple = TRUE),
                                    pickerInput(inputId="dbs3",label="Added",choices=AddedListFullDop, options = list(`actions-box` = TRUE),
                                                multiple = TRUE),
                                    
                                    tags$hr(),
                                    
                                    actionButton("act", "Get Results!"), tags$hr(),
                                    
                                    # radioButtons("ThresChoice", label = "Criteria:", 
                                    #              choices = list("Log2 FC" = "Log2FC", "ECDF" = "ecdf")),
                                    # 
                                    #sliderInput("CommThres", "Log2FC Threshold:", min = 0, max = 2, step = 0.1, value = 1),
                                    #sliderInput("CommThres2", "ECDF Threshold:", min = 0, max = 1, step = 0.01, value = 0.95),
                                    sliderInput("CommTop", "Top hits per dataset:", min = 100, max = 3000, step = 100, value = 1000),
                                    sliderInput("CommTop2", "Top Overlaping hits:", min = 100, max = 1000, step = 10, value = 100),
                                    switchInput(inputId = "showtable",size = "mini", label = "Display table?", value = T),
                                    #fileInput("DSinput", "Upload a dataset:", accept = c(".RDS")),
                                    #textInput("DSNameInput", "Name your dataset:"),
                                    #actionButton("DSupload", "Upload"),
                                    htmlOutput("err2"),
                                    verbatimTextOutput("targetMissing")
                                ),
                                mainPanel( 
                                    
                                    conditionalPanel(condition = "input.act", {
                                        tabsetPanel( id = "LUResults", 
                                                     tabPanel("Results", 
                                                              gt_output(outputId = "t1"),
                                                              tags$hr(),
                                                              # box(
                                                              #   title = "Lookup Table", width = NULL, status = "primary",
                                                              #   div(style = 'overflow-x: scroll', shiny::tableOutput('t1'))
                                                              # ),
                                                              verbatimTextOutput("targetListed"),
                                                              downloadButton("down_1", "Download LookUp table"),
                                                              tags$hr(),
                                                              gt_output(outputId = "overlapTable"),
                                                              tags$hr(),
                                                              downloadButton("down_2", "Download Summary table"),
                                                              tags$hr(),
                                                              box("Up", width = 12,
                                                                  verbatimTextOutput("classifyGenesUp")
                                                                  ),
                                                              box("Down", width = 12,
                                                                  verbatimTextOutput("classifyGenesDown")
                                                              ),
                                                     ),
                                                     tabPanel("Lookup Graph", 
                                                              sliderInput("lookupBarGraphPar1", "Log2FC Threshold:", min = 0, max = 4, step = 0.1, value = 0.3),
                                                              sliderInput("lookupBarGraphPar2", label = "text size", min = 1, max = 15, value = 7),
                                                              plotOutput("lookupBarGraph", height = "650px")
                                                              
                                                     ),
                                                     tabPanel("Log2FC HM", plotOutput("HM2", height = "700px"),
                                                              
                                                              fluidRow(
                                                                #box("Figure Adjustments", status = "primary",
                                                                    column(6,
                                                              sliderInput("heatmapTH1", "Slider:", min = 0.2, max = 10, value = 1),
                                                                    ),
                                                                    column(6,
                                                              sliderInput("heatmap2SizeRow", "row labels size:", min = 0, max = 3, value = 1, step = 0.1),
                                                              sliderInput("heatmap2SizeCol", "col labels size:", min = 0, max = 3, value = 0.6, step = 0.1)
                                                                    )
                                                                #)
                                                              ),
                                                              downloadButton("HM2_down", "Download")
                                                     ),
                                                     tabPanel("FC HM", plotOutput("HM3", height = "700px"),
                                                              
                                                              fluidRow(
                                                              #box("Figure Adjustments", status = "primary",
                                                                  column(6,

                                                                  sliderInput("heatmapTH2", "Slider:", min = 1.1, max = 10, value = 2)


                                                                  ),
                                                                  column(6,
                                                                  sliderInput("heatmap3SizeRow", "row labels size:", min = 0, max = 3, value = 1, step = 0.1),
                                                                  sliderInput("heatmap3SizeCol", "col labels size:", min = 0, max = 3, value = 0.6, step = 0.1)
                                                                  )
                                                              #)
                                                              ),
                                                              downloadButton("HM3_down", "Download")
                                                     ),
                                                     tabPanel("Overlapping HM", plotOutput("HM5", height = "700px"),
                                                              sliderInput("heatmap5SizeRow", "row labels size:", min = 1, max = 10, value = 4),
                                                              sliderInput("heatmap5SizeCol", "col labels size:", min = 1, max = 10, value = 2)
                                                     ),
                                                     tabPanel("HMAP4", plotly::plotlyOutput("HM4", height = "700px")),
                                                     tabPanel("Cor", 
                                                              radioButtons("corGenes", label = "Genes selected:", 
                                                                           choices = list("From list" = "fromList", "Use Full Signatures" = "fullSig")),
                                                              plotOutput("CorPlot", height = "700px")),
                                                     tabPanel("Info", 
                                                              DT::dataTableOutput("DatabasesFullInfo")
                                                     )
                                                     
                                        )
                                    })
                                )
                                
                            )
                        )
                        
                ),
                
                # iLINCS UI Tab -----
                tabItem("iLINCSTab", 
                        
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(textInput("iLINCSSearch", "Search iLINCS for Gene Knockdown:"),
                                             radioButtons("sepChoice2", label = "Select separation character:", choices = list("Comma" = ",", "Space" = " ", "Semicolon" = ";")),
                                             tags$hr(), actionButton("isearch", "Search")             
                                             
                                ),
                                mainPanel(
                                    tabsetPanel(id = "iLINCSTables", 
                                                
                                                tabPanel("Summary", DTOutput("iLINCST1")),
                                                tabPanel("Detailed", DTOutput("iLINCST2"))
                                                
                                    )
                                )
                            )
                        )
                        
                ),
                tabItem("GWAS", 
                        
                        fluidPage(
                            
                            
                            textInput("GWASSearch", "Search GWAS Catalog Database: "),
                            tags$hr(), actionButton("GWASBtn", "Search")             
                            
                            ,
                            
                            tabsetPanel(id = "GWASResults", 
                                        
                                        tabPanel("Table", DT::dataTableOutput("GWASTable1")),
                                        tabPanel("Graph", 
                                                 sliderInput("GWASPar2", label = h3("Top"), min = 0,
                                                             max = 100, value = 25, step = 5),
                                                 # switchInput(inputId = "GWASPar1",size = "mini", label = "Top10", value = F),
                                                 plotOutput("GWASGraph1", height = "750px")),
                                        tabPanel("Sankey", 
                                                 sliderInput("GWASPar3", label = h3("Top"), min = 1,
                                                             max = 30, value = 10, step = 1),
                                                 
                                                 sankeyNetworkOutput("GWASGraph2", width = "100%", height = "750px")
                                                 
                                        )
                                        
                                        
                            )
                            
                            
                        )
                        
                ),
                # Datasets Overview UI Tab ----
                # tabItem("DsInfo",
                #         
                #         fluidPage(
                #           sidebarLayout(
                #             sidebarPanel(radioButtons("DsSelect", "Select Dataset:", 
                #                                       choices = list(
                #                                         "Stanley Database" = "Stanley",
                #                                         "Mt.Sinai ACC" = "MtSinaiACC",
                #                                         "Mt.Sinai DLPFC" = "MtSinaiDLPFC",
                #                                         "Mt.Sinai TPA" = "MtSinaiTPA",
                #                                         "Mt.Sinai MTA" = "MtSinaiMTA",
                #                                         "Gandal Microarray" = "gandalMicro",
                #                                         "Gandal RNAseq" = "gandalRNAseq",
                #                                         "DISC1 RNAseq" = "DISC1_RNA",
                #                                         "DISC1 Proteomics" = "DISC1_Prot",
                #                                         "Super", "Deep", 
                #                                         "hiPSC_Neuron", 
                #                                         "hiPSC_NPC1",
                #                                         "Blood mRNA" = "BloodmRNA"
                #                                         
                #                                         
                #                                       ), 
                #                                       selected = "MtSinaiMTA"),
                #                          
                #                          sliderInput("DSslider2", label = h3("Threshold"), min = 0, 
                #                                      max = 10, value = 1.5, step = 0.1),
                #                          sliderInput("DSslider3", label = h3("Label"), min = 1, 
                #                                      max = 10, value = 5, step = 0.1)
                #                          
                #             ),
                #             mainPanel(plotOutput("DsGraph"),
                #                       tags$hr(),
                #                       verbatimTextOutput("DSoutput"),
                #                       shiny::dataTableOutput("DStable") 
                #                       
                #             )
                #           )
                #           
                #         )
                # ),
                # Braincloud UI Tab ----
                tabItem("brainCloud", 
                        fluidPage(
                            
                            textInput("brainCloudText", "Check Gene(s) Expression Levels in the Brain", placeholder = "", value = ""),
                            switchInput(inputId = "brainCloudPar1",size = "mini", label = "Points", value = T),
                            switchInput(inputId = "brainCloudPar2",size = "mini", label = "SE", value = T),
                            htmlOutput("errBC"),
                            actionButton("brainCloudButton", "Plot"),
                            plotOutput("brainCloudPlot", height = "750px")
                        )
                        
                ),
                # BrainAtlas UI Tab ----
                tabItem("brainAtlas", 
                        fluidPage(
                            
                            textInput("brainAtlasText", "Single Cell Transcriptional Profiling (Human, Middle Temporal Gyrus (MTG))",
                                      placeholder = "", value = ""),
                            #switchInput(inputId = "brainAtlasPar1",size = "mini", label = "Scale By Row", value = F),
                            #switchInput(inputId = "brainCloudPar2",size = "mini", label = "SE", value = T),
                            htmlOutput("errBA"),
                            actionButton("brainAtlasButton", "Get Results"),
                            tags$hr(),
                            
                            tabsetPanel( id = "BrainAtlasResults", 
                                         tabPanel("Table1", 
                                                  DT::dataTableOutput("BAtable1")
                                                  
                                         ),
                                         tabPanel("Table2", 
                                                  selectInput("BAtable2Set1", "Group 1", BACellTypesList, selected = "Exc"),
                                                  selectInput("BAtable2Set2", "Group 2", BACellTypesList, selected = "Inh"),
                                                  sliderInput("BAtabl2Crit", "Set Threshold:", min = .1, max = 1, value = 0.2, step = 0.1),
                                                  DT::dataTableOutput("BAtable2"),
                                                  verbatimTextOutput("BAtabl2PassedCrit")
                                                  
                                         ),
                                         tabPanel("HeatMap", 
                                                  
                                                  switchInput(inputId = "BAHM_Par1",size = "mini", label = "Cluster", value = T),
                                                  plotOutput("brainAtlasPlot",height = "650px")
                                         )
                            )
                            
                        )
                        
                ),
                
                
                # GTEx UI Tab ----
                tabItem("GTEx", 
                        fluidPage(
                            textInput("GTExText1", "",
                                      placeholder = "", value = ""),
                            actionButton("GTExButton1", "Get Results"),
                            tags$hr(),
                            tabsetPanel( id = "GTExResults", 
                                         tabPanel("ALL Tissues",
                                                  
                                                  switchInput(inputId = "GTEx_Par1",size = "mini", label = "Log", value = T),
                                                  switchInput(inputId = "GTEx_Par2",size = "mini", label = "Cluster", value = T),
                                                  plotOutput("GTExPlot1",height = "650px")
                                                  
                                         ),
                                         tabPanel("eQTL",
                                                  pickerInput(inputId="dbsGTEx",label="Tissue",choices=dbListGTEx, options = list(`actions-box` = TRUE), 
                                                              multiple = TRUE, selected = dbListGTEx),
                                                  tags$hr(),
                                                  DT::dataTableOutput("GTExeQTLTable")
                                                  
                                         )
                            )
                            
                        )
                        
                ),
                # Barres UI Tab ----
                tabItem("barresDB2", 
                        fluidPage(
                            textInput("checkEx", "Check Gene Expression Levels in the Brain (Mouse & Human)"),
                            actionButton("check", "Check!"),tags$hr(),
                            switchInput(inputId = "barresPar",size = "mini", label = "Log", value = F),
                            switchInput(inputId = "barresPar2",size = "mini", label = "Color", value = F),
                            tags$hr(),
                            plotOutput("barredMouseplot"),tags$hr(),
                            plotOutput("barredHumanplot")
                        )
                ),
                
                tabItem("barresDB3", 
                        fluidPage(
                            textInput("checkEx2", "Check Multiple Targets (Mouse & Human)"),
                            actionButton("check2", "Check!"),tags$hr(),
                            sliderInput("barredAllPlot4Par1", label = "text size", min = 1, max = 15, value = 7),
                            tags$hr(),
                            tabsetPanel( id = "BarresResults", 
                                         tabPanel("Mouse", 
                                                  plotOutput("barredAllplot2"),
                                                  tags$hr(),
                                                  plotOutput("barredAllplot3", height = "600px"),
                                                  tags$hr(),
                                                  plotOutput("barredAllplot4", height = "600px")
                                                  
                                         ),
                                         tabPanel("Human", 
                                                  plotOutput("barredAllplot5"),
                                                  tags$hr(),
                                                  plotOutput("barredAllplot6", height = "600px"),
                                                  tags$hr(),
                                                  plotOutput("barredAllplot7", height = "600px")
                                                  
                                         )
                                         
                            )
                            
                        )
                ),
                tabItem("KalidInfo", 
                        fluidPage(
                            verbatimTextOutput("SoftInfo")
                        )
                )
                
                
                ##### BrainSpan UI Tab ----
                # tabItem("brainSpanTab",
                #         fluidPage(
                #           
                #           textInput("brainSpanText", "Check Gene(s) Expression Levels in the Brain Regions", placeholder = "", value = ""),
                #           htmlOutput("errBS"),
                #           pickerInput(inputId="brainSpanRegions",label="Select Regions:",choices=BSRegions, options = list(`actions-box` = TRUE),
                #                       multiple = TRUE),
                #           switchInput(inputId = "brainSpanPar1",size = "mini", label = "Points", value = T),
                #           switchInput(inputId = "brainSpanPar2",size = "mini", label = "SE", value = T),
                #           actionButton("brainSpanButton", "Plot"),
                #           plotOutput("brainSpanPlot", height = "750px")
                #         )
                #         
                # )
                
            )
            
        )
    )


# Server ----
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    output$SoftInfo <- renderText("Brain RNA-Seq:
RNA-seq of cells isolated and purified from mouse and human brain from grey matter of cortex tissue. 
Purified using cell-type specific antibodies (anti-CD45 to capture microglia/macrophages, anti-GalC 
hybridoma supernatant to harvest oligodendrocytes, anti-O4 hybridoma to harvest OPCs, anti-Thy1 (CD90) 
to harvest neurons, anti-HepaCAM to harvest astrocytes, and BSL-1 to harvest endothelial cells).
https://www.cell.com/neuron/fulltext/S0896-6273(15)01019-3
                                
Braincloud: 
Transcription levels in the human prefrontal cortex across the lifespan. Post-mortem brains from fetal 
development through ageing to highlight the role of the human genome in cortical development, function 
and ageing. (n = 269 subjects without neuropathological or neuropsychiatric diagnosis). Age ranges: fetal, 
14–20 gestational weeks, infant: 0–6 months, child, 1–10 years; adolescent and adults till ~80 years. 
https://www.nature.com/articles/nature10524
                                
GTEx:
The Genotype-Tissue Expression (GTEx) database contains data of tissue-specific gene expression and regulation. 
Samples were collected from 53 tissues from almost 1000 individuals, primarily for molecular assays including WGS, 
WES, and RNA-Seq. Has eQTL, expression quantitative trait loci, data for all studied tissues to identify variant-gene
expression association
https://www.nature.com/articles/ng.2653
                                
BrainAtlas:
Includes single cell and nuclear transcriptomic profiles, assayed from human and mouse brain regions. Anatomical 
specificity is achieved by microdissecting tissue from defined brain areas. Currently this the tab contains: RNA-Seq 
data created from intact nuclei derived from frozen human brain specimens, to survey cell type diversity in the human
middle temporal gyrus (MTG). In total, 15,928 nuclei from 8 human tissue donors ranging in age from 24-66 years were 
analyzed. Analysis of these transcriptional profiles reveals approximately 75 transcriptionally distinct cell types, 
subdivided into 45 inhibitory neuron types, 24 excitatory neuron types, and 6 non-neuronal types.
http://portal.brain-map.org/
                                
BrainSpan: 
Is a resource for studying transcriptional mechanisms involved in human brain development. Currently, 10 brain regions 
are loaded into the software. Data is displayed as RPKM expression values.
http://www.brainspan.org/
                                
STRING:
STRING is a database of known and predicted protein-protein interactions. The interactions include direct (physical) 
and indirect (functional) associations; they stem from computational prediction, from knowledge transfer between organisms, 
and from interactions aggregated from other (primary) databases.
Experimental data: BIND, DIP, GRID, HPRD, IntAct, MINT, and PID.
Databases curated data: Biocarta, BioCyc, GO, KEGG, and Reactome.
https://www.ncbi.nlm.nih.gov/pubmed/30476243
                                
Lookup:
Disease based modules of previously published studies (RNAseq, microarray, Mass spectrometry) harmonized to be used as 
lookup replication studies. 
                                
iLINCS:
uses the L1000 assay which is a gene-expression profiling assay based on the direct measurement of a reduced representation 
of the transcriptome and computational inference of the portion of the transcriptome not explicitly measured under different 
perturbations (like genes knockdown, drugs treatments, gene overexpression .. etc).
http://www.ilincs.org/ilincs/
                                
GWAS Catalog:
A curated collection of all published genome-wide association studies that currently contains 3841 publications and 126603 
genetic variant - phenotype associations
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6323933/")
    
    # Load Data Files ----
    observe({
        if(input$tabsList == "lookupTab") {
            
            if(!exists("fullDataSet")) {
                withProgress(message = 'Loading lookup data ...', value = 0, {
                    incProgress(2/10)
                    fullDataSet <<- readRDS("./data/fullDataSet_Oct15.rds")
                    incProgress(7/10)
                    fullDBsInfo <<- read.csv("./data/SCZ _Datasets_Info.csv", header = T, stringsAsFactors = F)
                    incProgress(10/10)
                })
            }
        }
    })
    
    observe({
        
        
        if(input$tabsList == "GTEx") {
            
            if(!exists("GTExQTLData")) {
                withProgress(message = 'Loading GTEx data ...', value = 0, {
                    incProgress(1/10)
                    GTExQTLData <<- readRDS("./data/GTExQTLDataTidy_6Regions.rds")
                    GTExColoredGrouped <<- readRDS("./data/GTExColoredGrouped.rds")
                    names(GTExColoredGrouped) <- "Tissue"
                    incProgress(5/10)
                    GTExGeneExpMedian <<- read.delim("./data/GTEx_Analysis_2016-01-15_v7_RNASeQCv1.1.8_gene_median_tpm.gct", 
                                                     sep = "\t", na.strings = "", skip = 2, stringsAsFactors = F)
                    GTExSampleAtr2 <<- readRDS("./data/GTExSampleAtrrHM.rds")
                    GTExSampleAtr2 %>% rename(Tissue = SMTS) ->> GTExSampleAtr2
                    incProgress(10/10)
                })
            }
        }
        
        if(input$tabsList == "brainAtlas") {
            if(!exists("brainAtlasInfo")) {
                withProgress(message = 'Loading BrainAtlas data ...', value = 0.2, {
                    
                    brainAtlasInfo <<- readRDS("./data/CompleteGroupedByCLuster.rds")
                    brainAtlasGrouping <<- readRDS("./data/clusterHeatmapGrouping.rds")
                    incProgress(1)
                })
            }
        }
        
        if(input$tabsList == "GWAS") {
            if(!exists("GWASInfo")) {
                withProgress(message = 'Loading GWAS data ...', value = 0.2, {
                    GWASInfo <<- readRDS("./data/GWAS_asso.rds")
                    incProgress(1)
                })
            }
        }
        
        if(input$tabsList == "brainCloud") {
            if(!exists("brainCloudInfo")) {
                withProgress(message = 'Loading BrainCloud data ...', value = 0.2, {
                    brainCloudInfo <<- readRDS("./data/Braincloud_Fixed_Final.rds")
                    incProgress(1)
                })
            }
        }
        
        if(input$tabsList == "barresDB2" | input$tabsList == "barresDB3") {
            if(!exists("barresMouse")) {
                withProgress(message = 'Loading Brain RNA-Seq data ...', value = 0.2, {
                    barresMouse <<- readRDS("./data/BarresBrainSeqMouse.rds")
                    barresHuman <<- readRDS("./data/HumanBarres.rds")
                    incProgress(1)
                })
            }
        }
        
        
        
    })
    
    # STRING Server Tab ----
    KJHB <- c()
    observeEvent(input$slider1, {
        updateNumericInput(session, "scoreInp", value = input$slider1)
    })
    observeEvent(input$scoreInp, {
        updateSliderInput(session, "slider1", value = input$scoreInp)
    })
    
    observeEvent(input$getNet, {
        shinyjs::disable("getNet")
        shinyjs::hide("intctos")
        shinyjs::hide("myPlot")
        shinyjs::hide("trnsNet")
        shinyjs::hide("err")
        shinyjs::hide("stringInfoDIV")
        shinyjs::hide("STRING Table")
        shinyjs::hide("NetTable")
        withProgress(message = 'Connecting to STRING ...', value = 0.1, {
            err_out = FALSE
            strGene <- gsub(" ", "", input$strNet, fixed = TRUE)
            strGene <- toupper(strGene)
            s <- input$slider1
            s2 <- input$slider2
            
            url <- paste0("http://string-db.org/api/image/network?identifier=", strGene, "&required_score=",s,"&limit=",s2,"&species=9606&network_flavor=evidence")
            t <- content(GET(url))
            str_error <- grep("^\\{ \\\"Error", t)
            if (length(str_error) == 0) {
                
                output$myPlot <- renderPlot({
                    plot.new()
                    rasterImage(t,0,0,1,1)
                })
                
                
                urlString <- paste0("http://string-db.org/api/json/interaction_partners?identifiers=",strGene,"&species=9606&required_score=",s,"&limit=",s2)
                incProgress(0.3)
                fromJSON(content(GET(urlString))) -> tableString2
                select(bind_rows(lapply(tableString2, data.frame, stringsAsFactors = FALSE)), everything()) -> tableString2
                IdList <- c(strGene, pull(tableString2, preferredName_B))
                
                add_row(tableString2, preferredName_B = strGene) -> tableString2
                
                tableString2 %>% select(-preferredName_A, -stringId_A,-stringId_B,-ncbiTaxonId) %>% 
                    rename(preferredName = preferredName_B) -> tableString2
                

                incProgress(0.7)
                urlString <- paste0("http://string-db.org/api/json/resolve?identifiers=",paste(IdList,collapse ="%0D"),"&species=9606")
                tableString1 <- fromJSON(content(GET(urlString)))
                select(bind_rows(lapply(tableString1, data.frame, stringsAsFactors = FALSE)), preferredName, annotation) -> tableString1
                
                KJHB <<- pull(tableString1, preferredName)
                
                
                
                full_join(tableString1,tableString2) %>% select(preferredName, score, annotation, ends_with("score")) %>% arrange(desc(score)) %>% 
                    dplyr::rename(Protein = preferredName, CombinedScore = score, Description = annotation,Neighborhood = nscore, 
                                  `Gene Fusion` =  fscore, Phylogenetic = pscore, 
                                  Database = dscore, Textmining = tscore, CoExpression = ascore,
                                  Experimental = escore
                    ) %>% select(Protein, CombinedScore, Description, Experimental, Database, everything()) -> tableStringFinal
                
                output$NetTable = shiny::renderTable({ 
                    tableStringFinal
                }, na= "NA",
                align = "c", spacing = "m", bordered = T,
                striped = T, hover = T)
                
                
                
                output$intctos = renderText(paste(pull(tableString1, preferredName), collapse = ","))
                
                shinyjs::show("myPlot")
                shinyjs::show("stringInfoDIV")
                shinyjs::show("intctos")
                shinyjs::show("trnsNet")
                shinyjs::show("STRING Table")
                shinyjs::show("NetTable")
                shinyjs::enable("getNet")
            }
            
            else {
                error_msg <- "Target is not found in the STRING database"
                
                output$err <- renderText({ paste("<font color=\"#FF0000\"><b>",error_msg) })
                
                shinyjs::show("err")
                shinyjs::enable("getNet")
            }
            incProgress(1)
        })
        
    })
    observeEvent(input$trnsNet, {
        
        updateTextInput(session, "gene", value = KJHB)
        updateTextInput(session, "iLINCSSearch", value = KJHB)
        updateTabItems(session, "tabsList" ,selected = "lookupTab")
        
    })
    
    
    # iLINCS Server Tab ----
    observeEvent(input$isearch, {
        withProgress(message = 'Connecting to iLINCS ...', value = 0.1, {
            
            req(input$iLINCSSearch)
            shinyjs::disable("isearch")
            if (input$sepChoice2 == " ") {
                
                igenes <- strsplit(input$iLINCSSearch, "\\s+")
                
            }
            else if (input$sepChoice2 == ",") {
                igenes <- gsub(" ", "", input$iLINCSSearch, fixed = TRUE)
                igenes <- strsplit(igenes, ",")
                
            }
            
            else if (input$sepChoice2 == ";") {
                igenes <- gsub(" ", "", input$iLINCSSearch, fixed = TRUE)
                igenes <- strsplit(igenes, ";")
                
            }
            igenes <- as.vector(igenes[[1]])
            lincsList <- input$iLINCSSearch
            iLINCSt2 <- tibble()
            incProgress(0.4)
            for (i in 1:length(igenes)) {
                
                iLINCSt1 <- content(GET(url = paste0("http://www.ilincs.org/api/SignatureMeta?filter=%7B%22where%22%3A%7B%22treatment%22%3A%22",igenes[i],"%22%2C%20%22libraryid%22%3A%22LIB_6%22%7D%7D")))
                iLINCSt1 %>% {
                    tibble(
                        Gene = map_chr(., "treatment"),
                        CellLine = map_chr(., "cellline"),
                        SignatureID = map_chr(., "signatureid")
                    )
                } -> iLINCSt1
                
                iLINCSt2 <- bind_rows(iLINCSt2, iLINCSt1)
                
            }
            
            iLINCStTest <<- iLINCSt2
            
            output$iLINCST2 = DT::renderDataTable(DT::datatable({
                
                iLINCSt4 <- mutate(iLINCSt2,Link = paste0("<a href='http://www.ilincs.org/ilincs/signature/", SignatureID,"' target='_blank'>http://www.ilincs.org/ilincs/signature/", SignatureID,"</a>"))
                iLINCSt4
                
            }, escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                className = 'dt-center', targets = "_all")))
            ))
            iLINCSt2 %>% group_by(Gene) %>% count(name = "Knockdown Signatures") %>% 
                arrange(desc(`Knockdown Signatures`)) ->> iLINCSt3
            
            output$iLINCST1 = DT::renderDataTable(
                DT::datatable({
                    iLINCSt3},
                    escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                        className = 'dt-center', targets = "_all")))
                ))
            
            shinyjs::enable("isearch")
            incProgress(1)
        })
    }) 
    
    # Lookup Server Tab ----
    # Lookup datastes selections ----
    observeEvent(input$dbs,{
        if ("Cell" %in% input$dbs & "Region" %ni% input$dbs) {
            updatePickerInput(
                session, 'dbs', choices = dbListFull,
                selected = c(input$dbs,"Cell",dbListFull$`Cell Level`)
                
            )}
        else if ("Region" %in% input$dbs & "Cell" %ni% input$dbs) {
            updatePickerInput(
                session, 'dbs', choices = dbListFull,
                selected = c(input$dbs,"Region",dbListFull$`Region Level`)
                
            )
            
        }
        else if ("Cell" %in% input$dbs & "Region" %in% input$dbs) {
            updatePickerInput(
                session, 'dbs', choices = dbListFull,
                selected = c(input$dbs,dbListFull$`Cell Level`,
                             dbListFull$`Region Level`, "Region", "Cell")
            )
        }
        
    })
    
    # Lookup Results ----
    observeEvent(input$act, {
        #req(input$gene)
        #req(input$dbs)
        withProgress(message = 'Getting Results ...', value = 0.1, {
            
            shinyjs::disable("act")
            shinyjs::hide("targetMissing")
            shinyjs::hide("targetListed")
            shinyjs::hide("classifyGenesDown")
            shinyjs::hide("classifyGenesUp")
            
            #hideTab(LUResults,"HM5")
            notFound <- NULL
            
            sel <- c(input$dbs,input$dbs2, input$dbs3, input$dbs4, input$dbs5, input$dbs6)
            sel <- sel[sel!="Cell"& sel!="Region"]
            #print(sel)
            selLength <- length(sel)
            
            # Seperator Options ---- 
            
            if (input$sepChoice == " ") {
                
                genes <- strsplit(input$gene, "\\s+")
                
            }
            else if (input$sepChoice == ",") {
                genes <- gsub(" ", "", input$gene, fixed = TRUE)
                genes <- strsplit(genes, ",") 
                
            }
            
            else if (input$sepChoice == ";") {
                genes <- gsub("\\s", "", input$gene)
                genes <- strsplit(genes, ";") 
                
            }
            
            if (input$inputChoice == "ownList") {
                #shinyjs::enable(input$gene)
                genes <- genes[[1]] %>% map_chr(toupper)
                shinyjs::hide("down_2")
                shinyjs::hide("overlapTable")
                
            }
            
            else if (input$inputChoice == "CommHits") {
                
                fullDataSet$Log2FCAbs <- abs(fullDataSet$Log2FC)
                fullDataSet$Dir <- ifelse(fullDataSet$Log2FC>0,"Up", "Down")
                
                data.table::setorder(setDT(filter(fullDataSet, DataSet %in% sel)), DataSet,-Log2FCAbs)[, indx := seq_len(.N), DataSet][indx <= input$CommTop] %>%  
                  select(`Gene Symbol`, Group, Dir) %>% 
                  group_by(`Gene Symbol`) %>% mutate(Hits = n()) %>%
                  group_by(`Gene Symbol`,Group) %>% mutate(HitsPerGroup = n()) %>%
                  group_by(`Gene Symbol` , Dir) %>% mutate(All = n()) %>% 
                  group_by(`Gene Symbol`, Group , Dir) %>% 
                  mutate(byDir = n()) %>%  
                  distinct() %>%
                  pivot_wider(names_from = Dir, values_from = c(byDir, All)) %>%
                  rename(Down = byDir_Down, Up = byDir_Up) %>% 
                  pivot_wider(names_from = c(Group), values_from = c(HitsPerGroup,Down, Up), names_sep = ">") %>% 
                  rename(Up = All_Up, Down = All_Down) %>% 
                  arrange(desc(Hits)) %>% ungroup() %>% 
                  slice(1:input$CommTop2) -> commTable
                
                  
                
                
                
                # data.table::setorder(setDT(filter(fullDataSet, DataSet %in% sel)), DataSet, -Log2FCAbs)[, indx := seq_len(.N), DataSet][indx <= input$CommTop] %>% 
                #   #mutate(Dir = ifelse(Log2FC>0,"Up", "Down")) %>% group_by(Dir) %>% 
                #     count(`Gene Symbol`) %>% arrange(desc(n)) %>% slice(1:input$CommTop2) -> commTable
                
                genes<- commTable$`Gene Symbol` 
                print("from commtable")
                print(genes)
                commTable %>% select(`Gene Symbol`, Hits, Up, Down) %>% 
                  mutate(
                    isUp = case_when(
                      is.na(Up) ~ F,
                      is.na(Down) ~ T,
                      Up > Down ~ T,
                      Up < Down ~ F
                    )
                  ) -> commTableSum 
                
                classGenes <- split(commTableSum$`Gene Symbol`,commTableSum$isUp )
                
                
                output$overlapTable <- render_gt(
                  gt(commTable) %>% cols_split_delim(delim = ">") %>% 
                    fmt_missing(columns = 1:ncol(.), missing_text = "-") ,
                                                 height = px(400)
                )
                shinyjs::show("overlapTable")
                
                output$classifyGenesUp <- shiny::renderText(paste(classGenes[["TRUE"]], collapse = ","))
                output$classifyGenesDown <- shiny::renderText(paste(classGenes[["FALSE"]], collapse = ","))
                
                shinyjs::show("classifyGenesDown")
                shinyjs::show("classifyGenesUp")
                
                output$down_2 <- downloadHandler(
                    filename = function() {
                        paste("OverLapTable_results", ".csv", sep = "")
                    },
                    content = function(file) {
                        write.csv(commTable, file,row.names = FALSE)
                    }
                )
                
                shinyjs::show("down_2")
                
                data.table::setorder(setDT(filter(fullDataSet, DataSet %in% sel)), DataSet, -Log2FCAbs)[, indx := seq_len(.N), DataSet][indx <= input$CommTop] %>%   
                    filter(`Gene Symbol` %in% genes) %>% select(`Gene Symbol`, Log2FC, DataSet) %>% 
                    pivot_wider(names_from = DataSet, values_from = Log2FC) %>% column_to_rownames("Gene Symbol") %>% 
                    as.matrix() -> hh
                
                hh[!is.na(hh)] <- 1
                hh[is.na(hh)] <- 0
                
                #showTab(inputId = "LUResults","HM5")
                output$HM5 <- renderPlot({
                    pheatmap::pheatmap(hh,
                                       color = c("slategray3", "steelblue4"),
                                       breaks = c(0, 0.99,2),
                                       border_color = "black",
                                       fontsize_row = input$heatmap5SizeRow,
                                       fontsize_col = input$heatmap5SizeCol,
                                       legend_breaks = c(0,0.99),
                                       legend_labels = c("No", "Yes")
                    )
                })
                
                
                #shinyjs::disable(input$gene)
                # fullDataSet %>% select(`Gene Symbol`, Log2FC, DataSet, ecdfPlot) %>% 
                #   filter(DataSet %in% sel,
                #          
                #          if(input$ThresChoice == "Log2FC"){
                #            
                #            Log2FC >= input$CommThres | Log2FC <= (as.numeric(paste0("-", input$CommThres)))
                #          }
                #          
                #          else if (input$ThresChoice == "ecdf") {
                #            ecdfPlot >= input$CommThres2 | ecdfPlot <= (as.numeric(paste0("-", input$CommThres2)))
                #          }
                #          
                #          
                #          , !is.na(`Gene Symbol`), 
                #          !`Gene Symbol` %in% c("Y_RNA", "Metazoa_SRP"),
                #          !grepl("sno", `Gene Symbol`, ignore.case = T),
                #          !grepl("U\\d", `Gene Symbol`), 
                #          !grepl("RP11", `Gene Symbol`), 
                #          !grepl("LINC", `Gene Symbol`),
                #          !grepl("AC0", `Gene Symbol`),
                #          !grepl("LOC", `Gene Symbol`),
                #   ) %>% distinct(`Gene Symbol`, DataSet, .keep_all = T) %>% 
                #   count(`Gene Symbol`, sort = T)  %>% head(input$CommTop) %>% pull(`Gene Symbol`) -> genes
                
            }
            incProgress(3/10)
            
            
            
            fullDataSet %>% select(`Gene Symbol`, Log2FC, `Fold Change`, `P Value`, ecdfPlot,DataSet) %>% filter(`Gene Symbol` %in% genes, DataSet %in% sel) -> fullDataSet2
            
            fullDataSet2 %>% pull(`Gene Symbol`) %>% unique()  -> FoundGenes
            print("foundgenes")
            print(FoundGenes)
            FoundGenesLength <- length(FoundGenes)
            notFoundGenes <- setdiff(genes, FoundGenes)
            output$targetMissing <- shiny::renderText(paste(notFoundGenes, collapse = ","))
            shinyjs::show("targetMissing")
            
            output$targetListed <- shiny::renderText(paste(FoundGenes, collapse = ","))
            shinyjs::show("targetListed")
            
            # Get Results and store as df ----
            fullDataSet2 %>% 
                mutate(Log2FC = round(Log2FC, 3)) %>% 
                select(`Gene Symbol`, Log2FC, DataSet) %>% 
                distinct(`Gene Symbol`,DataSet, .keep_all=T) %>%
                spread(DataSet, Log2FC) -> mmHM2
            
            fullDataSet2 %>% 
                mutate(`Fold Change` = round(`Fold Change`, 3)) %>% 
                select(`Gene Symbol`, `Fold Change`, DataSet) %>% 
                distinct(`Gene Symbol`,DataSet, .keep_all=T) %>% 
                spread(DataSet, `Fold Change`) -> mmHM3
            
            fullDataSet2 %>% 
                select(`Gene Symbol`, ecdfPlot, DataSet) %>% 
                distinct(`Gene Symbol`,DataSet, .keep_all=T) %>% 
                spread(DataSet, ecdfPlot) -> mmHM4
            
            
            fullDataSet2 %>% 
                mutate(
                    fullInfo = paste("FC =", round(`Fold Change`,2), 
                                     "LogFC =", round(Log2FC,2), 
                                     "p =", round(`P Value`,2)
                                     #"score = ", round(ecdfPlot, 3)
                    )) %>% 
                select(`Gene Symbol`, fullInfo,DataSet) %>% 
                distinct(`Gene Symbol`,DataSet, .keep_all=T) %>% 
                spread(DataSet, fullInfo) -> lookup_table
            
            #lookup_table2$Hits <- as.integer(lookup_table2$Hits)
            
            #output$t1 = shiny::renderTable({ 
            
            spec <- fullDataSet2 %>% dplyr::rename(p = `P Value`) %>% 
                expand(DataSet, .value = c("Log2FC", "p")) %>% 
                mutate(.name = paste0(DataSet, ">", .value))
            
            
            
            fullDataSet2 %>% dplyr::rename(p = `P Value`) %>% distinct(`Gene Symbol`,DataSet, .keep_all=T) %>% 
                select(-`Fold Change`, -ecdfPlot) %>% 
                mutate_if(is.numeric, round, 3) %>% 
                pivot_wider_spec(spec = spec) %>% gt() %>% 
                cols_split_delim(delim = ">") %>% fmt_missing(columns = 1:ncol(.), missing_text = "-") -> lookup_gt_table
            
            if (input$showtable == T) {
            output$t1 = render_gt(
                expr = lookup_gt_table, 
                height = px(700)
            )
            shinyjs::show("t1")
            }
            else {shinyjs::hide("t1")}
      
            
            incProgress(5/10)
            
            # na= "NA",
            # align = "c", spacing = "m", bordered = T,
            # striped = T, hover = T)
            
            shinyjs::enable("act")
            
            output$lookupBarGraph <- renderPlot({
                fullDataSet2 %>% 
                    distinct(`Gene Symbol`,DataSet, .keep_all=T) %>%
                    mutate(hit = ifelse(Log2FC>=input$lookupBarGraphPar1, 1,
                                        ifelse(Log2FC<=as.numeric(paste("-", input$lookupBarGraphPar1,sep = "")), 1,0
                                               #ifelse(round(`P Value`,2)<=0.05, 1, 0
                                               
                                        ))) %>% 
                    group_by(`Gene Symbol`) %>% summarise(Hits = sum(hit, na.rm = T), 
                                                          TotalFound = length(unique(DataSet))) %>% 
                    mutate(prpor = Hits/TotalFound)  %>% ggplot(aes(reorder(`Gene Symbol`,-prpor), prpor)) +
                    geom_col(aes(fill = TotalFound)) + 
                    geom_hline(yintercept = 0.5, linetype = "dotted") + geom_hline(yintercept = 1, linetype = "dotted") +
                    theme_bw() + 
                    
                    theme(legend.position = "right",axis.text.x = element_text(size = input$lookupBarGraphPar2, angle = 30),
                          axis.text.y = element_text(size = 13),
                          plot.title = element_text(hjust = 0.5, size = 15),
                          plot.subtitle = element_text(hjust = 0.5, size = 10)
                    ) + labs(
                        title = "Lookup Hits",
                        subtitle = paste0("Using ", as.character(length(sel)), " Datasets"),
                        x = "Gene",
                        y = "Ratio"
                    )
            })
            
            output$down_1 <- downloadHandler(
                filename = function() {
                    paste("lookup_results", ".csv", sep = "")
                },
                content = function(file) {
                    write.csv(lookup_table, file, row.names = FALSE)
                }
            )
            
            
            
            # convert to matrix for heatmap ----
            
            #try NA_real_
            
            rownames(mmHM2) <- mmHM2$`Gene Symbol`
            mmHM2 %>% select(-`Gene Symbol`) %>% as.matrix() -> mmHM2
            mmHM2[is.na(mmHM2)] <- 0
            class(mmHM2) <- "numeric"
            
            # remove error of NA/NaN/Inf in foreign function call (arg 11)
            #mmHM2 <- mmHM2[rowSums(!is.na(mmHM2))!=0, colSums(!is.na(mmHM2))!=0] 
            #mmHM3[!is.finite(mmHM3)] <- NA
            #mmHM2[is.na(mmHM2)] <- 0
            
            rownames(mmHM3) <- mmHM3$`Gene Symbol`
            mmHM3 %>% select(-`Gene Symbol`) %>% as.matrix() -> mmHM3
            mmHM3[is.na(mmHM3)] <- 0
            class(mmHM3) <- "numeric"
            # remove error of NA/NaN/Inf in foreign function call (arg 11)
            #mmHM3[is.na(mmHM3)] <- 0
            
            # Heatmap Log2fc 
            col_breaks2 = reactive({
                c(seq(as.numeric(paste("-", input$heatmapTH1,sep = "")),
                      -0.01,length=50),
                  0,
                  seq(0.01,input$heatmapTH1,length=50))
            })
            
            hmcol2 <- colorRampPalette(c("yellow", "white", "red"))(n = 100)
            
            incProgress(7/10)
            output$HM2 <- renderPlot({
                # pheatmap::pheatmap(
                #   mmHM2,scale="none",
                #   cluster_rows =if(FoundGenesLength>1){T} else {F},
                #   cluster_cols =if(selLength>2){T} else {F},
                #   col = hmcol2,trace="none", na_col = "gray81",
                #   breaks = col_breaks2()
                # )
              
              hmpdf2 <<- function() {heatmap.2(
                    mmHM2,scale="none",
                    Rowv = if(FoundGenesLength>1){T} else {F},
                    Colv = if(selLength>2){T} else {F},
                    col = hmcol2,trace="none", na.color = "gray81",
                    margin=c(7, 5),
                    cexCol=input$heatmap2SizeCol,
                    cexRow=input$heatmap2SizeRow,
                    #cexCol=0.42,
                    #cexRow=0.8,
                    density.info="density",
                    breaks = col_breaks2()
                )
              }
              hmpdf2()
              #{ pdf("Log2FC_Heatmap.pdf")
              #  hmpdf
              #  dev.off()
              #}

              
            }
                # pheatmap(
                #   mmHM2,scale="none",
                #   cluster_rows =if(FoundGenesLength>1){T} else {F},
                #   col = hmcol2,trace="none", na_col = "gray81",
                #   margin=c(7, 5),
                #   breaks = col_breaks2()
                # )
            )
            
            output$HM2_down <- downloadHandler(
              filename = function() {
                "Log2FC_Heatmap_kalediscope.pdf"
              },
              content = function(file) {
                pdf(file)
                hmpdf2()
                dev.off()
                
                #file.copy("Log2FC_Heatmap.pdf", file)
              }
            )
            
            # heatmap fc
            col_breaks3 = reactive({
                c(seq(as.numeric(paste("-", input$heatmapTH2,sep = "")),-1,length=50),
                  0,
                  seq(1,input$heatmapTH2,length=50))
            })
            
            hmcol3 <- colorRampPalette(c("yellow", "white", "red"))(n = 100)
            output$HM3 <- renderPlot( {
              hmpdf3 <<- function() {heatmap.2(
                    
                    mmHM3,scale="none",
                    Rowv = if(FoundGenesLength>1){T} else {F},
                    Colv = if(selLength>2){T} else {F},
                    col = hmcol3,trace="none", na.color = "gray81",
                    
                    margin=c(7, 5),
                    cexCol=input$heatmap3SizeCol,
                    cexRow=input$heatmap3SizeRow,
                    #cexCol=0.42,cexRow=0.8,
                    density.info="density",
                    breaks = col_breaks3()
                )
              }
              
              hmpdf3()
            }
                
                # pheatmap::pheatmap(
                #   mmHM3,scale="none",
                #   cluster_rows =if(FoundGenesLength>1){T} else {F},
                #   cluster_cols =if(selLength>2){T} else {F},
                #   col = hmcol3,trace="none", na_col = "gray81",
                #   breaks = col_breaks3()
                # )
                
                # pheatmap(
                # 
                #   mmHM3,scale="none",
                #   cluster_rows  = if(FoundGenesLength>1){T} else {F},
                #   col = hmcol3,trace="none", na_col =  "gray81",
                #   margin=c(7, 5),
                #   breaks  = col_breaks3()
                # )
            )
            
            output$HM3_down <- downloadHandler(
              filename = function() {
                "FC_Heatmap_kalediscope.pdf"
              },
              content = function(file) {
                pdf(file)
                hmpdf3()
                dev.off()
                
                #file.copy("Log2FC_Heatmap.pdf", file)
              }
            )
            
            # heatmap Harmonized ecdf
            rownames(mmHM4) <- mmHM4$`Gene Symbol`
            mmHM4 %>% select(-`Gene Symbol`) %>% as.matrix() -> mmHM4
            class(mmHM4) <- "numeric"
            mmHM4[is.na(mmHM4)] <- 0
            
            
            
            
            output$DatabasesFullInfo <- DT::renderDataTable(DT::datatable(fullDBsInfo,
                                                                          escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                                                                              className = 'dt-center', targets = "_all")))
            ))
            
            # scale_fill_gradientn(colours = c("yellow","white","white","red"), values = rescale(c(-1, -0.99999,-0.25 ,0, 0.25 ,0.99999, 1))
            # col_breaks4 = c(seq(-1.2,-0.99,length=10), seq(-0.989, -0.96, length = 20), seq(-0.959, -0.80, length = 20), seq(-0.79, -0.50, length = 30), seq(-0.49, -0.1, length = 150),
            #                 0,
            #                 seq(0.1,0.49,length=150), seq(0.50, 0.79, length = 30), seq(0.80, 0.959, length = 20), seq(0.96, 0.989, length = 20), seq(0.99, 1.2, length = 10))
            # 
            # hmcol4 <- colorRampPalette(c("yellow","yellow2" ,"khaki1","lemonchiffon1","lightgoldenrodyellow","white", "lightpink1" ,"lightcoral","indianred1","orangered2","red"))(n = 460)
            # output$HM4 <- renderPlot(heatmap.2(mmHM4,scale="none",col = hmcol4,trace="none", na.color = "gray81",margin=c(7, 5),cexCol=0.8,cexRow=0.5,density.info="density",breaks = col_breaks4))
            
            output$HM4 <- plotly::renderPlotly(
                heatmaply::heatmaply(
                    mmHM4, limits = c(-1,1),
                    Rowv = if(FoundGenesLength>1){T} else {F},
                    scale_fill_gradient_fun = scale_fill_gradientn(
                        colours = c("yellow","white","white","red"),
                        values = scales::rescale(c(-1, -0.99999,-0.25 ,0, 0.25 ,0.99999, 1))
                    )
                )
            )
            
            incProgress(9/10)
            
            {if (input$corGenes == "fullSig") {fullDataSet} else {fullDataSet2}} %>% 
                filter(DataSet %in% sel) %>% 
                filter(!is.na(`Gene Symbol`), !is.na(Log2FC)) %>%
                select(`Gene Symbol`, Log2FC, DataSet) %>% 
                distinct(`Gene Symbol`, DataSet, .keep_all = T) %>% 
                spread(DataSet, Log2FC) %>% 
                column_to_rownames("Gene Symbol") -> cormatr
            
            
            if (FoundGenesLength > 4 & length(sel) >= 2 ) {
                cormatrRes<-rcorr(as.matrix(cormatr), type = "pearson")
                cormatrRes$r[cormatrRes$n<5]<-0 # ignore less than five observations
                
                
                output$CorPlot <- renderPlot(
                    
                    corrplot(cormatrRes$r, 
                             # insig = "p-value", sig.level = -1,
                             # p.mat = result2$P,
                             #p.mat = asdP, insig = "label_sig",
                             #sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white",
                             tl.col = "black", order = "hclust",
                             method = "circle",type = "full", tl.cex = 0.5, cl.cex = 0.4)
                )
            }
            incProgress(10/10)
        })
    })
    
    # Upload a dataset ----
    observeEvent(input$DSupload, {
        shinyjs::hide("err2")
        flagDBerror <<- 0
        targetFile = input$DSinput
        pd <- readRDS(targetFile$datapath)
        pd %>% mutate(DataSet = input$DSNameInput) -> pd
        pdname <- input$DSNameInput
        if (input$DSNameInput %in% levels(as.factor(fullDataSet$DataSet))) {
            
            errorMsgDB <- "Name is Already Taken"
            
            flagDBerror <<- 1
            error_msgDB <- "Name is Already Taken"
            output$err2 <- renderText({
                paste("<font color=\"#FF0000\"><b>",error_msgDB)
            })
            shinyjs::show("err2")
        }
        
        else {
            fullDataSet <<- bind_rows(fullDataSet, pd)
            AddedListFullDop <<- list(input$DSNameInput)
            updatePickerInput(
                session, 'dbs3', choices = AddedListFullDop)
        }
        
        
    })
    
    
    # Barres Server Tab ----
    observeEvent(input$check, {
        req(input$checkEx)
        
        ti <- gsub(" ", "", input$checkEx, fixed = TRUE) %>% 
            toupper()
        
        #barresMouse %>% filter(GeneSymbol == ti) -> mouseonegene
        
        # Mouse bar plot
        if (input$checkEx!="") {
            output$barredMouseplot <- renderPlot({
                barresMouse %>% filter(GeneSymbol == ti) %>% ggplot(aes(x = reorder(CellType,FPKM), 
                                                                        y = if(input$barresPar){log10(FPKM+1)} else {FPKM})) + 
                    geom_col(if(input$barresPar2) {aes(fill=CellType)}) + 
                    coord_flip() + theme_light() +
                    labs(
                        title = paste(ti, "in Mouse"),
                        x = "Cell Type",
                        y = ifelse(input$barresPar,"Log10 of (FPKM+1)","FPKM")
                        # caption = "Ye Zhang, Kenian Chen, Steven A Sloan, Mariko L Bennett, 
                        # Anja R Scholze, Sean O'Keefe, Hemali P Phatnani, Paolo Guarnieri, 
                        # Christine Caneda, Nadine Ruderisch, Shuyun Deng, Shane A Liddelow, 
                        # Chaolin Zhang, Richard Daneman, Tom Maniatis, Ben A Barres, 
                        # Jia Qian Wu. 'An RNA-Seq transcriptome and splicing database of glia, 
                        # neurons, and vascular cells of the cerebral cortex.: 
                        # Journal of Neuroscience. 2014."
                    ) +
                    theme(legend.position = "none",axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15))
            })
            
            # Human bar plot
            output$barredHumanplot <- renderPlot({
                barresHuman %>% filter(Gene == ti) %>% ggplot(aes(x = reorder(CellType,FPKM), 
                                                                  y = if(input$barresPar){log10(FPKM+1)} else {FPKM})) + 
                    geom_col(if(input$barresPar2) {aes(fill=CellType)}) + 
                    coord_flip() + theme_light() + 
                    #gghighlight(CellType %in% c("Neurons","FetalAstrocytes"), unhighlighted_colour = "gray50") +
                    labs(
                        title = paste(ti, "in Human"),
                        x = "Cell Type",
                        y = ifelse(input$barresPar,"Log10 of (FPKM+1)","FPKM")
                    ) +
                    theme(legend.position = "none", axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15)) 
            })
        }
    })
    
    # Mouse Boxplot
    observeEvent(input$check2, {
        ti2 <- gsub(" ", "", input$checkEx2, fixed = TRUE) %>% toupper() %>% 
            strsplit(",") %>% unlist()
        
        if (input$checkEx2!="") {
            output$barredAllplot2 <- renderPlot({
                barresMouse %>% filter(GeneSymbol %in% ti2) %>% ggplot(aes(x = reorder(CellType,FPKM), 
                                                                           y = {log10(FPKM+1)}, fill = CellType)) + 
                    geom_boxplot() + geom_jitter(width = 0.2) +
                    labs(
                        title = "Boxplot Mouse",
                        x = "Cell Type",
                        y = "Log10 of (FPKM+1)"
                    ) +
                    coord_flip() + 
                    theme_light() + 
                    theme(legend.position = "none", axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15))
                
            })
            
            output$barredAllplot3 <- renderPlot({
                barresMouse %>% filter(GeneSymbol %in% ti2) %>% ggplot(aes(x = reorder(CellType,FPKM), 
                                                                           y = {log10(FPKM+1)}
                )) + 
                    geom_col() + coord_flip() +
                    theme_bw() +
                    labs(
                        title = "Mouse",
                        x = "Cell Type",
                        y = "Log10 of (FPKM+1)"
                    ) +
                    theme(legend.position = "none",axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15)) +
                    facet_wrap(~ GeneSymbol)
                
            })
            
            output$barredAllplot4 <- renderPlot({
                barresMouse %>% filter(GeneSymbol %in% ti2) %>% ggplot(aes(x = GeneSymbol, 
                                                                           y = {log10(FPKM+1)}
                )) + 
                    geom_col(aes(fill = CellType), position = "fill")+
                    theme_bw() +
                    labs(
                        title = "Mouse",
                        x = "Cell Type",
                        y = "Propor"
                    ) +
                    theme(legend.position = "right",axis.text.x = element_text(size = input$barredAllPlot4Par1),
                          axis.text.y = element_text(size = 13),
                          plot.title = element_text(hjust = 0.5, size = 15)) +
                    scale_y_continuous(labels = scales::percent_format())
                
            })
            
            
        
            output$barredAllplot5 <- renderPlot({
                barresHuman %>% filter(Gene %in% ti2) %>% 
                    ggplot(aes(x = reorder(CellType,FPKM), 
                               y = {log10(FPKM+1)}, fill = CellType)) + 
                    geom_boxplot() + geom_jitter(width = 0.2) +
                    labs(
                        title = "Boxplot Human",
                        x = "Cell Type",
                        y = "Log10 of (FPKM+1)"
                    ) +
                    coord_flip() + theme_light() + 
                    theme(legend.position = "none", axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15))
            })
            
            output$barredAllplot6 <- renderPlot({
                barresHuman %>% filter(Gene %in% ti2) %>% ggplot(aes(x = reorder(CellType,FPKM), 
                                                                     y = {log10(FPKM+1)}
                )) + 
                    geom_col() + coord_flip() +
                    theme_bw() +
                    labs(
                        title = "Human",
                        x = "Cell Type",
                        y = "Log10 of (FPKM+1)"
                    ) +
                    theme(legend.position = "none",axis.text.x = element_text(size = 13),
                          axis.text.y = element_text(size = 13), 
                          plot.title = element_text(hjust = 0.5, size = 15)) +
                    facet_wrap(~ Gene)
                
            })
            
            
            output$barredAllplot7 <- renderPlot({
                barresHuman %>% filter(Gene %in% ti2) %>% ggplot(aes(x = Gene, 
                                                                     y = {log10(FPKM+1)}
                )) + 
                    geom_col(aes(fill = CellType), position = "fill")+
                    theme_bw() +
                    labs(
                        title = "Human",
                        x = "Cell Type",
                        y = "Propor"
                    ) +
                    theme(legend.position = "right",axis.text.x = element_text(size = input$barredAllPlot4Par1),
                          axis.text.y = element_text(size = 13),
                          plot.title = element_text(hjust = 0.5, size = 15)) +
                    scale_y_continuous(labels = scales::percent_format())
                
                
            })
            
            
        }
        
    })
    # BrainCloud Remote Server Tab ----
    # observeEvent(input$BCget, {
    #   shinyjs::disable("BCget")
    #   req(input$BCstr)
    #   
    #   BChidden <- setNames(
    #     html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("value"),
    #     html_nodes(pre_pg, "input[type='hidden']") %>% html_attr("name")
    #   )
    #   BCgene <- gsub(" ", "", input$BCstr, fixed = TRUE)
    #   BCcurl <- 
    #     POST(
    #       url = "http://braincloud.jhmi.edu/plots/",  
    #       body = list(
    #         `__VIEWSTATE` = BChidden["__VIEWSTATE"],
    #         `__VIEWSTATEGENERATOR` = BChidden["__VIEWSTATEGENERATOR"],
    #         `__EVENTVALIDATION` = BChidden["__EVENTVALIDATION"],
    #         txtSearch = BCgene, 
    #         btnGo = "Go"
    #       ), 
    #       encode = "form"
    #     )
    #   pg <- content(BCcurl, as="text")
    #   BCImageSrc <- paste0("http://braincloud.jhmi.edu/plots/",
    #                        regmatches(pg,
    #                                   regexpr(paste0("ProbeID[\\w-]+-",
    #                                                  toupper(BCgene),"_[\\w-]+\\.jpg"),
    #                                           pg, perl=T)))
    #   
    #   shinyalert(
    #     title = BCgene, imageUrl = BCImageSrc,
    #     imageWidth = 450, imageHeight = 400
    #     
    #   )
    #   
    #   shinyjs::enable("BCget")
    # })
    
    # DataSets Overview Server Tab ----
    # output$DsGraph <- renderPlot(
    #   
    #   {
    #     
    #     fullDataSet %>% dplyr::filter(DataSet == input$DsSelect, !is.na(`Gene Symbol`)) %>% 
    #       dplyr::select(`Gene Symbol`, Log2FC,`P Value`) %>% 
    #       mutate(
    #         threshold = ifelse(
    #           Log2FC >= input$DSslider2 | 
    #             Log2FC <= as.numeric(paste("-", input$DSslider2,sep = "")),
    #           "Yes", "No")) %>% 
    #       ggplot(aes(Log2FC, log10(`P Value`))) + 
    #       geom_point(aes(colour = threshold),
    #                  size = 1) +
    #       scale_y_continuous(trans = "reverse") +
    #       geom_text_repel(
    #         data = subset(fullDataSet %>% 
    #                         dplyr::filter(DataSet == input$DsSelect,!is.na(`Gene Symbol`)),
    #                       Log2FC>input$DSslider3 | 
    #                         Log2FC<as.numeric(paste("-", input$DSslider3,sep = ""))), 
    #         aes(label = `Gene Symbol`)) + 
    #       scale_color_manual(
    #         values = c( "Yes" ="red",
    #                     "No" = "black"),guide = F) + theme_light() +
    #       theme(
    #         plot.title = element_text(family = "Comic Sans MS" , color="black", size=14, face="italic", hjust = .5)) + 
    #       geom_hline(yintercept = as.numeric(paste("-", input$DSslider2,sep = "")), alpha = 1/5) + geom_hline(yintercept = input$DSslider2, alpha = 1/5) +
    #       ggtitle(input$DsSelect)
    #     
    #   }
    # )
    # 
    # output$DStable <- shiny::renderDataTable({
    #   fullDataSet %>% dplyr::filter(DataSet == input$DsSelect,!is.na(`Gene Symbol`)) %>% 
    #     dplyr::select(`Gene Symbol`, Log2FC) %>% 
    #     filter(Log2FC > input$DSslider2 | Log2FC < as.numeric(paste("-", input$DSslider2,sep = "")))
    #   
    # })
    # numGenes <- ""
    # numGenesAfter <- ""
    # output$DSoutput <- renderText({
    #   
    #   fullDataSet %>% dplyr::filter(DataSet == input$DsSelect,!is.na(`Gene Symbol`)) %>%
    #     dplyr::select(`Gene Symbol`) %>% group_by(`Gene Symbol`) %>% 
    #     summarise(count = n()) %>% summarise(sum(count)) %>% pull() -> numGenes
    #   
    #   fullDataSet %>% dplyr::filter(DataSet == input$DsSelect,!is.na(`Gene Symbol`)) %>%
    #     dplyr::select(`Gene Symbol`, Log2FC) %>% 
    #     filter(Log2FC > input$DSslider2 | Log2FC < as.numeric(paste("-", input$DSslider2,sep = ""))) %>% 
    #     group_by(`Gene Symbol`) %>% summarise(count = n()) %>% summarise(sum(count)) %>% pull() -> numGenesAfter
    #   paste(numGenesAfter, "out of",numGenes)
    #   
    # })
    
    # BrainSpan Server Tab ----
    
    # observeEvent(input$brainSpanButton, {
    #   shinyjs::hide("errBS")
    #   shinyjs::hide("brainSpanPlot")
    #   req(input$brainSpanText)
    #   req(input$brainSpanRegions)
    #   
    #   testbrainspan <- 0
    #   brainSpanGenes <- gsub(" ", "", input$brainSpanText, fixed = TRUE)
    #   brainSpanGenes <- strsplit(brainSpanGenes, ",")[[1]] %>% map_chr(toupper)
    #   brainSpanRegionList <- input$brainSpanRegions
    #   
    #   brainSpanInfo2 %>% filter(GeneSymbol %in% brainSpanGenes) %>%
    #     gather(Donor, Expr, 2:525) %>% pull(Donor) -> xx
    #   
    #   if (length(xx) == 0) {
    #     error_msgBS <- "Target is not found in the database"
    #     
    #     output$errBS <- renderText({
    #       paste("<font color=\"#FF0000\"><b>",error_msgBS)
    #     })
    #     shinyjs::show("errBS")
    #   }
    #   
    #   else {
    #     xx <- strsplit(xx, "_")
    #     unlist(lapply(xx, '[[', 2)) -> xxAge
    #     unlist(lapply(xx, '[[', 3)) -> xxReg
    #     
    #     output$brainSpanPlot <- renderPlot(brainSpanInfo2 %>% filter(GeneSymbol %in% brainSpanGenes) %>%
    #                                          gather(Donor, Expr, 2:525) %>% mutate(Reg = xxReg, Age = xxAge) %>%
    #                                          filter(Reg %in% brainSpanRegionList) %>%
    #                                          ggplot(aes(x = Age, y = log10(Expr), color = GeneSymbol, group = GeneSymbol)) +
    #                                          geom_smooth(se = input$brainSpanPar2) + {if(input$brainSpanPar1)geom_point()} +
    #                                          facet_wrap(~ Reg) + theme_light() + theme(axis.text.x = element_text(angle = 60, size = 8)) +
    #                                          scale_x_discrete(limits=c("8pcw", "9pcw", "12pcw", "13pcw", "16pcw", "17pcw", "19pcw", "21pcw",
    #                                                                    "24pcw", "25pcw", "26pcw", "35pcw", "37pcw", "4mos", "10mos",
    #                                                                    "1yrs", "2yrs", "3yrs", "4yrs", "8yrs", "11yrs", "13yrs", "15yrs", "18yrs",
    #                                                                    "19yrs", "21yrs", "23yrs", "30yrs", "36yrs", "37yrs", "40yrs"))
    #     )
    #     #output$brainSpanPlot <- renderPlot(ggplotly2)
    #     
    #     shinyjs::show("brainSpanPlot")
    #     
    #   }
    #   
    # })
    
    # BrainCloud Local Server Tab ----
    observeEvent(input$brainCloudButton, {
        shinyjs::hide("errBC")
        shinyjs::hide("brainCloudPlot")
        
        req(input$brainCloudText)
        
        
        
        brainCloudGenes <- gsub(" ", "", input$brainCloudText, fixed = TRUE)
        brainCloudGenes <- strsplit(brainCloudGenes, ",")[[1]] %>% map_chr(toupper)
        brainCloudInfo %>% filter(GeneSymbol %in% brainCloudGenes) %>% 
            distinct(GeneSymbol,.keep_all=T) %>% 
            gather(Donor, Expr, 2:270) %>% pull(Donor) -> xx
        
        if (length(xx) == 0) {
            error_msgBC <- "Target is not found in the database"
            
            output$errBC <- renderText({
                paste("<font color=\"#FF0000\"><b>",error_msgBC)
            })
            shinyjs::show("errBC")
        }
        
        else {
            
            xx <- strsplit(xx, "\\[")
            unlist(lapply(xx, '[[', 2)) -> xxAge
            
            output$brainCloudPlot <- renderPlot(
                brainCloudInfo %>% filter(GeneSymbol %in% brainCloudGenes) %>% 
                    distinct(GeneSymbol,.keep_all=T) %>%
                    gather(Donor, Expr, 2:270) %>% mutate(Age = as.double(xxAge)) %>% 
                    mutate(Group = case_when(
                        Age < 0 ~ "Fetal", 
                        Age < 1 ~ "Infant", 
                        Age < 10 ~ "Child", 
                        Age > 10 ~ "Adult")) %>%
                    mutate(GroupF = factor(Group, 
                                           levels = c("Fetal", "Infant", 
                                                      "Child", "Adult"))) %>% 
                    ggplot(aes(x = Age, y = Expr, 
                               color = GeneSymbol, group = GeneSymbol)) + {if(input$brainCloudPar1)geom_point()} +
                    geom_smooth(se = input$brainCloudPar2, family = "symmetric") + theme_light() +
                    facet_grid( ~ GroupF, scales = "free_x")
            )
            
            
            shinyjs::show("brainCloudPlot")
        }
        
    })
    
    # BrainAtlas Tab ----
    observeEvent(input$brainAtlasButton, {
        shinyjs::hide("errBA")
        shinyjs::hide("brainAtlasPlot")
        
        req(input$brainAtlasText)
        
        
        
        brainAtlasGenes <- gsub(" ", "", input$brainAtlasText, fixed = TRUE)
        brainAtlasGenes <- strsplit(brainAtlasGenes, ",")[[1]] %>% map_chr(toupper)
        
        brainAtlasInfo %>% filter(gene %in% brainAtlasGenes) %>% group_by(CellType, gene) %>% 
            summarise(mean_cpm = mean(CPM_mean), min_cpm = min(CPM_mean), max_cpm = max(CPM_mean)) %>% 
            mutate_if(is.numeric,round,4) -> BATable1
        
        
        
        
        output$BAtable1 = DT::renderDataTable(
            DT::datatable(
                BATable1, escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                    className = 'dt-center', targets = "_all")))
            )
        )
        
        
        
        output$BAtable2 = DT::renderDataTable({
            brainAtlasInfo %>% filter(CellType == input$BAtable2Set1,gene %in% brainAtlasGenes) %>% 
                mutate(prpor = CritPassCountsPerCluster/totalCountPerCluster) %>% 
                group_by(CellType, gene) %>% summarise(G1_prporMean = mean(prpor), 
                                                       G1_prporMax = max(prpor), G1_prporMin = min(prpor)) -> tt1
            
            brainAtlasInfo %>% filter(CellType==input$BAtable2Set2,gene %in% brainAtlasGenes) %>% 
                mutate(prpor = CritPassCountsPerCluster/totalCountPerCluster) %>% 
                group_by(CellType, gene) %>% summarise(G2_prporMean = mean(prpor), 
                                                       G2_prporMax = max(prpor), G2_prporMin = min(prpor)) -> tt2 
            
            inner_join(tt1,tt2, by = "gene")  %>% ungroup() %>% rename(G1 = CellType.x, G2=CellType.y) %>%
                mutate(prporMeanDiff = G1_prporMean-G2_prporMean, prporMaxDiff = G1_prporMax-G2_prporMax,
                       prporMinDiff = G1_prporMin-G2_prporMin) %>% 
                select(gene,G1,G2,contains("Mean"), contains("Max"),contains("Min")) %>% 
                mutate_if(is.numeric, round, 4) -> ttFull
            datatable(
                ttFull,
                class = 'cell-border stripe',
                options=list(scrollX=TRUE, searching = T,
                             rowCallback = JS("function(r,d) {$(r).attr('height', '60px')}")
                )
            )
            # BATable1 %>% select(CellType, gene, mean_cpm) %>% spread(CellType, mean_cpm) %>% 
            #   mutate_if(is.numeric, ~(.+1) ) %>% mutate_if(is.numeric, log10) %>% 
            #   mutate_if(is.numeric, round, 4) %>% 
            #   mutate(Group1VsGroup2 = get(input$BAtable2Set1) - get(input$BAtable2Set2))
        })
        
        output$BAtabl2PassedCrit = renderText({
            brainAtlasInfo %>% filter(CellType == input$BAtable2Set1,gene %in% brainAtlasGenes) %>% 
                mutate(prpor = CritPassCountsPerCluster/totalCountPerCluster) %>% 
                group_by(CellType, gene) %>% summarise(G1_prporMean = mean(prpor), 
                                                       G1_prporMax = max(prpor), G1_prporMin = min(prpor)) -> tt1L
            
            brainAtlasInfo %>% filter(CellType==input$BAtable2Set2,gene %in% brainAtlasGenes) %>% 
                mutate(prpor = CritPassCountsPerCluster/totalCountPerCluster) %>% 
                group_by(CellType, gene) %>% summarise(G2_prporMean = mean(prpor), 
                                                       G2_prporMax = max(prpor), G2_prporMin = min(prpor)) -> tt2L 
            
            inner_join(tt1L,tt2L, by = "gene")  %>% ungroup() %>% rename(G1 = CellType.x, G2=CellType.y) %>%
                mutate(prporMeanDiff = G1_prporMean-G2_prporMean, prporMaxDiff = G1_prporMax-G2_prporMax,
                       prporMinDiff = G1_prporMin-G2_prporMin) %>% 
                select(gene,G1,G2,contains("Mean"), contains("Max"),contains("Min")) %>% 
                mutate_if(is.numeric, round, 4) -> ttFullL
            
            paste0("Targets Enriched: ", 
                   paste(pull(filter(ttFullL, prporMeanDiff>=input$BAtabl2Crit), 
                              gene), collapse = ",")
            )
        })
        
        brainAtlasInfo %>% filter(gene %in% brainAtlasGenes) %>% select(gene, CPM_mean, cluster) %>% 
            spread(cluster, CPM_mean) %>%
            column_to_rownames("gene") %>% as.matrix() %>% `+` (1) %>% log10() -> brainAtlasMatrix
        
        mat_colors <- list(CellType = brewer.pal(7, "Dark2"))
        names(mat_colors$CellType) <- unique(brainAtlasGrouping$CellType)
        
        
        output$brainAtlasPlot <- renderPlot(
            pheatmap(brainAtlasMatrix, 
                     color = colorRampPalette(c("yellow", "red"))(n = 50),
                     annotation_col = brainAtlasGrouping,
                     annotation_colors = mat_colors,
                     fontsize_col = 8,
                     cluster_rows = if(length(brainAtlasGenes)>1) {T} else {F},
                     cluster_col = if(input$BAHM_Par1) {T} else {F}
                     
                     #scale = if(input$brainAtlasPar1) {"row"} else {"none"}
            )
        )
        
        shinyjs::show("brainAtlasPlot")
        
        
    })
    # GTEx Tab ----
    observeEvent(input$GTExButton1, {
        #shinyjs::hide("errBA")
        shinyjs::hide("GTExPlot1")
        
        req(input$GTExText1)
        
        
        
        GTEx1Genes1 <- gsub(" ", "", input$GTExText1, fixed = TRUE)
        GTEx1Genes1 <- strsplit(GTEx1Genes1, ",")[[1]] %>% map_chr(toupper)
        
        
        GTExGeneExpMedian %>% select(-gene_id) %>% filter(Description %in% GTEx1Genes1) %>% column_to_rownames("Description") %>% 
            as.matrix() -> geneExpMedianFinal
        
        output$GTExPlot1 <- renderPlot({
            pheatmap(if (input$GTEx_Par1) {log10(geneExpMedianFinal+1)} else {geneExpMedianFinal}, 
                     color = colorRampPalette(c("yellow", "red"))(n = 50),
                     annotation_col = GTExSampleAtr2,
                     annotation_colors = GTExColoredGrouped,
                     fontsize_col = 8,
                     cluster_rows = if(length(GTEx1Genes1)>1) {T} else {F},
                     cluster_col = if(input$GTEx_Par2) {T} else {F}
                     
                     #scale = if(input$brainAtlasPar1) {"row"} else {"none"}
            )
        })
        
        DT::datatable({})
        
        output$GTExeQTLTable <-  DT::renderDataTable({
            DT::datatable({
                GTExQTLData  %>% filter(gene_name %in% GTEx1Genes1, Tissue %in% input$dbsGTEx)
            }, escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                className = 'dt-center', targets = "_all")))
            
            )
        })
        shinyjs::show("GTExPlot1")
    })
    
    
    # observeEvent(input$GTExButton2, {
    # 
    #   req(input$GTExText2)
    # 
    #   GTEx1Genes2 <- gsub(" ", "", input$GTExText2, fixed = TRUE)
    #   GTEx1Genes2 <- strsplit(GTEx1Genes2, ",")[[1]] %>% map_chr(toupper)
    # 
    #   output$GTExeQTLTable <-  shiny::renderDataTable({
    #     GTExQTLData  %>% filter(gene_name %in% GTEx1Genes2, Tissue %in% input$dbsGTEx)
    #   })
    #   
    # })
    
    
    # GWAS Tab ----
    observeEvent(input$GWASBtn, {
        
        GWASGenes <- gsub(" ", "", input$GWASSearch, fixed = TRUE)
        GWASGenes <- strsplit(GWASGenes, ",")[[1]] %>% map_chr(toupper)
        
        
        
        output$GWASTable1 <- DT::renderDataTable(
            
            DT::datatable({
                
                GWASInfo %>% dplyr::filter(Gene %in% GWASGenes) %>% distinct() %>% 
                    mutate(LINK = paste0("<a href='http://", LINK,"' target='_blank'>http://", LINK,"</a>"))
            }, 
            escape = FALSE, rownames = F, style = "bootstrap", options = list(columnDefs = list(list(
                className = 'dt-center', targets = "_all"))))
        )
        
        output$GWASGraph1 <- renderPlot({
            GWASInfo %>% dplyr::filter(Gene %in% GWASGenes) %>% distinct() %>% group_by(DISEASE.TRAIT) %>% summarise(Counts = n()) %>% 
                arrange(desc(Counts)) %>% #{if (input$GWASPar1) {head(.,10)} else .} %>% 
                head(input$GWASPar2) %>% mutate(DISEASE.TRAIT = str_trunc(DISEASE.TRAIT,100)) %>%
                mutate(DISEASE.TRAIT = fct_reorder(DISEASE.TRAIT, Counts, .desc = FALSE)) %>% 
                ggplot(aes(DISEASE.TRAIT, Counts))  + geom_col() + theme_bw() + 
                theme(axis.text.x = element_text(size = 6)) + coord_flip()
            
        })
        
        output$GWASGraph2 <- renderSankeyNetwork({
            GWASInfo %>% filter(Gene %in% GWASGenes) %>% distinct() -> testSankey
            testSankey %>% group_by(DISEASE.TRAIT) %>% summarise(Counts = n()) %>% 
                arrange(desc(Counts)) %>%  head(input$GWASPar3) %>% pull(DISEASE.TRAIT) -> topTraits
            
            testSankey %>% dplyr::filter(DISEASE.TRAIT %in% topTraits) -> testSankey2
            
            Sankeynodes <- data.frame(
                "names" = c(unique(testSankey2$Gene), unique(testSankey2$CONTEXT), unique(testSankey2$DISEASE.TRAIT))
            )
            
            testSankey2 %>% group_by(Gene, CONTEXT) %>% summarise(count = n()) %>% rename(source = Gene, target = CONTEXT, value = count) -> links1
            testSankey2 %>% group_by(CONTEXT, DISEASE.TRAIT) %>% summarise(count = n()) %>% rename(source = CONTEXT, target = DISEASE.TRAIT, value = count) -> links2
            
            links <- rbind(links1, links2)
            linksmatric <- as.matrix(links)
            nrow(Sankeynodes) -1 -> fin
            Sankeynodes$ID = 0:fin
            
            left_join(links, Sankeynodes, by = c("source" = "names")) %>% ungroup() %>% select(ID, target, value) %>% rename(source = ID)-> linkMod1
            left_join(linkMod1, Sankeynodes, by = c("target" = "names"))  %>% select(-target) %>%
                rename(target = ID) %>% select(source, target, value) -> linkmodFin
            
            sankeyNetwork(Links = linkmodFin, Nodes = Sankeynodes,
                          Source = "source", Target = "target",
                          Value = "value", NodeID = "names",
                          fontSize= 12, nodeWidth = 30)
        })
        
    })
    
    # observeEvent(input$GTExButton2, {
    #   #shinyjs::hide("errBA")
    #   shinyjs::hide("GTExPlot2")
    # 
    #   req(input$GTExText2)
    # 
    #   GTEx1Genes2 <- gsub(" ", "", input$GTExText2, fixed = TRUE)
    #   GTEx1Genes2 <- strsplit(GTEx1Genes2, ",")[[1]] %>% map_chr(toupper)
    # 
    #   GTExGeneBrainExp  %>% filter(Description %in% GTEx1Genes2) %>% gather(Donor, Expr, 2:1672) -> GTExGeneBrainExpFinal
    # 
    #   sampleAtr$Donor = gsub("-", ".", sampleAtr$SAMPID)
    #   sampleAtr %>% select(-SAMPID) -> sampleAtr
    # 
    #   GTExGeneBrainExpFinal %>% pull(Donor) %>% unique() -> cc
    #   sampleAtr %>% filter(Donor %in% cc) -> sampleAtrBrain
    # 
    #   full_join(sampleAtrBrain, GTExGeneBrainExpFinal, by = "Donor") -> GTExGeneBrainExpFinal
    # 
    #   GTExGeneBrainExpFinal %>% select(-Donor) %>% rename(Gene = Description, Region = SMTS,
    #                                                       SubRegion = SMTSD,TPM = Expr) %>%
    #     select(Gene, Region, SubRegion, TPM) -> GTExGeneBrainExpFinal
    # 
    #   output$GTExPlot2 <- renderPlot({
    #   GTExGeneBrainExpFinal %>% ggplot(aes(SubRegion, if(input$GTEx_Par3){log10(TPM+1)} else {TPM} , fill = Gene)) +
    #     geom_boxplot() +
    #     theme_bw() + labs(y = if(input$GTEx_Par3){"Log10(TPM+1)"} else {"TPM"} ) +
    #     theme(axis.text.x = element_text(size = 10, angle = 30, hjust = 1))
    # 
    #   })
    # 
    #   shinyjs::show("GTExPlot2")
    # 
    # })
    
    
}

# Run App ----
shinyApp(ui = ui, server = server)
