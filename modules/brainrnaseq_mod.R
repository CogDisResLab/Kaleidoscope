
brainrnaseq_ui <- function(id) {
  
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes"))),
      ),hr(),
    fluidRow(
      column(width = 6,
             echarts4rOutput(ns("plot1_human"))
             ),
      column(width = 6, 
             echarts4rOutput(ns("plot1_mice"))
             )
    ),hr(),
    fluidRow(
      column(width = 6, echarts4rOutput(ns("plot2_human"))),
      column(width = 6, echarts4rOutput(ns("plot2_mice")))
    ),hr(),
    fluidRow(
      column(width = 6, echarts4rOutput(ns("plot3_human"))),
      column(width = 6, echarts4rOutput(ns("plot3_mice")))
      ),
    fluidRow(
      column(width = 12,align="center",
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn"), label = "Download Table")),
             
             )
    ),
    )
}

brainrnaseq_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = brainseq_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {

        genes <- isolate(genes_ids$genes())

        shinyjs::hide("plot1_human")
        shinyjs::hide("plot1_mice")
        shinyjs::hide("plot2_human")
        shinyjs::hide("plot2_mice")
        shinyjs::hide("plot3_human")
        shinyjs::hide("plot3_mice")
        shinyjs::hide("dn_btn_div")

        genes <- process_gene_input(genes)
          
        bs_res <- withProgress(message = "connecting to KS DB ...", {
          ks_brainseq(genes = genes, db = my_db) 
        })
        
        if(nrow(bs_res) > 0) {
          
          bs_res_human <- filter(bs_res, Species == "Human")
          bs_res_mice <- filter(bs_res, Species == "Mice")
          
          flag <- F
          
          flag <- any(c(length(unique(bs_res_human$HGNC_Symbol)), length(unique(bs_res_mice$HGNC_Symbol))) > 1)
          
          output$plot1_human <- renderEcharts4r({
          
            bs_res_human %>% 
              group_by(HGNC_Symbol) %>%
              e_charts(CellType, timeline = ifelse(flag, T, F), renderer="svg") %>%
              e_bar(FPKM, legend = F) %>% 
              e_flip_coords() %>% 
              e_grid(
                left = 200, # pixels
                top = "15%" # percentage = responsive
              ) %>% 
              e_x_axis(name = "log10(FPKM + 1)", 
                       axisLine = list(onZero = F),
                       nameLocation = "start", 
                       nameGap = 40) %>% 
              e_title("Brain RNA-Seq in Human", subtext = "Gene Expression values (log10(FPKM + 1)) Per Cell Type") %>% 
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
            
          })
          
          output$plot1_mice <- renderEcharts4r({
            
            bs_res_mice %>% 
              group_by(HGNC_Symbol) %>%
              e_charts(CellType, timeline = ifelse(flag, T, F), renderer="svg") %>%
              e_bar(FPKM, legend = F) %>% 
              e_flip_coords() %>% 
              e_grid(
                left = 200, # pixels
                top = "15%" # percentage = responsive
              ) %>% 
              e_x_axis(name = "log10(FPKM + 1)", 
                       axisLine = list(onZero = F),
                       nameLocation = "middle", 
                       nameGap = 40) %>% 
              e_title("Brain RNA-Seq in Mice", subtext = "Gene Expression values (log10(FPKM + 1)) Per Cell Type") %>% 
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
            
          })
          
          if(flag) {
            output$plot2_human <- renderEcharts4r({
              
              bs_res_human %>% 
                group_by(CellType) %>% 
                e_charts(renderer="svg") %>%
                e_boxplot(FPKM, itemStyle = list(color = "#b8c5f2"), layout = "horizontal") %>% 
                e_x_axis(type = "category", axisLabel =list(rotate = 45, fontSize = 8) ) %>% 
                e_y_axis(name = "log10(FPKM + 1)", 
                         axisLine = list(onZero = F),
                         nameLocation = "middle", 
                         nameGap = 40
                         ) %>% 
                e_title("Brain RNA-Seq in Human", subtext = "Gene Expression values ( log10(FPKM + 1)) Per Cell Type of Input Genes") %>% 
                e_tooltip() %>% 
                e_toolbox() %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView"))
              
            })
            
            output$plot2_mice <- renderEcharts4r({
              
              bs_res_mice %>% 
                group_by(CellType) %>% 
                e_charts(renderer="svg") %>%
                e_boxplot(FPKM, itemStyle = list(color = "#b8c5f2")) %>%
                e_x_axis(type = "category", axisLabel =list(rotate = 45, fontSize = 6) ) %>% 
                e_y_axis(name = "log10(FPKM + 1)", 
                         axisLine = list(onZero = F),
                         nameLocation = "middle",
                         nameGap = 40
                         ) %>% 
                e_title("Brain RNA-Seq in Mice", subtext = "Gene Expression values ( log10(FPKM + 1)) Per Cell Type of Input Genes") %>%
                e_tooltip() %>% 
                e_toolbox() %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView"))
              
            })
            
            shinyjs::show("plot2_human")
            shinyjs::show("plot2_mice")
            
          }
          
          output$plot3_human <- renderEcharts4r({
            
            bs_res_human %>% 
              group_by(CellType) %>% 
              summarise(FPKM = mean(FPKM)) %>% 
              mutate(total = sum(FPKM), prob = (FPKM/total)*100, 
                     Avg = "Input") %>% 
              rbind(all_human_prop) %>% select(CellType, Avg, prob) %>% 
              mutate_if(is.numeric, round, 2) %>% 
              pivot_wider(names_from = Avg, values_from = prob) %>% 
              e_charts(CellType, renderer="svg") %>% 
              e_radar(Avg, max = 50) %>% 
              e_radar(Input, max = 50) %>% 
              e_title("Brain RNA-Seq in Human", subtext = "Proportion of Gene Expression values Per Cell Type") %>%
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_legend(left = "left", top = "middle", orient = "vertical")
            
          })
          
          output$plot3_mice <- renderEcharts4r({
            bs_res_mice %>% 
              group_by(CellType) %>% 
              summarise(FPKM = sum(FPKM)) %>% 
              mutate(total = sum(FPKM), prob = (FPKM/total)*100, 
                     Avg = "Input") %>% 
              rbind(all_mice_prop) %>% select(CellType, Avg, prob) %>% 
              mutate_if(is.numeric, round, 2) %>% 
              pivot_wider(names_from = Avg, values_from = prob) %>% 
              e_charts(CellType, renderer="svg") %>% 
              e_radar(Avg, max = 50) %>% 
              e_radar(Input, max = 50) %>% 
              e_title("Brain RNA-Seq in Mice", subtext = "Proportion of Gene Expression values Per Cell Type") %>%
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_legend(left = "left", top = "middle", orient = "vertical")
          })
          
          
          download_btn_server(id = "dn_btn", 
                              tbl = rbind(
                                bs_res_mice %>% mutate(Species = "Mouse"), 
                                bs_res_human %>% mutate(Species = "Human")
                                ) %>% 
                                rename(`log10(FPKM+1)` = FPKM)
                              , name = "BarinRNASeq_Table")
          
          shinyjs::show("plot1_human")
          shinyjs::show("plot1_mice")
          shinyjs::show("plot3_human")
          shinyjs::show("plot3_mice")
          shinyjs::show("dn_btn_div")

        }
        
        else{
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
        }
        shinyFeedback::resetLoadingButton("genes-btn")
      })
    }
  )
}

ks_brainseq <- function(genes, db = my_db) {
  
  suppressWarnings({
  
  tbl(my_db,"brainseq_mouse_updated") %>% 
    filter(HGNC_Symbol %in% genes) %>% 
    collect() %>% 
    mutate(Species = "Mice", FPKM = round(log10(FPKM + 1), 2)) -> mice_table_one
  
  tbl(my_db,"brainseq_human_updated") %>% 
    filter(HGNC_Symbol %in% genes) %>% 
    collect() %>% 
    mutate(Species = "Human" ,FPKM = round(log10(FPKM + 1), 2)) -> human_table_one
  
  })
  
  rbind(mice_table_one,human_table_one)
  
}

brainseq_info_html <- HTML('<div class="features-icon"><span class="icon-bargraph"></span></div>
                  <h1 class="features-title font-alt">BrainRNA-Seq</h1>
                  <p>RNA-seq of cells isolated and purified from mouse and human brain from grey matter of cortex tissue. 
Purified using cell-type specific antibodies (anti-CD45 to capture microglia/macrophages, anti-GalC 
hybridoma supernatant to harvest oligodendrocytes, anti-O4 hybridoma to harvest OPCs, anti-Thy1 (CD90) 
to harvest neurons, anti-HepaCAM to harvest astrocytes, and BSL-1 to harvest endothelial cells).</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/26687838/" target="_blank" rel="noopener noreferrer">PMID: 26687838</a>
                         <br>
                         <a href="https://www.brainrnaseq.org/" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')
