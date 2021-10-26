# Gene Text Input Module UI function

gtex_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes"))),hr(),
      column(width = 12, echarts4rOutput(ns("plot1"), height = "500"))
      ),
      hr(),
    fluidRow(
      column(width = 6,align="center",hidden(switchInput(ns("switch1"), "log", value = T, size = "small"))),
      column(width = 6,align="center",hidden(switchInput(ns("switch3"), "Brain", value = F, size = "small")))
    ),
      hr(),
    fluidRow(
      column(width = 11, plotly::plotlyOutput(ns("plot2"), height = "800")),
      column(width = 1, 
             hidden(noUiSliderInput(ns("hm_slider2"),label = "Col Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                    orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
             ),
             hidden(noUiSliderInput(ns("hm_slider3"),label = "Row Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                    orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
             ),
             hidden(switchInput(ns("switch2"), "scale", value = T, size = "small"))
      ),
      column(width = 12, reactableOutput(ns("table")), 
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")))
             ),
    )
  )
  
}


gtex_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      w <- Waiter$new(ns(c("plot1", "plot2", "table")), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
                      )
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = gtex_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {
        
        #req(genes_ids$genes())
        genes <- isolate(genes_ids$genes())
        #req(genes)
        
        w$show()
        
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("hm_slider2")
        shinyjs::hide("hm_slider3")
        shinyjs::hide("switch1")
        shinyjs::hide("switch2")
        shinyjs::hide("switch3")
        shinyjs::hide("table")
        shinyjs::hide("dn_btn_div")

        genes <- process_gene_input(genes)
        
        gtex_res <- withProgress(message = "connecting to GTEx ...", {
          ks_gtex(genes = genes, db = my_db) 
        })
        
        if(!is.null(gtex_res)) {
          
          gtex_res %>% 
            pivot_longer(2:ncol(.), names_to = "Tissue", values_to = "TPM") -> gtex_res_processed
          
          # add colors to tissue groups
            #mutate(Group = str_extract(Tissue, "[^_]+")) -> gtex_res_processed
          
          output$plot1 <- renderEcharts4r({
            gtex_res_processed %>% 
              {if(input$switch3) filter(.,Tissue %in% brain_tissues) else . } %>% 
              {if(input$switch1)  mutate(.,TPM = log_transfomed(TPM))  else .} %>% 
              {if(length(genes) == 1) {
                arrange(.,desc(TPM)) %>% 
                  #group_by(Group) %>% 
                  e_chart(.,Tissue,renderer="svg", reorder = F) %>% 
                  e_bar(e = .,TPM, stack = "grp", legend = F)
              } else {
                group_by(.,Tissue) %>% 
                  arrange(.,Tissue, desc(TPM)) %>%
                  e_chart(.,renderer="svg", reorder = F) %>% 
                  e_boxplot(e = .,TPM, outlier = F)
              } } %>% 
              e_x_axis(type = 'category', 
                                axisLabel = list(interval=0, rotate = 45, fontSize = 6)) %>% 
              e_title("GTEx Gene Expression", subtext = ifelse(input$switch1, "Median Gene Expression, log10(TPM + 1), Across Different Tissues in Healthy Human", 
                                                               "Median Gene Expression (TPM) Across Different Tissues in Healthy Human")) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()  %>% 
              e_show_loading(color = "black")
          })
          
          output$plot2 <- plotly::renderPlotly({
            
            gtex_res %>% 
            column_to_rownames("gene") %>% 
              {if(input$switch1)  mutate_if(.,is.numeric, log_transfomed)  else .} %>% 
              {if(input$switch3) select(., one_of(brain_tissues)) else .} -> mm
            
            mm <- mm[apply(mm, 1, var) != 0, ] 
            
            heatmaply::heatmaply(
              mm, scale = ifelse(input$switch2, "row", "none"), 
              fontsize_col = input$hm_slider2,
              fontsize_row = input$hm_slider3,
              Rowv = ifelse(length(gtex_res$gene) > 1, T, F )
              )
          })
          
          output$table <- renderReactable({
            reactable(
              gtex_res_processed %>% 
                {if(input$switch1)  mutate(.,TPM = log_transfomed(TPM))  else .} %>% 
                {if(input$switch3) filter(.,Tissue %in% brain_tissues) else . },
              searchable = TRUE,
              striped = TRUE,
              bordered = TRUE,
              filterable = TRUE
                      )
          })
          
          
          download_btn_server(id = "dn_btn", tbl = gtex_res_processed %>% 
                                {if(input$switch1)  mutate(.,TPM = log_transfomed(TPM))  else .} %>% 
                                {if(input$switch3) filter(.,Tissue %in% brain_tissues) else . }, 
                              name = "GTEX_Table")

          
          shinyjs::show("plot1")
          shinyjs::show("plot2")
          shinyjs::show("hm_slider2")
          shinyjs::show("hm_slider3")
          shinyjs::show("switch1")
          shinyjs::show("switch2")
          shinyjs::show("switch3")
          shinyjs::show("table")
          shinyjs::show("dn_btn_div")

        }
        
        else {
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
        }
        
        shinyFeedback::resetLoadingButton("genes-btn")
        
      }
      
      
      
      
      )}
    
    
    
    )
  }

# gtex function 
ks_gtex <- function(genes, db = my_db) {
  
  suppressWarnings(
    tbl(my_db,"gtex_data") %>% 
      filter(Gene %in% genes) %>% 
      collect() %>% 
      janitor::clean_names() -> table_res
  )
  
  
  if(nrow(table_res) > 0) {
    return(table_res)
  }
  
  else {
    return(NULL)
  }
  
  
}

log_transfomed <- function(x) {
  round(log10(x + 1), 2)
}

gtex_info_html <- HTML('<div class="features-icon"><span class="icon-profile-male"></span></div>
                  <h1 class="features-title font-alt">GTEx</h1>
                  <p>Database contains data of tissue-specific gene expression and regulation. 
Samples were collected from 53 tissues from almost 1000 individuals, primarily for molecular assays including WGS, 
WES, and RNA-Seq. Has eQTL, expression quantitative trait loci, data for all studied tissues to identify variant-gene
expression association</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/23715323/" target="_blank" rel="noopener noreferrer">PMID: 23715323</a>
                         <br>
                         <a href="https://gtexportal.org/home/" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')


# gtex_res_processed %>%  
#   {if(T) filter(.,Tissue %in% brain_tissues) else . } %>% 
#   {if(T)  mutate(.,TPM = log_transfomed(TPM))  else .} %>% 
#   {if(T) {
#     arrange(.,desc(TPM)) %>% 
#     e_chart(.,Tissue,renderer="svg", reorder = F) %>% 
#     e_bar(e = .,TPM, stack = "grp", legend = F)
#     } else {
#       group_by(.,Tissue) %>% 
#       arrange(.,Tissue, desc(TPM)) %>%  
#       e_chart(.,renderer="svg", reorder = F) %>% 
#       e_boxplot(e = .,TPM, outlier = F)
#       } } %>% 
#   #e_boxplot(TPM, outlier = F) %>% 
#   e_x_axis(type = 'category', 
#            axisLabel = list(interval=0, rotate = 45, fontSize = 6)) %>% 
#   e_title("GTEx Gene Expression", subtext = ifelse(T, "Median Gene Expression, log10(TPM + 1), Across Different Tissues in Healthy Human", 
#                                                    "Median Gene Expression (TPM) Across Different Tissues in Healthy Human")) %>% 
#   e_toolbox() %>%
#   e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
#   e_tooltip() 
# 
# 
# gtex_res_processed$Tissue %>% unique()


brain_tissues <- c("brain_amygdala", "brain_anterior_cingulate_cortex_ba24","brain_caudate_basal_ganglia",
                   "brain_cerebellar_hemisphere" ,"brain_cerebellum" ,"brain_cortex","brain_frontal_cortex_ba9",
                   "brain_hippocampus" ,"brain_hypothalamus","brain_nucleus_accumbens_basal_ganglia" ,
                   "brain_putamen_basal_ganglia","brain_spinal_cord_cervical_c_1" ,"brain_substantia_nigra" )

