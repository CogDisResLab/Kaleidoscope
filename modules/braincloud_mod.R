
# Gene Text Input Module UI function

# TODO: use same color for points and fit

braincloud_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             geneInputUI(ns("genes"))
             )

             ),hr(),
    fluidRow(
      column(width = 12, 
             shinyjs::hidden(switchInput(inputId = ns("se"),size = "mini", label = "Fit", value = T))
      ),
      column(width = 12,
             echarts4rOutput(ns("plot1"), height = "700px")
             )
      ),hr(),
    fluidRow(
      column(6,
             echarts4rOutput(ns("plot2"))
             ),
      column(6,
             echarts4rOutput(ns("plot3"))
      )
    ),
    fluidRow(
      column(6,
             echarts4rOutput(ns("plot4"))
      ),
      column(6,
             echarts4rOutput(ns("plot5"))
      )
    ),
    fluidRow(
      column(width = 12,align="center",
             download_btn_ui(ns("dn_btn"), label = "Download Table")
      )
    )
  )
  }


# Gene Text Input Module Server function

## TODO: deal with NULL results (no genes) - stylize table 

braincloud_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      w <- Waiter$new(ns(c("plot1", "plot2", "plot3", "plot4", "plot5")), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
      )
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = braincloud_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })

      observeEvent(genes_ids$btn(), {
        
        genes <- isolate(genes_ids$genes())
        #req(genes)
        
        w$show()
        
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("plot3")
        shinyjs::hide("plot4")
        shinyjs::hide("plot5")
        shinyjs::hide("se")
        
        
        genes <- process_gene_input(genes)
          
        bs_res <- withProgress(message = "connecting to KS Database ...", {
          ks_braincloud(genes = genes, db = my_db) 
        })
        
        if(!is.null(bs_res)) {
          
          bs_res <- group_by(bs_res, GeneSymbol)
          
          output$plot1 <- renderEcharts4r({
            
            plot_bc(bs_res, c("Fetal", "Infant", "Child", "Adult"), "Human Lifespan", "", input$se, min = -0.5, max = 80, splitNumber = 20) %>% 
              e_datazoom() %>% 
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))

          })
          
          output$plot2 <- renderEcharts4r({
            plot_bc(bs_res, "Fetal", "Fetal", "", input$se, min = -0.5, max = -0.375) %>% 
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
          })
          
          output$plot3 <- renderEcharts4r({
            plot_bc(bs_res, "Infant", "Infant", "", input$se, min = 0, max = 0.55) %>% 
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
          })
          
          output$plot4 <- renderEcharts4r({
            plot_bc(bs_res, "Child", "Child", "", input$se, min = 1, max = 18) %>% 
              e_tooltip() %>% e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
          })
          
          output$plot5 <- renderEcharts4r({
            plot_bc(bs_res, "Adult", "Adult", "", input$se, min = 18, max = 80) %>% 
              e_tooltip() %>% 
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
          })
          
          
          download_btn_server(id = "dn_btn", 
                              tbl = bs_res, name = "BarinCloud_Table")
          
          shinyjs::show("plot1")
          shinyjs::show("plot2")
          shinyjs::show("plot3")
          shinyjs::show("plot4")
          shinyjs::show("plot5")
          shinyjs::show("se")
          
        }
        
        else {
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
        }
        
        shinyFeedback::resetLoadingButton("genes-btn")
          
        
        

      })
    
    }
    
  )
  
}



ks_braincloud <- function(genes, db = my_db) {
  suppressWarnings(
    tbl(my_db,"brain_cloud") %>% filter(GeneSymbol %in% genes) %>% 
      select(-ID) %>% collect() %>% distinct(GeneSymbol, .keep_all = T) -> bc_table
  )
  
  if(nrow(bc_table) > 0) {
  
    colnames(bc_table) <- sub(".*\\[", "", colnames(bc_table))
  
    bc_table %>% pivot_longer(-GeneSymbol, names_to = "Age", values_to = "Expression") %>%
      mutate(
        Age = as.numeric(Age),
        Group = case_when(
        Age <= 0 ~ "Fetal", 
        Age <= 1 ~ "Infant", 
        Age <= 18 ~ "Child", 
        Age > 18 ~ "Adult")) -> bc_table
  
    return(bc_table)
  }
  
  else{return(NULL)}
  
  
}

plot_bc <- function(df, group, title, text, se, min, max, ...) {
  df %>% 
    filter(Group %in% group) %>% 
    e_charts(Age, renderer="svg") %>%
    e_scatter(Expression, legend = T, symbol_size = 10) %>%
    {if (se) e_loess(.,Expression ~ Age , legend = F, showSymbol = F) else .} %>% 
    e_x_axis(min = min, max = max, name = "Age", position = "bottom", 
             axisLine = list(onZero = F), ...
    ) %>% 
    e_y_axis(name = "Normalized Expression", 
             axisLine = list(onZero = F)) %>% 
    e_title(text = title,subtext = text)
}

braincloud_info_html <- HTML('<div class="features-icon"><span class="icon-linegraph"></span></div>
                  <h1 class="features-title font-alt">BrainCloud</h1>
                  <p>Transcription levels in the human prefrontal cortex across the lifespan. Post-mortem brains from fetal 
development through ageing to highlight the role of the human genome in cortical development, function 
and ageing. (n = 269 subjects without neuropathological or neuropsychiatric diagnosis). Age ranges: fetal, 
14–20 gestational weeks, infant: 0–6 months, child, 1–10 years; adolescent and adults till ~80 years.</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/22031444/" target="_blank" rel="noopener noreferrer">PMID: 22031444</a>
                         <br>
                         <a href="https://www.libd.org/brain-cloud/" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')

 



    

