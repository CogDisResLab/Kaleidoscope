shinyUI(
  navbarPage(title = "Kaleidoscope",
                   theme = "style.css",
                   id = "tb_sets",
                   header = "",

                   # Home tab ----
                   tabPanel("Home", icon = icon("home"),
                            useShinyjs(),
                            useShinyalert(),
                            use_bs_popover(),
                            use_bs_tooltip(),
                            useShinydashboard(),
                            use_waiter(),
                            rclipboard::rclipboardSetup(),
                            # waiter_on_busy(
                            #   html = spin_loaders(36, color = "black"),
                            #   color = transparent(.5)
                            #   ),
                            value = "home",
                            includeHTML("www/index.html")
                   ),
                   
                   # App Tab ----
                   tabPanel("App", icon = icon("th", lib = "glyphicon"),
                            value = "app",
                            navbarPage(title = "",
                                       
                                       # BrainSeq ----
                                       tabPanel("BrainRNA-Seq",icon = icon("", class = "icon-bargraph"),
                                                brainrnaseq_ui("brainrnaseqTab"),
                                                
                                                # css and GA tags ----
                                                fluidRow(tags$style(type = "text/css", ".navbar {margin-bottom: 0;}"),
                                                         tags$style(type = "text/css", ".container-fluid {padding-left:5px;
                    padding-right:5px;}"),
                    #tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%")),
                                                         tags$head(HTML(
                                                           "<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-149864972-1'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-149864972-1');
</script>"
                                                           
                                                         ))
                                                         )
                                                ),
                                       # String ----
                                       tabPanel("STRING", icon = icon("", class = "icon-genius"),
                                                string_ui("stringTab")
                                                ),
                                       # iLINCS ----
                                       tabPanel("iLINCS", icon = icon("", class = "icon-lifesaver"),
                                                iincs_ui("lincsTab")
                                                ),
                                       # Lookup ----
                                       tabPanel("Lookup", icon = icon("", class = "icon-search"),
                                                lookup_ui("lookupTab")
                                                ),
                                       # BrainCloud ----
                                       tabPanel("BrainCloud", icon = icon("", class = "icon-linegraph"),
                                                braincloud_ui("braincloudTab")
                                                ),
                                       # GTEx ----
                                       tabPanel("GTEx", icon = icon("", class = "icon-profile-male"),
                                                gtex_ui("gtexTab")
                                                ),
                                       # BrainAtlas ----
                                       tabPanel("BrainAtlas", icon = icon("", class = "icon-aperture"),
                                                brainatlas_ui("brainatlasTab")
                                                ),
                                       # GWAS ----
                                       tabPanel("GWAS", icon = icon("", class = "icon-map"),
                                                gwascatalog_UI("gwascatalogTab")
                                                ),
                                       tabPanel("Report", icon = icon("", class = "file-contract"),
                                                report_ui("reportTab")
                                       )

                                       
                                       )
                            )
             )
  )
