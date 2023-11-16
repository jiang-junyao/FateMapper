library(shiny)
library(bslib)
library(htmltools)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjqui)
library(FateMapper)
library(shinyWidgets)
options(shiny.maxRequestSize=1024*1024^2)


load('coretable.Rdata')



link_github <- tags$a(shiny::icon("github"), "FateMapper", href = "https://github.com/jiang-junyao/FateMapper", target = "_blank")


ui <- page_navbar(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    title = '',
    window_title = 'scLTDB',
    bg = "#0062cc",
    #nav_item(shiny::icon("circle-info")),
    ###Home-----
    nav_panel(title = "Home",
              icon = shiny::icon("circle-info"),
              div(class = 'home',
              fluidPage(
                  card(
                      card_header("Introduction",class = "Introduction"),
                      status = "primary",
                      width = 12,
                      height = NULL,
                      card_body(
                          uiOutput("HOME_output_text")
                      )

                  ),

                  layout_column_wrap(
                      width = 1/3,
                      height = NULL,
                      card(
                          class = 'species',
                          card_header("Species"),
                          height = NULL,
                          style = "font-size: 20px;margin: 0 auto;text-align:center;",
                          card_image(
                              file = "www/pie_species.png"
                          )

                      ),
                      card(
                          class = 'tissue',
                          card_header("Tissue"),
                          height = NULL,
                          style = "font-size: 20px;margin: 0 auto;text-align:center;background:#FFFFFF;",
                          card_image(
                              file = "www/pie_tissue.png"
                          )

                      ),
                      card(
                          card_header("Technology"),
                          height = NULL,
                          card_image(
                              file = "www/pie_tech.png"
                          )
                      )
                  )
                  ,
                  tags$br(),
                  card(
                      card_image(file = "www/intro_img.png" ),
                      style = 'background:#FFFFFF'
                  )

              )
              )
        ),
     ###Search-----
     nav_panel(title = "Search",
               fluidPage(
                   div(
                   card(
                       card_header("Dataset overview"),
                       div(DT::dataTableOutput('coretable',width = "100%"))
                   ),
                   card(
                       card_header("Dataset Selected"),
                       verbatimTextOutput('seleted_row')
                   )

                    ),style = "font-size:150%;width:80%;")
               ),


    ###Results-----
    nav_panel(title = "Results",
              fluidPage(
                  layout_column_wrap(
                      width = 1/2,
                      height = NULL,
                      navset_card_tab(
                          title = textInput('input_barcode',NULL,value = 'Please input barcode...'),
                          id = "umap", height = "800px",
                          full_screen = TRUE,
                          #static umap
                          # nav_panel("raw_uamp",
                          #          div(imageOutput("show_umap1"),style = "margin-left: auto; margin-right: auto;background:#FFFFFF;")
                          # ),
                          #dynamic umap
                          div(uiOutput("show_umap"),style = "margin-left: auto; margin-right: auto;")
                      ),
                      navset_card_tab(
                          title = "1231231",
                          id = "tabset1", height = "800px",
                          full_screen = TRUE,
                          nav_panel("Barcode_count",
                                   div(imageOutput("barcode_count_plot"),style = "margin-left: auto; margin-right: auto;")
                          ),
                          nav_panel("cell_type_similarity",
                                   div(imageOutput("cell_type_similarity_plot"),style = "margin-left: auto; margin-right: auto;")
                          ),
                          nav_panel('fate_bias',
                                   div(class = 'fate_bias_table',style = "font-size:150%",
                                       dataTableOutput('look_fate_bias',width = "100%",)
                                   )
                          ),
                          nav_panel("lineage tree",
                                   div(imageOutput("lineage_tree_plot"),style = "margin-left: auto; margin-right: auto;")
                          )

                      )
                  )
              )

              ),

    ###Online tools-----
    nav_panel(title = "Online tools",
              layout_column_wrap(
                width = 1/2,
                height = NULL,
                navset_card_tab(
                  title =   fluidRow(
                    class = "vertical-center-row",  # 添加自定义的类名
                    column(6,
                           tags$label(
                             "Please choose 2 datasets:",
                             style = 'margin-left: 20px;margin-top: 2px;font-size: 22px;'
                           )
                    ),
                    column(6,
                           selectizeInput(
                             "Compare_dataset",
                             label = NULL,
                             choices = coretable$Dataset,
                             multiple = TRUE,
                             width = 600,
                             options = list(maxItems = 2)
                           )
                    )
                  )

                ),

                navset_card_tab(
                  title = div(fileInput("upload_metadata", NULL,
                                    buttonLabel = "Upload...",
                                    width = 250,multiple = FALSE,accept = c(".csv"),
                                    ),style = 'margin-top:25px;'),
                  id = "tools2", height = "800px",
                  full_screen = TRUE,
                  nav_panel("clone profile"

                  ),
                  nav_panel("cell type similarity"

                  ),
                  nav_panel("Clone fate bias"

                  ),
                  nav_panel("Clone embedding"

                  ),
                )


              )

              ),
    nav_panel(title = "Download", p("Third tab content")),
    nav_panel(title = "Contact", p("Third tab content")),
    nav_spacer(),
    nav_item(link_github)

)
