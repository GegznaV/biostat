# # TODO: **[!!!]**
# #
# # 1. Create a window to edit dataframe.
# # 2. include js library "sortable" (???)
# #
#
# # n <- 10
# # DF <- data.frame(int  = rep(NA_integer_, n),
# #                  bool = rep(NA, n),
# #                  euro = round(rnorm(n, 30, 5), 2),
# #                  chr  = rep(NA_character_, n),
# #                  small = factor(rep(NA, n),
# #                                 levels = c("vyras", "moteris", "vaikas"),
# #                                 ordered = FALSE),
# #                  dt = as.Date(rep(NA, n)),
# #
# #                  stringsAsFactors = FALSE)
# #
# # nDF <- edit_df(DF); nDF
#
# add_empty_rows <- function(data, n = 0) {
#     checkmate::assert_integerish(n, lower = 0)
#     data[nrow(data) + n, ] <- NA
#     data
# }
#
# # edit_df =====================================================================
# edit_df <- function(DF,
#                     outdir = getwd(),
#                     outfilename = paste0("table",
#                                          format(Sys.time(), "%_Y-%m-%d_%Hh%Mm%Ss"))) {
#
#     checkmate::assert_data_frame(DF)
#     # Packages ================================================================
#     library(rhandsontable)
#
#     suppressPackageStartupMessages(library(shiny))
#     suppressPackageStartupMessages(library(ggplot2))
#     suppressPackageStartupMessages(library(shinyjs))
#     suppressPackageStartupMessages(library(DT))
#     suppressPackageStartupMessages(library(colourpicker))
#     library(shinyBS)
#     library(shinythemes)
#     library(scales)
#     library(Cairo)
#     library(shinyAce)
#
#
#     # UI =====================================================================
#     ui <- shinyUI(
#         tagList(
#             shinyjs::useShinyjs(),
#             # includeCSS("css/easyPlot.css"),
#             div(
#                 id = "loader",
#                 h2("Loading..."),
#                 tags$style(type = "text/css",
#                            ".recalculating {opacity: 1.0;}") # No blinking
#             ),
#
#             navbarPage(
#                 title = "Data Editor",
#                 id = "navbar",
#                 fluid = TRUE,
#                 theme = shinytheme("flatly"),
#                 # tabPanel: Size -------------------------------------------------------------
#                 tabPanel(
#                     title = "Initialize data ",
#                     icon = icon("fullscreen", lib = "glyphicon"),
#
#                     fluidPage(
#                         titlePanel("Choose the size of your dataframe")
#                     )
#                 ),
#
#                 # tabPanel: Variables -------------------------------------------------------------
#                 tabPanel(
#                     title = "Edit variables ",
#                     icon = icon("sort", lib = "glyphicon"),
#
#                     fluidPage(
#                         titlePanel("Specify the variables")
#                     )
#                 ),
#
#                 # tabPanel: Editor -------------------------------------------------------------
#                 tabPanel(
#                     title = "Edit data ",
#                     icon = icon("wrench", lib = "glyphicon"),
#
#                     fluidPage(
#
#                         titlePanel("Enter the values, edit the dataframe"),
#                         sidebarLayout(
#                             sidebarPanel(
#                                 helpText("Shiny app based on an example given in the `rhandsontable` package.",
#                                          "Right-click on the table to delete/insert rows.",
#                                          "Double-click on a cell to edit."),
#
#                                 wellPanel(
#                                     h3("Table options"),
#                                     radioButtons("useType",
#                                                  label = "Use Data Types",
#                                                  choices = c("TRUE", "FALSE"))
#                                 ),
#                                 br(),
#
#                                 wellPanel(
#                                     h3("Save"),
#                                     actionButton("save", label = "Save table")
#                                 )
#                             ),
#
#                             mainPanel(
#                                 rHandsontableOutput("hot", width = "90%", height = "100%")
#                             )
#                         )
#                     )
#                 ),
#
#
#                 # tabPanel: Summary -----------------------------------------------
#                 tabPanel(
#                     title = "Summary ",
#                     icon = icon("list-alt", lib = "glyphicon"),
#
#                     fluidPage(
#                         titlePanel("Choose the size of your dataframe"),
#
#                         aceEditor(outputId = "print_code_sc",
#                                   value = "",
#                                   mode = "r",
#                                   theme = "cobalt",
#                                   readOnly = TRUE)
#                     )
#                 ),
#
#                 # Done -----------------------------------------------------------
#                 tabPanel(
#                     title = "Done ",
#                     value = "quit",
#                     icon = icon("power-off"),
#                     br(),
#                     br(),
#                     br(),
#                     br(),
#                     br(),
#                     br(),
#                     fluidRow(column(3), column(6, h2("Thank you for using Data Editor!"))),
#                     fluidRow(column(3), column(6, h4("Now you should close this window.")))
#                 )
#
#
#             )
#         )
#     )
#     # Server =================================================================
#     server <- shinyServer(function(input, output, session) {
#
#         Sys.sleep(0.75)
#         hide(id = "loader", anim = TRUE, animType = "fade")
#
#         session$onSessionEnded(function() {
#             # rm(name, envir = easyPlotEnv)
#             # rm(easyPlotEnv, envir = .GlobalEnv)
#         })
#
#
#
#         values <- reactiveValues()
#
#         ## Handsontable
#         observe({
#             if (!is.null(input$hot)) {
#                 DF <- hot_to_r(input$hot)
#             } else {
#                 if (is.null(values[["DF"]]))
#                     DF <- DF
#                 else
#                     DF <- values[["DF"]]
#             }
#             values[["DF"]] <- DF
#         })
#
#         output$hot <- renderRHandsontable({
#
#             search_opts <-
#                 list(name = "Search",
#                      callback = htmlwidgets::JS(
#                          "function (key, options) {
#                          var srch = prompt('Search criteria');
#
#                          this.search.query(srch);
#                          this.render();
#         }"))
#
#
#
#             DF <- values[["DF"]]
#             if (!is.null(DF)) {
#                 (
#                     rhandsontable(data = DF,
#                                   width = 750,
#                                   height = 600,
#                                   useTypes = TRUE,
#                                   # useTypes = as.logical(input$useType),
#                                   # stretchH = "all",
#                                   # search = search_opts,
#                                   search = TRUE,
#                                   readOnly = FALSE
#                                   # The table can be streched to the full width by using stretchH.
#                                   # , stretchH = "all"
#                     ) %>%
#                         hot_context_menu(allowRowEdit = TRUE,
#                                          allowColEdit = FALSE,
#                                          allowCustomBorders = TRUE) %>%
#                         hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
#                         hot_col("dt", dateFormat = "YYYY-MM-DD", type = "date")
#                     # %>%
#                     #     hot_heatmap()
#                 )
#             }
#
#         })
#
#         ## Save data ---------------------------------------------------------
#
#         observeEvent(input$save, {
#             finalDF <- isolate(values[["DF"]])
#             saveRDS(finalDF,
#                     file = file.path(outdir,
#                                      sprintf("%s.rds", outfilename)))
#         })
#
#
#         # Stop ---------------------------------------------------------------
#         observe({
#             if (input$navbar == "quit") {
#                 stopApp(returnValue = isolate(values[["DF"]]))
#             }
#         })
#
#     })
#
#     ## run app ===============================================================
#     runApp(list(ui = ui, server = server))
# }
