# ---- dependencies ----
pkgs <- c("dplyr", "shiny", "shinyBS", "pander", "tibble", "shinythemes", "devtools", "DT")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pkgs, character.only = T)
source("/Users/Anna/Documents/workflows/rmacroRDM/R/functions.R", local = T)




ui = fluidPage(theme = shinytheme("cerulean"),
               mainPanel(
                 tabsetPanel(
                   fileInput('csv_enc', 'Choose file to upload',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )
                   ),
                   h1("Check fileEncodings"),
                   fluidRow(
                     column(4,
                            bsButton("enc_reset", "Reset encoding", 
                                     block = T, 
                                     icon = icon("repeat", lib = "glyphicon")),
                            br(),
                            bsButton("enc_update", "Update fileEncodings",
                                     style="primary", block = T, 
                                     icon = icon("ok-circle", lib = "glyphicon")),
                            br(),
                            bsButton("enc_save", "Save changes",
                                     style="success", block = T, 
                                     icon = icon("check", lib = "glyphicon")),
                            uiOutput("enc_select")),
                     column(6, 
                            h5(textOutput("enc_msg")),
                            tableOutput('fileEncodings')),
                     column(2,
                            radioButtons("view", label = h5("view options"),
                                         choices = list("paginate" = 1, "scroll" = 2),
                                         selected = 1),
                            h6("Scroll can be slow. Not recommended for large datasets")),
                     tags$hr()),
                   br(),
                   div(DT::dataTableOutput('enc_table'), style = tab_cex)))
)


server = function(input, output, session) {
}

runApp(list(ui = ui, server = server))
  # ---- v:reactives ----
  v <- reactiveValues(data_log = data_log, msg = "", vnames = vnames, msg2 = "", error = "",
                      format = "wide", fileEncodings = fileEncodings, enc_msg = "", 
                      enc_table = NULL, input.values = NULL, options = list(bPaginate = T),
                      input.vars = NULL, inputs = NULL, enc_table = NULL,
                      dup.df = NULL, dat = NULL,
                      vars = NULL, l.vars = NULL, tmp_vnames = NULL)
  