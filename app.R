# ---- dependencies ----
pkgs <- c("dplyr", "shiny", "shinyBS", "shinyFeedback", "pander", "tibble", "shinythemes", "devtools", "DT",
          "stringi", "openssl", "readr")
lapply(pkgs, require, character.only = TRUE)
tab_cex <- "font-size:80%"
options(stringsAsFactors = FALSE, shiny.maxRequestSize=30*1024^2)

guess_encodings <- function (file, n_max = 10000, threshold = 0.1) 
{
    if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("stringi package required for encoding operations", 
             call. = FALSE)
    }
    lines <- readr::read_lines_raw(file, n_max = n_max)
    all <- unlist(lines)
    if (stringi::stri_enc_isascii(all)) {
        df <- data.frame(encoding = "ASCII", Language = "", Confidence = 1)
        names(df) <- tolower(names(df))
        return(df)
    }
    guess <- stringi::stri_enc_detect(all)
    df <- as.data.frame(guess[[1]], stringsAsFactors = FALSE)
    names(df) <- tolower(names(df))
    df$encoding <- toupper(df$encoding)
    df[df$confidence > threshold, ,drop = FALSE]
}

# ---- ui ----
ui = fluidPage(theme = shinythemes::shinytheme("cerulean"),
               mainPanel(
                   h1("Check fileEncodings"),
                   shiny::br(),
                   shiny::h4("Use this app to check file encoding for a .csv file."),
                   shiny::p("On file upload, the app will retun a table of top encoding guesses."),
                   h3("upload file"),
                   fileInput('csv_enc', 'Choose file to upload',
                             accept = c(
                                 'text/csv',
                                 'text/comma-separated-values',
                                 '.csv'
                             )
                   ),
                   shiny::hr(),
                   h3("choose encoding"),
                   shiny::h5('Use dropdown menu to toggle encodings. Top guess selected by default'),
                   fluidRow(
                       column(4,
                              shinyBS::bsButton("enc_reset", "Reset encoding", 
                                                block = T, 
                                                icon = icon("repeat", lib = "glyphicon")),
                              br(),
                              shinyBS::bsButton("enc_update", "add to output",
                                                style="primary", block = T, 
                                                icon = icon("ok-circle", lib = "glyphicon")),
                              br(),
                              shinyBS::bsButton("enc_save", "view output",
                                                style="success", block = T, 
                                                icon = icon("check", lib = "glyphicon")),
                              shinyBS::bsModal("view_output", "Edit vnames entry", "enc_save", size = "large",
                                               tableOutput("output"),
                                               downloadButton('downloadData', 'Download')),
                              uiOutput("enc_select"),
                              shiny::div(strong(textOutput("enc_msg")), style = "color:red")),
                       column(6, 
                              shiny::tableOutput('fileEncodings')),
                       column(2,
                              radioButtons("view", label = h5("view options"),
                                           choices = list("paginate" = 1, "scroll" = 2),
                                           selected = 1),
                              h6("Scroll can be slow. Not recommended for large datasets"))),
                   br(),
                   shiny::hr(),
                   div(DT::dataTableOutput('enc_table'), style = tab_cex)))


server = function(input, output, session) {
    v <- reactiveValues(fileEncodings = NULL, enc_msg = "", encoding = "UTF-8",
                        out_table = NULL, options = list(bPaginate = T),
                        output = data.frame(filename = character(0),
                                            encoding = character(0),
                                            confidence = character(0),
                                            assessment = character(0)),
                        load.file = 0)
    # ---- fileEncodings ----
    
    observeEvent(input$csv_enc, {
        inFile <- input$csv_enc
        v$fileEncodings <- head(guess_encodings(inFile$datapath),7)
        v$encoding <- v$fileEncodings[1, "encoding"]
        
        output$enc_select <- renderUI({
            selectInput("encodingIN", label = "select input file encoding", 
                        choices = iconvlist(), 
                        selected = v$encoding, 
                        multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)})
    })
    
    output$fileEncodings <- renderTable({if(is.null(v$fileEncodings)){return(NULL)}else{
        v$fileEncodings  
    }})
    
    observeEvent(input$csv_enc,{v$load.file <- v$load.file + 1})
    observeEvent(input$encodingIN, {v$load.file <- v$load.file + 1})
    
    
    observeEvent({input$encodingIN}, {
        inFile <- input$csv_enc
        if(is.null(inFile)){
        }else{
            table <- try(read.csv(inFile$datapath, header = T,
                                  fileEncoding = input$encodingIN,
                                  strip.white = T, stringsAsFactors = F,
                                  na.strings = c("NA", "", " ", "   "), 
                                  blank.lines.skip = TRUE), silent = T)
            empty.cols <- F
            if(class(table) == "data.frame"){
                empty.cols <- apply(table, 2, FUN = function(x){all(is.na(x))})
            }
            if(any(class(table) == "try-error", all(empty.cols))){
                enc <- input$encodingIN
                output$enc_msg <- renderText({paste(enc,"invalid enc for file")})
                output$enc_select <- renderUI({
                    shiny::div(selectInput("encodingIN", label = "select input file encoding", 
                                           choices = iconvlist(), 
                                           selected = v$encoding, 
                                           multiple = FALSE, selectize = FALSE, width = NULL, 
                                           size = NULL), 
                               class="form-group has-error")})
            }else{
                output$enc_select <- renderUI({
                    shiny::div(selectInput("encodingIN", label = "select input file encoding", 
                                           choices = iconvlist(), 
                                           selected = input$encodingIN, 
                                           multiple = FALSE, selectize = FALSE, width = NULL, 
                                           size = NULL), 
                               class="form-group has-success")})
                table <- table[,!empty.cols]
                v$out_table <- table
            }
        }})
    
    # ---- buttons ----
    observeEvent(input$enc_update, {
        if(is.null(input$enc_update)){}else{
            inFile <- input$csv_enc
            if(v$fileEncodings$encoding[1] == input$encodingIN){
                cols <- data.frame(confidence = v$fileEncodings[1, "confidence"], assessment = "auto")
            }else{
                cols <- data.frame(confidence = NA, assessment = "user") 
            }
            row <- cbind(data.frame(filename = inFile$name, encoding = input$encodingIN),cols)
            if(inFile$name %in% v$output$filename){
                v$output[v$output$filename == inFile$name, ] <- row
                v$enc_msg <- "file encoding updated"
            }else{
                v$output <- rbind(v$output, row)
                v$enc_msg <- "file encoding added"
            }
        }
    })
    
    output$output <- shiny::renderTable(v$output)
    
    
    observeEvent(input$enc_reset, {
        if(is.null(input$enc_reset)){}else{
            v$enc_msg <- "encoding reset to auto."
            output$enc_select <- renderUI({
                selectInput("encodingIN", label = "select input file encoding", 
                            choices = iconvlist(), 
                            selected = v$encoding, 
                            multiple = FALSE, selectize = FALSE, width = NULL, size = NULL)
            })
        }
    })
    
    observeEvent(input$view, {
        if(input$view == 1){v$options <- list(bPaginate = T)}
        if(input$view == 2){v$options <- list(sScrollX = "800px", 
                                              sScrollY = "400px", 
                                              bPaginate = FALSE)}
    })
    
    # ---- outputs ----
    output$enc_table <- DT::renderDataTable({
        if(is.null(v$out_table)){return(NULL)}else{
            datatable(v$out_table,options = v$options, filter = "top")}
        })
    
    output$downloadData <- downloadHandler(
        filename =  "fileEnconding_checker_output.csv" ,
        content = function(file) {
            write.csv(v$output, file)
        }
    )
    
    
    
}

shinyApp(ui = ui, server = server)


