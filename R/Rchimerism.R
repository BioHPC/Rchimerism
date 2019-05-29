

#' Rchimerism
#'
#' Use Rchimerism through the shiny interface
#'
#' This function allows the user to input data files
#' and view the chimerism percentages along with the input data matrices
#' through the browser interface, with the options to download the results
#'
#' @name Rchimerism
#' @importFrom shiny fluidPage
#' @importFrom shiny titlePanel
#' @importFrom shiny br
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny fileInput
#' @importFrom shiny actionButton
#' @importFrom shiny radioButtons
#' @importFrom shiny conditionalPanel
#' @importFrom DT dataTableOutput
#' @importFrom shiny h4
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny downloadButton
#' @importFrom shiny h5
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT formatStyle
#' @importFrom DT styleEqual
#' @importFrom tools file_ext
#' @importFrom shiny reactive
#' @importFrom shiny validate
#' @importFrom shiny need
#' @importFrom shiny downloadHandler
#' @importFrom shiny runApp
#' @importFrom shiny stopApp
#' @importFrom shiny shinyApp
#' @importFrom shiny shinyUI
#'
#' @return Returns nothing

#' @export


# Define UI for application
Rchimerism <- function() {
ui <- shiny::shinyUI({
  shiny::fluidPage(

  # Application title
  shiny::titlePanel("R Chimerism"),
  shiny::br(),
  shiny::br(),

  # Sidebar with a slider input for number of bins
  shiny::fluidRow(
    shiny::column(3,
       shiny::fileInput("markers", label = "Choose Marker File Input",
                 accept=c("csv",
                          ".csv")
       ),
      shiny::fileInput("ddata", label = "Choose Donor Data Input",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv", ".tsv")
                ),
      shiny::radioButtons("donor_type", label = NULL,
                   choices = list("Single Donor" = 1, "Double Donor" = 2),
                   selected = 1),
      shiny::conditionalPanel(condition = "input.donor_type == 2",
                       shiny::fileInput("d2data", label = "Choose Second Donor Data Input",
                                 accept=c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv", ".tsv")
                       )
                       ),
      shiny::fileInput("rdata", label = "Choose Recipient Data Input",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv", ".tsv")
      ),
      shiny::fileInput("sdata", label = "Choose Sample Data Input",
                accept=c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv", ".tsv")
      ),
      shiny::actionButton("run_locSD_button","Read input files")

    ),
    shiny::conditionalPanel("input.donor_type == 1",

    shiny::column(3, offset=1, shiny::h4("Final Results"),
      shiny::br(),
      shiny::fluidRow(DT::dataTableOutput("results_table"))
          ),
    shiny::column(2, offset = 1,
           shiny::h4(shiny::strong("Donor% Mean")),
           shiny::br(),
           shiny::fluidRow(shiny::strong(shiny::verbatimTextOutput("donor_p_mean"))),
           shiny::h5("Recipient% Mean"),
           shiny::fluidRow(shiny::verbatimTextOutput("recip_p_mean")),
           shiny::h5("Donor% SD"),
           shiny::fluidRow(shiny::verbatimTextOutput("donor_p_SD")),
           shiny::h5("Donor% CV"),
           shiny::fluidRow(shiny::verbatimTextOutput("donor_p_CV")),
           shiny::br(),
           shiny::fluidRow(shiny::downloadButton("check_file.txt", "Download Check File")),
           shiny::fluidRow(shiny::downloadButton("results.xls","Download Results Excel File"))
           )
    ),
    shiny::conditionalPanel("input.donor_type == 2",
                            shiny::column(6, shiny::h4("Final Results"),
                            shiny::br(),
                            shiny::fluidRow(DT::dataTableOutput("dd_results_table")),
                            shiny::fluidRow(shiny::h4("% Mean"), shiny::br(),

                                      shiny::column(4,
                                        shiny::fluidRow(
                                          shiny::column(5,shiny::strong("Donor_1 % Mean")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_1_p_mean"))
                                          ),
                                        shiny::fluidRow(
                                          shiny::column(5,shiny::strong("Donor_2 % Mean")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_2_p_mean"))
                                          ),
                                        shiny::fluidRow(
                                          shiny::column(5,shiny::strong("Recipient % Mean")),
                                          shiny::column(6,shiny::verbatimTextOutput("recipient_p_mean"))
                                          )

                                      ),
                                      shiny::column(4,
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Donor_1 % SD")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_1_SD"))
                                        ),
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Donor_2 % SD")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_2_SD"))
                                        ),
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Recipient % SD")),
                                          shiny::column(6,shiny::verbatimTextOutput("recipient_SD"))
                                        )

                                      ),
                                      shiny::column(4,
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Donor_1 % CV")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_1_CV"))
                                        ),
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Donor_2 % CV")),
                                          shiny::column(6,shiny::verbatimTextOutput("donor_2_CV"))
                                        ),
                                        shiny::fluidRow(
                                          shiny::column(4,shiny::h5("Recipient % CV")),
                                          shiny::column(6,shiny::verbatimTextOutput("recipient_CV"))
                                        )

                                      )

                            )

                     ),
                     shiny::column(2, offset = 1,
                                   shiny::br(),
                                   shiny::br(),
                                   shiny::br(),
                                   shiny::br(),
                                   shiny::fluidRow(shiny::downloadButton("dd_check_file.txt",
                                                                         "Download Check File")),
                                   shiny::fluidRow(shiny::downloadButton("dd_results.xls",
                                                                         "Download Results
                                                                                Excel File")))
                )
  ),
  shiny::conditionalPanel("input.donor_type ==1",
                     shiny::fluidRow(
                      shiny::column(4, shiny::h4("Donor Matrix"),DT::dataTableOutput("d_matrix")),
                      shiny::column(4, shiny::h4("Recipient Matrix"),DT::dataTableOutput("r_matrix")),
                      shiny::column(4, shiny::h4("Sample Matrix"),DT::dataTableOutput("s_matrix"))
                      )
                   ),
  shiny::conditionalPanel("input.donor_type ==2",
                   shiny::fluidRow(
                     shiny::column(6, shiny::h4("Donor_1 Matrix"),DT::dataTableOutput("d1_matrix")),
                     shiny::column(6, shiny::h4("Donor_2 Matrix"),DT::dataTableOutput("d2_matrix"))
                    ),
                     shiny::fluidRow(
                     shiny::column(6, shiny::h4("Recipient Matrix"),DT::dataTableOutput("r_dd_matrix")),
                     shiny::column(6, shiny::h4("Sample Matrix"),DT::dataTableOutput("s_dd_matrix"))
                     )

  )

)
})


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #"Read input data" button is pushed
  observeEvent(input$run_locSD_button, {

    #assign("ddata",input$ddata,inherits=TRUE)
    #assign("rdata",input$rdata,inherits=TRUE)
    #assign("sdata",input$sdata,inherits=TRUE)

    #Displays error dialog for invalid input file extension
    bad_input <- function(input_file,ext) {
      if (is.null(input_file)) {
        showModal(modalDialog(title=paste("Missing input")))
        return(FALSE)
      } else if (ext != tools::file_ext(input_file$datapath)){
        showModal(modalDialog(title=paste("Wrong File Extension for ","'",input_file[1],"'",
                                          ", requires '.", ext,"'", sep="")))
        return(FALSE)
      }
      else {
        return(NULL)
      }
    }


    check_input <- function(input_file,ext) {
      shiny::validate(
        #shiny::need(!is.null(input_file),"Missing input")
        bad_input(input_file,ext)

      )

    }


    check_input(input$markers,"csv")
    check_input(input$ddata,"txt")
    check_input(input$rdata,"txt")
    check_input(input$sdata,"txt")

    markers_csv <- strsplit(readLines(input$markers$datapath), ",")
    markers <- markers_csv[[1]]
    #markers = c('D3S1358','TH01','D21S11','D18S51','Penta E','D5S818','D13S317','D7S820','D16S539','CSF1PO','Penta D','vWA','D8S1179','TPOX','FGA')

    #Checks for error string output from loc/chi.R programs
    incoherent_input <- function(input){
      if(is.character(input)) {
        showModal(modalDialog(
          title = input
        ))
        return(FALSE)
      } else {
        return(NULL)
      }
    }
    is_coherent_input <- function(any_output) {
      shiny::validate(
        incoherent_input(any_output)
      )
    }

    #Handles noisy sample data, displays false call table from chi.R programs
    check_sample_data <- function(any_chi_output) {
      if (length(any_chi_output)==3) {
        showModal(modalDialog(
          title = "Sample Data Format Error",
          DT::renderDataTable(any_chi_output)
        )
        )
      }
      shiny::validate(
        shiny::need(length(any_chi_output)!=3, "Sample Data Format Error")
      )
    }


    #Double donor button was pressed
    if (input$donor_type == 2) {
      assign("d2data",input$d2data,inherits=TRUE)
      check_input(input$d2data,"txt")

      #source("R/locDD.R")
      #source("R/chiDD.R")

      loc_dd_output <- locDD(input$ddata,input$d2data,input$rdata,markers)

      is_coherent_input(loc_dd_output)


      #markers,profile,ru,rt,rnn,d1nn,d2nn,d1u,d2u,d1t,d2t,r
      #markers <- loc_dd_output[[1]]

      #Variables from loc.R needed by chi.R and/or Rchimerism
      profile <- loc_dd_output[[2]]
      ru <- loc_dd_output[[3]]
      rt <- loc_dd_output[[4]]
      rnn <- loc_dd_output[[5]]
      d1nn <- loc_dd_output[[6]]
      d2nn <- loc_dd_output[[7]]
      d1u <- loc_dd_output[[8]]
      d2u <- loc_dd_output[[9]]
      d1t <- loc_dd_output[[10]]
      d2t <- loc_dd_output[[11]]
      r <- loc_dd_output[[12]]
      d1m <- loc_dd_output[[13]]
      d2m <- loc_dd_output[[14]]
      rm <- loc_dd_output[[15]]

      chi_dd_output <-  chiDD(input$sdata,markers,profile,
                             ru,rt,rnn,d1nn,d2nn,d1u,d2u,d1t,d2t,r)

      is_coherent_input(chi_dd_output)


      check_sample_data(chi_dd_output)

      results <- chi_dd_output[[1]]
      sm <- chi_dd_output[[2]]

      #Dealing with Single Donor
    } else {

#      source("R/locSD.R")
#      source("R/chiSD.R")

      loc_sd_output <- locSD(input$ddata,input$rdata,markers)

      is_coherent_input(loc_sd_output)


      #markers <- loc_sd_output[[1]]
      profile <- loc_sd_output[[2]]
      rt <- loc_sd_output[[3]]
      dt <- loc_sd_output[[4]]
      dm <- loc_sd_output[[5]]
      rm <- loc_sd_output[[6]]
      d <- loc_sd_output[[7]]
      r <- loc_sd_output[[8]]
#input$sdata,markers,rt,dm,rm
#

      chi_sd_output <- chiSD(input$sdata,markers,profile,rt,dt,d,r)
      is_coherent_input(chi_sd_output)
      check_sample_data(chi_sd_output)

      results <- chi_sd_output[[1]]
      sm <- chi_sd_output[[2]]
    }



    #Dealing with Single Donor
    if (input$donor_type == 1) {


    #  printed_result <- results[,1:2]
      printed_result <- results[,1:3]
      printed_result[,1] <- sapply(printed_result[,1],as.character)
      printed_result[,2] <- results[,2]*100
#      output$results_table <- DT::renderDataTable({
      #   DT::datatable(printed_result, rownames = TRUE, options = list(pageLength = 5,
      #                                                           searching = FALSE,
      #                                                           lengthChange=FALSE))
      # })
      rdt <- DT::datatable(printed_result, rownames = TRUE, options = list(pageLength = 5,
                                                                  searching = FALSE,
                                                                  lengthChange=FALSE,
                                                                  columnDefs = list(list(visible=FALSE, targets= c(3)))))

      #Gray out donor%s not used in donor% mean calcs (non-informative/outlier)
      frdt <-  DT::formatStyle(rdt, columns = 2, valueColumns = 3, target = "row", color = DT::styleEqual(NA, "gray"))
      output$results_table <- DT::renderDataTable({ frdt })
      #Double Donor case
    } else {
      # printed_result <- results[-nrow(results),c(1,5,9,13)]
      #   output$dd_results_table <- DT::renderDataTable({
      #   DT::datatable(printed_result, rownames = TRUE, options = list(pageLength = 5,
      #                                                           searching = FALSE,
      #                                                           lengthChange=FALSE))
      # })
      # # printed_result <- results[-nrow(results),c(1,5,9,13)]
      dd_printed_result <- results[-nrow(results),c(1,2,5,6,9,10,13)]
      dd_printed_result <- dd_printed_result[,c(1:7)]*100
      dd_rdt <-  DT::datatable(dd_printed_result, rownames = TRUE, options = list(pageLength = 5,
                                                                  searching = FALSE,
                                                                  lengthChange=FALSE,
                                                                  columnDefs = list(list(visible=FALSE, targets= c(2,4,6)))
                                                                  ))
      #
      #Gray out donor%s not used in donor% mean calcs (non-informative/outlier)
      dd_frdt <-  DT::formatStyle(dd_rdt, columns = c(1,3,5), valueColumns = c(2,4,6), color = DT::styleEqual(c(NA), c("gray")))
      output$dd_results_table <- DT::renderDataTable(dd_frdt)

    }
#    p_output <- function(output_txt,num) {
#      output_txt <- renderText(max(results[-nrow(results),num], na.rm = TRUE)*100)
#      return(output_txt)
#    }
    p_output <- function(num) {
      output_txt <- renderText(format(round(max(results[-nrow(results),num], na.rm = TRUE)*100,3)))
      return(output_txt)
    }
    if (input$donor_type == 1) {
      output$donor_p_mean <- renderText(max(results[,3], na.rm = TRUE)*100)
      output$recip_p_mean <- renderText(max(results[,6], na.rm = TRUE)*100)
      output$donor_p_SD <- renderText(max(results[,4], na.rm = TRUE)*100)
      output$donor_p_CV <- renderText(max(results[,5], na.rm = TRUE)*100)
    } else {
        output$donor_1_p_mean <- p_output(2)
        output$donor_2_p_mean <- p_output(6)
        output$recipient_p_mean <- p_output(10)
        output$donor_1_SD <- p_output(3)
        output$donor_2_SD <- p_output(7)
        output$recipient_SD <- p_output(11)
        output$donor_1_CV <- p_output(4)
        output$donor_2_CV <- p_output(8)

        if (length(na.omit(results[-length(results[,12]),12])) == 0) {
          output$recipient_CV <- renderText(NaN)
        }
        else {
          output$recipient_CV <- p_output(12)
        }
#        ifelse(!max(results[,12]), output$recipient_CV <- renderText(NaN),
#               output$recipient_CV <-  renderText(max(results[-length(results[,12]),12], na.rm = TRUE)*100))

      }


    matrix_output <- function(output_matrix,internal_matrix,rcol,srch) {
      if (missing(srch)) {
        srch = ""
      }
      output_matrix <- DT::renderDataTable({
        DT::datatable(internal_matrix,
                  rownames = TRUE, extensions = "FixedColumns",
                  options = list(pageLength = 5, scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1, rightColumns=rcol),
                                 autoWidth = TRUE, search = list(search=srch)
                                 ))
      })
      return(output_matrix)

    }

    if(input$donor_type ==1) {
      dm_minus_profile_col <- dm[,-ncol(dm)]
      output$d_matrix <-  matrix_output(output$d_matrix,dm_minus_profile_col,1)
      output$r_matrix <-  matrix_output(output$r_matrix,rm,1)

      #Warning for checksum data generated
      if(999 %in% sm) {
        output$s_matrix <- matrix_output(output$r_dd_matrix,sm,1,"999")
        showModal(modalDialog("Clear '999' from Sample Matrix search box to remove filter",
                              title = "Found '999' value(s) in Sample Allele Matrix, review sample data"
        ))
      } else {
        output$s_matrix <- matrix_output(output$r_dd_matrix,sm,1)
      }
    }
    else {
      output$d1_matrix <-  matrix_output(output$d1_matrix,d1m,3)
      output$d2_matrix <-  matrix_output(output$d2_matrix,d2m,3)
      output$r_dd_matrix <- matrix_output(output$r_dd_matrix,rm,3)

      if(999 %in% sm) {
        output$s_dd_matrix <- matrix_output(output$r_dd_matrix,sm,1,"999")
        showModal(modalDialog("Clear '999' from Sample Matrix search box to remove filter",
                              title = "Found '999' value(s) in Sample Allele Matrix, review sample data"
        ))
      } else {
        output$s_dd_matrix <- matrix_output(output$r_dd_matrix,sm,1)
      }
    }



    results_xls <- shiny::reactive(results)
    return_xls <- function() {
     out_xls<- shiny::downloadHandler(

      filename = function(){"results.xls"},
      content = function(fname)
      write.table(results_xls(),fname,sep="\t",col.names=FALSE)

    )
     return(out_xls)
    }


    return_txt <- function() {
     out_txt <- shiny::downloadHandler(
      filename = function(){"check.txt"},
      content = function(file) {

        sink(file)
        print(getwd(),quote=F)
        if (input$donor_type ==1) {
          print("Donor Allele Matrix", quote = F)
          print(dm)
        } else {
          print("Donor 1 Allele Matrix", quote=F)
          print(d1m)
          print("Donor 2 Allele Matrix", quote=F)
          print(d2m)
        }
        print("Receipient Allele Matrix", quote = F)
        print(rm)
        print("Sample Allele Matrix", quote = F)
        print(sm)
        print("Final Results", quote = F)
        print(results)
        sink()

      }

    )
     return(out_txt)
    }

    if(input$donor_type == 1) {
      output$results.xls <- return_xls()
      output$check_file.txt <- return_txt()
    } else {
      output$dd_results.xls <- return_xls()
      output$dd_check_file.txt <- return_txt()
    }


  })
  session$onSessionEnded(shiny::stopApp)

}


shiny::runApp(shiny::shinyApp(ui, server), quiet=TRUE, launch.browser=TRUE)
#shinyApp(ui = ui, server = server)
}

