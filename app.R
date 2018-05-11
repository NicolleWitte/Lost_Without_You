library(shiny)
library("lattice")
#lattice.options(default.theme = standard.theme(color = FALSE))
# tweaks, a list object to set up multicols for checkboxGroupInput
num_of_cols<-29 #20xnum_of_cols for 100px
tweaks <- 
  list(tags$head(tags$style(HTML(paste("
                                 .shiny-options-group {
                                 display: flex;
                                 flex-wrap: wrap;
                                 width: ",  num_of_cols*20,"px;
                                 }
                                 .checkbox-inline { 
                                 margin-left: 0px;
                                 margin-right: 0px;
                                 height: 16px;
                                 display: inline-block;
                                 flex: 0 0", 100/num_of_cols, "%;
                                 width: ", 100/num_of_cols, "%;
                                 }
                                 .checkbox-inline span {
                                 color: #fff;
                                 display: none;
                                 font-size: 1px;
                                 }
                                 .checkbox-inline+.checkbox-inline {
                                 margin-left: 0px;
                                 margin-right: 0px;
                                 }
                                 .multicol { 
                                 height: auto;
                                 -webkit-column-count: 1; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 1;    /* Firefox */ 
                                 column-count: 1; 
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 }
                                 ", sep="")))))

# values to show, or not show, these will be the 'choices' and 'selected' values
# for the checkboxGroupInput()
#all_rows <- rep(" ", 25)
#names(all_rows) <- paste(" ", all_rows)

all_rows <- 1:num_of_cols^2
#names(all_rows) <- paste("Row", all_rows)

# data control: the checkboxes will control the data values plotted
controls <-
  list(h3("Boxes"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'numSelector',
                                   label = NULL,
                                   choices  = all_rows,
                                   inline   = TRUE))) 

# run the app
runApp(list(
  ui = fluidPage(titlePanel("LWY Level Maker"), tweaks,
                 fluidRow(column(width = 5, controls), column(width =4, plotOutput("heatmap"))),
                 fluidRow(column(width = 5, downloadButton("downloadData", "Download"))),
                 fluidRow(column(width = 5, verbatimTextOutput("value"))),
                 # Copy the line below to make a text input box
                 #textInput("text", label = h3("Level Name:"), value = "Enter text..."),hr()),
                 #fluidRow(column(2, verbatimTextOutput("value"))), 
                 sidebarPanel(textOutput("numSelector"), textInput("text", label = h3("Level Name:"), value = " "), hr(), textOutput("text"))),
                 #mainPanel(plotOutput("heatmap"), tableOutput("table")),# Button
                 
  
  server = function(input, output) { 
    #plot_data <- reactive(input$numSelector),
    
    #output$plot <- renderDataTable(plot_data)
    
    # Table of selected dataset ----
    datasetInput <- reactive({
      unselected<-setdiff((1:(num_of_cols^2)),as.numeric(input$numSelector))
      selected<-as.numeric(input$numSelector)
      mat_nums<-1:num_of_cols^2
      mat_nums[unselected]<-0
      mat_nums[selected]<-3
      mat<-matrix(mat_nums, nrow=num_of_cols, ncol=num_of_cols)
      t(mat)
    })
    
    #datasetHeatmap <- reactive({
      #finalmat<-datasetInput()
      #finalmat[finalmat>0]<-100
    #})
    output$heatmap <- renderPlot({levelplot(t(datasetInput()[c(nrow(datasetInput()):1) , ]), colorkey=FALSE, labels=FALSE, col.regions = gray(100:0/100))})
    #output$table <- renderTable({datasetInput()})
    #output$numSelector <- renderText({setdiff((1:(num_of_cols^2)),as.numeric(input$numSelector))})
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$text, ".txt", sep = "")
      },
      content = function(file) {
        finalmat<-datasetInput()
        finalmat[finalmat>0]<-3
        write.table(finalmat, file, row.names = FALSE, col.names=FALSE, sep=" ")
      }
    )
  }))
# Define UI for app that draws a histogram ----
# ui <- fluidPage(
#     
#     # Copy the chunk below to make a group of checkboxes
#     checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
#                        choices = list(" " = 1, " " = 2, " " = 3),
#                        selected = 1),
#     
#     
#     hr(),
#     fluidRow(column(3, verbatimTextOutput("value")),
#     
#     checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
#                        choices = list(" " = 1, " " = 2, " " = 3),
#                        selected = 1),
#     
#     
#     hr(),
#     fluidRow(column(3, verbatimTextOutput("value")))
#     ))
# 
# 
# # Define server logic required to draw a histogram ----
# server <- 
#   
#   function(input, output) {
#     
#     # You can access the values of the widget (as a vector)
#     # with input$checkGroup, e.g.
#     output$value <- renderPrint({ input$checkGroup })
#     
#   }
# 


# Create Shiny app ----
#shinyApp(ui = ui, server = server)