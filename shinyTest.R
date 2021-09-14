library(shiny)


options(shiny.port = 7775)
options(shiny.host = "134.176.53.229")


counties <- readRDS("counties.rds")
StateNames <- c("ALL",toupper(unique(unlist(
  lapply (counties$name,function (x) strsplit(x,',')[[1]][1])))))
# User interface ----


ui <- shinyUI(fluidPage(
  titlePanel("Census Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore county-level demographic info
        as maps and data, using information from the 
        2010 US Census. View data for one state or for
        the entire United States."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("White", "Black",
                              "Hispanic", "Asian"),
                  selected = "White"),
      
      selectInput("state", label="Choose a state",
                  choices = StateNames, selected = "ALL"),
      
      
      selectInput("pctOrTot", label="As",
                  choices = c("Percent", "Total"), selected = "Percent"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0,100))
    ),
    
    mainPanel(
      tabPanel ("Interactive Map", plotOutput("map")),
      tabPanel("Data explorer", dataTableOutput("countyTable"))
    )
    
    
  )
))
# Server logic ----
library(maps)
library(mapproj)
source("helpers.R")

# read data
counties <- readRDS("counties.rds")
# add state column to data
counties$state <- unlist(lapply (counties$name,
                                 function (x) strsplit(x,',')[[1]][1]))


server <- shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      
      idx <- rep(TRUE,length(counties$state))
      if (input$state!="ALL"){
        idx <- counties$state==tolower(input$state)
      }
      labels <-  switch(input$pctOrTot, 
                        "Percent" = c("% White","% Black","% Hispanic","% Asian"),
                        "Total" = c("White","Black","Hispanic","Asian"))
      args <- switch(input$var,
                     "White" = list(counties$white[idx], "darkgreen", labels[1]),
                     "Black" = list(counties$black[idx], "black", labels[2]),
                     "Hispanic" = list(counties$hispanic[idx], "darkorange", labels[3]),
                     "Asian" = list(counties$asian[idx], "darkviolet", labels[4]))
      args$state <- input$state
      if (input$pctOrTot=="Percent"){
        args$min <- input$range[1]
        args$max <- input$range[2]
        do.call(percent_map, args)
      }else{
        args$min <- input$range[1]*7/100
        args$max <- input$range[2]*7/100
        args[[1]] <- log10(args[[1]] * counties$total.pop[idx] / 100)
        do.call(count_map, args)
      }
      
    })
    output$countyTable <- renderDataTable({
      do.call(makeTable, list(counties, input))
    })
  }
)
# Run app ----
shinyApp(ui, server)
