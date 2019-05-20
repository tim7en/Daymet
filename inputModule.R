library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ncdf4)
library(leaflet)
library(ncdf4.helpers)
library(shinyWidgets)
source("shared.R")

ui <- dashboardPage(
  dashboardHeader(title = "ncdf4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "input",
        sidebarLayout(
          sidebarPanel(
            uiOutput("varDim"),
            dateInput("date1", "Date:", value = "1980-01-01"),
            dateInput("date2", "Date:", value = "1980-09-01"),
            selectInput("DaymetVar", "Select variable", choices = c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp"), selected = "prcp"),
            actionButton("Getncdf4", "Get DAYMET files"),
            verbatimTextOutput("getBBX")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel(
                title = "Map",
                shinyWidgets::progressBar(id = "pb", value = 0, display_pct = TRUE),
                leafletOutput("map", width = "100%", height = 800)
              ),
              tabPanel(
                title = "File info",
                verbatimTextOutput("details")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 3000 * 1024^2)

  contents <- reactive({
    ncin <- nc_open(mypath())
  })

  mypath <- eventReactive(input$myaction, {
    file.choose()
  })
  
  mydir <- eventReactive (input$Getncdf4, {
    choose.dir()
  })

  output$details <- renderPrint(
    if (!is.null(contents())) {
      x <- contents()
      print(x)
    }
  )

  output$varListx <- renderUI({
    req(contents())
    myvars <- ncdf4.helpers::nc.get.variable.list(contents())
    selectInput("myVars", "Variable", choices = myvars, selected = myvars[2])
  })

  output$varListy <- renderUI({
    req(contents())
    myvars <- ncdf4.helpers::nc.get.variable.list(contents())
    if (length(myvars) < 2) {
      myvars <- c(NULL, NULL)
    }
    selectInput("myVars2", "Time dimension", choices = myvars, selected = myvars[1])
  })

  output$varDim <- renderUI({
    req(contents())
    req(input$myVars2)
    mydim <- ncdf4::ncvar_get(contents(), input$myVars2)
    selectInput("myDim", "Dimension unit", choices = mydim, selected = mydim[1])
  })

  map <- reactive({
    m <- leaflet() %>%
      setView(lng = -76.651722, lat = 39.298497, zoom = 09) %>%
      addTiles()
    m
  })

  output$map <- renderLeaflet({
    m <- map()
    if (!is.null(Markers$lat)) {
      m %>% addMarkers(lng = Markers$lng, lat = Markers$lat)
    } else {
      m
    }
  })

  Markers <- reactiveValues()
  Markers$lat <- NULL
  Markers$lng <- NULL

  observeEvent(input$map_click, {
    Markers$lat <- c(Markers$lat, input$map_click$lat)
    Markers$lng <- c(Markers$lng, input$map_click$lng)
  })

  output$getBBX <- renderPrint({
    input$map_bounds
  })

  observeEvent(input$Getncdf4, {
    Y <- getYear(input$date1) %>% as.numeric()
    Y2 <- getYear(input$date2) %>% as.numeric()
    dif <- Y2 - Y
    setwd (paste0(mydir()))
    
    if (dif < 0) {
      return(NULL)
    } else if (dif > 0) {
      date1 <- input$date1
      date2 <- input$date2
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0)
      
      for (i in seq(0, dif)) {
        url <- getNcdf4(date1, date2, input$DaymetVar, input$map_bounds$north, input$map_bounds$west, input$map_bounds$east, input$map_bounds$south)
        download.file (url, paste0(date1,input$DaymetVar, '.nc'), mode = "wb", quiet = TRUE)
        date1 <- sub(Y, Y + 1, date1)
        Y <- Y+1
        shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100/dif*i)
      }
      
    } else {
      url <- getNcdf4(input$date1, input$date2, input$DaymetVar, input$map_bounds$north, input$map_bounds$west, input$map_bounds$east, input$map_bounds$south)
      download.file (url, paste0(date1,input$DaymetVar, '.nc'), mode = "wb", quiet = TRUE)
    }
  })
}

shinyApp(ui, server)