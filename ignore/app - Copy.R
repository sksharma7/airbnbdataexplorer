setwd("F:\\RProjects\\jadeng177")

#Specify list of packages and install any missing packages and load
list.of.packages <- c("DT","ggplot2", "shiny", "gridExtra", "tidyverse", "shinydashboard"
                      , "plotly", "shinyalert", "readxl", "zoo", "dplyr", "tidyr", "data.table", "timeDate",
                      "bizdays", "writexl", "shinyjs", "purrr", "formattable", "shinycssloaders", "rmarkdown", 
                      "knitr", "leaflet")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

airbnb_Data <- readxl::read_excel("airbnb_data.xlsx" )
#------------------------------------------Ui -----------------------------------
ui <- dashboardPage(skin = "black",

dashboardHeader(title = "Airbnb",
            tags$li(a(href = 'https://www.airbnb.com', img(src = "airbnb.jpg", title = "Airbnb", height = "40px"),
                      style = "padding-top:2px; padding-bottom:2px;"), class = "dropdown"), disable = FALSE),

dashboardSidebar(

  #filter by neighbourhood
  selectInput("nhd","Select Neighbourhood", choices=unique(airbnb_Data$neighbourhood), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL),
  
  uiOutput("selYear"),
  
  uiOutput("selRoomType")
  
),

dashboardBody(
  
  tabsetPanel(type = "tabs",
      tabPanel("Plots",
        fluidRow(
          splitLayout(cellWidths = c("100%"), style='padding:5px;',
            plotlyOutput("ListingsPlot", width = "100%", height = "500px") %>% withSpinner()
          )),
        
        fluidRow(
          splitLayout(cellWidths = c("100%"), style='padding:5px;',
                      plotlyOutput("ListingsAvailPlot", width = "100%", height = "500px") %>% withSpinner()
          )),
        
        fluidRow(
          splitLayout(cellWidths = c("100%"), style='padding:5px;',
                      plotlyOutput("ListingsPerHost", width = "100%", height = "500px") %>% withSpinner()
          ))
      ),
      
      tabPanel("Map",
          leafletOutput("nhdMap", width = "100%", height = "500px") %>% withSpinner()
        )
      
      
  )
)

)

server <- function( input, output, session) {
  
  #Filter by neighbourhood selected
  getNHD <- reactive({
  
    nhd <- input$nhd
    
    if (length(nhd)>0){
      sel_nhd <- dplyr::filter(airbnb_Data, neighbourhood %in% nhd)
    }else{
      sel_nhd <- airbnb_Data
    }

    return(sel_nhd)
  })
  
  output$selYear <- renderUI({
    sel_nhd <- getNHD()
    selectInput("Year","Select Year", choices=unique(sel_nhd$Year), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
  })
  
  output$selRoomType <- renderUI({
    sel_nhd <- getNHD()
    selectInput("RoomType","Select Room Type", choices=unique(sel_nhd$room_type), selected = NULL, multiple = FALSE,selectize = TRUE, width = NULL, size = NULL)
  })
  
  
  #ListingsPlot
  output$ListingsPlot <- renderPlotly({
    
    sel_nhd <- getNHD()
    
    ListingDF = sel_nhd %>% 
      group_by(Year, room_type) %>% 
      summarise(TotalListings = sum(calculated_host_listings_count))
  
    roomtypeUV = data.frame(
      "RoomType" = unique(ListingDF$room_type)
    )
    
    for (i in 1:nrow(roomtypeUV)){
      
      traceData = subset(ListingDF, room_type == roomtypeUV$RoomType[i])
      
      if(i==1){
        p <- plot_ly(y=traceData$TotalListings, x=as.factor(traceData$Year), type="scatter", 
                mode="markers+lines", name =roomtypeUV$RoomType[i] ,  fill = 'tozeroy')
      }else(
        p <- add_trace(p, y=traceData$TotalListings, x=as.factor(traceData$Year) , type="scatter", 
                mode="markers+lines", name =roomtypeUV$RoomType[i] , fill = 'tozeroy')
      )
      
    }
  
    
    p <- p %>% 
      layout(
          title = paste0("Total Listings Yearwise for ", input$nhd),
          xaxis = list(title = 'Year'),
          yaxis = list(title = 'Total Listings')
             )
    
    return(p)  
  
  })
  
  
  #Listings Avail Plot
  output$ListingsAvailPlot <- renderPlotly({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    ListingDF = df %>% 
      group_by(availability_365) %>% 
      summarise(TotalListings = sum(calculated_host_listings_count))
    
    #print(ListingDF)
    
    colfunc <- colorRampPalette(c("blue", "red"))
    pal <- colfunc(max(ListingDF$TotalListings))[ListingDF$TotalListings]
    
    p <- plot_ly(data = ListingDF,
      x = as.factor(ListingDF$availability_365),
      y = ListingDF$TotalListings,
      name = "SF Zoo",
      marker = list(color =  pal), 
      type = "bar")
    
    p <- p %>% 
      layout(
        title = paste0("Availability for ", input$Year, ". Room Type: ", input$RoomType, ". Neighbourhood: ", input$nhd),
        xaxis = list(title = 'Available Days in Year'),
        yaxis = list(title = 'Listing')
      )
    
    return(p)  
    
  })
  
  
  #Listings Per Host
  output$ListingsPerHost <- renderPlotly({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
  
    colfunc <- colorRampPalette(c("blue", "red"))
    pal <- colfunc(max(df$calculated_host_listings_count))[df$calculated_host_listings_count]
    
    p <- plot_ly(data = ListingDF,
                 x = as.factor(ListingDF$calculated_host_listings_count),
                 y = ListingDF$TotalListings,
                 name = "SF Zoo",
                 marker = list(color =  pal),
                 type = "bar")

    
    p <- p %>% 
      layout(
        title = paste0("Listings Per Host in ", input$Year, ". Room Type: ", input$RoomType, ". Neighbourhood: ", input$nhd),
        xaxis = list(title = 'Listings per host'),
        yaxis = list(title = 'Listing')
      )
    
    return(p)  
    
  })
  
  #Customer Map
  output$nhdMap <- renderLeaflet({
  
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = sel_nhd
    
    quakeIcons <- iconList(blue = makeIcon("blue.png", iconWidth = 24, iconHeight =32),
                           green = makeIcon("green.png", iconWidth = 24, iconHeight =32),
                           orange = makeIcon("orange.png", iconWidth = 24, iconHeight =32))
    
    df$color = ifelse(df$room_type == "Private room", "blue" , 
                      ifelse(df$room_type == "Entire home/apt", "orange", "green"))
    
    map <- leaflet(data = df) %>% 
      addTiles() %>%
      addMarkers(~longitude,~latitude, icon = ~quakeIcons[color] , label = ~as.character(df$host_name),
                 popup = ~as.character(paste(df$host_name, df$neighbourhood, df$room_type,  sep = "<br/>"))
                 )
    
  })
  
  
}

shinyApp(ui, server)

