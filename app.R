setwd("F:\\RProjects\\jadeng177")

#Specify list of packages and install any missing packages and load
list.of.packages <- c("DT", "shiny", "tidyverse", "googlesheets"
                      , "plotly", "readxl", "zoo", "dplyr", "tidyr", "data.table"
                      , "leaflet")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

airbnb_Data <- readxl::read_excel("airbnb_data.xlsx")

ui <- fluidPage(
  
  sidebarLayout(position="left", fluid=TRUE,
                
           
                sidebarPanel(
                  
                  selectInput("nhd","Select Neighbourhood", choices=unique(airbnb_Data$neighbourhood), selected = NULL, 
                              multiple = FALSE,selectize = TRUE, width = NULL, size = NULL),
                  uiOutput("selYear"),
                  uiOutput("selRoomType"),
                  
                  plotlyOutput("ListingsPlot", height = 200),
                  br(),
                  plotlyOutput("ListingsAvailPlot", height = 200)
                  
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  
                  leafletOutput("nhdMap",width = "100%", height = "350px"), #, width = "100%", height = "100%") %>% withSpinner()
                  
                  fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), style='padding:5px;',
                              
                    plotlyOutput("AvailPiePlot", height = 200), 
                    plotlyOutput("ListingsPerHost", height = 200) 
                    
                    )
                  ),
                  
                  wellPanel(style = "background: white",
                            textOutput("totalHighAvail"), 
                            textOutput("totalLowAvail")
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
        #title = paste0(input$nhd),
        xaxis = list(tickangle = 90),
        yaxis = list(title = 'Total Listings'), 
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0, y=1.4)
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
        title = paste0(input$RoomType , "-", input$Year ),
        xaxis = list(title = 'Available Days in Year'),
        yaxis = list(title = 'Listing')
      )
    
    return(p)  
    
  })
  
  
  #Listings Avail Pie Plot
  output$AvailPiePlot <- renderPlotly({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    ListingDF = df %>% 
      group_by(availability_365) %>% 
      summarise(TotalListings = sum(calculated_host_listings_count))
    
    highAvail = subset(ListingDF, availability_365 >= 90)[,2]
    lowAvail = subset(ListingDF, availability_365 < 90)[,2]
    
    if (nrow(highAvail)==0){
      highAvail = 0
    }else{
      highAvail = sum(subset(ListingDF, availability_365 >= 90)[,2])
    }
    
    if (nrow(lowAvail)==0){
      lowAvail = 0
    }else{
      lowAvail = sum(subset(ListingDF, availability_365 < 90)[,2])
    }
    
    highAvailP = (highAvail/(highAvail+lowAvail))*100
    lowAvailP = (lowAvail/(highAvail+lowAvail))*100
    
    p <- plot_ly( labels =c("Low: <90 days", "High: >=90 days") , values = c(lowAvailP, highAvailP), type = 'pie') %>%
      layout(#title = "Availability Distribution",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(p)  
    
  })
  
  
  #Listings High Avail
  output$totalHighAvail <- renderText({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    ListingDF = df %>% 
      group_by(availability_365) %>% 
      summarise(TotalListings = sum(calculated_host_listings_count))
    
    
    highAvail = subset(ListingDF, availability_365 >= 90)[,2]
    lowAvail = subset(ListingDF, availability_365 < 90)[,2]
    
    if (nrow(highAvail)==0){
      highAvail = 0
    }else{
      highAvail = sum(subset(ListingDF, availability_365 >= 90)[,2])
    }
    
    if (nrow(lowAvail)==0){
      lowAvail = 0
    }else{
      lowAvail = sum(subset(ListingDF, availability_365 < 90)[,2])
    }
    
    highAvailP = (highAvail/(highAvail+lowAvail))*100
    lowAvailP = (lowAvail/(highAvail+lowAvail))*100
    
    p <- paste0("Total Listings with high availability: ", highAvail)
    return(p)  
    
  })
  
  #Listings  Low Avail 
  output$totalLowAvail <- renderText({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    ListingDF = df %>% 
      group_by(availability_365) %>% 
      summarise(TotalListings = sum(calculated_host_listings_count))
    
    
    highAvail = subset(ListingDF, availability_365 >= 90)[,2]
    lowAvail = subset(ListingDF, availability_365 < 90)[,2]
    
    if (nrow(highAvail)==0){
      highAvail = 0
    }else{
      highAvail = sum(subset(ListingDF, availability_365 >= 90)[,2])
    }
    
    if (nrow(lowAvail)==0){
      lowAvail = 0
    }else{
      lowAvail = sum(subset(ListingDF, availability_365 < 90)[,2])
    }
    
    highAvailP = (highAvail/(highAvail+lowAvail))*100
    lowAvailP = (lowAvail/(highAvail+lowAvail))*100
    
    
    p <- paste0("Total Listings with low availability: ", lowAvail)
    return(p)  
    
  })
  
  #Listings Per Host
  output$ListingsPerHost <- renderPlotly({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    colfunc <- colorRampPalette(c("blue", "red"))
    pal <- colfunc(max(df$calculated_host_listings_count))[df$calculated_host_listings_count]
    
    p <- plot_ly(x = as.factor(df$calculated_host_listings_count), 
                 name = "SF Zoo",
                 marker = list(color =  pal), 
                 type = "histogram")
    
    p <- p %>% 
      layout(
        title = paste0(input$RoomType , "-", input$Year ),
        xaxis = list(title = 'Listings per host'),
        yaxis = list(title = 'Listing')
      )
    
    return(p)  
    
  })
  
  #Customer Map
  output$nhdMap <- renderLeaflet({
    
    req(input$nhd, input$Year)
    
    sel_nhd <- getNHD()
    df = subset(sel_nhd, Year == input$Year & neighbourhood ==  input$nhd  &  room_type == input$RoomType)
    
    
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

