# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1}) 
  
  df1 <- eventReactive(input$clicks, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_apb766', PASS='orcl_apb766', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))  %>% select(., EVENT_TYPE, STATE, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME) %>% mutate(DAMAGE_CROPS = strtoi(DAMAGE_CROPS), DAMAGE_PROPERTY = strtoi(DAMAGE_PROPERTY)) %>% filter(., DAMAGE_CROPS > 1 | DAMAGE_PROPERTY > 1) %>% filter(., DAMAGE_CROPS != "null" & DAMAGE_PROPERTY != "null") %>% mutate(., DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (as.numeric(END_TIME) - as.numeric(BEGIN_TIME))) %>% filter(DAMAGE_KPI != Inf) %>% arrange(STATE) %>% distinct() %>% group_by(., EVENT_TYPE, STATE) %>% mutate(KPI = cumsum(DAMAGE_KPI)) %>% mutate(MAX = max(KPI)) %>% select(EVENT_TYPE, STATE, MAX) %>% distinct() %>% filter(MAX > KPI_Low_Max_value()) })
    
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      scale_fill_gradient2(low="white", mid = "red", high= "darkred", midpoint = 7500) +
      labs(title=input$title) +
      labs(x=paste("Disaster"), y=paste("State")) +
      layer(data=df1(), 
            mapping=aes(x=EVENT_TYPE, y=STATE, fill = MAX), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(), 
            position=position_identity()
      )
    
    plot
  })
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
})