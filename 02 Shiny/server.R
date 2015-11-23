# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(gridExtra)

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
  
  # Begin code for Second Tab:
  dfs <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) %>% select(BEGIN_YEARMONTH, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, BEGIN_DAY) %>% filter(DEATHS_DIRECT != 0)})
  #dfs1 <- select(dfs, BEGIN_YEARMONTH, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, BEGIN_DAY) %>% filter(DEATHS_DIRECT != 0)
  #df2 <- eventReactive(input$clicks2, {dfs1})
  
  output$distPlot2 <- renderPlot(height=1000, width=2000, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      facet_wrap(~BEGIN_DAY, ncol = 1) +
      labs(title='StormEvents Barchart\ndeaths_direct, avg(deaths_direct), ') +
      labs(x=paste("Begin Day"), y=paste("Deaths Direct")) +
      layer(data=dfs(), 
            mapping=aes(x=BEGIN_DAY, y=(DEATHS_DIRECT)), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=dfs(), 
            mapping=aes(x=BEGIN_DAY, y=DEATHS_DIRECT, label=(DEATHS_DIRECT)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=0.5), 
            position=position_identity()
      ) +
      layer(data=dfs(), 
            mapping=aes(yintercept = mean(DEATHS_DIRECT)), 
            geom="hline",
            geom_params=list(colour="red")
      )
    plot1
  })
  
  # Begin code for Third Tab:
  
  df4 <- eventReactive(input$clicks4, {data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select EVENT_TYPE, DAMAGE_CROPS, DAMAGE_PROPERTY, END_TIME, BEGIN_TIME from STORMEVENTS where DAMAGE_CROPS is not null and DAMAGE_PROPERTY is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jdg3666', PASS='orcl_jdg3666', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))  %>% select(EVENT_TYPE, DAMAGE_CROPS, DAMAGE_PROPERTY, END_TIME, BEGIN_TIME) %>% filter(DAMAGE_PROPERTY > 0, DAMAGE_CROPS > 0) %>% mutate (TOTAL_TIME = (abs(END_TIME - BEGIN_TIME)) / 100) %>% filter(TOTAL_TIME > 0) })
  
  output$distPlot4 <- renderPlot({ 
    crop <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='Crop and Property Damage across Disasters less than 24 hours') +
      labs(x="Total Time of Disaster", y=paste("Crop Damage")) +
      layer(data=df4(), 
            mapping=aes(as.numeric(TOTAL_TIME), y=as.numeric(as.character(DAMAGE_CROPS)), color=EVENT_TYPE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    
    property <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='Crop and Property Damage across Disasters less than 24 hours') +
      labs(x="Total Time of Disaster", y=paste("Crop Damage")) +
      layer(data=df4(), 
            mapping=aes(as.numeric(TOTAL_TIME), y=as.numeric(as.character(DAMAGE_PROPERTY)), color=EVENT_TYPE), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            #position=position_identity()
            position=position_jitter(width=0.3, height=0)
      )
    plot4 <- grid.arrange(crop, property)
    plot4
  })
})
