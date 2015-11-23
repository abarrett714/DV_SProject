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
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  df1 <-  data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STORMEVENTS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_apb766', PASS='orcl_apb766', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))%>% select(., EVENT_TYPE, STATE, DAMAGE_CROPS, DAMAGE_PROPERTY, BEGIN_TIME, END_TIME) %>% mutate(DAMAGE_CROPS = strtoi(DAMAGE_CROPS), DAMAGE_PROPERTY = strtoi(DAMAGE_PROPERTY)) %>% filter(., DAMAGE_CROPS > 1 | DAMAGE_PROPERTY > 1) %>% filter(., DAMAGE_CROPS != "null" & DAMAGE_PROPERTY != "null") %>% mutate(., DAMAGE_KPI = (as.numeric(DAMAGE_CROPS) + as.numeric(DAMAGE_PROPERTY)) / (as.numeric(END_TIME) - as.numeric(BEGIN_TIME))) %>% filter(DAMAGE_KPI != Inf) %>% arrange(STATE) %>% distinct() %>% group_by(., EVENT_TYPE, STATE) %>% mutate(KPI = cumsum(DAMAGE_KPI)) %>% mutate(MAX = max(KPI)) %>% select(EVENT_TYPE, STATE, MAX) %>% distinct()
  
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      scale_fill_gradient2(low="white", mid = "red", high= "darkred", midpoint = 7500) +
      labs(title=input$title) +
      labs(x=paste("Disaster"), y=paste("State")) +
      layer(data=df1, 
            mapping=aes(x=EVENT_TYPE, y=STATE, fill = MAX), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(), 
            position=position_identity()
      )
    observeEvent(input$clicks, { plot})
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  # Begin code for Second Tab:
  
  df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "select color, clarity, avg_price, avg(avg_price) 
                                                                                 OVER (PARTITION BY clarity ) as window_avg_price
                                                                                 from (select color, clarity, avg(price) avg_price
                                                                                 from diamonds
                                                                                 group by color, clarity)
                                                                                 order by clarity;"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot2 <- renderPlot(height=1000, width=2000, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~CLARITY, ncol=1) +
      labs(title='Diamonds Barchart\nAVERAGE_PRICE, WINDOW_AVG_PRICE, ') +
      labs(x=paste("COLOR"), y=paste("AVG_PRICE")) +
      layer(data=df2(), 
            mapping=aes(x=COLOR, y=AVG_PRICE), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df2(), 
            mapping=aes(x=COLOR, y=AVG_PRICE, label=round(AVG_PRICE - WINDOW_AVG_PRICE)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-1), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(yintercept = WINDOW_AVG_PRICE), 
            geom="hline",
            geom_params=list(colour="red")
      )
    plot1
  })
  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 """select region || \\\' \\\' || \\\'Sales\\\' as measure_names, sum(sales) as measure_values from SUPERSTORE_SALES_ORDERS
                                                                                 where country_region = \\\'United States of America\\\'
                                                                                 group by region
                                                                                 union all
                                                                                 select market || \\\' \\\' || \\\'Coffee_Sales\\\' as measure_names, sum(coffee_sales) as measure_values from COFFEE_CHAIN
                                                                                 group by market
                                                                                 order by 1;"""
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
  })
  
  output$distPlot3 <- renderPlot(height=1000, width=2000, {
    plot3 <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      #facet_wrap(~CLARITY, ncol=1) +
      labs(title='Blending 2 Data Sources') +
      labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(colour="blue"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df3(), 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      )
    plot3
  })
})
