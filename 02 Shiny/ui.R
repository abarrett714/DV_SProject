#ui.R 

library(shiny)

navbarPage(
  title = "Storm Event Shiny App",
  tabPanel(title = "Crosstab",
           sidebarPanel(
             sliderInput("KPI1", "KPI Cutoff:", 
                         min = 1, max = 2600,  value = 80),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Damage KPI by State and Type"),
             actionButton(inputId = "clicks", 
                          label = "Generate Plot")
           ),
           
           mainPanel(plotOutput("distPlot1")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             actionButton(inputId = "clicks2",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot2")
           )
  ),
  tabPanel(title = "Blending 2 Data Sources",
           sidebarPanel(
             actionButton(inputId = "clicks3",  label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot3")
           )        
  ),
  tabPanel(title = "Scatter Plot",
           sidebarPanel(
             actionButton(inputId = "clicks4", label = "Click me")
           ),
           mainPanel(plotOutput("distPlot4")
           )
  )
)

