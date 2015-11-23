#ui.R 

library(shiny)

navbarPage(
  title = "Elements of Visualization",
  tabPanel(title = "Crosstab",
           sidebarPanel(
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark"),
             sliderInput("KPI1", "KPI_Low_Max_value:", 
                         min = 1, max = 2600,  value = 80),
             textInput(inputId = "title", 
                       label = "Crosstab Title",
                       value = "Damage KPI by State and Type"),
             actionButton(inputId = "clicks1",  label = "Reset")
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
  )
)
