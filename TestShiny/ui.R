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
                          label = "Click me")
           ),
           
           mainPanel(plotOutput("distPlot1")
           )
  )
)