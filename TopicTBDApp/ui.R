#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("2019 Fantasy Football Data"),

    # Sidebar with a slider input for number of bins
  tabsetPanel(
    tabPanel("Information Page", 
             sidebarLayout(
        sidebarPanel(),
        mainPanel()
    )
),

tabPanel("DataExploration",
         sidebarLayout(
             sidebarPanel(),
             mainPanel(
                 plotOutput("output$heatmap"))
         )
         ),

tabPanel("Clustering or Principal Components Analysis", ),

tabPanel("Modeling",),

tabPanel("Data Table",)
)
))
