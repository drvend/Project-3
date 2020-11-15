#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# choices <- c("Player", "avgProjectedPoints", "avgActualPoints", "avgPassingAttempts", "avgRushingAttempts", "avgTargets")

# Define UI 
shinyUI(fluidPage(

    # Application title
    titlePanel("2019 Fantasy Football Data"),

    # Tabs 
  tabsetPanel(
    tabPanel("Information Page", 
             sidebarLayout(
               sidebarPanel("On other tabs you can use widgets on the sidebar to select variables and toggle between different variables, graphs and analysis options."), 
        mainPanel(h4("This data is from multiple data sets, all provided by"),
        h4(strong(("Fantasy Football Data Pros."))), h4("The links to download or access the data are here:"), a("Fantasy Football Pros Website", href="https://www.fantasyfootballdatapros.com/csv_files"), br(), a("Fantasy Football Pros Github Repository", href="https://github.com/fantasydatapros/data/blob/master/README.md"), br(),br(), "This data contains information on NFL football players and their Fantasy Football Scoring as well as ancillary variables for the 2019 season.", br(), br(), "You may navigate the app using the tabs above which contain exploratory data analysis, Clustering Analysis, Predictive Modeling, and a tab from which you can view and download the raw data. Options to subset data, select variables and other options are located onthe sidebar of each page.", 
        withMathJax(),
        helpText('Fantasy Points are normally calculated as $$1-\\frac{1}{2}$$')
        )
    )
),

tabPanel("Data Exploration",
         sidebarLayout(
             sidebarPanel(
               radioButtons("pos", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All")
              # numericInput("nI", "Select the Number of Digits for Rounding",                        min=0,max=3,value=0,step=1),
               
            #  varSelectInput("variables", "Variable:", choices, multiple = TRUE),
              ),
             mainPanel(
               dataTableOutput("summary"),
               plotOutput("heatmap"))
         )
         ),


tabPanel("Clustering Analysis",
         
         sidebarLayout(
           headerPanel('Fantasy Football Data k-means clustering'),
           sidebarPanel(
             selectInput('xcol', 'X Variable', names(scaledData)),
             selectInput('ycol', 'Y Variable', names(scaledData),
                         selected=names(scaledData)[[2]]),
             numericInput('clusters', 'Cluster count', 3,
                          min = 1, max = 9)
           ),
           mainPanel(
             plotOutput('plot1')
         
         
         ))),

tabPanel("Modeling"),

tabPanel("Data Table", sidebarLayout(
  sidebarPanel(
    radioButtons("pos2", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All")),
  mainPanel(dataTableOutput("rawData")))
)
)))
