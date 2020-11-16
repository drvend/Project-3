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
        h4(strong(("Fantasy Football Data Pros."))), h4("The links to download or access the data are here:"), a("Fantasy Football Pros Website", href="https://www.fantasyfootballdatapros.com/csv_files"), br(), a("Fantasy Football Pros Github Repository", href="https://github.com/fantasydatapros/data/blob/master/README.md"), br(),br(), "This data contains information on NFL football players and their Fantasy Football Scoring as well as ancillary variables for the 2019 season.", br(), br(), "You may navigate the app using the tabs above which contain exploratory data analysis, Clustering Analysis, Predictive Modeling, and a tab from which you can view and download the raw data. Options to subset data, select variables and other options are located onthe sidebar of each page."
        
        )
    )
),

tabPanel("Data Exploration",
         sidebarLayout(
             sidebarPanel(
               radioButtons("pos", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All"),
               selectInput('var1', 'Summary Variable 1' , c( "avgProjectedPoints", "avgActualPoints")),
                selectInput('var2', 'Summary Variable 2', c("avgPassingAttempts", "avgRushingAttempts", "avgTargets", "avgCompletions")) 
              # numericInput("nI", "Select the Number of Digits for Rounding",                        min=0,max=3,value=0,step=1),
               
            #  varSelectInput("variables", "Variable:", choices, multiple = TRUE),
              ),
             mainPanel(
               dataTableOutput("summary"),
               plotOutput("heatmap"))
         )
         ),


tabPanel("Clustering", 
  pageWithSidebar(
          headerPanel('Fantasy Football Data k-means clustering'),
          sidebarPanel(
           # radioButtons("pos5", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All"),
             selectInput('xcol', 'X Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints") 
                                                 ),
             selectInput('ycol', 'Y Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints"),
                         selected="Proj"),
             numericInput('clusters', 'Cluster count', 3,
                          min = 1, max = 9)
           ),
          
           mainPanel(
             plotOutput('plot1'),
             plotOutput('plot2'), h4("Details of K means Clustering"), "Look at within cluster variation. For kth cluster, sum all pairwise squared euclidean distances between the observations in the kth cluster. Divide by number of observations.", withMathJax(),
             helpText('$$\\frac{1}{\\# of obs in cluster}$$')
             #plotOutput("plot1", click = "plot_click"),
             #verbatimTextOutput("info")
             
         ))
         ),

tabPanel("Modeling",
sidebarLayout(
  sidebarPanel(
    selectInput('response', 'Response Variable' , c( "avgProjectedPoints", "avgActualPoints")), 
    selectInput('predictor', 'Predictor Variable', c("avgPassingAttempts", "avgRushingAttempts", "avgTargets", "avgCompletions"))), 
    mainPanel(verbatimTextOutput("lmFit"), 
              verbatimTextOutput("rfFit"))
)),

tabPanel("Data Table", sidebarLayout(
  sidebarPanel(
    radioButtons("pos2", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All")),
  mainPanel(dataTableOutput("rawData")))
)
)))
