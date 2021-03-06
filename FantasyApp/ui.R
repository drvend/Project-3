#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
        h4(strong(("Fantasy Football Data Pros."))), h4("The links to download or access the data are here:"), a("Fantasy Football Pros Website", href="https://www.fantasyfootballdatapros.com/csv_files"), br(), a("Fantasy Football Pros Github Repository", href="https://github.com/fantasydatapros/data/blob/master/README.md"), br(),br(), "This data contains information on NFL football players and their Fantasy Football Scoring as well as ancillary variables for the 2019 season.", br(), br(), "You may navigate the app using the tabs above which contain exploratory data analysis, Clustering Analysis, Predictive Modeling, and a tab from which you can view and download the raw data. Options to subset data, select variables and other options are located on the sidebar of each page."
        
        )
    )
),

tabPanel("Data Exploration",
         sidebarLayout(
             sidebarPanel(
               radioButtons("pos", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All"),
               selectInput('var1', 'Summary Variable 1' , c( "avgProjectedPoints", "avgActualPoints")),
                selectInput('var2', 'Summary Variable 2', c("avgPassingAttempts", "avgRushingAttempts", "avgTargets", "avgCompletions")),
               
              # textInput('filename', "Filename"),
               
               downloadButton("downloadPlot", 'Download Plot')
               
               # selectInput('xcolscat', 'X Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints") 
               # ),
               # selectInput('ycolscat', 'Y Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints")),
               
              ),
             mainPanel(h5("This page provides a table with summary data as well as a heatmap that shows the correlations between the variables of the data set. The Summary Variable 2 will change as the inputted position changes which is one of my dynamic UI elements."),
               dataTableOutput("summary"),
               plotOutput("heatmap"),
               "Select the portion of the plot you would like to zoom in on by clicking and dragging the box. Then double click to zoom.",
              
               fluidRow(
                 column(width = 10, class = "well",
                        h4("Completions vs. Projected Fantasy Points Scatterplot"),
                        plotOutput("plot3", height = 300, width = 500,
                                   dblclick = "plot3_dblclick",
                                   brush = brushOpts(
                                     id = "plot3_brush",
                                     resetOnNew = TRUE
                                   )
                        )
                 ),
               )
             )
               
              )
         ),


tabPanel("Clustering", 
  pageWithSidebar(
          headerPanel('Fantasy Football Data k-means clustering'),
          sidebarPanel(
           
             selectInput('xcol', 'X Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints") 
                                                 ),
             selectInput('ycol', 'Y Variable', c("Week", "Team", "Player", "Slot", "Pos.x", "Status", "Proj", "Actual", "Pos.y", "Tm", "PassingYds", "PassingTD", "Int", "PassingAtt", "Cmp", "RushingAtt", "RushingYds", "RushingTD", "Rec", "Tgt", "ReceivingYds", "ReceivingTD", "FL", "PPRFantasyPoints", "StandardFantasyPoints", "HalfPPRFantasyPoints"),
                         selected="Proj"),
             numericInput('clusters', 'Cluster count', 3,
                          min = 1, max = 9)
           ),
          
           mainPanel(
             h5("This page has the functionality to visualize cluster analysis on the data and provdes a dendrogram for heirarchical clustering analysis. You can change the variables and the # of clusters using the sidebarpanel."),
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
    selectInput('response', 'Response Variable' , c( "Proj", "Actual")), 
    selectInput('predictor', 'Predictor Variable', c("PassingAtt", "RushingAtt", "Tgt", "Cmp")),
    numericInput('predictorvalue', 'Predictor Value', 20, min =0, max = 100),
    numericInput("cvval", "Cross Validation folds input for Random Forest Model", 5, min=1, max=10)
   ), 
    mainPanel( h5("This page contains output from training a linear and a random forest model on the data. You can use the sidebar panel to select the predictor and response variables, as well as enter a value to return a prediction from the linear model."), "Linear Regression Model Predition Values", verbatimTextOutput("lmFit"),"Linear Regression Model Summary Statistics", verbatimTextOutput("lmsumm"),
              "Linear Regression Model prediction of User Selected Numeric Input", verbatimTextOutput("lmFit2"),
              "Random Forest Model Prediction Values Predicting Actual from Completions", verbatimTextOutput("rfFit1")
               , "Random Forest Model Summary Statistics", verbatimTextOutput("rfsumm")
              )
)),

tabPanel("Data Table", sidebarLayout(
  sidebarPanel(
    radioButtons("pos2", "Position", choices = c("All", "QB", "WR", "RB", "TE"), selected = "All")),
  mainPanel("You can subset and download the raw data from this tab.", dataTableOutput("rawData")))
)
)))
