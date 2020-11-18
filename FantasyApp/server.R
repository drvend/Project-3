#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Packages required to run this coded are listed below
library(shiny)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(caret)
library(randomForest)
library(DT)

# Server
shinyServer(function(input, output, session) {
    # # Download data from the web if needed 
    # FFdataw1 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week1.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw1 <- mutate(FFdataw1, Week = 1)
    # 
    # FFdataw2 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week2.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw2 <- mutate(FFdataw2, Week = 2)
    # 
    # FFdataw3 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week3.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw3 <- mutate(FFdataw3, Week = 3)
    # 
    # FFdataw4 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week4.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw4 <- mutate(FFdataw4, Week = 4)
    # 
    # FFdataw5 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week5.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw5 <- mutate(FFdataw5, Week = 5)
    # 
    # FFdataw6 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week6.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw6 <- mutate(FFdataw6, Week = 6)
    # 
    # FFdataw7 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week7.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw7 <- mutate(FFdataw7, Week = 7)
    # 
    # FFdataw8 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week8.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw8 <- mutate(FFdataw8, Week = 8)
    # 
    # FFdataw9 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week9.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw9 <- mutate(FFdataw9, Week = 9)
    # 
    # FFdataw10 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week10.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw10 <- mutate(FFdataw10, Week = 10)
    # 
    # FFdataw11 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week11.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw11 <- mutate(FFdataw11, Week = 11)
    # 
    # FFdataw12 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week12.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw12 <- mutate(FFdataw12, Week = 12)
    # 
    # FFdataw13 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week13.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw13 <- mutate(FFdataw13, Week = 13)
    # 
    # FFdataw14 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week14.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw14 <- mutate(FFdataw14, Week = 14)
    # 
    # FFdataw15 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week15.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw15 <- mutate(FFdataw15, Week = 15)
    # 
    # FFdataw16 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week16.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw16 <- mutate(FFdataw16, Week = 16)
    # 
    # FFdataw17 <- read.csv('https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/2019/week17.csv', header= TRUE, stringsAsFactors = TRUE)
    # FFdataw17 <- mutate(FFdataw17, Week = 17)
  
  # read in data set for prediction data here 
    # 
     Predictiondata <- read.csv('C:/Users/drven/Documents/ST 558/Project-3/2019projections.csv')
    # 
    # FFdatatotal <- rbind(FFdataw1, FFdataw2, FFdataw3, FFdataw4, FFdataw5, FFdataw6, FFdataw7, FFdataw8, FFdataw9, FFdataw10, FFdataw11, FFdataw12, FFdataw13, FFdataw14, FFdataw15, FFdataw16, FFdataw17)
    # 
    # saveRDS(FFdatatotal, "FFdatatotal.RDS")
    
    
    
    ########
    
    # Read in data set downloaded from the web here 
     
    FFdatatotal = readRDS("C:/Users/drven/Documents/ST 558/Project-3/FFdatatotal.RDS")
    
    # Join data sets 
    fantasyData <- as.data.frame(left_join(Predictiondata, FFdatatotal, by = c("Player", "Week")))
    
    # Replace na data with 0 
     fantasyData[is.na(fantasyData)] <- 0 
     
     # replace "hb" with "rb" 
     fantasyData$Pos.y[fantasyData$Pos.y == 'HB'] <- 'RB'
    

        # Create Correlation heatmap
        
        fantasyDataNumeric <- fantasyData

        fantasyDataNumeric$Player <- as.numeric(as.factor(fantasyData$Player))
        fantasyDataNumeric$Pos.x <- as.numeric(as.factor(fantasyData$Pos.x))
        fantasyDataNumeric$Status <- as.numeric(as.factor(fantasyData$Status))
        fantasyDataNumeric$Pos.y <- as.numeric(as.factor(fantasyData$Pos.y))
        fantasyDataNumeric$Tm <- as.numeric(as.factor(fantasyData$Tm))
        
        cormat <- round(cor(fantasyDataNumeric, use = "complete.obs"),2)
        
        # Create Correlation heatmap
        
        melted_cor <- melt(cormat)
        
        # filter data for Var1 only = StandardFantasyPoints, PPRFantasyPoints, HalfPPRFantasyPoints
        #Subset only for Shares 
      #  melted_cor <- filter(melted_cor, melted_cor$Var2 == "Actual")
        
        
melted_cor <- filter(melted_cor, Var1 %in% c("StandardFantasyPoints", "PPRFantasyPoints", "HalfPPRFantasyPoints", "Proj", "Actual"))
        
        ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
            geom_tile() + 
            theme(axis.text.x=element_text(angle=90,hjust=1),legend.position = "none")
        
        # Get lower triangle of the correlation matrix
        get_lower_tri<-function(cormat){
            cormat[upper.tri(cormat)] <- NA
            return(cormat)
        }
        # Get upper triangle of the correlation matrix
        get_upper_tri <- function(cormat){
            cormat[lower.tri(cormat)]<- NA
            return(cormat)
        }
        
        upper_tri <- get_upper_tri(cormat)
        
        melted_cor <- melt(upper_tri, na.rm = TRUE)
        
        # Heatmap
        ggplot(data = melted_cor, aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 10, hjust = 1))+
            coord_fixed()
        
        reorder_cormat <- function(cormat){
            # Use correlation between variables as distance
            dd <- as.dist((1-cormat)/2)
            hc <- hclust(dd)
            cormat <-cormat[hc$order, hc$order]
        }
        
        # Reorder the correlation matrix
        cormat <- reorder_cormat(cormat)
        upper_tri <- get_upper_tri(cormat)
        # Melt the correlation matrix
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        # Create a ggheatmap
        ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ # minimal theme
            theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                             size = 10, hjust = 1))+
            coord_fixed()
        
        # Print the heatmap
        print(ggheatmap)
        
        ggheatmap <- ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 1) +theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.6, 0.7),
                legend.direction = "horizontal")+
            guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                         title.position = "top", title.hjust = 0.5))
        
        print(ggheatmap)
        
        #Output Heatmap 

        output$heatmap <- renderPlot({ggheatmap})
        
        # Creating Summary Data Sets 
        
        output$summary <- renderDataTable({
        
        sALL <- fantasyData  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp), avgTargets = mean(Tgt)) %>%  dplyr::select(Player, input$var1, input$var2)
        
      #  sALL <- round(sALL[2:5], input$nI)
        
        
        sQB <- filter(fantasyData, Pos.y == "QB", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp), avgTargets = mean(Tgt))%>%  dplyr::select(Player, input$var1, input$var2)
        
    #    sQB <- round(sQB[2:5], input$nI)
        
        sWR <- filter(fantasyData, Pos.y == "WR", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp), avgTargets = mean(Tgt)) %>%  dplyr::select(Player, input$var1, input$var2)
        
     #   sWR <- round(sWR[2:5], input$nI)
        
        sRB <- filter(fantasyData, Pos.y == "RB", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp), avgTargets = mean(Tgt)) %>%  dplyr::select(Player, input$var1, input$var2)
        
     #   sRB <- round(sRB[2:5], input$nI)
        
        sTE <- filter(fantasyData, Pos.y == "TE", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp), avgTargets = mean(Tgt)) %>%  dplyr::select(Player, input$var1, input$var2)
        
     #   sTE <- round(sTE[2:5], input$nI)
        

        
        #If - else logic to select data for summary display 
        
        
          if(input$pos == "QB"){sQB}
          else{
            if(input$pos == "WR"){sWR}
            else{
              if(input$pos == "RB"){sRB}
              else{
                if(input$pos == "TE"){sTE}
                else{sALL}
          }
            }
              }
                }
          )
        
        {observe(
        if(input$pos == "QB")
          {updateRadioButtons(session, "var2", selected = "avgPassingAttempts")}
        else{
          if(input$pos == "WR")
            {updateRadioButtons(session, "var2", "Summary Variable 2",selected =  "avgTargets")}
          else{
            if(input$pos == "RB")
              {updateRadioButtons(session, "var2", "Summary Variable 2", selected =  "avgRushingAttempts")}
            else{
              if(input$pos == "TE")
      {updateRadioButtons(session, "var2", "Summary Variable 2", selected =  "avgTargets")}
              else{updateRadioButtons(session, "var2", "Summary Variable 2", selected = "PassingAttempts")}
            }
          }
        }
)}
        
        
        
        
        
        
        
        
    #     
    #   #  QBscatter1 <- ggplot(sQB, aes(x = avgProjectedPoints, y = AvgPassingA)) + geom_point()
    #     
    # #    QBscatter2 <- ggplot(sQB, aes(x = avgCompletions, y = AvgPA)) + geom_point()
    #     
     #   output$qbScatter <- renderDataTable({
     #    if(input$qbScatter == "Passing Attempts vs. Actual Points"){QBscatter1}
     #  else{QBscatter2}
     # })
        
# Clustering with Dendogram 

    scaledData <- as.data.frame(scale(fantasyDataNumeric, center = TRUE, scale = TRUE))

    # Combine the selected variables into a new data frame
    selectedData <- reactive({
     
      scaledData[, c(input$xcol, input$ycol)]
    })

    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })

    output$plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })

    output$plot2 <- renderPlot({
      hierClust <- hclust(dist(selectedData()))
    plot(hierClust, xlab = "")
      })
    
    


# Modeling 
    
    # Train and Test sets 
    
    qbData <- filter(fantasyData, Pos.y == "QB")
    
    set.seed(420)
    qbIndex <- createDataPartition(qbData$X, p = 0.8, list = FALSE)
    qbTrain <- qbData[qbIndex, ]
    qbTest <- qbData[-qbIndex, ]
    
    # Linear model 
    
    lmFitvalue <- reactive({lmFit <- train(as.formula(input$response ~ input$predictor), data = qbTrain, method = "lm")})
    
    
    
    output$lmFit <- renderPrint({
      predlm <- predict(lmFitvalue(), newdata = qbTest)
      postResample(predlm, qbTest$X)
      lmFitvalue()}) 
    
    # Linear Model Output for Numeric Input 
    
     predlm2value <- reactive({ predlm2<-predict(as.formula(input$response ~ input$predictor), newdata = data.frame(Cmp = input$predictorvalue))})
    
    output$lmFit2 <- renderPrint({predlm2value()})

    # Random Forest Model 

    rfFit <- train(Actual ~ Cmp, data = qbTrain,
                   method = "rf",
                   trControl = trainControl(method = "cv",
                                            number = 5)
                   #,tuneGrid = data.frame(mtry = 1:9)
                   )
    
    output$rfFit <- renderPrint({rfFit})
        
    # Output Data Table for the Data Table tab 
        
        rdQB <- filter(fantasyData, Pos.y == "QB", na.rm = TRUE)
        
        rdWR <- filter(fantasyData, Pos.y == "WR", na.rm = TRUE)
        
        rdRB <- filter(fantasyData, Pos.y == "RB", na.rm = TRUE)
        
        rdTE <- filter(fantasyData, Pos.y == "TE", na.rm = TRUE)
        
        output$rawData <- renderDataTable({
          if(input$pos2 == "QB"){rdQB}
          else{
            if(input$pos2 == "WR"){rdWR}
            else{
              if(input$pos2 == "RB"){rdRB}
              else{
                if(input$pos2 == "TE"){rdTE}
                else{fantasyData}
              }}}}, extensions = 'Buttons', options = list("dom" = 'T<"clear">lBfrtip', buttons = list('copy', 'csv', 'excel', 'pdf', 'print'))) 
                                      
    
})
