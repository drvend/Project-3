#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# packages required to run this coded are listed below
library(shiny)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(caret)
#library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # 
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
    # 
     Predictiondata <- read.csv('C:/Users/drven/Documents/ST 558/Project-3/2019projections.csv')
    # 
    # FFdatatotal <- rbind(FFdataw1, FFdataw2, FFdataw3, FFdataw4, FFdataw5, FFdataw6, FFdataw7, FFdataw8, FFdataw9, FFdataw10, FFdataw11, FFdataw12, FFdataw13, FFdataw14, FFdataw15, FFdataw16, FFdataw17)
    # 
    # saveRDS(FFdatatotal, "FFdatatotal.RDS")
    
    
    
    ########
    
    
    FFdatatotal = readRDS("C:/Users/drven/Documents/ST 558/Project-3/FFdatatotal.RDS")
    
    fantasyData <- as.data.frame(left_join(Predictiondata, FFdatatotal, by = c("Player", "Week")))
    
     fantasyData[is.na(fantasyData)] <- 0 
     
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
        
        sALL <- fantasyData  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgTargets = mean(Tgt))
        
        sQB <- filter(fantasyData, Pos.y == "QB", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgPassingAttempts = mean(PassingAtt), avgRushingAttempts = mean(RushingAtt), avgCompletions = mean(Cmp))
        
        sWR <- filter(fantasyData, Pos.y == "WR", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgRushingAttempts = mean(RushingAtt), avgTargets = mean(Tgt))
        
        sRB <- filter(fantasyData, Pos.y == "RB", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgRushingAttempts = mean(RushingAtt), avgTargets = mean(Tgt))
        
        sTE <- filter(fantasyData, Pos.y == "TE", na.rm = TRUE)  %>% group_by(Player) %>%  summarise(avgProjectedPoints = mean(Proj), avgActualPoints = mean(Actual), avgRushingAttempts = mean(RushingAtt), avgTargets = mean(Tgt))
        

        
        #If - else logic to select data for summary display 
        
        output$summary <- renderDataTable({
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
        
        
        
      #  QBscatter1 <- ggplot(sQB, aes(x = avgProjectedPoints, y = AvgPassingA)) + geom_point()
        
    #    QBscatter2 <- ggplot(sQB, aes(x = avgCompletions, y = AvgPA)) + geom_point()
        
     #   output$qbScatter <- renderDataTable({
      #    if(input$qbScatter == "Passing Attempts vs. Actual Points"){QBscatter1}
        #  else{QBscatter2}
       # })
        
# Clustering with Dendogram 
        
  #  scaledData <- scale(fantasyDataNumeric, center = TRUE, scale = TRUE)
        
#clustmodel <- hclust()
        
        
# Modeling 
        
        
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
