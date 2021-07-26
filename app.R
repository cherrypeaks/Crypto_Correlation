###################################
#### SHINY APP 
# MscFE 690 Capstone (July 2021)
# Last modified: 26 July 2021
# WorldQuant University
# Using Principal Component Analysis on Crypto Correlations to Build a Diversified Portfolio
# Author: María Guinda > mariaguinda@gmail.com

# Period: 2017 - 2021
# Crypto Source: coinmarketcap.com
# Equity Source: Yahoo Finance
###################################

library(shiny)
library(shinythemes)
library(readxl) #To read XLS files
library(data.table)
library(ggplot2) #To create the heat maps and other visual results
library(ggbiplot) # To plot PCA 
library(magrittr) # for pipe %>%
library(tibble) # for `rownames_to_column` and `column_to_rownames`
library(dplyr)   # everything else
library(factoextra)
library(dplyr)   # everything else
library(PerformanceAnalytics) # for performance analysis

# Collecting input data  
periods <- read_xlsx("Data.xlsx", sheet="PERIODS")
equities_data <- read_xlsx("Data.xlsx", sheet="EQUITIES")    # Collecting equity data
asset_names <- colnames(equities_data[,2:38])


# Define UI for the App
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage(
        title = "WorldQuant University: María Guinda",
        # TAB 1
        tabPanel(title = "Correlation Heatmap", 
                 p("This is an application build for the MscFE 690 Capstone Project at WorldQuant University."),
                 p("The aim of the project is to offer an updated correlation analysis of 31 top crypto assets, among them and with some equity and gold indices."),
                 p("Furthermore, we conduct a PCA to identify the group of cryptos that present different correlation patterns and may help us build a diversified crypto portfolio."),
                 hr(),
                 sidebarPanel( 
                     # Title
                     h3("Input Data:"),
                     # Input: Group of data
                     radioButtons(inputId = "AssetData",
                                  label = "Select the group of assets",
                                  c("Only crypto" = "crypto_data", "Including equities and gold" = "equities_data", "Bitcoin and equities" = "bitcoin_data")),
                     # br(),
                     hr(),
                     # Input: Type of date
                     radioButtons(inputId = "DateType",
                                  label = "Preselected or custum date period?",
                                  c("Preselected Periods" = "preselected", "Custom Date Range" = "customdate")),
                     br(),
                     # Input: Study dates
                     selectInput(inputId = "SelectedPeriod",
                                 label = "Preselected Periods:",
                                 choices = periods$PERIOD),
                     # Input: Custom Range
                     dateRangeInput('dateRange',
                                    label = 'Custom Rate Range:',
                                    start = "2017-01-01", "2021-06-23" , min = "2017-01-01" , max = "2021-06-23"),
                     hr(),
                     helpText("Crypto data from coinmarketcap.com. Equities' data from yahoo! Finance. Available data from 2017-01-01 to 2021-06-23.")
                 ), # Closing sidebarPanel
                 
                 # Output panel
                 mainPanel(
                     # Header 
                     h3("Correlation Heatmap"),
                     # Output: Correlation Heatmap
                     plotOutput(outputId = "CorrelationHeatmap"),
                     # Header 
                     h3("Average Correlations Heatmap"),
                     # Output: Correlation Heatmap
                     plotOutput(outputId = "AverageHeatmap")
                 ) # Closing mainPanel
                 
        ), # Closing TAB1
        
        # TAB 2
        tabPanel(title = "Historical Correlation", 
                 p("This is an application build for the MscFE 690 Capstone Project at WorldQuant University."),
                 p("The aim of the project is to offer an updated correlation analysis of 31 top crypto assets, among them and with some equity and gold indices."),
                 p("Furthermore, we conduct a PCA to identify the group of cryptos that present different correlation patterns and may help us build a diversified crypto portfolio."),
                 hr(),
                 sidebarPanel( 
                     # Title
                     h3("Input Data:"),
                     # Input: Asset1
                     selectInput(inputId = "Asset1",
                                 label = "Select Asset #1:",
                                 choices = asset_names),
                     # Input: Asset2
                     selectInput(inputId = "Asset2",
                                 label = "Select Asset #2:", selected = "ETH",
                                 choices = asset_names),
                     # Input: Rolling Days
                     selectInput(inputId = "days",
                                 label = "Select Rolling Days:",
                                 choices = c(10, 20, 30, 40, 50, 60, 90, 180, 360), selected = c(50)
                     ),
                     br(),
                     helpText("Crypto data from coinmarketcap.com. Equities'data from yahoo! Finance. Available data from 2017-01-01 to 2021-06-23.")
                 ), # Closing sidebarPanel
                 
                 # Output panel
                 mainPanel(
                     # Header 
                     h3("Historical Rolling Correlation"),
                     # Output: Rolling Correlation
                     plotOutput(outputId = "RollingOut"),
                     # Bull and bear legend
                     imageOutput("image1")
                 ) # Closing mainPanel
        ), # Closing TAB2
        # TAB 3
        tabPanel(title = "PCA & Portfolio Optimization", 
                 p("This is an application build for the MscFE 690 Capstone Project at WorldQuant University."),
                 p("The aim of the project is to offer an updated correlation analysis of 31 top crypto assets, among them and with some equity and gold indices."),
                 p("Furthermore, we conduct a PCA to identify the group of cryptos that present different correlation patterns and may help us build a diversified crypto portfolio."),
                 hr(),
                 sidebarPanel( 
                     # Title
                     h3("Input Data:"),
                     # Input: Visualization type
                     radioButtons(inputId = "Grouping",
                                  label = "Group assets by:",
                                  c("Crypto Categories" = "selected.categories", "K-means clusters" = "selected.clusters")),
                     br(),
                     # Input: Number of clusters
                     sliderInput(inputId = "clusters", 
                                 label = "Number of clusters:",
                                 min = 2, max = 7,
                                 value = 3),
                     # Input: Custom Range
                     dateRangeInput(inputId = "TestingPeriod",
                                    label = "Testing Period:",
                                    start = "2020-08-21", "2021-06-24" , min = "2018-01-06" , max = "2021-06-24"),
                     br(),
                     helpText("Crypto data from coinmarketcap.com. Equities' data from yahoo! Finance. Available data from 2017-01-01 to 2021-06-23.")
                 ), # Closing sidebarPanel
                 # Output panel
                 mainPanel(
                     # Header 
                     h3("Principal Component Analysis Crypto Results"),
                     p("Principal Component Analysis (PCA) is a powerful technique that allow us to reduce the number of variables and visualize our assets (components) using the two best dimensions that better represent our data."),
                     p("On this PCA we can use a K-means clustering technique, a data grouping method which aims to distribute the set of observations into k defined number of groups, in which each observation goes to the group whose mean value is closest."),
                     # Output: Rolling Correlation
                     plotOutput(outputId = "PCAOut"),
                     # Header 
                     h3("Portfolio Optimization Results"),
                     p("In order to test our analysis, we have built three portfolios. Our first portfolio includes the 31 selected cryptoassets. This, theoretically, represent the most diversify crypto portfolio. Additionally, using the results from the PCA we have built two portfolios. One includes five assets, each being the one that best represents the different crypto categories as per our PCA analysis. And the third portfolio includes an asset from each cluster, being the result of the clusters under the K-means analysis. "),
                     # Output: Portfolio Resume Table
                     tableOutput(outputId = "PortfolioTable")
                 ) # Closing mainPanel
        ) # Closing TAB3
    ) # Closing navbarPage
) # Closing Fluid Page 



# Define server function  
server <- function(input, output) {
    
    # LOADING THE DATA ####
    crypto_data <- read_xlsx("Data.xlsx", sheet="CRYPTO") # Collecting crypto data
    equities_data <- read_xlsx("Data.xlsx", sheet="EQUITIES")    # Collecting equity data
    bitcoin_data <- read_xlsx("Data.xlsx", sheet="BITCOIN")    # Collecting Bitcoin + equities data
    
    # Collecting crypto categories
    crypto_category <- read_xlsx("Data.xlsx", sheet="CATEGORY") 
    crypto_cat <- as.character(crypto_category[1,])
    
    # Collecting period data  
    periods <- read_xlsx("Data.xlsx", sheet="PERIODS") 
    
    
    # DEFINE FUNCTIONS FOR WORK ####
    
    correlation <- function(asset_data, asset1, asset2, start_date, end_date) {
        # DESCRIPTION: Function that computes Pearson correlation coefficient for two pair of assets . 
        # INPUTS: asset_data> data table with the asset's daily returns. asset 1 and 2> name of the selected assets. start_day> first day of period. end_day> last day of period.
        sub_data <- subset(asset_data, asset_data["DATE"]>=start_date & asset_data["DATE"]<=end_date) 
        result <- cor(sub_data[asset1], sub_data[asset2], use = "pairwise.complete.obs")
        return(result)
    }
    
    correlation_matrix <- function(asset_data, start_date, end_date) {
        # DESCRIPTION: Function that computes a triangle correlation matrix. 
        # INPUTS: asset_data> data table with the asset's daily returns. start_day> first day of period. end_day> last day of period.
        sub_data <- subset(asset_data, asset_data["DATE"]>=start_date & asset_data["DATE"]<=end_date)  #Select period
        cleandata <- sub_data[, c(-1)] #Clean data for correlation
        
        for(i in 1:length(cleandata)){    #Loop to check if the available data for that period is suficient
            if( sum(is.na(cleandata[,i]))/nrow(cleandata) > 0.7 ){ #if we are missing more than 70% of the data, fill all with NA
                cleandata[,i]=NA
            }   
        }
        
        cormatrix <- round(cor(cleandata, use = "pairwise.complete.obs"),2) #Compute correlation matrix, ignore NA and round 2 decimals
        cormatrix[lower.tri(cormatrix)]<- NA # Get upper triangle of the correlation matrix
        return(cormatrix)
    }
    
    correlation_matrix_full <- function(asset_data, start_date, end_date) {
        # DESCRIPTION: Function that computes correlation matrix for a selected group of assets.
        # INPUTS: asset_data> data table with the asset's daily returns. start_day> first day of period. end_day> last day of period.
        sub_data <- subset(asset_data, asset_data["DATE"]>=start_date & asset_data["DATE"]<=end_date)  #Select period
        cleandata <- sub_data[, c(-1)] #Clean data for correlation
        cormatrix <- round(cor(cleandata, use = "pairwise.complete.obs"),2) #Compute correlation matrix, ignore NA and round 2 decimals
        return(cormatrix)
    }
    
    correlation_heatmap <- function(asset_data, start_date, end_date, titulo) {
        # DESCRIPTION: Function that plots correlation heatmap for a seleected group of assets.
        # INPUTS: asset_data> data table with the asset's daily returns. start_day> first day of period. end_day> last day of period.
        cormatrix <- correlation_matrix(asset_data, start_date, end_date) # Compute correlation matrix
        melted_cormatrix <- melt(cormatrix, na.rm = TRUE) # Melt the correlation matrix
        ggplot(data = melted_cormatrix, aes(Var2, Var1, fill = value))+    # Heat map
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 12, hjust = 1))+
            xlab("Assets") + ylab("Assets") + ggtitle(paste(titulo)) +
            coord_fixed()
    }
    
    average_correlations<- function(asset_data, start_date, end_date) {   
        # DESCRIPTION: Function that computes average correlation for a correlation matrix, for each asset. 
        # INPUTS: asset_data> data table with the asset's daily returns. start_day> first day of period. end_day> last day of period.
        sub_data <- subset(asset_data, asset_data["DATE"]>=start_date & asset_data["DATE"]<=end_date)  #Select period
        cleandata <- sub_data[, c(-1)] #Clean data for correlation
        for(i in 1:length(cleandata)){    #Loop to check if the available data for that period is suficient
            if( sum(is.na(cleandata[,i]))/nrow(cleandata) > 0.7 ){ #if we are missing more than 70% of the data, fill all with NA
                cleandata[,i]=NA
            }   
        }
        cormatrix <- round(cor(cleandata, use = "pairwise.complete.obs"),2) #Compute correlation matrix, ignore NA and round 2 decimals
        aver_cor <- data.frame(t(round((colSums(cormatrix, na.rm=TRUE)-1)/(length(cleandata)-1-sum(is.na(cormatrix[1,]))) ,2 ))) #Compute average correlation for each asset
        aver_cor[aver_cor == round(((-1)/(length(cleandata)-1-sum(is.na(cormatrix[1,])))),2)] <- "-"
        return(aver_cor)  #Return the average value for each asset
    }
    
    average_table <- function(asset_data, initial_period, end_period) {   
        # DESCRIPTION: Compute average correlations table for selected assets and the diferent periods defined.
        # INPUTS: asset_data> data table with the asset's daily returns. 
        ave_table <-  average_correlations(asset_data, c(periods[1,"START"]), c(periods[1,"END"]))  # Total average and initiate table
        rownames(ave_table) <- c(periods[1,"PERIOD"]) # Add label
        
        for(i in initial_period:end_period){
            ave <- average_correlations(asset_data, c(periods[i,"START"]), c(periods[i,"END"]))  # Compute average
            rownames(ave) <- c(periods[i,"PERIOD"]) # Add label
            ave_table <- rbind(ave_table, ave) # Add to table
        }
        
        ave_matrix <- data.matrix(ave_table)
        
        ave_melt <- melt(ave_matrix, na.rm = TRUE) # Melt the correlation matrix
        ggplot(data = ave_melt, aes(Var2, Var1, fill = value))+    # Heat map
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Average\nPearson\nCorrelation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 12, hjust = 1))+
            xlab("Asset") + ylab("Period") + ggtitle("Average Correlation Among Assets") +
            coord_fixed() 
        
    }
    
    rolling_correlation <- function(asset_data, days, asset1, asset2) {
        # DESCRIPTION: Plots rolling correlation for selected assets 
        # INPUTS: asset_data> data table with the asset's daily returns. Days> rolling number of days. asset1 and 2> name of the assets, has to be like the col names. 
        x <- asset_data[asset1]
        y <- asset_data[asset2]
        z <- data.frame(x,y)
        corr.ret <- rollapply(z, width = days, function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"), by.column = FALSE)
        rolcorr <- data.frame(asset_data[1:length(corr.ret),1], corr.ret)
        ggplot() + 
            geom_rect(data = periods[c(2,4,6), ], aes(xmin = START, xmax = END, ymin = -Inf, ymax = Inf), fill = "#ebfaeb") + #BULL PERIODS
            geom_rect(data = periods[c(3,5,7), ], aes(xmin = START, xmax = END, ymin = -Inf, ymax = Inf), fill = "#ffebe6") + #BEAR PERIODS
            geom_line(data = rolcorr, aes(x=DATE, y=corr.ret)) +  
            xlab("Date") + ylab("Rolling correlation") + 
            ggtitle(paste("Rolling", days,"days correlation for", asset1, "and", asset2)) 
        
    }
    
    
    # OUTPUTS ####
    
    # OUTPUT TAB1: Ploting CorrelationHeatmap
    
    output$CorrelationHeatmap <- renderPlot( {
        library(factoextra)
        
        if(input$AssetData == "crypto_data"){
            crypto.data = crypto_data
        } else if (input$AssetData == "equities_data"){
            crypto.data = equities_data
        } else if (input$AssetData == "bitcoin_data"){
            crypto.data = bitcoin_data
        }
        
        if(input$DateType == "preselected"){
            start.date = periods$START[which(grepl(input$SelectedPeriod, periods$PERIOD))]
            end.date = periods$END[which(grepl(input$SelectedPeriod, periods$PERIOD))]
        } else {
            start.date = input$dateRange[1]
            end.date = input$dateRange[2]
        }
        
        correlation_heatmap(crypto.data, as.character(start.date), as.character(end.date), paste("Pearson correlation for the period from", start.date,"to", end.date))
        
    })
    
    output$AverageHeatmap <- renderPlot( {
        library(factoextra)
        
        if(input$AssetData == "crypto_data"){
            crypto.data = crypto_data
        } else if (input$AssetData == "equities_data"){
            crypto.data = equities_data
        } else if (input$AssetData == "bitcoin_data"){
            crypto.data = bitcoin_data
        }
        
        average_table(crypto.data, 2, 15)
        
    })
    
    # OUTPUT TAB2: Rolling Correlation
    
    output$RollingOut <- renderPlot({
        rolling_correlation(equities_data, as.double(input$days), input$Asset1, input$Asset2)
    })
    
    output$image1 <- renderImage({
        return(list(
            src = "www/BullBear.png",
            contentType = "image/png",
            alt = "Legend"
        ))
    })
    
    # OUTPUT TAB3: PCA
    
    output$PCAOut <- renderPlot({
        
        # Define study periods
        start_date_train = as.character(input$TestingPeriod[1]-365)
        end_date_train = as.character(input$TestingPeriod[1]-1)
        start_date_test = as.character(input$TestingPeriod[1])
        end_date_test = as.character(input$TestingPeriod[2])
   
        # Compute Correlation Matrix and clean for assets with no sufficient data
        corrmatrix_pca <- data.frame(correlation_matrix_full(crypto_data, start_date_train, end_date_train))
        delete_asset <- names(which(colSums(is.na(corrmatrix_pca)) == dim(corrmatrix_pca)[1]))  # Clean correlation matrix of NA variables
        delete_number <- grep(delete_asset[1], colnames(corrmatrix_pca))
        if(length(delete_asset)>1){
            for (i in 2:length(delete_asset)){
                delete_number[i] <- grep(delete_asset[i], colnames(corrmatrix_pca))
            }
        }
        if( sum(is.na.data.frame(corrmatrix_pca)) == 0){ # no NA variables
        } else {
            corrmatrix_pca <- corrmatrix_pca[-c(delete_number),-c(delete_number)] # Delete the NA assets
            crypto_cat <- as.character(crypto_category[1,-c(delete_number)])  # Select associated categories
        }
        corrmatrix_pca[is.na(corrmatrix_pca)] <- 0  # If there is any NA replace for 0 (no relation)
        
        # Compute PCA and K-means
        res.pca <- prcomp(corrmatrix_pca, scale = TRUE) 
        set.seed(123)
        res.km <- kmeans(corrmatrix_pca, input$clusters, )
        
        if(input$Grouping == "selected.categories"){
            fviz_pca_ind(res.pca,            # Plot groups
                         col.ind = crypto_cat,       # Color by groups
                         palette = c("#00AFBB",  "#FC4E07", "#E7B800", "#2E9FDF", "#696969"),
                         addEllipses = TRUE,         # Concentration ellipses
                         ellipse.type = "convex",
                         legend.title = "Groups",
                         repel = TRUE, 
                         title = "PCA: Association by crypto categories" )
        } else if (input$Grouping == "selected.clusters"){
            fviz_cluster(res.km, data = corrmatrix_pca,    # Plot results
                         palette = palette(), 
                         geom = c("point", "text"),
                         ellipse.type = "convex", 
                         ggtheme = theme_bw(),
                         repel = TRUE, 
                         title = "PCA: Association by clusters"
            )
        }
        
    })
    
    output$PortfolioTable <- renderTable({
        
        # Define study periods
        start_date_train = as.character(input$TestingPeriod[1]-365)
        end_date_train = as.character(input$TestingPeriod[1]-1)
        start_date_test = as.character(input$TestingPeriod[1])
        end_date_test = as.character(input$TestingPeriod[2])
        
        # Compute Correlation Matrix and clean for assets with no sufficient data
        corrmatrix_pca <- data.frame(correlation_matrix_full(crypto_data, start_date_train, end_date_train))
        delete_asset <- names(which(colSums(is.na(corrmatrix_pca)) == dim(corrmatrix_pca)[1]))  # Clean correlation matrix of NA variables
        delete_number <- grep(delete_asset[1], colnames(corrmatrix_pca))
        if(length(delete_asset)>1){
            for (i in 2:length(delete_asset)){
                delete_number[i] <- grep(delete_asset[i], colnames(corrmatrix_pca))
            }
        }
        if( sum(is.na.data.frame(corrmatrix_pca)) == 0){ # no NA variables
        } else {
            corrmatrix_pca <- corrmatrix_pca[-c(delete_number),-c(delete_number)] # Delete the NA assets
            crypto_cat <- as.character(crypto_category[1,-c(delete_number)])  # Select associated categories
        }
        corrmatrix_pca[is.na(corrmatrix_pca)] <- 0  # If there is any NA replace for 0 (no relation)
        
        # Compute PCA and K-means
        res.pca <- prcomp(corrmatrix_pca, scale = TRUE) 
        set.seed(123)
        res.km <- kmeans(corrmatrix_pca, input$clusters, )
        
        # Finding assets for each portfolio
        # PORTFOLIO 1
        res.ind <- get_pca_ind(res.pca)  # Obtain individual coordinates

        # Coordinate of groups
        coord.groups <- res.ind$coord
        coord.groups <- data.frame(coord.groups)
        coord.groups <- coord.groups[, 1:2]
        coord.groups <- mutate(coord.groups, category = crypto_cat)
        coord.result <- data.frame(aggregate(coord.groups[, 1:2], list(coord.groups$category), mean))

        # Find the individuals that fits closer to the Group mean
        for (i in 1:nrow(coord.groups)){
            if(coord.groups[i, "category"] == as.character(coord.result[1,1])) {
                coord.groups[i, "dist1"] <- as.double(coord.result[1,2]) - coord.groups[i, "Dim.1"]
                coord.groups[i, "dist2"] <- as.double(coord.result[1,3]) - coord.groups[i, "Dim.2"]
            } else if (coord.groups[i, "category"] == as.character(coord.result[2,1])) {
                coord.groups[i, "dist1"] <- as.double(coord.result[2,2]) - coord.groups[i, "Dim.1"]
                coord.groups[i, "dist2"] <- as.double(coord.result[2,3]) - coord.groups[i, "Dim.2"]
            } else if (coord.groups[i, "category"] == as.character(coord.result[3,1])) {
                coord.groups[i, "dist1"] <- as.double(coord.result[3,2]) - coord.groups[i, "Dim.1"]
                coord.groups[i, "dist2"] <- as.double(coord.result[3,3]) - coord.groups[i, "Dim.2"]
            } else {
                coord.groups[i, "dist1"] <- as.double(coord.result[4,2]) - coord.groups[i, "Dim.1"]
                coord.groups[i, "dist2"] <- as.double(coord.result[4,3]) - coord.groups[i, "Dim.2"]
            }
        }
        coord.groups$distT <- sqrt(as.double(coord.groups$dist1)^2 + as.double(coord.groups$dist2)^2) #Compute distance

        # Find minimum elements
        minvalues <- coord.groups %>% rownames_to_column('name') %>% group_by(category) %>% slice(which.min(distT)) %>% column_to_rownames('name')

        # PORTFOLIO 2
        # Coordinates of individuals
        km.ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
        km.ind.coord  <- km.ind.coord [, 1:2]
        km.ind.coord$cluster <- as.double(factor(res.km$cluster))   # Add clusters obtained using the K-means algorithm
        km.ind.coord <- mutate(km.ind.coord, category = crypto_cat)  # Add original category
        coord.cluster <- data.frame(aggregate(km.ind.coord[, 1:2], list(km.ind.coord$cluster), mean))  #Compute coordinates of the clusters

        # Find the individuals that fits closer to the Cluster mean
        for (i in 1:nrow(km.ind.coord)){
            if(km.ind.coord[i, "cluster"] == as.double(coord.cluster[1,1])) {
                km.ind.coord[i, "dist1"] <- as.double(coord.cluster[1,2]) - km.ind.coord[i, "Dim.1"]
                km.ind.coord[i, "dist2"] <- as.double(coord.cluster[1,3]) - km.ind.coord[i, "Dim.2"]
            } else if (km.ind.coord[i, "cluster"] == as.double(coord.cluster[2,1])) {
                km.ind.coord[i, "dist1"] <- as.double(coord.cluster[2,2]) - km.ind.coord[i, "Dim.1"]
                km.ind.coord[i, "dist2"] <- as.double(coord.cluster[2,3]) - km.ind.coord[i, "Dim.2"]
            } else if (km.ind.coord[i, "cluster"] == as.double(coord.cluster[3,1])) {
                km.ind.coord[i, "dist1"] <- as.double(coord.cluster[3,2]) - km.ind.coord[i, "Dim.1"]
                km.ind.coord[i, "dist2"] <- as.double(coord.cluster[3,3]) - km.ind.coord[i, "Dim.2"]
            } else {
                km.ind.coord[i, "dist1"] <- as.double(coord.cluster[4,2]) - km.ind.coord[i, "Dim.1"]
                km.ind.coord[i, "dist2"] <- as.double(coord.cluster[4,3]) - km.ind.coord[i, "Dim.2"]
            }
        }
        km.ind.coord$distT <- sqrt(as.double(km.ind.coord$dist1)^2 + as.double(km.ind.coord$dist2)^2) #Compute distance

        # Find minimum elements
        km.minvalues <- km.ind.coord %>% rownames_to_column('name') %>% group_by(cluster) %>% slice(which.min(distT)) %>% column_to_rownames('name')

        # BUILDING PORTFOLIOS
        portfolio1 <- subset(crypto_data, crypto_data["DATE"]>=start_date_test & crypto_data["DATE"]<=end_date_test)  #Select period and all crypto
        portfolio1 <- column_to_rownames(portfolio1, "DATE")
        if( sum(is.na(delete_number)) == 0){ # no NA variables
            portfolio1 <- portfolio1[,-c(delete_number)]  # Delete non study assets
        } else {
        }
        portfolio1.value <- data.frame(matrix(100, dim(portfolio1)[1], dim(portfolio1)[2])) # Create empty matrix to store portfolio values
        portfolio2.value <- data.frame(matrix(100, dim(portfolio1)[1], dim(minvalues)[1]))
        portfolio3.value <- data.frame(matrix(100, dim(portfolio1)[1], dim(km.minvalues)[1]))
        
        for (i in 1:(dim(portfolio1.value)[1]-1)){   # Compute value of initial investment of 100 in each asset
            portfolio1.value[dim(portfolio1.value)[1]-i,] <- ifelse(is.na(portfolio1[dim(portfolio1.value)[1]-i,]), portfolio1.value[dim(portfolio1.value)[1]-i+1,], (as.numeric(portfolio1[dim(portfolio1.value)[1]-i,]) + 1) * as.numeric(portfolio1.value[dim(portfolio1.value)[1]-i+1,]))
        }
        
        rownames(portfolio1.value) <- rownames(portfolio1)
        colnames(portfolio1.value) <- colnames(portfolio1)
        portfolio2.value <- portfolio1.value[, rownames(minvalues)]  # Portfolio with category min
        portfolio3.value <- portfolio1.value[, rownames(km.minvalues)]  # Portfolio with cluster min
        
        portfolio1.value$TOTAL <- rowSums(portfolio1.value)  # Compute portfolio 1 total value
        portfolio2.value$TOTAL <- rowSums(portfolio2.value)  # Compute portfolio 1 total value
        portfolio3.value$TOTAL <- rowSums(portfolio3.value)  # Compute portfolio 1 total value
        
        n <- nrow(portfolio1.value)
        
        for (i in 1:n-1){  # Compute daily performance
            portfolio1.value$ret[i] <- ((portfolio1.value$TOTAL[i] / portfolio1.value$TOTAL[i+1]) - 1) 
            portfolio2.value$ret[i] <- ((portfolio2.value$TOTAL[i] / portfolio2.value$TOTAL[i+1]) - 1) 
            portfolio3.value$ret[i] <- ((portfolio3.value$TOTAL[i] / portfolio3.value$TOTAL[i+1]) - 1) 
        }
        
        portfolio1.ret <- portfolio1.value[1:n-1, ncol(portfolio1.value), drop=FALSE]
        portfolio2.ret <- portfolio2.value[1:n-1, ncol(portfolio2.value), drop=FALSE]
        portfolio3.ret <- portfolio3.value[1:n-1, ncol(portfolio3.value), drop=FALSE]
        
        returns <- portfolio1.value[1:n-1, ncol(portfolio1.value), drop=FALSE]
        names(returns)[1] <- "Portfolio 1: All crypto"
        returns["Portfolio 2: Categories"] <- portfolio2.value[1:n-1, ncol(portfolio2.value), drop=FALSE]
        returns["Portfolio 3: Clusters"] <- portfolio3.value[1:n-1, ncol(portfolio3.value), drop=FALSE]
        returns$BTC <- subset(crypto_data, crypto_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & crypto_data["DATE"]<=end_date_test)$BTC
        
        
        # Summary Table
        
        portfolio.tables <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Portfolio 1", "Portfolio 2", "Portfolio 3", "BTC"))
        portfolio.tables["Number of assets",] = c(dim(portfolio1.value)[2], dim(portfolio2.value)[2], dim(portfolio3.value)[2], 3)-2
        portfolio.tables["Cryptos",] = c("All study assets", paste(row.names(minvalues), collapse = (",")), paste(row.names(km.minvalues), collapse = (",")), "BTC")
        portfolio.tables["Annualized return (%)",] =  round(Return.annualized(returns)[1,]*100, 2)                                      
        portfolio.tables["Cumulative Return (%)",] =  round(PerformanceAnalytics::Return.cumulative(returns)[1,]*100, 2)                                      
        portfolio.tables["Max Drawdown (%)",] =  round(maxDrawdown(returns)[1,]*100, 2)                                      
        portfolio.tables["Annualized Standard Deviation",] =  round(PerformanceAnalytics::sd.annualized(returns), 2)                                      
        portfolio.tables["Sharpe Ratio",] =  round(Return.annualized(returns)/PerformanceAnalytics::sd.annualized(returns), 2)                                      
        
        table.Stats(returns)
        portfolio.tables
        
        
        # Ploting results
        charts.PerformanceSummary(portfolio1.ret, main= "Performance Summary for All Crypto Portfolio", ylim=c(-1,10.5), col = "#ff0000")
        charts.PerformanceSummary(portfolio2.ret, main= "Performance Summary for Category Portfolio", ylim=c(-1,10.5), col="#3399ff")
        charts.PerformanceSummary(portfolio3.ret, main= "Performance Summary for Cluster Portfolio", ylim=c(-1,10.5), col="#33cc33")
        
        
        # Correlation analysis
        returns$World <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$World
        returns$Asia <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$Asia
        returns$Europe <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$Europe
        returns$DowJones <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$DowJones
        returns$Nasdaq <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$Nasdaq
        returns$Gold <- subset(equities_data, equities_data["DATE"]>=(as.character(as.Date(start_date_test)+1)) & equities_data["DATE"]<=end_date_test)$Gold
        returns <- rownames_to_column(returns, "DATE")
        result_matrix <- correlation_matrix(returns, start_date_test, end_date_test)
        
        portfolio.tables["Correlation with BTC", ] = c(result_matrix["Portfolio 1: All crypto","BTC"], result_matrix["Portfolio 2: Categories","BTC"], result_matrix["Portfolio 3: Clusters","BTC"], 1)
        
        # PLOT FINAL TABLE
        portfolio.tables
    }, rownames = TRUE)

} # server


# Create Shiny object
shinyApp(ui = ui, server = server)

