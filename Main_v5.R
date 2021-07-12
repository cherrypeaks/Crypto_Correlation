# MscFE 690 Capstone (July 2021)
# Last modified: 12 July 2021
# WorldQuant University
# What Can Be Inferred from Crypto Correlations?
# Author: María Guinda > mariaguinda@gmail.com

# Period: 2017 - 2021
# Crypto Source: coinmarketcap.com
# Equity Source: Yahoo Finance

# LIBRARIES
# library(fBasics) #Basic stats
# library(tseries) #Time series analysis (ADF test, etc)
# library(testit) # To test warnings and error
# library(quantmod)
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



# STEP 1: LOADING THE DATA ####
crypto_data <- read_xlsx("Data.xlsx", sheet="CRYPTO") # Collecting crypto data
equities_data <- read_xlsx("Data.xlsx", sheet="EQUITIES")    # Collecting equity data
bitcoin_data <- read_xlsx("Data.xlsx", sheet="BITCOIN")    # Collecting Bitcoin + equities data

# Collecting crypto categories
crypto_category <- read_xlsx("Data.xlsx", sheet="CATEGORY") 
crypto_cat <- as.character(crypto_category[1,])

crypto_cat2 <- as.character(crypto_category[1,-c(delete_number)])

crypto_cat2

# STEP 2: TREAT THE DATA####
# This step has been done using execel, as the data was collected and downloaded from the original sources using excel


# STEP 3: DEFINE STUDY PERIODS ####
periods <- read_xlsx("Data.xlsx", sheet="PERIODS") # Collecting period data


# STEP 4: COMPUTE CORRELATIONS ####

# DEFINE FUNCTIONS FOR WORK

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
  for(i in 1:length(cleandata)){    #Loop to check if the av data for that period is suficient
    if( sum(is.na(cleandata[,i]))/nrow(cleandata) > 0.7 ){ #if we are missing more than 70% of the data, fill all with NA
      cleandata[,i]=NA
    }   
  }
  cormatrix <- round(cor(cleandata, use = "pairwise.complete.obs"),2) #Compute correlation matrix, ignore NA and round 2 decimals
  aver_cor <- data.frame(t(round((colSums(cormatrix, na.rm=TRUE)-1)/(length(cleandata)-1-sum(is.na(cormatrix[1,]))) ,2 ))) #Compute average correlation for each asset
  aver_cor[aver_cor == ((-1)/(length(cleandata)-1-sum(is.na(cormatrix[1,]))))] <- "-"
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


# COMPUTATIONS

# 1. Correlation heatmaps

correlation_heatmap(crypto_data, "2017-01-03", "2021-06-23", "Pearson Correlation Heatmap")  #TOTAL PERIOD
# BULL PERIODS
correlation_heatmap(crypto_data[,-17, with=FALSE], "2017-01-03", "2017-12-17", "Pearson Correlation Heatmap")  #BULL PERIOD 1. (Excluding FIL for insuficient trading days)
correlation_heatmap(crypto_data[,-21, with=FALSE], "2018-12-15", "2019-06-26", "Pearson Correlation Heatmap")  #BULL PERIOD 2. (Excluding ALGO for insuficient trading days)
correlation_heatmap(crypto_data, "2020-03-13", "2021-04-12", "Pearson Correlation Heatmap")  #BULL PERIOD 3
# BEAR PERIODS
correlation_heatmap(crypto_data, "2017-12-17", "2018-12-14", "Pearson Correlation Heatmap")  #BEAR PERIOD 1. 
correlation_heatmap(crypto_data, "2019-06-26", "2020-03-12", "Pearson Correlation Heatmap")  #BEAR PERIOD 2. 
correlation_heatmap(crypto_data, "2021-04-13", "2021-06-23", "Pearson Correlation Heatmap")  #BEAR PERIOD 3.

correlation_heatmap(bitcoin_data, "2017-01-03", "2021-06-23", "2017-2021 Total Period")  #TOTAL PERIOD
correlation_heatmap(bitcoin_data, "2017-06-01", "2017-07-01", periods$PERIOD[8])  
correlation_heatmap(bitcoin_data, "2020-01-01", "2020-04-10", periods$PERIOD[9])  
correlation_heatmap(bitcoin_data, "2020-01-16", "2020-02-15", periods$PERIOD[10])  
correlation_heatmap(bitcoin_data, "2020-01-31", "2020-03-01", periods$PERIOD[11])  
correlation_heatmap(bitcoin_data, "2020-05-18", "2020-06-17", periods$PERIOD[12])  
correlation_heatmap(bitcoin_data, "2020-11-03", "2020-12-03", periods$PERIOD[13])  
correlation_heatmap(bitcoin_data, "2021-01-06", "2021-01-26", periods$PERIOD[14])  
correlation_heatmap(bitcoin_data, "2021-03-23", "2021-04-22", periods$PERIOD[15])  
 

# 1.2. Average Correlation Tables
average_table(crypto_data, 2, 7)
average_table(bitcoin_data, 8, 15)


# 2. Moving correlation
rolling_correlation(crypto_data, 50, "BTC", "ETH")
rolling_correlation(equities_data, 50, "BTC", "Gold")
rolling_correlation(equities_data, 50, "BTC", "DowJones")
rolling_correlation(equities_data, 50, "BTC", "Nasdaq")
rolling_correlation(equities_data, 50, "BTC", "Asia")





# STEP 5: CONDUCT PRINCIPAL COMPONENT ANALYSIS (PCA) ####

# Define study periods
start_date_train = "2019-08-20"
end_date_train = "2020-08-20"
start_date_test = "2020-08-21"
end_date_test = "2021-06-30"

# Compute Correlation Matrix and clean for assets with no sufficient data
corrmatrix_pca <- data.frame(correlation_matrix_full(crypto_data, start_date_train, end_date_train))
delete_asset <- names(which(colSums(is.na(corrmatrix_pca)) == dim(corrmatrix_pca)[1]))  # Clean correlation matrix of NA variables
delete_number <- grep(delete_asset, colnames(corrmatrix_pca))
corrmatrix_pca <- corrmatrix_pca[-c(delete_number),-c(delete_number)]
crypto_cat <- as.character(crypto_category[1,-c(delete_number)])  # Select associated categories

# Compute PCA
res.pca <- prcomp(corrmatrix_pca, scale = TRUE) 

# Plot results
fviz_eig(res.pca, main = "PCA: Percentage of variances explained by each principal component")  # Percentage of explained variance

fviz_pca_var(res.pca,      #Plot VAriables PCA
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,     # Avoid text overlapping
             title = "PCA: Variables' contribution"
)

fviz_pca_biplot(res.pca, repel = TRUE,      # Plot biplot, variables and elements
                col.var = "#ff9933", # Variables color
                col.ind = "#696969",  # Individuals color
                title = "PCA: Variables and elements"
)

fviz_pca_ind(res.pca,            # Plot groups
             col.ind = crypto_cat,       # Color by groups
             palette = c("#00AFBB",  "#FC4E07", "#E7B800", "#2E9FDF"),
             addEllipses = TRUE,         # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups",
             repel = TRUE, 
             title = "PCA: Association by groups"
)

# Identify main assets by groups
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
coord.groups$distT <- sqrt(as.double(coord.groups$dist1)^2 + as.double(coord.groups$dist2)^2)

# Find minimum elements
minvalues <- coord.groups %>% rownames_to_column('name') %>% group_by(category) %>% slice(which.min(distT)) %>% column_to_rownames('name')
minvalues


## K-MEANS ANALYSIS ##
# Compute Kmeans
set.seed(123)
res.km <- kmeans(corrmatrix_pca, 3, )

# Plot results
fviz_cluster(res.km, data = corrmatrix_pca,
             palette = c("#00AFBB",  "#FC4E07", "#E7B800"), 
             geom = c("point", "text"),
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             repel = TRUE, 
             title = "PCA: Association by clusters"
)


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
km.ind.coord$distT <- sqrt(as.double(km.ind.coord$dist1)^2 + as.double(km.ind.coord$dist2)^2)

# Find minimum elements
km.minvalues <- km.ind.coord %>% rownames_to_column('name') %>% group_by(cluster) %>% slice(which.min(distT)) %>% column_to_rownames('name')
km.minvalues



# STEP 6: BUILDING PORTFOLIOS ####

portfolio1 <- subset(crypto_data, crypto_data["DATE"]>=start_date_test & crypto_data["DATE"]<=end_date_test)  #Select period and all crypto
portfolio1 <- column_to_rownames(portfolio1, "DATE")
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
names(returns)[1] <- "Portfolio all crypto"
returns["Portfolio Categories"] <- portfolio2.value[1:n-1, ncol(portfolio2.value), drop=FALSE]
returns["Portfolio Clusters"] <- portfolio3.value[1:n-1, ncol(portfolio3.value), drop=FALSE]

table.Stats(returns)
Return.annualized(returns)
maxDrawdown(returns)
PerformanceAnalytics::Return.cumulative(returns)
PerformanceAnalytics::sd.annualized(returns)
Return.annualized(returns)/PerformanceAnalytics::sd.annualized(returns)

# Ploting results
charts.PerformanceSummary(portfolio1.ret, main= "Performance Summary for All Crypto Portfolio", ylim=c(-1,10.5))
charts.PerformanceSummary(portfolio2.ret, main= "Performance Summary for Category Portfolio", ylim=c(-1,10.5), col="dark blue")
charts.PerformanceSummary(portfolio3.ret, main= "Performance Summary for Cluster Portfolio", ylim=c(-1,10.5), col="dark green")

