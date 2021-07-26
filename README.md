# Crypto_Correlation
WorldQuant University MScFE Capstone Project
---
### Author: 
María Guinda - mariaguinda@cherrypeaks.com

## Description
This is an R package containing a set of models (apps) that have been organized into two groups; 
* Correlation tool set: the first part consist of a correlation tool set composed of a series of functions that will allow you to analyze a series of assets: calculate the correlation matrix, paint the correlation heatmap, calculate the average correlations, obtain the historical correlation graph ... etc.
* PCA analysis and portfolio building: The second part of this code is built to analyze a correlation matrix using Principal Component Analysis. And from this build a series of portfolios that seek to be more efficient.

All models can be explored through a graphical user interface that has been built into Shiny App, no reading or writing code is required. Each of part has been included into different tabs of the App where users can interact with the models and variables. The app includes instructions which teach important concepts of these tools.


## Data
These models have been build using historical crypto assets data and some equity and gold ETFs. The data has been included in an XLSX file called Data which can be consulted for replication purposes. The working data, which is historical daily returns, is included in the first 3 tabs (CRYPTO, EQUITIES and BITCOIN). The 4th tab includes the categories list for each crypto asset, which is used for the PCA and the 5th tab includes a list of predetermined historical periods for analysis. 
* Crypto data has been extracted manually from coinmarketcap.com, for automatic pulling a paying subscription is required. 
* Equities and gold ETFs data has been extracted from yahoo! Finance website. 


## Installation and Access App
For a quick and easy access please use the Shiny App available at [Maria Guinda Shiny App](https://cherrited.shinyapps.io/Capstone_MariaGuinda/), you won't need any installation or coding skills for this. Historical data historical information is regularly updated.

For a custom use, please download the Main.R and Data.xlsx documents and add them to your own R project directory. You can modify the Data file with your own assets and use the code functions as needed. You will need a stable internet connection for this. 


## Platforms
This application was tested on Mac and Windows 10. 
The Shiny App was tested on Firefox, Chrome and Safari. 


## Comments and suggestions
For any comments or suggestions, do not hesitate to write to me at mariaguinda@cherrypeaks.com.
Thank you very much for your attention, I hope you enjoyed.

