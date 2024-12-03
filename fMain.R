# Load Libraries/Packages
fLoadPackages <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(X = libs, FUN = require, character.only=TRUE))
    need <- libs[req==FALSE]
    
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}

fLoadPackages('tidyverse', 'leaflet', 'shiny', 'DT', 'shinydashboard', 'scales')


# Dependancies
source('R_Functions/fIngestData.R')
source('R_Functions/fTransformData.R')
source('R_Functions/fAnalyseData.R')


# Data Ingestion
cat(paste0('Ingesting the Data', '\n'))
DataFramesList <- fIngestData()

# Data Transformation
cat(paste0('Transforming the Data', '\n'))
TransformedData <- fTransformData(DataFramesList)

# Exploartory Analysis
cat(paste0('Preparing the data for use in the App', '\n'))
AnalysisData <- fAnalyseData(TransformedData, DataFramesList)
