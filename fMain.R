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

fLoadPackages('tidyverse')


# Dependancies
source('R/fIngestData.R')
# source('R/fTransformData.R')
# source('R/fAnalyseData.R')


# Data Ingestion
DataFramesList <- fIngestData()
    
# Data Transformation

