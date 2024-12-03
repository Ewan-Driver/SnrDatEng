fIngestData <- function() {
    
    ### Data Ingestion ###
    
    # Function to Read in CSV Files
    ReadCSVFiles <- function(FileName) {
        print(paste0('Processing...', FileName))
        readr::read_csv(paste(FilePath, FileName, sep = ""), show_col_types = FALSE, col_names = TRUE)
    }
    
    
    
    # Set File Path for csv Files
    FilePath <- 'SampleFiles/'
    
    # List all csv Files in Folder
    FileList <- list.files(FilePath, pattern = "*.csv$", full.names = F)
    
    
    # Load in csv files and assign name to data frames
    DataFramesList <- list()
    
    for (f in 1:length(FileList)) {
        
        # Set parameters
        filename <- FileList[f]
        DFName <- sub('.csv', '', filename)
        
        # Read csv Files
        DataFramesList[[DFName]] <- ReadCSVFiles(filename)
        
        # Rename variables in each dataframe for easier identification
        names(DataFramesList[[DFName]]) <- paste0(DFName, '_', names(DataFramesList[[DFName]]))
        
    }
    
    return(DataFramesList)
}