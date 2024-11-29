fTransformData <- function(DataFramesList) {
    
    TransformedData <- list()

    
    # Calculate Age/Age at Death/Age at Encounter
    TransformedData$Demographics <- DataFramesList$patients %>% 
        dplyr::mutate(Age = dplyr::case_when(is.na(patients_DEATHDATE) ~ lubridate::as.period(interval(start = patients_BIRTHDATE, end = Sys.Date())),
                                           T ~ lubridate::as.period(interval(start = patients_BIRTHDATE, end = patients_DEATHDATE)))) %>% 
        dplyr::relocate(Age, .before = patients_SSN)
    
    
    # Create Merged Data Set
    TransformedData$MergedData <- TransformedData$Demographics %>% 
        dplyr::left_join(DataFramesList$encounters, by = c('patients_Id' = 'encounters_PATIENT'), relationship = 'one-to-many') %>% 
        dplyr::left_join(DataFramesList$organizations, by = c('encounters_ORGANIZATION' = 'organizations_Id'), relationship = 'many-to-one') %>% 
        dplyr::left_join(DataFramesList$conditions, by = c('patients_Id' = 'conditions_PATIENT', 'encounters_Id' = 'conditions_ENCOUNTER'), relationship = 'one-to-many') %>% 
        dplyr::left_join(DataFramesList$procedures, by = c('encounters_Id' = 'procedures_ENCOUNTER'), relationship = 'many-to-many') %>% 
        dplyr::left_join(DataFramesList$medications, by = c('encounters_Id' = 'medications_ENCOUNTER'), relationship = 'many-to-many')

    
    return(TransformedData)
}

