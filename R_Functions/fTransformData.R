fTransformData <- function(DataFramesList) {
    
    TransformedData <- list()

    
    # Calculate Age/Age at Death/Age at Encounter
    TransformedData$Demographics <- DataFramesList$patients %>% 
        dplyr::mutate(Age = dplyr::case_when(is.na(patients_DEATHDATE) ~ lubridate::as.period(interval(start = patients_BIRTHDATE, end = Sys.Date())),
                                           T ~ lubridate::as.period(interval(start = patients_BIRTHDATE, end = patients_DEATHDATE))),
                      AgeGroup = dplyr::case_when(Age@year > 90 ~ '90+',
                                                  Age@year > 79 ~ '80-89',
                                                  Age@year > 69 ~ '70-79',
                                                  Age@year > 59 ~ '60-69',
                                                  Age@year > 49 ~ '50-59',
                                                  Age@year > 39 ~ '40-49',
                                                  Age@year > 29 ~ '30-39',
                                                  Age@year > 19 ~ '20-29',
                                                  T ~ '<20')) %>% 
        
        dplyr::relocate(c(Age, AgeGroup), .before = patients_SSN)
    
    
    # Create Merged Data Set
    TransformedData$MergedData <- TransformedData$Demographics %>% 
        dplyr::left_join(DataFramesList$encounters, by = c('patients_Id' = 'encounters_PATIENT'), relationship = 'one-to-many') %>% 
        dplyr::left_join(DataFramesList$organizations, by = c('encounters_ORGANIZATION' = 'organizations_Id'), relationship = 'many-to-one') %>% 
        dplyr::left_join(DataFramesList$conditions, by = c('patients_Id' = 'conditions_PATIENT', 'encounters_Id' = 'conditions_ENCOUNTER'), relationship = 'one-to-many') %>% 
        dplyr::left_join(DataFramesList$procedures, by = c('encounters_Id' = 'procedures_ENCOUNTER'), relationship = 'many-to-many') %>% 
        dplyr::left_join(DataFramesList$medications, by = c('encounters_Id' = 'medications_ENCOUNTER'), relationship = 'many-to-many')

    
    return(TransformedData)
}

