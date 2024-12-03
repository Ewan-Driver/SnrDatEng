fAnalyseData <- function(TransformedData, DataFramesList) {
    
    
    AnalysisData <- list()
    
    
    # Statistical Analysis
    AnalysisData$MeanAge      <- mean(TransformedData$MergedData$Age@year)
    AnalysisData$MedianAge    <- median(TransformedData$MergedData$Age@year)
    AnalysisData$StdDev       <- sd(TransformedData$MergedData$Age@year)
    AnalysisData$SummaryStats <- summary(TransformedData$MergedData$Age@year)
    
    # Data Coverage
    AnalysisData$StartDate <- min(TransformedData$MergedData$encounters_START)
    AnalysisData$EndDate   <- max(TransformedData$MergedData$encounters_START)
    
    
    
    
    # Distribution by Age (current age or age at death) 
    AnalysisData$Age <- TransformedData$MergedData %>% 
        distinct(patients_Id, age = TransformedData$MergedData$Age@year) %>% 
        dplyr::group_by(age) %>% 
        dplyr::summarise(n = n())
    
    # Distribution by Gender
    AnalysisData$Gender <- TransformedData$MergedData %>% 
        distinct(patients_Id, patients_GENDER) %>% 
        dplyr::group_by(patients_GENDER) %>% 
        dplyr::summarise(n = n())
    
    # Distribution by Age and Gender
    AnalysisData$AgeAndGender <- TransformedData$MergedData %>% 
        distinct(patients_Id, age = TransformedData$MergedData$Age@year, patients_GENDER) %>% 
        dplyr::group_by(age, patients_GENDER) %>% 
        dplyr::summarise(n = n(), .groups = 'keep')
    
    
    # Encounters per Patient
    AnalysisData$EncPerPatient <- TransformedData$MergedData %>% 
        distinct(patients_Id, encounters_Id) %>% 
        dplyr::group_by(patients_Id) %>% 
        dplyr::summarise(n = length(unique(encounters_Id)))
    
    
    
    
    # Final Data for Analysis and Visualisation
    FinalData <- TransformedData$MergedData %>% 
        dplyr::group_by(encounters_Id, TreatmentDescription, encounters_START, encounters_REASONDESCRIPTION, encounters_TOTAL_CLAIM_COST, 
                        conditions_DESCRIPTION,
                        procedures_DESCRIPTION, procedures_BASE_COST, procedures_REASONDESCRIPTION, 
                        medications_DESCRIPTION, medications_BASE_COST, medications_TOTALCOST, medications_REASONDESCRIPTION, 
                        careplans_DESCRIPTION, careplans_REASONDESCRIPTION, 
                        patients_Id, AgeGroup, ActAge = Age@year, patients_GENDER,
                        organizations_NAME, organizations_LAT, organizations_LON, organizations_REVENUE) %>% 
        dplyr::summarise(n = n(), .groups = 'keep')
    
    
    AnalysisData$Top10Treatments <- FinalData %>% 
        dplyr::group_by(TreatmentDescription) %>% 
        dplyr::summarise(n = n(),
                         Costs = sum(procedures_BASE_COST, na.rm = TRUE) + sum(medications_TOTALCOST, na.rm = TRUE)) %>% 
        dplyr::arrange(desc(Costs)) %>% 
        dplyr::filter(!is.na(TreatmentDescription)) %>% 
        head(10)  
    
    
    AnalysisData$FinalData <- FinalData %>% 
        dplyr::mutate(Condition = dplyr::case_when(TreatmentDescription %in% AnalysisData$Top10Treatments$TreatmentDescription ~ TreatmentDescription,
                                                   T ~ 'Other'))
    
    
    return(AnalysisData)
}
