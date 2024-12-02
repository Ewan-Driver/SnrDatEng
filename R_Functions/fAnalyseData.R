fAnalyseData <- function(TransformedData, DataFramesList) {
    
    
    AnalysisData <- list()
    
    # Distribution by Age (current age or age at death) 
    AnalysisData$Age <- TransformedData$Demographics %>% 
        distinct(patients_Id, age = TransformedData$Demographics$Age@year) %>% 
        dplyr::group_by(age) %>% 
        dplyr::summarise(n = n())
    
    # Distribution by Gender
    AnalysisData$Gender <- TransformedData$Demographics %>% 
        distinct(patients_Id, patients_GENDER) %>% 
        dplyr::group_by(patients_GENDER) %>% 
        dplyr::summarise(n = n())
    
    # Distribution by Age and Gender
    AnalysisData$AgeAndGender <- TransformedData$Demographics %>% 
        distinct(patients_Id, age = TransformedData$Demographics$Age@year, patients_GENDER) %>% 
        dplyr::group_by(age, patients_GENDER) %>% 
        dplyr::summarise(n = n(), .groups = 'keep')
    
    
    # Encounters per Patient
    AnalysisData$EncPerPatient <- TransformedData$MergedData %>% 
        distinct(patients_Id, encounters_Id) %>% 
        dplyr::group_by(patients_Id) %>% 
        dplyr::summarise(n = length(unique(encounters_Id)))
    
    # Conditions
    AnalysisData$Diagnosis <- DataFramesList$conditions %>% 
        dplyr::group_by(conditions_DESCRIPTION) %>% 
        dplyr::summarise(n = n())
    
    # Conditions And Meds
    AnalysisData$DiagnosisMeds <- DataFramesList$conditions %>% 
        dplyr::left_join(DataFramesList$medications, by = c('conditions_ENCOUNTER' = 'medications_ENCOUNTER')) %>% 
        dplyr::group_by(conditions_DESCRIPTION, medications_DESCRIPTION) %>% 
        dplyr::summarise(n = n(),
                         Dispenses = sum(medications_DISPENSES),
                         DrugCost = sum(medications_BASE_COST), 
                         .groups = 'keep')    

    # Medications
    AnalysisData$Medications <- DataFramesList$medications %>% 
        dplyr::group_by(medications_REASONDESCRIPTION) %>% 
        dplyr::summarise(n = n(),
                         Dispenses = sum(medications_DISPENSES),
                         DrugCost = sum(medications_BASE_COST), 
                         .groups = 'keep')
    
    # Procedures
    AnalysisData$Procedures <- DataFramesList$procedures %>% 
        dplyr::group_by(procedures_DESCRIPTION) %>% 
        dplyr::summarise(n = n())
    
    
    # Statistical Analysis
    MeanAge <- mean(TransformedData$Demographics$Age@year)
    MedianAge <- median(TransformedData$Demographics$Age@year)
    StdDev <- sd(TransformedData$Demographics$Age@year)
    SummaryStats <- summary(TransformedData$Demographics$Age@year)
    
    # Data Coverage
    min(TransformedData$MergedData$encounters_START)
    max(TransformedData$MergedData$encounters_START)
    
    
    return(AnalysisData)
}