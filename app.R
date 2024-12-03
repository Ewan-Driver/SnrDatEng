
source('fMain.R')


# UI ----
ui <- 
    dashboardPage(
        
        dashboardHeader(title = 'Senior Data Engineer Interview Tasks', titleWidth = '400px'),
        
        dashboardSidebar(collapsed = TRUE,
                         includeCSS("./www/style.css")),                
        
        dashboardBody(
            # Filters
            fluidRow(
                box(
                    column(width = 3, 
                           sliderInput('ageinput', 'Patient Age at Encounter', min = 0, max = 100, value = c(0,100)),
                    ),
                    column(width = 3,
                           checkboxGroupInput('gender', 'Gender', choices = c('Male', 'Female'), selected = c('Male', 'Female'), inline = TRUE),
                    ),
                    column(width = 3,
                           selectInput('condition', 'Select Medical Condition', choices = NULL)
                    ),
                    column(width = 3,
                           actionButton('applyfilters', 'Apply Filters')
                    )
                )
            ),
            
            
            fluidRow(
                column(width = 12,
                       h3(style = 'margin-left: 5%; color: #000;', 'Medical Conditions Over Time and Associated Costs'))
            ),
            
            # Graph and Table
            fluidRow(
                box(
                    column(width = 8,
                           shinycssloaders::withSpinner(plotOutput('poppyramid'))    
                    ),
                    column(width = 4,
                           shinycssloaders::withSpinner(dataTableOutput('medications'))    
                    )
                )       
            ),
            
            fluidRow(
                column(width = 12,
                       h3(style = 'margin-left: 5%; color: #000;', 'Healthcare Facilities'))
            ),
            # Map
            fluidRow(
                box(
                    column(width = 12,
                           shinycssloaders::withSpinner(leafletOutput('map'))
                    )
                )
            )
        )
    )






# SERVER ----
server <- function(input, output, session) {
    
    
    # Load Data ----
    # ====================================================
    
    Data <- AnalysisData$FinalData
    FilteredData <- reactiveVal(Data)
    
    
    # Set Column Names
    ColumnNames <- c(' ', 'Unique Patients', 'Patient Encounters', 'Medication Costs', 'Procedure Costs')
    
    
    
    # Update Inputs
    # ====================================================
    
    observe ({
        updateSelectInput(session, 'condition', choices = c('All', unique(Data$Condition)))
        updateSliderInput(session, 'ageinput', 
                          min = min(Data$ActAge, na.rm = T), 
                          max = max(Data$ActAge, na.rm = T), 
                          value = c(min(Data$ActAge, na.rm = T),max(Data$ActAge, na.rm = T)))
        updateCheckboxGroupInput(session, 'gender', choices = c('Male', 'Female'), selected = c('Male', 'Female'))
    })
    
    
    # Observe Filters
    # ====================================================
    
    observeEvent(input$applyfilters, {
        
        showModal(modalDialog('Loading....'))
        if (input$condition == 'All') {
            Data <- Data %>% 
                dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1))
            
        } else {
            Data <- Data %>% 
                dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1) & Condition == input$condition)
        }
        
        FilteredData(Data)
        removeModal()
    })
    
    
    
    # Outputs ----
    # ====================================================
    observe({
        
        # Graph Output
        output$poppyramid <- renderPlot({     
            
            FilteredData() %>%  
                dplyr::group_by(Condition, Year = year(encounters_START)) %>% 
                dplyr::summarise(n = n(),
                                 Procedure_Costs = sum(procedures_BASE_COST, na.rm = TRUE),
                                 Medication_Costs = sum(medications_TOTALCOST, na.rm = TRUE),
                                 .groups = 'keep') %>% 
                
                
                ggplot(aes(x = Year, y = Medication_Costs + Procedure_Costs, fill = Condition)) + 
                geom_bar(stat = 'identity') + 
                labs(x = 'Year of Encounter', y = 'Associated Medication + Procedure Costs', fill='Top 10 Medical Conditions') + 
                scale_y_continuous(labels = scales::unit_format(unit = "M $", scale = 1e-6)) + 
                theme_minimal()
            
        })
        
        
        
        # Table Output    
        output$medications <- renderDataTable({
            
            FilteredData() %>% 
                dplyr::group_by(patients_GENDER) %>% 
                dplyr::mutate(patients_GENDER = dplyr::case_when(patients_GENDER == 'M' ~ '<img src="img/Male.png" height = "32px">',
                                                                 T ~ '<img src="img/Female.png" height = "32px">')) %>% 
                dplyr::summarise(`Unique Patients` = length(unique(patients_Id)),
                                 `Patient Encounters` = length(encounters_Id),
                                 `Medication Costs` = paste('$',formatC(round(sum(medications_TOTALCOST, na.rm = T), 0), big.mark=',', format = 'f', digits = 0)),
                                 `Procedure Costs` = paste('$',formatC(round(sum(procedures_BASE_COST, na.rm = T), 0), big.mark=',', format = 'f', digits = 0)),
                                 .groups = 'keep') %>% 
                
                DT::datatable(colnames = ColumnNames,
                              rownames = FALSE,
                              escape = FALSE,
                              options = list(
                                  dom = 't'
                              ))
        })
        
        
        
        
        
        
        # Map Output
        output$map <- renderLeaflet({
            
            mapData <- FilteredData() %>%
                dplyr::group_by(organizations_NAME, organizations_LON, organizations_LAT) %>% 
                dplyr::summarise(Encounters = length(encounters_Id),
                                 MedicationCosts = paste('$',formatC(round(sum(medications_TOTALCOST, na.rm = T), 0), big.mark=',', format = 'f', digits = 0)),
                                 ProcedureCosts = paste('$',formatC(round(sum(procedures_BASE_COST, na.rm = T), 0), big.mark=',', format = 'f', digits = 0)),
                                 .groups = 'keep')
            
            # Set color palette
            pal <- leaflet::colorNumeric(palette = c("#002664", "#D7153A"), domain = unique(mapData$Encounters))
            
            leaflet::leaflet(data = mapData) %>%
                leaflet::addTiles() %>%
                leaflet::addCircleMarkers(lng = mapData$organizations_LON,
                                          lat = mapData$organizations_LAT,
                                          popup = paste0('Facility: ', mapData$organizations_NAME,
                                                         '<br>Encounters: ', mapData$Encounters,
                                                         '<br>Medication Costs: ', mapData$MedicationCosts,
                                                         '<br>Procedure Costs: ', mapData$ProcedureCosts),
                                          fillColor = ~pal(mapData$Encounters),
                                          fillOpacity = 1,
                                          radius = sqrt(mapData$Encounters)/5,
                                          color = ~pal(mapData$Encounters)) %>%
                leaflet::addLegend("topright", pal = pal, values = mapData$Encounters, title = 'Encounters per Facility', opacity = 1) %>% 
                setView(-71.85206, 42.16108, zoom = 8)
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
