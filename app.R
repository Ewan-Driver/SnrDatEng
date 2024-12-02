
source('fMain.R')


# UI ----
ui <- 
    dashboardPage(
    
        dashboardHeader(title = 'Senior Data Engineer Interview Tasks', titleWidth = '400px'),
    
        dashboardSidebar(collapsed = TRUE,
                         includeCSS("./www/style.css")),                
                        
        dashboardBody(
            fluidRow(
                box(
                    column(width = 3, 
                           sliderInput('ageinput', 'Select Patient Ages to Display', min = min(Data$ActAge), max = max(Data$ActAge), value = c(min(Data$ActAge),max(Data$ActAge))),
                           ),
                    column(width = 3,
                           checkboxGroupInput('gender', 'Gender', choices = c('Male', 'Female'), inline = TRUE, selected = c('Male', 'Female')),
                           ),
                    column(width = 3,
                           selectInput('condition', 'Select Medical Condition', choices = c('All', AnalysisData$Diagnosis$conditions_DESCRIPTION[AnalysisData$Diagnosis$n > 200]))
                    )
                )
            ),
            
            fluidRow(
                column(width = 12,
                       h3(style = 'margin-left: 5%; color: #fff;', 'Population Distribution and Medications Information'))
            ),
            
            fluidRow(
                box(
                    column(width = 8,
                           plotOutput(('poppyramid'))     
                    ),
                    column(width = 4,
                           dataTableOutput('medications')     
                    )
                )       
            ),

            fluidRow(
                column(width = 12,
                       h3(style = 'margin-left: 5%; color: #fff;', 'Geographical Patient Distribution'))
            ),
            
            fluidRow(
                box(
                    column(width = 12,
                           leafletOutput('map'))
                )
            )
        )
)






# SERVER ----
server <- function(input, output, session) {

    
    # Load Data ----
    # ====================================================
    Data <- TransformedData$MergedData %>% 
        dplyr::group_by(patients_Id, longitude = patients_LON, latitude = patients_LAT, patients_CITY, patients_RACE, ActAge = Age@year, AGE = AgeGroup,
                        patients_GENDER, conditions_DESCRIPTION) %>% 
        dplyr::summarise(n = n(), .groups = 'keep')
    
    mapData <- reactiveVal(Data)
    
    popData <- reactiveVal(
        Data
    )
    
    
    MedicationsData <- TransformedData$MergedData %>% 
        dplyr::filter(!is.na(medications_DESCRIPTION)) %>% 
        dplyr::select(conditions_DESCRIPTION, medications_DESCRIPTION, medications_DISPENSES, medications_BASE_COST, patients_RACE, 
                      ActAge = Age, AGE = AgeGroup, patients_GENDER) %>% 
        dplyr::mutate(ActAge = ActAge@year)
    
    MedsData <- reactiveVal(MedicationsData)
    
    # Set color palette
    pal <- leaflet::colorFactor(c('#D7153A', '#002664'), levels = c('M', 'F'))
    
    # Set Column Names
    ColumnNames <- c(' ', 'Patient Encounters', 'Medications Dispensed', 'Associated Costs')
    
    

    # Outputs ----
    # ====================================================
    observe({
    

    output$poppyramid <- renderPlot({
        
        if(input$condition == 'All') {
            
            popData() %>% 
                dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1)) %>%
                dplyr::mutate(Gender = dplyr::case_when(patients_GENDER == 'F' ~ 'Female', T ~ 'Male')) %>% 
                dplyr::group_by(`AGE GROUP` = AGE, Gender) %>%      
                dplyr::summarise(Count = n(), .groups = 'keep') %>%
                dplyr::mutate(Count = dplyr::case_when(Gender == 'Female' ~ -Count, T ~ Count),
                              Legend = paste0(Gender)) %>%
                ggplot(aes(x = Count, y = `AGE GROUP`, fill = Gender)) + 
                geom_col() + 
                scale_fill_manual(values = c('#002664', '#D7153A')) +
                theme_minimal() + 
                theme(legend.position = 'right') + 
                theme(panel.grid = element_blank(), axis.text.x = element_blank())
            
        } else {
            
            popData() %>% 
                dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1)) %>% 
                dplyr::mutate(Category = dplyr::case_when(conditions_DESCRIPTION != input$condition | is.na(conditions_DESCRIPTION) ~ 'Without Selected Condition',
                                                          T ~ 'With Selected Condition'),
                              Gender = dplyr::case_when(patients_GENDER == 'F' ~ 'Female', T ~ 'Male'),
                              Legend = paste0(Gender, ' ', Category)) %>% 
                dplyr::group_by(`AGE GROUP` = AGE, Gender, Legend) %>%
                dplyr::summarise(Count = n(), .groups = 'keep') %>%
                dplyr::mutate(Count = dplyr::case_when(Gender == 'Female' ~ -Count, T ~ Count)) %>%
                ggplot(aes(x = Count, y = `AGE GROUP`, fill = Legend)) + 
                geom_col() + 
                scale_fill_manual(values = c('#002664', '#00266490', '#D7153A', '#D7153A90')) +
                theme_minimal() + 
                theme(legend.position = 'right') + 
                theme(panel.grid = element_blank(), axis.text.x = element_blank())
        }
    })

    
    output$medications <- renderDataTable({
        
        if (input$condition == 'All') {
            
            MedsData(MedicationsData %>% dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1)))

            DT::datatable(MedsData() %>% 
                              dplyr::group_by(patients_GENDER) %>%
                              dplyr::mutate(patients_GENDER = dplyr::case_when(patients_GENDER == 'M' ~ '<img src="img/Male.png" height = "32px">',
                                                                               T ~ '<img src="img/Female.png" height = "32px">')) %>% 
                              dplyr::summarise(Patients = n(),
                                               Dispenses = sum(medications_DISPENSES),
                                               DrugCost = paste('$',formatC(round(sum(medications_BASE_COST, 0)), big.mark=',', format = 'f', digits = 0)), 
                                               .groups = 'keep') %>% 
                              dplyr::arrange(desc(DrugCost)),
                          
                          colnames = ColumnNames,
                          rownames = FALSE,
                          escape = FALSE,
                          options = list(
                              dom = 't'
                          ))
            
        } else {
            
            MedsData(MedicationsData %>% dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1) & 
                                                           conditions_DESCRIPTION == input$condition))
            
             DT::datatable(MedsData() %>% 
                               dplyr::group_by(patients_GENDER) %>%
                               dplyr::mutate(patients_GENDER = dplyr::case_when(patients_GENDER == 'M' ~ '<img src="img/Male.png" height = "32px">',
                                                                                T ~ '<img src="img/Female.png" height = "32px">')) %>% 
                               dplyr::summarise(Patients = n(),
                                                Dispenses = sum(medications_DISPENSES),
                                                DrugCost = paste('$',formatC(round(sum(medications_BASE_COST, 0)), big.mark=',', format = 'f', digits = 0)), 
                                                .groups = 'keep') %>% 
                               dplyr::arrange(desc(DrugCost)),
                           
                           colnames = ColumnNames,
                           rownames = FALSE,
                           escape = FALSE,
                           options = list(
                               dom = 't'
                           ))
        }
    })
        
    

    output$map <- renderLeaflet({
        
        if (input$condition == 'All') {
            mapData(Data %>% dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1)))
            
        mapData() %>% 
            leaflet::leaflet() %>% 
            leaflet::addTiles() %>% 
            leaflet::addCircleMarkers(data = mapData(), 
                                      lng = mapData()$longitude,
                                      lat = mapData()$latitude,
                                      popup = paste0('City: ', mapData()$patients_CITY, 
                                                     '<br>Age: ', mapData()$ActAge, ' years old',
                                                     '<br>Gender: ', mapData()$patients_GENDER),
                                      fillColor = ~pal(mapData()$patients_GENDER),
                                      fillOpacity = 1,
                                      radius = 1,
                                      color = ~pal(mapData()$patients_GENDER)) %>% 
            setView(-72.05206, 42.36108, zoom = 8)
        
        } else {
            
            mapData(Data %>% dplyr::filter(ActAge >= input$ageinput[1] & ActAge <= input$ageinput[2] & patients_GENDER %in% str_sub(input$gender,1,1) & 
                                               conditions_DESCRIPTION == input$condition))
            mapData() %>% 
                leaflet::leaflet() %>% 
                leaflet::addTiles() %>% 
                leaflet::addCircleMarkers(data = mapData(), 
                                          lng = mapData()$longitude,
                                          lat = mapData()$latitude,
                                          popup = paste0('City: ', mapData()$patients_CITY, 
                                                         '<br>Age: ', mapData()$ActAge, ' years old',
                                                         '<br>Gender: ', mapData()$patients_GENDER),
                                          fillColor = ~pal(mapData()$patients_GENDER),
                                          fillOpacity = 1,
                                          radius = 1,
                                          color = ~pal(mapData()$patients_GENDER)) %>% 
                setView(-72.05206, 42.36108, zoom = 8)
            
        }
    })
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
