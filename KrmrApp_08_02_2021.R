library(shiny)
library(datasets)
library(tidyverse)
library(stringi)
library(dplyr)
library(data.table)
library(ggplot2)

#IF INTERACTIVE OPTION
if (interactive()) {
  
  #Setting work directory to the folder containing required .tsv files
  setwd("~/KD_work_directory")
  
  #Reading datatables from .tsv files
  require(data.table) #require space for the large .csv file
  data1 <- as.data.frame(fread("Random_LabValuesInfo_2020.tsv"))
  data2 <- as.data.frame(fread("Random_PatientLevelInfo_2020.tsv"))

  
##############= CALCULATIONS =##############
  
  # The purpose is to define exact number of different nationalities in the (.tsv) file,
  # Below procedure ensures automatic updating of the nationalities 
  # While the another patient would be added into the .tsv raw file, their nationality would be automatically added into the sctipt
  
  
  ##= NATIONALITIES =##
  #extracting vector containing nationalities from the data1("Random_LabValuesInfo_2020.tsv") list:
  vector_nationality = as.vector(data1[[2]])
  #extracting 3-letter code of the nationality, e.g. "BRA", "CHN" from the "vector_nationality"
  variable_nationality = substr(vector_nationality, start = 9, stop = 11)
  #extracting only unique values of nationality
  variable_nationality_unique = unique(variable_nationality)
  
  ##= UNIQUE ID's =##
  #extracting vector containing nationalities linked with their ID's
  variable_nationality_plus_id = substring(vector_nationality, 9)
  #extracting only unique values of nationalities linked with their ID's
  variable_nationality_plus_id_unique = unique(variable_nationality_plus_id)
  
  #Specifying the number od unique nationalities provided in the .tsv file
  kk <- 1:length(variable_nationality_unique)
  
  # This the the list() contains row numbers (positions) of each nationality specified in the .tsv file
  position <- list()
  for (i in kk) {
                 position[[i]] <- c(grep(variable_nationality_unique[i], variable_nationality))
                }
  
  #Values of 'BMRKR1' by nationality of patients
  vector_biomarker = as.vector(data1[[3]])
  biomarker <- list()
  for (i in kk) {
                 biomarker[[i]] <- c(vector_biomarker[min(c(position[[i]])):max(c(position[[i]]))])
                }
  
  #Average biomarker by nationality
  average_biomarker <- list()
  for (i in kk) {
                 average_biomarker[i] <- as.vector(mean(biomarker[[i]]))
                }
  
  
  ##Patient ID's included in each nationality
  
  #Splitting of the vector (from data1) contaning mixed data into separated nationality and id
  vector_nationality_ids_splited <- str_split(variable_nationality_plus_id, "-", simplify = TRUE)
  
  #Concatenate only patient id and nationality
  unrepeatable_patient <- paste(as.vector(vector_nationality_ids_splited[,1]),as.vector(vector_nationality_ids_splited[,4]), sep = "")
  
  #Extracting unrepeatable patients with its id by their nationality
  patient_id_by_nationality <- list()
  for (i in kk) {
                 patient_id_by_nationality[[i]] <- as.numeric(
                        c(str_sub(unique(unrepeatable_patient[min(c(position[[i]])):max(c(position[[i]]))]), 4))
                                                              )
                }
  

  #List of a list containing biomarker by unique id
  #vector_aval <- as.vector(data1$AVAL)
  #aval_list <- list()
  #id_len <- list()
  #for (i in kk) {
          #id_len[[i]] <- as.vector(seq(length(c(patient_id_by_nationality[[i]])))) #up to this line, the code is correct
          #for (ii in id_len[[i]]) {
                           #aval_list[[i]][ii] <- c(vector_aval[match(patient_id_by_nationality[[i]][ii],vector_nationality_ids_splited[,4])])
                                  #}  
                #}
  ### the problem with an iteration over a two variables [[i]] and [ii] - to be resolved ###
     
 
## Creating a vector containing days of the treatment
  days1 <- replace(as.vector(data1$AVISIT), data1$AVISIT == "SCREENING", "WEEK 0 DAY 1")
  days2 <- replace(days1, days1 == "BASELINE", "NA")
    
#updating major data.frame by dplyr operations   
    data_x <- data1 %>%
                       mutate(NAT = vector_nationality_ids_splited[,1]) %>%
                       mutate(ID = vector_nationality_ids_splited[,4]) %>%
                       mutate(DAYS = c(days2)) %>%
                       select(NAT, ID, BMRKR1, BMRKR2, LBTESTCD, LBTEST, LBCAT, AVAL, AVALU, DAYS)

 #update_1: 09.02.2021
 ## Merging data from data.frames 'data1' and 'data2':
    #Spliting string which contains patient id and nationality:
    data2_splited_id <- as.data.frame(str_split(data2$USUBJID, "-", simplify = TRUE))
    
    #adding SEX column:
    add_sex_column <- data_x %>% 
                                mutate(VAR1 = "X") %>%
                                select(VAR1)
    
    vector_SEX <- replace(add_sex_column$VAR1, add_sex_column$VAR1 == "X", data2$SEX[match(data_x$ID, data2_splited_id$V5)]) #A second condition should be added to match() simlar ID & similar NAT

    #adding AGE column:
    add_age_column <- data_x %>% 
      mutate(VAR1 = "Y") %>%
      select(VAR1)
    
    vector_AGE <- replace(add_age_column$VAR1, add_age_column$VAR1 == "Y", data2$AGE[match(data_x$ID, data2_splited_id$V5)]) #A second condition should be added to match() simlar ID & similar NAT
    
    #Updated data.frame data_y:
    data_y <- data_x %>%
                        mutate(SEX = vector_SEX, AGE = vector_AGE)
    
######################################################
##################### SHINY APP ######################
    
### USER INTERFACE
ui <- shinyUI(
        fluidPage(
          titlePanel(tags$h1("Data analysis of patients laboratory results")),
              tabsetPanel(
                          #First Tab Panel
                          tabPanel("Biomarker by Nationality",
                                   titlePanel("Select nationality"),
                                   sidebarLayout(
                                           sidebarPanel(
                                                        selectInput('select_nationality',
                                                                    'Select patient nationality',
                                                                    choices = variable_nationality_unique,
                                                                    selected = variable_nationality_unique[1]
                                                                    ),
                                                        width = 3
                                                        ),
                                           mainPanel(plotOutput('MyPlot'),  
                                                     fluidRow(column(12,
                                                                     align = "center",
                                                                     wellPanel(
                                                                               textOutput('Text1')
                                                                              )
                                                                    )
                                                              )
                                                     )
                                                )
                                  ),
                          
                          #Second Tab Panel
                          tabPanel("Raw tabular data",
                                   tableOutput('contents')
                                  ),
                          
                          #Third Tab Panel
                          tabPanel("Browser",
                                   titlePanel("Browse patient history by unique ID"),
                                   sidebarLayout(
                                                 sidebarPanel(
                                                             # "Empty inputs" - they will be updated after the data is uploaded
                                                             selectInput(inputId = "select_nationality_2",
                                                                         label = "Select patient nationality",
                                                                         choices = variable_nationality_unique,
                                                                         selected = variable_nationality_unique[1]
                                                                         ),
                                                             selectInput(inputId = "select_id",
                                                                         label = "Select patient id",
                                                                         choices = ""
                                                                         ),
                                                             selectInput(inputId = "select_labtest",
                                                                         label = "Select laboratory test",
                                                                         choices = ""
                                                                         ),
                                                             actionButton(inputId = "click_button",
                                                                          label = "Plot histogram")
                                                              ),
                                                 mainPanel(
                                                   plotOutput('MyPlot2'), 
                                                   fluidRow(
                                                            column(12,
                                                                   align = "center",
                                                                   wellPanel(
                                                                              textOutput('Text2')
                                                                            )
                                                                  )
                                                            ),
                                                   fluidRow(
                                                            column(12,
                                                                   align = "center",
                                                                   tableOutput('reactive_tab')
                                                                  )
                                                           )
                                                          )
                                                )
                                  )
                          
                         )
                 )
              )


### SERVER
server <- shinyServer(function(input, output, session) {

  ### Outputs for First Tab "Biomarker by Nationality":
    
    #Reactive function for extracting 'biomarker' from filtered by nationality
    biomarker_by_nationality <- reactive({
                                          biomarker[[match(input$select_nationality,variable_nationality_unique)]]
                                         }) 
    

    #Creation of a data.frame which contains data for histogram
    dff_1 <- reactive(
                      filter(data_x, 
                             NAT == input$select_nationality
                            )
                     )
    
    dff_2 <- reactive(
                      select(dff_1(), BMRKR1, ID)
                     )

    # Histogram by ggplot2        
    gg_1 <- reactive(
                     ggplot(
                            dff_2(), aes(BMRKR1)
                           ) +
                            geom_bar(aes(fill = ID), width = 0.3) +
                            theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
                            labs(title = "Histogram on Biomarker variable",
                                 subtitle = "with specified unique patient ID") 
                    )

    #Output
      output$MyPlot <- renderPlot({
                                   plot(
                                        gg_1()
                                       )
                                    #hist( biomarker_by_nationality(), 
                                    #      col = "#75AADB", 
                                    #      border = 'white', 
                                    #      xlab = "Value of 'BMRKR1'", 
                                    #      main = "Histogram by Nationalities"
                                    #    )
                                  })
      
    #Reactive function - calculation of an average biomarker for each nationality group 
    average_biomarker_by_nationality <- reactive({
                                                  average_biomarker[match(input$select_nationality,variable_nationality_unique)]
                                                })
    
    #Output
    output$Text1 <- renderText({
                                paste("Average biomarker 'BMRKR1' for the ",
                                      input$select_nationality,
                                      "nationality is equal to",
                                      average_biomarker_by_nationality(),
                                      "units."
                                      )
                              })
    
      
  ### Outputs for Second Tab "Tabular data":
      output$contents <- renderTable(
                                     {data1}
                                    )
      
      
  ### Outputs for Third Tab "Browser":
      
      #Reactive function - extracting unique patient id for each nationality separately
      id_by_nationality <- reactive({ 
                                     c(patient_id_by_nationality[match(input$select_nationality_2,variable_nationality_unique)])
                                    })
      
      #Updating of selectInput 'select_id' based on the choice in selectInput 'select_nationality_2'
      observe({
               choice_of_input_1 <- input$select_nationality_2
                
               updateSelectInput(session, "select_id",
                                 label = "Select patient unique ID",
                                 choices = c(patient_id_by_nationality[[match(choice_of_input_1,variable_nationality_unique)]]),
                                 selected = NULL
                                 )
              })
      
      #Output - text message  
      output$Text2 <- renderText({
                                  paste("Browsing patient from ",
                                        input$select_nationality_2,
                                        "with an individual ID:",
                                        input$select_id,
                                        "."
                                        )
                                })
      
      #Laboratory test for individual patients nationality and ID
      lab_test_by_nat_id <- reactive(
                                     unique(filter(data_x, 
                                                   NAT == input$select_nationality_2,
                                                   ID == input$select_id
                                                  )$LBTEST
                                           )
                                    )
      
      #Results of the specified laboratory test, for individual patient id and nationality
      lab_test_results_by_nat_id <- reactive(
                                             filter(data_x, 
                                                    NAT == input$select_nationality_2,
                                                    ID == input$select_id,
                                                    LBTEST == input$select_labtest
                                                    )$AVAL   
                                            )
      #X-axis length for AVAL plot
      x_axis_sequence <- reactive(
                                  seq(lab_test_results_by_nat_id())
                                 )
      
      #Updating of selectInput 'select_labtest' based on the choice in selectInput 'select_id'
      observe({
        choice_of_input_id <- input$select_id
        
        updateSelectInput(session, "select_labtest",
                          label = "Select laboratory test",
                          choices = lab_test_by_nat_id(),
                          selected = NULL
                         )
            })
      
      #Creating new Reactive data.frame 'data_R' which contain only important columns
      data_R <- reactive(
                         filter(data_x, 
                                NAT == input$select_nationality_2,
                                ID == input$select_id,
                                LBTEST == input$select_labtest
                                )
                        )
      
      #EventReactive button - TRIGGER - after clicking, the histogram will be plotted
      aval_plot <- eventReactive(
                            input$click_button, {
                                                 data_R()$AVAL 
                                                }
                                )
      

      #Creation of a data.frame as an input for the ggplot 
      #It is done in 3 steps, since I faced some issues with a pipe operator %>% within a 'reactive()' function
      df <- reactive(
                     filter(data_x, 
                            NAT == input$select_nationality_2,
                            ID == input$select_id,
                            LBTEST == input$select_labtest
                            )
                    )
      
      df_2 <- reactive(
                      select(df(), AVAL)
                      )
      
      df_3 <- reactive(
                       mutate(df_2(), xaxis = x_axis_sequence())
                      )
      
      # Reactive function containing interesting data for ggplot2 package
      gg <- reactive(
                     ggplot(
                            df_3(), aes(x = xaxis, y = AVAL, color = AVAL)
                           ) + 
                            geom_point() +
                            theme_bw() +
                            geom_smooth() +
                            labs(
                                 subtitle = "by patient id", 
                                 y = "AVAL", 
                                 x = "Days", 
                                 title = "Treatment history", 
                                 caption = "ggplot2 package"
                                 )
                    ) # there are not already days in the xaxis, since I didn't knew what is the meaning of 'BASELINE' in the test history, so I decided to treat it as a normal day.
      
      #Output - plot
      output$MyPlot2 <- renderPlot({
                                    plot(gg()
                                         #Old version of MyPlot2
                                         # x, y plot with days and AVAL
                                         #x_axis_sequence(),
                                         #lab_test_results_by_nat_id()
                                        )
                                    #Old version of histogram 
                                    #histogram plot
                                    #hist(aval_plot(), 
                                    #     col = "#75AADB", 
                                    #     border = 'white', 
                                    #     xlab = "Value of 'AVAL'", 
                                    #     main = "Histogram by individual patient ID"
                                    #     )
                                  })
      
      #Output - Reactive table which contain data for selected Nationality and individual ID
      output$reactive_tab <- renderTable(
                                         {data_R()}
                                         )
      
})

shinyApp(ui = ui, server = server)

}
