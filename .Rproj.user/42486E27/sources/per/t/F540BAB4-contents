# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Assuming functions.R is necessary and contains needed components
source("functions.R")

# Function to read HTML content and maintain formatting
readHtmlContent <- function(filepath) {
  # Read the file
  lines <- readLines(filepath, warn = FALSE)
  # Collapse into a single HTML string, preserving HTML structure
  htmlContent <- paste(lines, collapse = "\n")
  return(htmlContent)
}

ui <- dashboardPage(
  dashboardHeader(
    title = "Trips Generated - 2023 Household Travel Survey",
    titleWidth = 600,
    tags$li(class = "dropdown",
            tags$a(href = "https://unifiedplan.org/household-travel-surveys/", target = "_blank",
                   "HTS Reference Material",
                   style = "float: right; font-size: 16px; padding: 15px;")
    ),
    tags$li(class = "dropdown",
            actionLink("showMethodology", "Methodology", icon = icon("book"),
                       style = "float: right; font-size: 16px; padding: 15px;")
    )
  ),
  dashboardSidebar(
    selectInput("groupSampleSegment", "Sample Segment Group:",
                choices = setNames(labelsSampleSegmentGroups$value, labelsSampleSegmentGroups$label)),
    selectInput("groupModeTypeBroad", "Travel Mode:",
                choices = setNames(labelsModeTypeBroad$value, labelsModeTypeBroad$label)),
    selectInput("groupNumVehicles", "Household Number of Vehicles:",
                choices = setNames(labelsNumVehicles$value, labelsNumVehicles$label)),
    selectInput("groupNumWorkers", "Household Number of Workers:",
                choices = setNames(labelsNumWorkers$value, labelsNumWorkers$label))
  ),
  dashboardBody(
    uiOutput("loadingMessage"),
    tags$head(
      tags$style(HTML("
                .shiny-output-error-validation { color: red; }
                #container {
                    display: flex;
                    flex-wrap: wrap;
                }
                #dataPlot {
                    flex: 2 1 80%; /* flex-grow, flex-shrink, flex-basis */
                }
                #inputsContainer {
                    flex: 1 1 40%;
                    display: flex;
                    flex-direction: column;
                }
                #selectedInputs, #dataTable {
                    flex: 1;
                    padding-top: 20px; /* Add padding on top */
                    padding-bottom: 10px; /* Add padding on bottom */
                }
            "))
    ),
    div(id = "container",
        plotOutput("dataPlot"),
        div(id = "inputsContainer",
            textOutput("selectedInputs"),
            tableOutput("dataTable")
        )
    )
  )
)

server <- function(input, output, session) {
  # Display the alert when the app is opened
  shinyalert(
    text ="The 2023 Utah Moves household travel survey was designed and conducted for use in regional and statewide travel demand modeling. The sample size and frame is suitable for that purpose.
    \n Proper application of the dataset and use of this application is the responsibility of the user. In using the information or data herein, users assume the risk for relying on such data or information, and further agree to hold Utah's transportation agencies harmless for all liability of any accuracy or correctness of the information or data provided.
    \n Users are encouraged to contact analytics@wfrc.org with questions or to discuss proper uses and application of this data.",
    closeOnClickOutside = FALSE,
    closeOnEsc = FALSE,
    confirmButtonText = "I acknowledge and agree",
    confirmButtonCol = 'navy'
  )
  
  # Handle the methodology modal dialog
  observeEvent(input$showMethodology, {
    showModal(modalDialog(
      title = "Methodology",
      tags$p("The trips described in this app are from the the Household Travel Survey's cleaned, weighted trip table delivered by RSG, the contractor for the survey project. The dataset has not been further post-processed."),
      tags$p("Trip generation rates per household were calculated using the following steps:"),
      tags$p(tags$b("Prepare grouping tables")),
      tags$p("Four separate views were created to further group values from 4 key dimensions to the data: sample segment (geography and population), household number of workers, household number of vehicles, trip travel mode type, and trip type (purpose). The only grouping added to the current survey breakdown was an 'All' group that includes all possible values for each respective field. The 'All' group uses an attribute value of -1 to not overlap with existing attribute values. An example SQL for number of vehicles is shown in QUERY 1."),
      tags$p(tags$b("Calculate number of trips")),
      tags$p("The trips query was created using the household table and the trip table joined on the hh_id field. The four views for groupings are also added by using key values to link the tables. The trip_weight and number of records is aggregated for each key dimension using their respective group fields. Resulting record counts in the numTripRecords field were used to judge rough accuracy of the query structure. See QUERY 2."),
      tags$p(tags$b("Calculate number of households")),
      tags$p("The households query was created using the household table and the trip table joined on the hh_id field. The four views for groupings are also added by using key values to link the tables. The trip_weight and number of records is aggregated for each key dimension using their respective group fields. Resulting record counts in the numHhRecords field were used to judge rough accuracy of the query structure. See QUERY 3."),
      tags$p(tags$b("Calculate histogram distribution and cumulative distribution")),
      tags$p("A jupyter notebook was used to divide the number of trips by the number of households for each key dimension. The notebook can be found in this repo: https://github.com/WFRCAnalytics/HTS-Trip-Purpose-R-Shiny"),
      tags$h4("QUERY 1"),
      HTML(readHtmlContent("queries/query1.html")),
      tags$h4("QUERY 2"),
      HTML(readHtmlContent("queries/query2.html")),
      tags$h4("QUERY 3"),
      HTML(readHtmlContent("queries/query3.html")),
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observe({
    output$dataPlot <- renderPlot({
      filtered_data <- data %>%
        filter(groupSampleSegment == input$groupSampleSegment,
               groupNumWorkers == input$groupNumWorkers,
               groupNumVehicles == input$groupNumVehicles,
               groupModeTypeBroad == input$groupModeTypeBroad)
      ggplot(filtered_data, aes(x = tripTypeLabel, y = tripsPerHh)) +
        geom_bar(stat = "identity", fill = "blue") +
        theme_minimal() +
        labs(title = "Number of Trips per Household by Trip Purpose",
             x = "",
             y = "Trips perHousehold") +  # Inserted line break
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16, lineheight = 0.8),  # Adjust line height if needed
          axis.text = element_text(size = 14)
        )
    })
    
    
    output$selectedInputs <- renderText({
      paste("\n",
            "Current Filter:",
            paste0("\u00A0\u00A0\u00A0Sample Segment Group: ", labelsSampleSegmentGroups$label[labelsSampleSegmentGroups$value == input$groupSampleSegment]),
            paste0("\u00A0\u00A0\u00A0Travel Mode: ", labelsModeTypeBroad$label[labelsModeTypeBroad$value == input$groupModeTypeBroad]),
            paste0("\u00A0\u00A0\u00A0Number of Vehicles: ", labelsNumVehicles$label[labelsNumVehicles$value == input$groupNumVehicles]),
            paste0("\u00A0\u00A0\u00A0Number of Workers: ", labelsNumWorkers$label[labelsNumWorkers$value == input$groupNumWorkers]),
            sep = "\n")
    })
    
    output$dataTable <- renderTable({
      # Placeholder for your data table logic
      filtered_data <- data %>%
        filter(groupSampleSegment == input$groupSampleSegment,
               groupNumWorkers == input$groupNumWorkers,
               groupNumVehicles == input$groupNumVehicles,
               groupModeTypeBroad == input$groupModeTypeBroad) %>%
        select(tripTypeLabel, numTripRecords, sumTripWeight, numHhRecords, sumHhWeight, tripsPerHh)  # Select specific columns
      # Display the filtered and selected data
      filtered_data
    })
  })
}

shinyApp(ui = ui, server = server)
