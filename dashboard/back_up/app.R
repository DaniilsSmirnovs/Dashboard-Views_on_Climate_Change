
# Libraries

library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)


# ui 

ui <- dashboardPage(
  
  dashboardHeader(title = "Views on Climate Change",
                  titleWidth = 400),
  
  dashboardSidebar(
    selectInput(
      inputId = "select_parameter",
      label = HTML('<p style="color:black;">Select Demographic Parameter</p>'),
      choices = parameters_list,
      selected = "Overall Population"
    )
  ),
        
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #005eb8; 
          color: white;
        }
        .skin-blue .main-header .navbar {
          background-color: #0078ec;  
        }
        
        .skin-blue .main-sidebar {
          background-color:  #e4e4e4;
        }
      "))
    ),
    
    fluidPage(
      fluidRow(
        column(
          width = 6,
          box(
            
            selectInput(
              inputId = "select_knowledge_input",
              label = "Select response(s) (Optional)",
              choices = knowledge_response_list,
              selected = NULL,
              multiple = TRUE),
            
            selectInput(
              inputId = "knowledge_subgroups",
              label = "Select Demographic Subgroup(s) (Optional)",
              choices = select_subgroups$subgroup,
              selected = NULL,
              multiple = TRUE
            ),
            
            plotlyOutput("knowledge"),
            title = "Knowledge",
            width = NULL
          )
        ),
        column(
          width = 6,
          box(
            
            selectInput(
              inputId = "select_urgency_input",
              label = "Select statement",
              choices = urgency_response_list,
              selected = urgency_response_list[1]),
            
            selectInput(
              inputId = "urgency_subgroups",
              label = "Select Demographic Subgroup(s) (Optional)",
              choices = select_subgroups$subgroup,
              selected = NULL,
              multiple = TRUE
            ),
            
            plotlyOutput("urgency"),
            title = "Urgency",
            width = NULL
          )
        )
    ),
    fluidRow(
      column(
        width = 6, 
        box(
          
          numericInput(
            inputId = "select_topsources",
            label = "Select number of most trusted sources to display",
            value = 5, max = 11, min = 3, step =1
          ),
          
          selectInput(
            inputId = "sources_select_subgroups",
            label = "Select Demographic Subgroup",
            choices = select_subgroups$subgroup,
            selected = "Overall Population"
          ),
          
          plotlyOutput("sources"),
          title = "Sources of Information",
          width = NULL
        )
      ),
      column(
        width = 6,
        box(
          
          selectInput(
            inputId = "select_negative_impact_input",
            label = "Select response(s) (Optional)",
            choices = negative_impact_response_list,
            selected = NULL,
            multiple = TRUE),
          
          selectInput(
            inputId = "negative_impact_subgroups",
            label = "Select Demographic Subgroup(s) (Optional)",
            choices = select_subgroups$subgroup,
            selected = "Overall Population",
            multiple = TRUE
          ),
          
          plotlyOutput("negative_impact"),
          title = "Negative Impact",
          width = NULL
        )
      )
    ) 
  )
 )
)

# server

server <- function(input, output, session) {
  
  valid_topsources <- reactive({
    validate(
      need(input$select_topsources >= 3 & input$select_topsources <= 11,
           "Please select a number between 3 and 11")
    )
  })
  
  observe({
    
    req(input$select_parameter)  
    
    new_subgroups <- scs_data_cleaned |>
      filter(demographic_parameter == input$select_parameter) |>
      distinct(subgroup) |>
      pull(subgroup)
    
    
    updateSelectInput(
      session,
      inputId = "sources_select_subgroups",
      choices = new_subgroups,
      selected = new_subgroups[1]
    )
    
    updateSelectInput(
      session, 
      "knowledge_subgroups", 
      choices = new_subgroups, 
      selected = NULL
    )
    
    updateSelectInput(
      session, 
      "urgency_subgroups", 
      choices = new_subgroups, 
      selected = NULL
    )
    
    updateSelectInput(
      session, 
      "negative_impact_subgroups", 
      choices = new_subgroups, 
      selected = NULL
    )
    
  })
  
  
  output$knowledge <- renderPlotly({
    plot_knowledge(scs_data_cleaned, parameter = input$select_parameter,
                   knowledge_responses = input$select_knowledge_input,
                   subgroup_values = input$knowledge_subgroups)
  })
  
  output$urgency <- renderPlotly({
    plot_urgency(scs_data_cleaned, parameter = input$select_parameter, 
                 urgency_responses = input$select_urgency_input,
                 subgroup_values = input$urgency_subgroups)
  })
  
  output$sources <- renderPlotly({
    valid_topsources()
    
    plot_sources(scs_data_cleaned, parameter = input$select_parameter,
                 ntop = input$select_topsources, subgroup_value = input$sources_select_subgroups)
  })

  output$negative_impact <- renderPlotly({
    plot_negative_impact(scs_data_cleaned, parameter = input$select_parameter,
                         negative_impact_responses = input$select_negative_impact_input,
                         subgroup_values = input$negative_impact_subgroups)
  })
}

# shinyApp

shinyApp(ui, server)