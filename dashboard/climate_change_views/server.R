

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