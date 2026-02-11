# ============================================================
# Shiny Server Logic
# Purpose:
#   - Manage reactive behaviour
#   - Update UI inputs dynamically
#   - Render plots based on user selections
#
# QA Principles:
#   - Explicit validation of inputs
#   - Reactive dependencies clearly scoped
#   - No hidden side effects
#   - Plot logic separated from rendering logic
# ============================================================


server <- function(input, output, session) {
  
  
  # ============================================================
  # 1. Input Validation
  # ============================================================
  
  # Reactive validation for number of top trusted sources.
  # Ensures:
  #   - User cannot request unrealistic values
  #   - Prevents downstream plotting errors
  #   - Improves robustness of dashboard
  
  valid_topsources <- reactive({
    validate(
      need(input$select_topsources >= 3 & input$select_topsources <= 11,
           "Please select a number between 3 and 11")
    )
  })
  
  
  # ============================================================
  # 2. Dynamic Subgroup Updating
  # ============================================================
  
  # Observer updates subgroup dropdowns whenever
  # demographic parameter selection changes.
  #
  # This ensures:
  #   - UI remains logically consistent
  #   - Only valid subgroup choices are displayed
  #   - No invalid parameter–subgroup combinations occur
  #   - Clean reactive dependency structure
  
  observe({
    
    # Require parameter selection before proceeding
    req(input$select_parameter)  
    
    # Extract valid subgroups for selected parameter
    new_subgroups <- scs_data_cleaned |>
      filter(demographic_parameter == input$select_parameter) |>
      distinct(subgroup) |>
      pull(subgroup)
    
    
    # Update subgroup selector for Trusted Sources plot
    updateSelectInput(
      session,
      inputId = "sources_select_subgroups",
      choices = new_subgroups,
      selected = new_subgroups[1]  # Default to first valid subgroup
    )
    
    
    # Update subgroup selector for Knowledge plot
    updateSelectInput(
      session, 
      "knowledge_subgroups", 
      choices = new_subgroups, 
      selected = NULL  # Allow multi-selection
    )
    
    
    # Update subgroup selector for Urgency plot
    updateSelectInput(
      session, 
      "urgency_subgroups", 
      choices = new_subgroups, 
      selected = NULL
    )
    
    
    # Update subgroup selector for Negative Impact plot
    updateSelectInput(
      session, 
      "negative_impact_subgroups", 
      choices = new_subgroups, 
      selected = NULL
    )
    
  })
  
  
  # ============================================================
  # 3. Plot Rendering
  # ============================================================
  
  # Each output:
  #   - Calls a dedicated plotting function
  #   - Passes filtered inputs explicitly
  #   - Avoids embedding transformation logic here
  #
  # This maintains:
  #   - Separation of concerns
  #   - Cleaner debugging
  #   - Reusability of plotting functions
  # ============================================================
  
  
  # ---------------------------
  # Plot 1: Knowledge (T1)
  # ---------------------------
  
  output$knowledge <- renderPlotly({
    plot_knowledge(
      scs_data_cleaned, 
      parameter = input$select_parameter,
      knowledge_responses = input$select_knowledge_input,
      subgroup_values = input$knowledge_subgroups
    )
  })
  
  
  # ---------------------------
  # Plot 2: Urgency (T3)
  # ---------------------------
  
  output$urgency <- renderPlotly({
    plot_urgency(
      scs_data_cleaned, 
      parameter = input$select_parameter, 
      urgency_responses = input$select_urgency_input,
      subgroup_values = input$urgency_subgroups
    )
  })
  
  
  # ---------------------------
  # Plot 3: Trusted Sources (T7)
  # ---------------------------
  
  output$sources <- renderPlotly({
    
    # Ensure valid top-N input before rendering
    valid_topsources()
    
    plot_sources(
      scs_data_cleaned, 
      parameter = input$select_parameter,
      ntop = input$select_topsources, 
      subgroup_value = input$sources_select_subgroups
    )
  })
  
  
  # ---------------------------
  # Plot 4: Negative Impact (T11)
  # ---------------------------
  
  output$negative_impact <- renderPlotly({
    plot_negative_impact(
      scs_data_cleaned, 
      parameter = input$select_parameter,
      negative_impact_responses = input$select_negative_impact_input,
      subgroup_values = input$negative_impact_subgroups
    )
  })
  
}