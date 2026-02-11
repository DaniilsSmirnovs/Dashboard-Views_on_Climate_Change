ui <- dashboardPage(
  
  # Header section of the dashboard
  dashboardHeader(title = "Views on Climate Change",
                  titleWidth = 400),  # Adjusts width of title area
  
  # Sidebar section (left-hand panel)
  dashboardSidebar(
    selectInput(
      inputId = "select_parameter",  # Input ID used in server
      label = HTML('<p style="color:black;">Select Demographic Parameter</p>'),
      choices = parameters_list,  # List of available demographic parameters
      selected = "Overall Population"  # Default selection
    )
  ),
  
  # Main body of dashboard
  dashboardBody(
    
    # Custom CSS styling
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #005eb8;  /* Logo background colour */
          color: white;               /* Logo text colour */
        }
        .skin-blue .main-header .navbar {
          background-color: #0078ec;  /* Navbar background colour */
        }
        
        .skin-blue .main-sidebar {
          background-color:  #e4e4e4; /* Sidebar background colour */
        }
      "))
    ),
    
    # Fluid layout for responsive design
    fluidPage(
      
      # -------- FIRST ROW --------
      fluidRow(
        
        # Left column (50% width)
        column(
          width = 6,
          box(
            
            
            # Multiple selection for knowledge responses
            selectInput(
              inputId = "select_knowledge_input",
              label = "Select response(s) (Optional)",
              choices = knowledge_response_list,
              selected = NULL,
              multiple = TRUE),
            
            # Multiple selection for demographic subgroups
            selectInput(
              inputId = "knowledge_subgroups",
              label = "Select Demographic Subgroup(s) (Optional)",
              choices = select_subgroups$subgroup,
              selected = NULL,
              multiple = TRUE
            ),
            
            # Plot output (linked to output$knowledge in server)
            plotlyOutput("knowledge"),
            
            title = tagList(
              "Knowledge",
              tags$h5("Self-reported knowledge of climate change")
            ),  # Box title
            
            width = NULL          # Box fills column width
          )
        ),
        
        # Right column (50% width)
        column(
          width = 6,
          box(
            
            # Single selection for urgency statement
            selectInput(
              inputId = "select_urgency_input",
              label = "Select statement",
              choices = urgency_response_list,
              selected = urgency_response_list[1]),  # Default first statement
            
            # Multiple subgroup selection
            selectInput(
              inputId = "urgency_subgroups",
              label = "Select Demographic Subgroup(s) (Optional)",
              choices = select_subgroups$subgroup,
              selected = NULL,
              multiple = TRUE
            ),
            
            # Plot output (linked to output$urgency)
            plotlyOutput("urgency"),
            
            title = tagList(
              "Attitude",
              tags$h5("Attitude towards climate change")
            ),
            width = NULL
          )
        )
      ),
      
      # -------- SECOND ROW --------
      fluidRow(
        
        # Left column (Sources of Information)
        column(
          width = 6, 
          box(
            
            # Numeric input controlling number of top sources displayed
            numericInput(
              inputId = "select_topsources",
              label = "Select number of most trusted sources to display",
              value = 5,  # Default value
              max = 11,   # Maximum allowed
              min = 3,    # Minimum allowed
              step = 1    # Increment step
            ),
            
            # Single subgroup selection
            selectInput(
              inputId = "sources_select_subgroups",
              label = "Select Demographic Subgroup",
              choices = select_subgroups$subgroup,
              selected = "Overall Population"
            ),
            
            # Plot output (linked to output$sources)
            plotlyOutput("sources"),
            
            title = tagList(
              "Source of Information",
              tags$h5("Ranking of most trusted sources of information")
            ),
            width = NULL
          )
        ),
        
        # Right column (Negative Impact)
        column(
          width = 6,
          box(
            
            # Multiple selection for negative impact responses
            selectInput(
              inputId = "select_negative_impact_input",
              label = "Select response(s) (Optional)",
              choices = negative_impact_response_list,
              selected = NULL,
              multiple = TRUE),
            
            # Multiple subgroup selection
            selectInput(
              inputId = "negative_impact_subgroups",
              label = "Select Demographic Subgroup(s) (Optional)",
              choices = select_subgroups$subgroup,
              selected = "Overall Population",
              multiple = TRUE
            ),
            
            # Plot output (linked to output$negative_impact)
            plotlyOutput("negative_impact"),
            
            title = tagList(
              "Negative Impact",
              tags$h5("Frequency that feelings about climate change negatively impact daily activities")
            ),
            width = NULL
          )
        )
      ) 
    )
  )
)