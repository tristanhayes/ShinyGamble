library(shiny)
library(plotly)
library(shinyWidgets)
library(sortable)
library(shinyjs)

# UI definition
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-link.disabled {
        color: #ddd;
        background-color: transparent;
        pointer-events: none;
      }
    "))
  ),
  titlePanel("Time Trade-Off Interview"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Your Age", value = 25, min = 18, max = 80),
      selectInput("gender", "Your Gender", choices = c("Male", "Female", "Other or Prefer Not to Answer")),
      rank_list(
        text = "Imagine you have well controlled diabetes. Now we want to see how you compare this quality of life with that of life living with three complications related to diabetes. Rank the conditions with worst quality of life at the top and best at the bottom.",
        labels = c("Diabetic Neuropathy: Complication of diabetes that results in nerve pain. Most commonly, this causes a burning and stinging sensation in your hands and feet. This may eventually progress to the point where you can’t feel things well with your fingers or more commonly your feet. Loss of sensation in your feet can lead to diabetic foot infections from minor injuries you don’t feel.", 
                   "Diabetic Foot Infection: A serious complication of diabetes, often stemming from neuropathy or peripheral arterial disease. It begins with seemingly benign sores or blisters on the feet that, without the usual pain to signal a problem due to neuropathy, can deteriorate unnoticed. Poor blood flow complicates healing, risking infection that can spread, leading to severe consequences without prompt treatment.", 
                   "Diabetic Retinopathy: A diabetes complication that affects the eyes and is caused by damage to the blood vessels of the light-sensitive tissue at the back of the eye (retina). Initially, diabetic retinopathy may cause no symptoms or only mild vision problems. However, it can lead to blindness. The condition can develop in anyone who has type 1 or type 2 diabetes, especially if the diabetes is poorly controlled."), 
        input_id = "conditionRank"
      ),
      actionButton("start", "Start Interview")
    ),
    mainPanel(
      titlePanel("Instructions"),
      fluidRow(column(12, textOutput("gambleExplanation"))),
      fluidRow(column(12, br())),
      tabsetPanel(id = "gambleTabs",
                  tabPanel("Condition 1", value = "cond1",
                           br(),
                           actionButton("cond1Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond1Choice2", "I am indifferent."),
                           actionButton("cond1Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond1Desc"),
                           br(),
                           plotlyOutput("cond1BarChart")), # Changed to BarChart
                  tabPanel("Condition 2", value = "cond2",
                           br(),
                           actionButton("cond2Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond2Choice2", "I am indifferent."),
                           actionButton("cond2Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond2Desc"),
                           br(),
                           br(),
                           plotlyOutput("cond2BarChart")), # Changed to BarChart
                  tabPanel("Condition 3", value = "cond3",
                           br(),
                           actionButton("cond3Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond3Choice2", "I am indifferent."),
                           actionButton("cond3Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond3Desc"),
                           br(),
                           br(),
                           plotlyOutput("cond3BarChart"))  # Changed to BarChart
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Load the life expectancy data
  lifeExpectancyTable <- read.csv("SSA2020LifeTable.csv")
  
  # Initialize reactive values for user responses and life expectancy calculations
  userResponses <- reactiveValues(age = NULL, gender = NULL, lifeExpectancy = NULL)
  
  # Initialize reactive values for managing the TTO states (healthy years and lost years)
  gambleStatesTTO <- reactiveValues(
    healthyYears = list(cond1 = NA, cond2 = NA, cond3 = NA), 
    lostYears = list(cond1 = NA, cond2 = NA, cond3 = NA)
  )
  
  # React to the start button to initialize values and set up the UI
  observeEvent(input$start, {
    userResponses$age <- input$age
    userResponses$gender <- input$gender
    
    # Calculate life expectancy based on the user's input
    if (input$gender %in% c("Male", "Female")) {
      userResponses$lifeExpectancy <- round(
        approx(lifeExpectancyTable$Age, lifeExpectancyTable[[input$gender]], xout = input$age)$y
      )
    } else {  # Use the average if gender is 'Other' or not specified
      avgLifeExpectancy <- rowMeans(cbind(lifeExpectancyTable$Male, lifeExpectancyTable$Female))
      userResponses$lifeExpectancy <- round(
        approx(lifeExpectancyTable$Age, avgLifeExpectancy, xout = input$age)$y
      )
    }
    
    # Set initial values for the TTO based on full life expectancy
    gambleStatesTTO$healthyYears$cond1 <- userResponses$lifeExpectancy
    gambleStatesTTO$lostYears$cond1 <- 0
    disable("cond2")
    disable("cond3")
    updateTabsetPanel(session, "gambleTabs", selected = "cond1")
    renderBarChart("cond1")
  })
  
  # Function to render the bar chart visualization for TTO
  renderBarChart <- function(conditionId) {
    output[[paste0(conditionId, "BarChart")]] <- renderPlotly({
      # Initialize if not already set
      if(is.na(gambleStatesTTO$healthyYears[[conditionId]])) {
        totalYears <- userResponses$lifeExpectancy
        healthyYears <- totalYears / 2  # Start with half healthy
        lostYears <- totalYears - healthyYears  # The rest is lost
        gambleStatesTTO$healthyYears[[conditionId]] <- healthyYears
        gambleStatesTTO$lostYears[[conditionId]] <- lostYears
      } else {
        healthyYears <- gambleStatesTTO$healthyYears[[conditionId]]
        lostYears <- gambleStatesTTO$lostYears[[conditionId]]
      }
      
      # Create data frame for Plotly
      data <- data.frame(
        Category = "Total Life Expectancy",  # Same category for both segments
        Years = c(healthyYears, lostYears),
        Type = c("Healthy Years", "Years Lost"),
        Colors = c('#ABEBC6', '#E74C3C')
      )
      
      # Create the plot
      plot_ly(data, x = ~Years, y = ~Category, type = 'bar', orientation = 'h',
              color = ~Type, colors = ~Colors) %>%
        layout(
          title = "Life Expectancy Distribution",
          barmode = 'stack',
          xaxis = list(title = "Years"),
          yaxis = list(title = "", showticklabels = FALSE),  # Single category, no tick labels
          showlegend = FALSE  # Optionally hide the legend if not needed
        )
    })
  }
  
  
  # Handle TTO choices to adjust life years
  handleTTOChoice <- function(conditionId, choice) {
    currentHealthyYears <- gambleStatesTTO$healthyYears[[conditionId]]
    if (choice == "shorter") {
      # Reduce healthy years, increase lost years
      newHealthyYears <- max(0, currentHealthyYears - 1)
    } else {  # "longer"
      # Increase healthy years, reduce lost years
      newHealthyYears <- min(userResponses$lifeExpectancy, currentHealthyYears + 1)
    }
    gambleStatesTTO$healthyYears[[conditionId]] <- newHealthyYears
    gambleStatesTTO$lostYears[[conditionId]] <- userResponses$lifeExpectancy - newHealthyYears
    
    renderBarChart(conditionId)
  }
  
  # Attach choice handlers to UI buttons
  lapply(c("cond1", "cond2", "cond3"), function(conditionId) {
    observeEvent(input[[paste0(conditionId, "Choice1")]], {
      handleTTOChoice(conditionId, "shorter")
    })
    observeEvent(input[[paste0(conditionId, "Choice3")]], {
      handleTTOChoice(conditionId, "longer")
    })
  })
  
  # Update when "I am indifferent" is selected and calculate TTO values
  lapply(c("cond1", "cond2", "cond3"), function(conditionId) {
    observeEvent(input[[paste0(conditionId, "Choice2")]], {
      if (conditionId != "cond3") {
        nextConditionId <- paste0("cond", as.character(as.numeric(substr(conditionId, 5, 5)) + 1))
        gambleStatesTTO$healthyYears[[nextConditionId]] <- gambleStatesTTO$healthyYears[[conditionId]]
        gambleStatesTTO$lostYears[[nextConditionId]] <- gambleStatesTTO$lostYears[[conditionId]]
        enable(nextConditionId)
        updateTabsetPanel(session, "gambleTabs", selected = nextConditionId)
        renderBarChart(nextConditionId)
      } else {
        # Display results at the end
        showModal(modalDialog(
          title = "Your Time Trade-Off Results",
          paste("For Condition 1 (", simpleConditionName(gambleStatesTTO$conditionsRanked[1]), "):",
                "you chose to live", gambleStatesTTO$healthyYears$cond1, "healthy years,",
                "valuing each year at", round(100 * gambleStatesTTO$healthyYears$cond1 / userResponses$lifeExpectancy, 2), "% of a full life."),
          paste("For Condition 2 (", simpleConditionName(gambleStatesTTO$conditionsRanked[2]), "):",
                "you chose", gambleStatesTTO$healthyYears$cond2, "healthy years,",
                "valuing each year at", round(100 * gambleStatesTTO$healthyYears$cond2 / userResponses$lifeExpectancy, 2), "% of a full life."),
          paste("For Condition 3 (", simpleConditionName(gambleStatesTTO$conditionsRanked[3]), "):",
                "you chose", gambleStatesTTO$healthyYears$cond3, "healthy years,",
                "valuing each year at", round(100 * gambleStatesTTO$healthyYears$cond3 / userResponses$lifeExpectancy, 2), "% of a full life.")
        ))
      }
    })
  })
  
  # Helper function to extract simple condition names from full descriptions
  simpleConditionName <- function(fullDescription) {
    sapply(strsplit(fullDescription, ": "), `[`, 1)
  }
}

# Run the application
shinyApp(ui, server)

