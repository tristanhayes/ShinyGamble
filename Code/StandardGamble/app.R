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
  titlePanel("Standard Gamble Interview"),
  sidebarLayout(
    sidebarPanel(
      rank_list(
        text = "Imagine you have well controlled diabetes. You take insulin and check your blood sugar regularly, but otherwise lead a life free of complications. Now we want to see how you would compare this quality of life with that of life living with three complications related to diabetes. Rank the conditions with worst quality of life at the top (the closest to dying) and best quality of life (perfect health with controlled diabetes) at the bottom. ",
        labels = c("Diabetic Neuropathy: Complication of diabetes that results in nerve pain. Most commonly, this causes a burning and stinging sensation in your hands and feet. This may eventually progress to the point where you can’t feel things well with your fingers or more commonly your feet. Loss of sensation in your feet can lead to diabetic foot infections from minor injuries you don’t feel.", 
                   "Diabetic Foot Infection: A serious complication of diabetes, often stemming from neuropathy or peripheral arterial disease. It begins with seemingly benign sores or blisters on the feet that, without the usual pain to signal a problem due to neuropathy, can deteriorate unnoticed. Poor blood flow complicates healing, risking infection that can spread, leading to severe consequences without prompt treatment.", 
                   "Diabetic Retinopathy: A diabetes complication that affects the eyes and is caused by damage to the blood vessels of the light-sensitive tissue at the back of the eye (retina). Initially, diabetic retinopathy may cause no symptoms or only mild vision problems. However, it can lead to blindness. The condition can develop in anyone who has type 1 or type 2 diabetes, especially if the diabetes is poorly controlled."), 
        input_id = "conditionRank"
      ),
      actionButton("start", "Start Interview")
    ),
    mainPanel(    titlePanel("Instructions"),
      fluidRow(
      column(12, textOutput("gambleExplanation"))
    ),fluidRow(
      column(12, br())
    ),
      tabsetPanel(id = "gambleTabs",
                  tabPanel("Condition 1", value = "cond1",
                           br(),
                           actionButton("cond1Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond1Choice2", "I am indifferent."),
                           actionButton("cond1Choice3", "That's too high of a risk of dieing. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond1Desc"),
                           br(),
                           plotlyOutput("cond1PieChart")),
                  tabPanel("Condition 2", value = "cond2",
                           br(),
                           actionButton("cond2Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond2Choice2", "I am indifferent."),
                           actionButton("cond2Choice3", "That's too high of a risk of dieing. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond2Desc"),
                           br(),
                           br(),
                           plotlyOutput("cond2PieChart")),
                  tabPanel("Condition 3", value = "cond3",
                           br(),
                           actionButton("cond3Choice1", "This condition is bad. I would risk the cure."),
                           actionButton("cond3Choice2", "I am indifferent."),
                           actionButton("cond3Choice3", "That's too high of a risk of dieing. I would rather live with the condition."),
                           br(),
                           br(),
                           textOutput("cond3Desc"),
                           br(),
                           br(),
                           plotlyOutput("cond3PieChart"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Render the explanation text
  output$gambleExplanation <- renderText({
    "Here we want to evaluate your perception of the quality of life under the below conditions using a method called the Standard Gamble. You have below a scenario where you can choose between living with a health condition for the rest of your life or taking a 'gamble' with a risky treatment where you have a chance of being cured or dieing from the treatment. The higher risk you are willing to take to be cured tells us how low the quality of life with that condition is. When you select I am indifferent, then we move to the enxt condition. You cannot take a higher risk of death than the prior condition."
  })
  # Initialize reactive values
  gambleStates <- reactiveValues(
    cond1 = 50, cond2 = 50, cond3 = 50,
    conditionsRanked = NULL, currentCond = 1,
    lastChoice = list(cond1 = NULL, cond2 = NULL, cond3 = NULL),
    stepSize = list(cond1 = 10, cond2 = 10, cond3 = 10),
    maxRisk = list(cond1 = 100, cond2 = 100, cond3 = 100)  # Initialize maximum risks
  )
  
  observeEvent(input$start, {
    gambleStates$conditionsRanked <- input$conditionRank
    gambleStates$cond1 <- 50  # Reset to starting default
    gambleStates$cond2 <- 50
    gambleStates$cond3 <- 50
    gambleStates$maxRisk <- list(cond1 = 100, cond2 = 100, cond3 = 100)  # Reset maximum risks
    disable("cond2")
    disable("cond3")
    updateTabsetPanel(session, "gambleTabs", selected = "cond1")
    renderPieChart("cond1")
    
    # Update condition descriptions
    output$cond1Desc <- renderText({ gambleStates$conditionsRanked[1] })
    output$cond2Desc <- renderText({ gambleStates$conditionsRanked[2] })
    output$cond3Desc <- renderText({ gambleStates$conditionsRanked[3] })
    
    # JavaScript to scroll to the main panel
    runjs('document.getElementById("gambleTabs").scrollIntoView();')
  })
  
  handleChoice <- function(conditionId, choice) {
    # Adjust the step size if the direction is flipped
    if (!is.null(gambleStates$lastChoice[[conditionId]]) && gambleStates$lastChoice[[conditionId]] != choice) {
      gambleStates$stepSize[[conditionId]] <- 5  # Flipped direction
    } else {
      gambleStates$stepSize[[conditionId]] <- 10  # Same direction or first choice
    }
    gambleStates$lastChoice[[conditionId]] <- choice
    
    # Update risk and re-render pie chart
    newRisk <- if (choice == "take") {
      min(100, gambleStates[[conditionId]] + gambleStates$stepSize[[conditionId]], gambleStates$maxRisk[[conditionId]])
    } else {
      max(0, gambleStates[[conditionId]] - gambleStates$stepSize[[conditionId]])
    }
    
    # Apply maximum risk constraints and update risk value
    gambleStates[[conditionId]] <- newRisk
    
    renderPieChart(conditionId)
  }
  
  # Update settings when "I am indifferent" is selected
  observeEvent(input$cond1Choice2, {
    gambleStates$maxRisk$cond2 <- gambleStates$cond1  # Set max risk for cond2
    gambleStates$cond2 <- gambleStates$cond1          # Carry over value to cond2
    enable("cond2")
    updateTabsetPanel(session, "gambleTabs", selected = "cond2")
    renderPieChart("cond2")  # Ensure the pie chart updates immediately
  })
  
  observeEvent(input$cond2Choice2, {
    gambleStates$maxRisk$cond3 <- gambleStates$cond2  # Set max risk for cond3
    gambleStates$cond3 <- gambleStates$cond2          # Carry over value to cond3
    enable("cond3")
    updateTabsetPanel(session, "gambleTabs", selected = "cond3")
    renderPieChart("cond3")  # Ensure the pie chart updates immediately
  })
  
  observeEvent(input$cond3Choice2, {
    # Show results modal with just the condition names
    showModal(modalDialog(
      title = "Results",
      paste("Condition 1:", simpleConditionName(gambleStates$conditionsRanked[1]), "- Final Chance of Dying:", gambleStates$cond1, "%"),
      paste("Condition 2:", simpleConditionName(gambleStates$conditionsRanked[2]), "- Final Chance of Dying:", gambleStates$cond2, "%"),
      paste("Condition 3:", simpleConditionName(gambleStates$conditionsRanked[3]), "- Final Chance of Dying:", gambleStates$cond3, "%"),
      size = "l"
    ))
  })
  
  # Helper function to extract simple condition names
  simpleConditionName <- function(fullDescription) {
    sapply(strsplit(fullDescription, ": "), `[`, 1)
  }
  
  lapply(c("cond1", "cond2", "cond3"), function(conditionId) {
    observeEvent(input[[paste0(conditionId, "Choice1")]], {
      handleChoice(conditionId, "take")
    })
    observeEvent(input[[paste0(conditionId, "Choice3")]], {
      handleChoice(conditionId, "live")
    })
  })
  
  renderPieChart <- function(conditionId) {
    output[[paste0(conditionId, "PieChart")]] <- renderPlotly({
      df <- data.frame(
        labels = c("Cure", "Death"),
        values = c(100 - gambleStates[[conditionId]], gambleStates[[conditionId]])
      )
      plot_ly(df, labels = ~labels, values = ~values, type = 'pie',
              marker = list(colors = c('#ABEBC6', '#E74C3C')),
              hoverinfo = 'label+percent', textinfo = 'label+percent') %>%
        layout(title = "Chance of Death from Cure")
    })
  }
}

# Run the application
shinyApp(ui, server)