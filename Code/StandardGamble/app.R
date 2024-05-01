library(plotly)
library(shinyWidgets)
library(sortable)
library(shinyjs)

# UI definition
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-link.disabled {
        color: #ddd;
        background-color: transparent;
        pointer-events: none;
      }
    "))
  ),
  titlePanel("Health Condition Interview"),
  # Replace sidebarLayout with fluidRow for a single column layout
  fluidRow(
    column(12,
           numericInput("age", "Your Age", value = 25, min = 18, max = 80),
           selectInput("gender", "Your Gender", choices = c("Male", "Female", "Other or Prefer Not to Answer")),
           rank_list(
             text = "Imagine you have well controlled diabetes. You take insulin and check your blood sugar regularly, but otherwise lead a life free of complications. Now we want to see how you would compare this quality of life with that of life living with three complications related to diabetes. Rank the conditions with worst quality of life at the top (the closest to dying) and best quality of life (perfect health with controlled diabetes) at the bottom.",
             labels = c("Diabetic Neuropathy: Complication of diabetes that results in nerve pain. Most commonly, this causes a burning and stinging sensation in your hands and feet. This may eventually progress to the point where you can’t feel things well with your fingers or more commonly your feet. Loss of sensation in your feet can lead to diabetic foot infections from minor injuries you don’t feel.",
                        "Diabetic Foot Infection: A serious complication of diabetes, often stemming from neuropathy or peripheral arterial disease. It begins with seemingly benign sores or blisters on the feet that, without the usual pain to signal a problem due to neuropathy, can deteriorate unnoticed. Poor blood flow complicates healing, risking infection that can spread, leading to severe consequences without prompt treatment.",
                        "Diabetic Retinopathy: A diabetes complication that affects the eyes and is caused by damage to the blood vessels of the light-sensitive tissue at the back of the eye (retina). Initially, diabetic retinopathy may cause no symptoms or only mild vision problems. However, it can lead to blindness. The condition can develop in anyone who has type 1 or type 2 diabetes, especially if the diabetes is poorly controlled."),
             input_id = "conditionRank"
           ),
           actionButton("start", "Start Interview")
    ),
    column(12,
           titlePanel("Instructions"),
           fluidRow(
             column(12, textOutput("gambleExplanation"))
           ),
           fluidRow(
             column(12, br())
           ),
           tabsetPanel(
             id = "mainTabs",
             tabPanel("Interview 1 Visual Acuity Scale",
                      h4("Visual Acuity Scale"),
                      textOutput("visualAcuityInstructions"),
                      br(),
                      uiOutput("visualScaleUI"),
                      actionButton("submitVAS", "Submit")
             ),
             tabPanel("Interview 2 Standard Gamble",
                      tabsetPanel(
                        id = "gambleTabs",
                        tabPanel("Condition 1", value = "cond1",
                                 br(),
                                 actionButton("cond1Choice1", "This condition is bad. I would risk the cure."),
                                 actionButton("cond1Choice2", "I am indifferent."),
                                 actionButton("cond1Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                                 br(),
                                 br(),
                                 textOutput("cond1Desc"),
                                 br(),
                                 plotlyOutput("cond1PieChart")),
                        tabPanel("Condition 2", value = "cond2",
                                 br(),
                                 actionButton("cond2Choice1", "This condition is bad. I would risk the cure."),
                                 actionButton("cond2Choice2", "I am indifferent."),
                                 actionButton("cond2Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                                 br(),
                                 br(),
                                 textOutput("cond2Desc"),
                                 br(),
                                 plotlyOutput("cond2PieChart")),
                        tabPanel("Condition 3", value = "cond3",
                                 br(),
                                 actionButton("cond3Choice1", "This condition is bad. I would risk the cure."),
                                 actionButton("cond3Choice2", "I am indifferent."),
                                 actionButton("cond3Choice3", "That's too high of a risk of death. I would rather live with the condition."),
                                 br(),
                                 br(),
                                 textOutput("cond3Desc"),
                                 br(),
                                 plotlyOutput("cond3PieChart"))
                      )
             ),
             tabPanel("Interview 3 Time Trade Off",
                      h4("Placeholder for Time Trade Off Content")
             )
           )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Initial rendering of the explanation text
  output$gambleExplanation <- renderText({
    "Welcome to the Health Condition Interview. Please click 'Start Interview' to begin."
  })
  
  observeEvent(input$start, {
    updateTabsetPanel(session, "mainTabs", selected = "Interview 1 Visual Acuity Scale")
    runjs('document.getElementById("mainTabs").scrollIntoView();')
  })
  
  # Dynamic instruction text for the Visual Acuity Scale
  output$visualAcuityInstructions <- renderText({
    "This is a visual acuity scale. Let's think of this as a warmup for the other two interviews. You have already ranked the three conditions in order of your perceived quality of life. Now, we want to get your perception of what percent quality of life each condition gives you. In this case a value of 100% would be perfect health living with well controlled diabetes, and 0% would be the quality of life associated with death. As you choose the percentages, try to think of the percentages as living with this condition for a year would be equivalent to X% of the quality of life of a year of perfect health under well controlled diabetes."
  })
  
  # Update the slider labels and visibility based on the ranking
  output$visualScaleUI <- renderUI({
    req(input$conditionRank)
    # Create sliders with labels based on the ranking
    fluidPage(
      sliderInput("slider1", label = strsplit(input$conditionRank[1],split=":")[[1]][1], min = 0, max = 100, value = 100),
      sliderInput("slider2", label = strsplit(input$conditionRank[2],split=":")[[1]][1], min = 0, max = 100, value = 100),
      sliderInput("slider3", label = strsplit(input$conditionRank[3],split=":")[[1]][1], min = 0, max = 100, value = 100)
    )
  })
  
  observeEvent(input$submitVAS, {
    # Check if the slider values are in the correct order according to the ranking
    if (input$slider1 < input$slider2 || input$slider2 < input$slider3) {
      showModal(modalDialog(
        title = "Input Error",
        "In step 1, you ranked the three conditions from highest to lowest quality of life. You cannot give a lower ranked condition a higher quality of life percentage than the condition before it.",
        easyClose = TRUE,
        footer = modalButton("Ok")
      ))
    } else {
      # Move to the Standard Gamble tab
      updateTabsetPanel(session, "mainTabs", selected = "Interview 2 Standard Gamble")
      
      # Display a summary of their Visual Acuity Scale input
      showModal(modalDialog(
        title = "Summary of your Visual Acuity Scale Input",
        paste("You rated:",
              "\n- ", strsplit(input$conditionRank[1], split=":")[[1]][1], ": ", input$slider1, "%",
              "\n- ", strsplit(input$conditionRank[2], split=":")[[1]][1], ": ", input$slider2, "%",
              "\n- ", strsplit(input$conditionRank[3], split=":")[[1]][1], ": ", input$slider3, "%"),
        footer = modalButton("Proceed to Standard Gamble")
      ))
    }
  })
  
  # Initialize reactive values
  gambleStates <- reactiveValues(
    cond1 = 50, cond2 = 50, cond3 = 50,
    conditionsRanked = NULL, currentCond = 1,
    lastChoice = list(cond1 = NULL, cond2 = NULL, cond3 = NULL),
    stepSize = list(cond1 = 10, cond2 = 10, cond3 = 10),
    maxRisk = list(cond1 = 100, cond2 = 100, cond3 = 100)  # Initialize maximum risks
  )
  
  # Reactive values to store user responses and calculated life expectancy
  lifeExpectancyTable <- read.csv("SSA2020LifeTable.csv")
  userResponses <- reactiveValues(age = NULL, gender = NULL, lifeExpectancy = NULL)
  
  
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
    
    userResponses$age <- input$age
    userResponses$gender <- input$gender
    
    # Calculate remaining life expectancy and round to nearest year
    if (input$gender %in% c("Male", "Female")) {
      userResponses$lifeExpectancy <- round(
        approx(lifeExpectancyTable$Age, lifeExpectancyTable[[input$gender]], xout = input$age)$y
      )
    } else {
      # If gender is "Other", use the average of Male and Female
      avgLifeExpectancy <- rowMeans(cbind(lifeExpectancyTable$Male, lifeExpectancyTable$Female))
      userResponses$lifeExpectancy <- round(
        approx(lifeExpectancyTable$Age, avgLifeExpectancy, xout = input$age)$y
      )
    }
    
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
    # First, move to the Time Trade Off interview tab
    updateTabsetPanel(session, "mainTabs", selected = "Interview 3 Time Trade Off")
    
    # Then, calculate the results
    valueCondition1 <- 100 - gambleStates$cond1
    valueCondition2 <- 100 - gambleStates$cond2
    valueCondition3 <- 100 - gambleStates$cond3
    
    # Finally, display the results in a modal dialog
    showModal(modalDialog(
      title = "Your Results from Standard Gamble",
      paste("Your choices imply that for Condition 1 (", simpleConditionName(gambleStates$conditionsRanked[1]), 
            "), you value the quality of life of a year living under this condition at", valueCondition1, 
            "% of a year living healthily with diabetes."),
      paste("For Condition 2 (", simpleConditionName(gambleStates$conditionsRanked[2]), 
            "), that is", valueCondition2, "%."),
      paste("For Condition 3 (", simpleConditionName(gambleStates$conditionsRanked[3]), 
            "), it is", valueCondition3, "%."),
      footer = modalButton("Close")
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
        layout(title = "Chance of Death from Treatment or Cure")
    })
  }
}

# Run the application
shinyApp(ui, server)