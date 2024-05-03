library(plotly)
library(shinyWidgets)
library(sortable)
library(shinyjs)
library(DT)

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
  titlePanel("rShiny Gambler: Simulated Quality Adjusted Life Years Interviews"),
  fluidRow(
    column(12,
           div(id = "inputSection",  # Enclose inputs and ranking in a div with an ID
               tags$p("In Pharmacoeconomics we use a series of interviews to evaluate the quality of life under different health conditions. This interactive rShiny interview is designed to introduce you to the interview methods used to generate utilities such as Quality Adjusted Life Years. Here you will be walked through a series of three interviews: Visual Acuity Scale, Standard Gamble, and Time Trade Off. Feel free to enter any age and gender you would like. We simply ask to create realistic life expectancy values for the last interview. This should only take 15 minutes. We would appreciate it if you would take a pre-survey before starting."),
               tags$a(href = "https://your-survey-link.com", "Complete our pre-knowledge survey", target = "_blank"),
               tags$br(),  # Adds a line break for better spacing
               tags$br(),  # Adds a line break for better spacing
               tags$p("Acknowledgement: This is a remake in rShiny of the excellent original Automated Tool for Health Utility Assessments: The Gambler II by Adejare and Eckman."),
               tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/32215320/", "Pubmed Link to the Original", target = "_blank"),
               tags$br(),  # Adds a line break for better spacing
               tags$br(),  # Adds a line break for better spacing
               numericInput("age", "Your Age", value = 50, min = 18, max = 80),
               selectInput("gender", "Your Gender", choices = c( "Other or Prefer Not to Answer","Male", "Female")),
               rank_list(
                 text = "Imagine you have well controlled diabetes. You take insulin and check your blood sugar regularly, but otherwise lead a life free of complications. Now we want to see how you would compare this quality of life with that of life living with three complications related to diabetes. Rank the conditions with worst quality of life at the top (the closest to dying) and best quality of life (perfect health with controlled diabetes) at the bottom.",
                 labels = c("Diabetic Neuropathy: Complication of diabetes that results in nerve pain. Most commonly, this causes a burning and stinging sensation in your hands and feet. This may eventually progress to the point where you can’t feel things well with your fingers or more commonly your feet. Loss of sensation in your feet can lead to diabetic foot infections from minor injuries you don’t feel.",
                            "Diabetic Foot Infection: A serious complication of diabetes, often stemming from neuropathy or peripheral arterial disease. It begins with seemingly benign sores or blisters on the feet that, without the usual pain to signal a problem due to neuropathy, can deteriorate unnoticed. Poor blood flow complicates healing, risking infection that can spread, leading to severe consequences without prompt treatment.",
                            "Diabetic Retinopathy: A diabetes complication that affects the eyes and is caused by damage to the blood vessels of the light-sensitive tissue at the back of the eye (retina). Initially, diabetic retinopathy may cause no symptoms or only mild vision problems. However, it can lead to blindness. The condition can develop in anyone who has type 1 or type 2 diabetes, especially if the diabetes is poorly controlled."),
                 input_id = "conditionRank"
               )             
               
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
                      h4("Standard Gamble"),
                      textOutput("standardGambleIntro"),
                      br(),
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
                      h4("Time Trade-Off"),
                      textOutput("timeTradeOffIntro"),
                      br(),
                      tabsetPanel(
                        id = "ttoTabs",
                        tabPanel("Condition 1 TTO",
                                 br(),
                                 textOutput("ttoCond1Desc"),  # Add description output
                                 br(),
                                 actionButton("cond1Opt1", "I would take a treatment to live a shorter but perfectly healthy life"),
                                 actionButton("cond1Opt2", "I am indifferent"),
                                 actionButton("cond1Opt3", "I would rather live a longer life and suffer with the condition"),
                                 br(),
                                 plotlyOutput("cond1BarChart")
                        ),
                        tabPanel("Condition 2 TTO",
                                 br(),
                                 textOutput("ttoCond2Desc"),  # Add description output
                                 br(),
                                 actionButton("cond2Opt1", "I would take a treatment to live a shorter but perfectly healthy life"),
                                 actionButton("cond2Opt2", "I am indifferent"),
                                 actionButton("cond2Opt3", "I would rather live a longer life and suffer with the condition"),
                                 br(),
                                 plotlyOutput("cond2BarChart")
                        ),
                        tabPanel("Condition 3 TTO",
                                 br(),
                                 textOutput("ttoCond3Desc"),  # Add description output
                                 br(),
                                 actionButton("cond3Opt1", "I would take a treatment to live a shorter but perfectly healthy life"),
                                 actionButton("cond3Opt2", "I am indifferent"),
                                 actionButton("cond3Opt3", "I would rather live a longer life and suffer with the condition"),
                                 br(),
                                 plotlyOutput("cond3BarChart")
                        )
                      )
             ),
             tabPanel("Interview Results",
                      h4("Results from All Interviews"),
                      DTOutput("resultsTable")  
                     )
           )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive data frame to store interview results
  results <- reactiveValues(df = NULL)
  
  output$standardGambleIntro <- renderText({
    "In the Standard Gamble section, imagine you have an option to undergo a treatment that could either completely cure you or result in death. You need to decide how much risk of dying you are willing to take to be cured of the condition. The higher the risk you are willing to take tells us how you perceive the quality of life to be with that condition."
  })
  
  output$timeTradeOffIntro <- renderText({
    "In the Time Trade-Off section, imagine that there is a treatment which can completely alleviate your condition but will shorten your life. Alternatively you can choose not to take the medicine and live a longer life but you must suffer from the condition. Would you accept a shorter but fuller life over a longer life with the condition?"
  })
  
  # Initial rendering of the explanation text
  output$gambleExplanation <- renderText({
    "Welcome to the Health Condition Interview. Please click 'Start Interview' to begin."
  })
  
  observeEvent(input$start, {
    # Update descriptions for each condition
    output$ttoCond1Desc <- renderText({ gambleStates$conditionsRanked[1] })
    output$ttoCond2Desc <- renderText({ gambleStates$conditionsRanked[2] })
    output$ttoCond3Desc <- renderText({ gambleStates$conditionsRanked[3] })
    conditionNames <- sapply(strsplit(input$conditionRank, ": "), `[`, 1)
    results$df <- data.frame(
      Condition = conditionNames,
      VAS = rep(100, length(conditionNames)),
      StandardGamble = rep(100, length(conditionNames)),
      TimeTradeOff = rep(100, length(conditionNames))
    )
    hide("inputSection")  # Hide the div containing inputs and ranking
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
      sliderInput("slider1", label = strsplit(input$conditionRank[1],split=":")[[1]][1], min = 0, max = 100, value = 50),
      sliderInput("slider2", label = strsplit(input$conditionRank[2],split=":")[[1]][1], min = 0, max = 100, value = 50),
      sliderInput("slider3", label = strsplit(input$conditionRank[3],split=":")[[1]][1], min = 0, max = 100, value = 50)
    )
  })
  
  observeEvent(input$submitVAS, {
    if (input$slider1 > input$slider2 || input$slider2 > input$slider3) {
      showModal(modalDialog(
        title = "Input Error",
        "In step 1, you ranked the three conditions from lowest to lowest quality of life. You cannot give a lower ranked condition a higher quality of life percentage than the condition before it.",
        easyClose = TRUE,
        footer = modalButton("Ok")
      ))
    } else {
      updateTabsetPanel(session, "mainTabs", selected = "Interview 2 Standard Gamble")
      showModal(modalDialog(
        title = "Summary of your Visual Acuity Scale Input",
        paste("Your choices imply that for Condition 1 (", simpleConditionName(gambleStates$conditionsRanked[1]), 
              "), you perceive the quality of life to be", input$slider1, 
              "% of a year living healthily with well controlled diabetes."),
        paste("For Condition 2 (", simpleConditionName(gambleStates$conditionsRanked[2]), 
              "), that is", input$slider2, "%."),
        paste("For Condition 3 (", simpleConditionName(gambleStates$conditionsRanked[3]), 
              "), it is", input$slider3, "%."),
        footer = modalButton("Proceed to Standard Gamble")
      ))
      for (i in 1:3) {
        results$df[i, "VAS"] <- input[[paste0("slider", i)]]
      }
      output$resultsTable <- renderDT({
        req(results$df)  # Ensure the data frame is initialized
        
        # Use formatRound to specify the decimal places for the columns
        datatable(results$df, 
                  options = list(
                    pageLength = 5,
                    searching = FALSE,  # Disable the search box
                    lengthChange = FALSE  # Disable the dropdown for page length
                  ), 
                  editable = TRUE) %>%
          formatRound(columns = c('VAS', 'StandardGamble', 'TimeTradeOff'), digits = 1)
      })
      
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
    
    # Set initial values for the TTO based on half life expectancy, rounding up
    totalYears <- userResponses$lifeExpectancy
    healthyYears <- ceiling(totalYears / 2)  # Start with half healthy, rounded up if .5
    lostYears <- totalYears - healthyYears  # The rest is lost
    
    
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
    
    results$df[1, "StandardGamble"] <- 100 - gambleStates$cond1  # Assume gambleStates$cond1 holds the risk % for condition 1
    results$df[2, "StandardGamble"] <- 100 - gambleStates$cond2  # Similar for condition 2
    results$df[3, "StandardGamble"] <- 100 - gambleStates$cond3  # Similar for condition 3
    
    
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
  
  # Initialize reactive values to store TTO calculations
  gambleStatesTTO <- reactiveValues(
    healthyYears = list(cond1 = NULL, cond2 = NULL, cond3 = NULL),
    lostYears = list(cond1 = NULL, cond2 = NULL, cond3 = NULL)
  )
  
  showTTOResults <- function() {
    # Extract the percentages of healthy years based on the total life expectancy
    percentages <- sapply(names(gambleStatesTTO$healthyYears), function(conditionId) {
      100 * gambleStatesTTO$healthyYears[[conditionId]] / userResponses$lifeExpectancy
    })
    results$df[,"TimeTradeOff"]<-percentages
    updateTabsetPanel(session, "mainTabs", selected = "Interview Results")
    
    # Create the results text, using the simpleConditionName function for clarity
    resultText <- paste(sapply(seq_along(percentages), function(i) {
      paste("Your choices imply that for", simpleConditionName(gambleStates$conditionsRanked[i]), 
            "you value the quality of life of a year living under this condition at", sprintf("%.2f%%", percentages[[i]]),
            "of a year living healthily with diabetes.")
    }), collapse = "\n")
    
    # Show the modal with the formatted results
    showModal(modalDialog(
      title = "Results from Time Trade Off",
      resultText,
      footer = modalButton("Close")
    ))
  }
  
  
  # Function to handle TTO responses and navigate between tabs
  handleTTOChoice <- function(conditionId, option) {
    if (is.na(gambleStatesTTO$healthyYears[[conditionId]])) {
      gambleStatesTTO$healthyYears[[conditionId]] <- userResponses$lifeExpectancy / 2
      gambleStatesTTO$lostYears[[conditionId]] <- userResponses$lifeExpectancy - gambleStatesTTO$healthyYears[[conditionId]]
    }
    
    switch(option,
           "shorter" = {
             if (gambleStatesTTO$healthyYears[[conditionId]] > 0) {
               gambleStatesTTO$healthyYears[[conditionId]] <- gambleStatesTTO$healthyYears[[conditionId]] - 1
             }
           },
           "indifferent" = {
             # Set the next condition's starting healthy years to the current if indifferent is selected
             if (conditionId == "cond1") {
               gambleStatesTTO$healthyYears[["cond2"]] <- gambleStatesTTO$healthyYears[["cond1"]]
             } else if (conditionId == "cond2") {
               gambleStatesTTO$healthyYears[["cond3"]] <- gambleStatesTTO$healthyYears[["cond2"]]
             }
             # Automatically switch to the next TTO tab
             nextTabId <- getNextTTO(conditionId)
             if (!is.null(nextTabId)) {
               updateTabsetPanel(session, "ttoTabs", selected = nextTabId)
             }
             if (conditionId == "cond3") {
               showTTOResults()
             }
           },
           "longer" = {
             gambleStatesTTO$healthyYears[[conditionId]] <- gambleStatesTTO$healthyYears[[conditionId]] + 1
           }
    )
    gambleStatesTTO$lostYears[[conditionId]] <- userResponses$lifeExpectancy - gambleStatesTTO$healthyYears[[conditionId]]
    renderBarChart(conditionId)
  }
  
  
  
  # Listen for changes in the TTO tab selection to render the appropriate bar chart
  observe({
    req(input$ttoTabs)  # Require the ttoTabs input to initialize
    # Depending on the currently active TTO tab, render the respective bar chart
    if (input$ttoTabs == "Condition 1 TTO") {
      renderBarChart("cond1")
    } else if (input$ttoTabs == "Condition 2 TTO") {
      renderBarChart("cond2")
    } else if (input$ttoTabs == "Condition 3 TTO") {
      renderBarChart("cond3")
    }
  })
  
  # Define a helper function to determine the next TTO tab
  getNextTTO <- function(currentConditionId) {
    conditions <- c("cond1", "cond2", "cond3")
    currentIndex <- match(currentConditionId, conditions)
    if (!is.na(currentIndex) && currentIndex < length(conditions)) {
      return(paste0("Condition ", currentIndex + 1, " TTO"))
    }
    return(NULL)  # No next tab, perhaps handle the end of the survey
  }
  
  # Observers for each "I am indifferent" button
  lapply(c("cond1", "cond2", "cond3"), function(conditionId) {
    observeEvent(input[[paste0(conditionId, "Opt1")]], {
      handleTTOChoice(conditionId, "shorter")
    })
    observeEvent(input[[paste0(conditionId, "Opt2")]], {
      handleTTOChoice(conditionId, "indifferent")
      if (conditionId == "cond3") {  # Check if it is the last condition
        showTTOResults()  # Call function to show results
      }
    })
    observeEvent(input[[paste0(conditionId, "Opt3")]], {
      handleTTOChoice(conditionId, "longer")
    })
  })
  
  # Initialize the TTO states reactively
  observeEvent(input$start, {
    # Example initialization, ensuring there's always a value for the TTO calculations
    initialYears <- round(userResponses$lifeExpectancy / 2)
    gambleStatesTTO$healthyYears <- list(cond1 = initialYears, cond2 = initialYears, cond3 = initialYears)
    gambleStatesTTO$lostYears <- list(cond1 = userResponses$lifeExpectancy - initialYears,
                                      cond2 = userResponses$lifeExpectancy - initialYears,
                                      cond3 = userResponses$lifeExpectancy - initialYears)
  })
  
  # Rendering bar charts for each condition
  renderBarChart <- function(conditionId) {
    output[[paste0(conditionId, "BarChart")]] <- renderPlotly({
      # Ensure the necessary data is available
      req(gambleStatesTTO$healthyYears[[conditionId]], gambleStatesTTO$lostYears[[conditionId]])
      
      # Data for the full life expectancy bar chart (to appear on top)
      data2 <- data.frame(
        Category = "Live With the Condition",
        Years = userResponses$lifeExpectancy,
        Type = "Full Life Expectancy",
        Colors = '#3498DB'  # Blue for full life expectancy
      )
      
      # Data for the TTO bar chart
      data1 <- data.frame(
        Category = "Shorter but Better Quality of Life",
        Years = c(gambleStatesTTO$healthyYears[[conditionId]], gambleStatesTTO$lostYears[[conditionId]]),
        Type = c("Healthy Years", "Years Given Up"),
        Colors = c('#2ECC71', '#FF5733')  # Green for healthy years, red for years given up
      )
      
      # Combine data for both charts
      data_combined <- rbind(data1, data2)
      
      # Create plot
      plot_ly(data_combined, x = ~Years, y = ~Category, type = 'bar', orientation = 'h',
              color = ~Type, colors = c('Healthy Years' = '#2ECC71', 'Years Given Up' = '#FF5733', 'Full Life Expectancy' = '#3498DB'),
              text = ~paste0(Years, " years"), textposition = 'auto',
              height = 400) %>%  # Adjusted height to better accommodate two charts
        layout(
          title = "Which Would You Prefer",
          barmode = 'stack',
          xaxis = list(title = "Years"),
          yaxis = list(title = ""),
          margin = list(l = 50, r = 50, t = 50, b = 50),
          hovermode = 'closest'
        )
    })
  }
  
  
  
  # Ensure charts are rendered when the TTO tab is shown
  observeEvent(input$ttoTabs, {
    if (input$ttoTabs == "Condition 1 TTO") {
      renderBarChart("cond1")
    } else if (input$ttoTabs == "Condition 2 TTO") {
      renderBarChart("cond2")
    } else if (input$ttoTabs == "Condition 3 TTO") {
      renderBarChart("cond3")
    }
  }, ignoreInit = TRUE)
}


# Run the application
shinyApp(ui, server)