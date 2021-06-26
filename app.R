library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  navbarPage(
    "My First Application!",
    
    tabPanel("Introduction",
             div(img(src="1.png"),style="text-align: center;"),
             br(),
             div(img(src="2.png"),style="text-align: center;"),
             br()
             ), # Navbar 1, tabPanel
    tabPanel("The Basic Terms",
             div(img(src="3.png"),style="text-align: center;"),
    ),# Navbar 2, tabPanel
    tabPanel("The Calculator",
             sidebarPanel(
               h4(tags$b("Input:")),
               numericInput('weight', 'Weight (kg):', 55, min = 40, max = 300, step = 0.1),
               numericInput('height', 'Height (cm):', 150, min = 80, max = 200, step = 0.1),
               numericInput('age', 'Age:', 20, min = 1, max = 100, step = 1),
               selectInput('gender','Gender:',
                           c("Male" = "male",
                             "Female" = "female")),
               selectInput('activity','Activity level:',
                           c("Sedentary (Little or no exercise, desk job)" = "sedentary",
                             "Lightly Active (Exercise 1-2 days per week)" = "lightly_active",
                             "Moderately Active (Exercise 3-5 days per week)" = "moderately_active",
                             "Very Active (Exercise 6-7 days per week)" = "very_active",
                             "Athlete (Exercise 2x per day) " = "extra_active")),
               selectInput('goal','What is your goal?',
                           c("Lose Weight" = "lose_weight",
                             "Maintain Weight" = "maintain_weight",
                             "Gain Weight" = "gain_weight")),
               h5("Please press calculate after input and selection."),
               submitButton('Calculate')
             ), # sidebarPanel
             mainPanel(
               h3(tags$b('How much calories (kcal) shall you take?')),
               
               h4('Your', tags$b("Basal Metabolic Rate (BMR)"),'is '),
               verbatimTextOutput("BMRCalculation"), #change according to output
               tags$h4(tags$style(HTML("
                            #BMRCalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4('Your', tags$b("Total Daily Energy Expenditure (TDEE)"), 'is'),
               verbatimTextOutput("TDEECalculation"), #change according to output, TDEE is the "multiplier" variable in the RMD file.
               tags$h4(tags$style(HTML("
                            #TDEECalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4(tags$b('To achieve your goal, the amount of calories you shall take is')),
               verbatimTextOutput("GoalCalculation"), #change according to output
               tags$h4(tags$style(HTML("
                            #GoalCalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4(tags$b('Your recommended macros:')),
               plotOutput("GoalNutrients") #change according to output
             ) # mainPanel
             
    )# Navbar 3, tabPanel
    ,
    tabPanel("Food",
             h4(tags$b("I did not implement this function because MyFitnessPal has over 11 million foods in their database."),align="center"),
             h4(tags$b("Therefore, kindly press the button below to search for food information on their website."),align="center"),
             div(shiny::actionButton(inputId='myfitnesspal', label="Visit MyFitnessPal", 
                                     onclick ="window.open('https://myfitnesspal.com')"),
                 style="text-align: center;"
                 )
             
    )# Navbar 4, tabPanel
    ,
    
    tabPanel("About me",
             div(img(src="4.png"),style="text-align: center;"), 
             tags$head(tags$style('.btn{ margin-left: 15px;}')),  # add the spacing
             div(shiny::actionButton(inputId='ab1', label="", 
                                 icon = icon("fab fa-youtube-square","fa-5x"), 
                                 onclick ="window.open('https://www.youtube.com/channel/UCSTaX4kxkpHHVPC_GSnb9Jg')"),
                 shiny::actionButton(inputId='ab2', label="", 
                                     icon = icon("fab fa-instagram", "fa-5x"), 
                                     onclick ="window.open('http://instagram.com/meowmeowxuhao')"),
                 shiny::actionButton(inputId='ab3', label="", 
                                     icon = icon("fab fa-facebook-square","fa-5x"), 
                                     onclick ="window.open('http://facebook.com/xuhaowrites')"),
                 style="text-align: center;"
             )
    )
    )
  # navbarPage
)# fluidPage

BMR <- function(gender,weight, height, age) {
  if (gender == 'male'){
    return(5 + (10*weight)+ 6.25*(height) - (5*age))
  }
  else if (gender == 'female'){
    return(-161 +(10*weight)+6.25*(height)-(5*age))
  }
}
TDEE <- function(activity,b) {
  if(activity == 'sedentary'){
    b*1.2
  }else if(activity == 'lightly_active'){
    b*1.375
  }else if(activity == 'moderately_active'){
    b * 1.55
  }else if(activity == 'very_active'){
    b *1.725
  }else if(activity == 'extra_active'){
    b*1.9
  }
}

GOAL_CAL <- function(goal,t) {
  if(goal == 'lose_weight'){
    t*0.9
  }else if(goal == 'maintain_weight'){
    t*1
  }else{
    t*1.1
  }
}

GOAL_NUTRIENTS <- function(goal,gc) {
  if(goal == 'lose_weight'){
    
    carbs_cal = (40/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (30/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
  }else if(goal == 'maintain_weight'){
    
    carbs_cal = (50/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (20/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    # return("50% Carbohydrates, 30% Protein, 20% Fat")
  }else if(goal == 'gain_weight'){
    
    carbs_cal = (55/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (15/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    #return("55% Carbohydrates, 30% Protein, 15% Fat")
  }
  
}

# Define server function  
server <- function(input, output,session) {
  library(ggplot2)
  bmr <- reactive({BMR(input$gender,input$weight,input$height,input$age)})
  output$BMRCalculation <- renderPrint(bmr())
  
  tdee <- reactive({TDEE(input$activity,bmr())})
  output$TDEECalculation <- renderPrint(tdee())
  
  goal_cal <- reactive({GOAL_CAL(input$goal,tdee())})
  output$GoalCalculation <- renderPrint(goal_cal())
  
  output$GoalNutrients <- renderPlot(GOAL_NUTRIENTS(input$goal,goal_cal()),bg="gray96")
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)