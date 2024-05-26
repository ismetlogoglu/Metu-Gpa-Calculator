library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("GPA Calculator"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_btn", "Add Course"),
      actionButton("calculate_gpa", "Calculate GPA"),
      hr(),
      uiOutput("courses_ui")
    ),
    mainPanel(
      h4("Your GPA is:"),
      textOutput("gpa_output")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the number of courses
  course_count <- reactiveVal(1)
  
  # Observe add button to add new course inputs
  observeEvent(input$add_btn, {
    course_count(course_count() + 1)
  })
  
  # Generate UI for courses
  output$courses_ui <- renderUI({
    course_count_val <- course_count()
    lapply(1:course_count_val, function(i) {
      fluidRow(
        column(6, numericInput(paste0("credit_", i), "Credit Hours", value = 3, min = 1)),
        column(6, selectInput(paste0("grade_", i), "Grade",
                              choices = c("AA" = 4.0, "BA" = 3.5, "BB" = 3.0, 
                                          "CB" = 2.5, "CC" = 2.0, "DC" = 1.5, 
                                          "DD" = 1.0, "FD" = 0.5, "FF" = 0)))
      )
    })
  })
  
  # Calculate GPA
  observeEvent(input$calculate_gpa, {
    course_count_val <- course_count()
    total_credits <- 0
    total_points <- 0
    
    for(i in 1:course_count_val) {
      credit <- input[[paste0("credit_", i)]]
      grade <- as.numeric(input[[paste0("grade_", i)]])
      total_credits <- total_credits + credit
      total_points <- total_points + (credit * grade)
    }
    
    gpa <- total_points / total_credits
    output$gpa_output <- renderText({
      sprintf("CGPA: %.2f", gpa)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

