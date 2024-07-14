# Declare libraries and global dataframe

# Sited source of dataset:
# Kimmons R (2018). Students Performance in Exams [Fictional dataset]. https://www.kaggle.com/datasets/spscientist/students-performance-in-exams/data

library(shiny)
library(ggplot2)
library(dplyr)

df = read.csv("D:/Professional/KUMC Files/2024 Summer/Data 824/Final Project/Data sets/StudentsPerformance.csv")

ui <- fluidPage(
  #Title and disclaimer for Shiny App
  titlePanel("Compare Test Score backgrounds"),
  HTML('<p style="font-size:small; color:gray;">If the chart disappears after selecting a filter, there is no data on the combination of filtered items selected.</p>'),
  
  # Create 3 sets of 5 dropdowns for filter on Gender, Race/Ethnicity, Parents educational background, lunch accessibility, and Test Prep and plot bar chart from filters
  fluidRow(
    column(4, 
      # Dropdown menus and bar chart 1
      h3("Chart 1"),
      selectInput("gender1", "Select a Gender option:", choices = c("All", unique(df$gender)[order(unique(df$gender))])),
      selectInput("raceEthn1", "Select a Race/Ethnic group:", choices = c("All", unique(df$raceEthn)[order(unique(df$raceEthn))])),
      selectInput("parentEducation1", "Select a Parents educational background:", choices = c("All", unique(df$parentEducation)[order(unique(df$parentEducation))])),
      selectInput("lunch1", "Select a lunch accessability option:", choices = c("All", unique(df$lunch)[order(unique(df$lunch))])),
      selectInput("testPrep1", "Select a test prep option:", choices = c("All", unique(df$testPrep)[order(unique(df$testPrep))])),
      plotOutput("barChart1")
    ),
      
    column(4, 
      # Dropdown menus and bar chart 2
      h3("Chart 2"),
      selectInput("gender2", "Select a Gender option:", choices = c("All", unique(df$gender)[order(unique(df$gender))])),
      selectInput("raceEthn2", "Select a Race/Ethnic group:", choices = c("All", unique(df$raceEthn)[order(unique(df$raceEthn))])),
      selectInput("parentEducation2", "Select a Parents educational background:", choices = c("All", unique(df$parentEducation)[order(unique(df$parentEducation))])),
      selectInput("lunch2", "Select a lunch accessability option:", choices = c("All", unique(df$lunch)[order(unique(df$lunch))])),
      selectInput("testPrep2", "Select a test prep option:", choices = c("All", unique(df$testPrep)[order(unique(df$testPrep))])),
      plotOutput("barChart2")
    ),
      
    # Dropdown menus and bar chart 3
    column(4, 
      h3("Chart 3"),
      selectInput("gender3", "Select a Gender option:", choices = c("All", unique(df$gender)[order(unique(df$gender))])),
      selectInput("raceEthn3", "Select a Race/Ethnic group:", choices = c("All", unique(df$raceEthn)[order(unique(df$raceEthn))])),
      selectInput("parentEducation3", "Select a Parents educational background:", choices = c("All", unique(df$parentEducation)[order(unique(df$parentEducation))])),
      selectInput("lunch3", "Select a lunch accessability option:", choices = c("All", unique(df$lunch)[order(unique(df$lunch))])),
      selectInput("testPrep3", "Select a test prep option:", choices = c("All", unique(df$testPrep)[order(unique(df$testPrep))])),
      plotOutput("barChart3")
    )
  )
)
    

# Create server
server <- function(input, output) {
  
  # Generate Barchart(s) based off of filters selected
  generateBarChart <- function(Igender, IraceEthn, IparentEducation, Ilunch, ItestPrep) {
    filtered_data <- df %>%
      filter(
        (gender == Igender | Igender == "All"),
        (raceEthn == IraceEthn | IraceEthn == "All"),
        (parentEducation == IparentEducation | IparentEducation == "All"),
        (lunch == Ilunch | Ilunch == "All"),
        (testPrep == ItestPrep | ItestPrep == "All")
      ) %>%
      group_by(exam) %>%
      summarize(avgScore = mean(score))
    
    n = count(df %>% filter(
        (gender == Igender | Igender == "All"),
        (raceEthn == IraceEthn | IraceEthn == "All"),
        (parentEducation == IparentEducation | IparentEducation == "All"),
        (lunch == Ilunch | Ilunch == "All"),
        (testPrep == ItestPrep | ItestPrep == "All"))) / 3
        
    
    ggplot(filtered_data, aes(x = exam, y = avgScore, fill = exam)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = avgScore), position = position_dodge(width = 0.9), vjust = -0.25) +
      theme_minimal() +
      guides(fill = FALSE) +
      coord_cartesian(ylim = c(0, 100)) +
      xlab("Exam") +
      ylab("Average Score (Out of 100)") +
      labs(title = paste0("Average Test Scores (n = ", n, ")"),
           subtitle = paste0("Gender: ", Igender,
                             "\nRace/Ethnicity: ", IraceEthn,
                             "\nParents Education: ", IparentEducation,
                             "\nLunch accessibility: ", Ilunch,
                             "\nTest Prep: ", ItestPrep)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20, face = "bold"))
  }
  
  
  output$barChart1 <- renderPlot({
    generateBarChart(input$gender1, input$raceEthn1, input$parentEducation1, input$lunch1, input$testPrep1)
  })
  
  output$barChart2 <- renderPlot({
    generateBarChart(input$gender2, input$raceEthn2, input$parentEducation2, input$lunch2, input$testPrep2)
  })
  
  output$barChart3 <- renderPlot({
    generateBarChart(input$gender3, input$raceEthn3, input$parentEducation3, input$lunch3, input$testPrep3)
  })
}

shinyApp(ui = ui, server = server)
