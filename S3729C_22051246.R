# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)
library(DT)

# Load data --------------------------------------------------------------------

HMv10 <- read.csv(file = "https://raw.githubusercontent.com/lme2022/S3729C_Intake04/main/HealthyMediav10.csv", header = TRUE, sep = ",")
all_Ages <- sort(unique(HMv10$Age))
min_date <- min(as.numeric(as.character(HMv10$DateDone)))
max_date <- max(as.numeric(as.character(HMv10$DateDone)))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("united"),

h1("Reviews of the Ageing: Mobility & Fitness Video"),
h4(tags$a(href = "https://shiny.rstudio.com/", "Powered by R Shiny")),
                
    sidebarLayout(
    sidebarPanel(
      
      HTML(paste0("HMv10 released between the following dates will be plotted.")),
      
      br(), br(),
      
      dateRangeInput(
        inputId = "date",
        label = "Select dates:",
        start = "2022-08-27", end = "2022-09-03",
        min = min_date, max = max_date,
        startview = "year"
      ),
      
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "People found information useful" = "InformationUseful",
          "People found video duration sufficient " = "VideoDuration",
          "People improved their exercise knowledge" = "ImproveExerciseKnowledge",
          "People understood importance of exercise" = "UnderstandExerciseImportance",
          "People learnt how to exercise" = "KnowHowToExercise"
        ),
        selected = "VideoDuration"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "People found information useful" = "InformationUseful",
          "People found video duration sufficient " = "VideoDuration",
          "People improved their exercise knowledge" = "ImproveExerciseKnowledge",
          "People understood importance of exercise" = "UnderstandExerciseImportance",
          "People learnt how to exercise" = "KnowHowToExercise"
        ),
        selected = "InformationUseful"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Date completed" = "DateDone",
          "Age" = "Age",
          "Gender" = "Gender"
          ),
        selected = "Age"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Please enter title for your plot"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      ),
      
      br(), br(),
      
      selectInput(
        inputId = "Age",
        label = "Select the Age range:",
        choices = all_Ages,
        selected = "50-59",
        multiple = TRUE
      ),
      
      downloadButton('download',"Download data")
      
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", hover = "plot_hover"),
      dataTableOutput(outputId = "HMv10tablehover"),
      br(),
      dataTableOutput(outputId = "HMv10table")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    req(input$date)
    movies_selected_date <- HMv10 %>%
      filter(DateDone >= as.POSIXct(input$date[1]) & DateDone <= as.POSIXct(input$date[2]))
      ggplot(data = HMv10, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$HMv10tablehover <- renderDataTable({
    nearPoints(HMv10, input$plot_hover) %>%
      select(ID, DateDone, Age, Gender)
    
  })
    
  output$HMv10table <- renderDataTable({
    req(input$Age)
    newdata_from_selected_ages <- HMv10 %>%
      filter(Age %in% input$Age) %>%
      select(DateDone:Gender)
    DT::datatable(
      data = newdata_from_selected_ages,
      options = list(pageLength = 10),
      rownames = FALSE)
    
  })

  
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
