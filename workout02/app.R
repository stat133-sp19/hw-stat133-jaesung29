# title: "app.R"
# author: "Jaesung Lee"
# date: "April 15, 2019"

library(shiny)
library(ggplot2)
library(rsconnect)

# source functions
source('functions.R')

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Savings Simulations"),
  
  hr(),
  fluidRow(
    column(4,
           sliderInput("initial_a",
                       label = "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000),
           sliderInput("annual_c",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000)),
    column(4,
           sliderInput("return_r",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 5),
           sliderInput("growth_r",
                       label = "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)),
    column(4,
           sliderInput("years",
                       label = "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20),
           selectInput("facet",
                       label = "Facet?",
                       choices = c("No", "Yes"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Timelines"),
      plotOutput(width = "auto", "freqs_plot"),
      h4("Balances"),
      tableOutput("summary_table")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  modalities2 <- reactive({
    A <- rep(0, input$years)
    B <- rep(0, input$years)
    C <- rep(0, input$years)
    
    for (i in 0:input$years) {
      A[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i)
      B[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i) + annuity(contrib = input$annual_c, rate = 0.01*input$return_r, years = i)
      C[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i) + growing_annuity(contrib = input$annual_c, rate = 0.01*input$return_r, growth = 0.01*input$growth_r, years = i)
    }
    
    savings = c(rep("nocontrib", input$years+1), rep("fixed_contrib", input$years+1), rep("growing_contrib", input$years+1))
    
    modalities2 <- data.frame(
      year = c(0:input$years),
      returns = c(A, B, C),
      variable = factor(savings, levels = c("nocontrib", "fixed_contrib", "growing_contrib"))
    )
  })
    
  modalities <- reactive({
    A <- rep(0, input$years)
    B <- rep(0, input$years)
    C <- rep(0, input$years)
    
    for (i in 0:input$years) {
      A[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i)
      B[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i) + annuity(contrib = input$annual_c, rate = 0.01*input$return_r, years = i)
      C[i+1] <- future_value(amount = input$initial_a, rate = 0.01*input$return_r, years = i) + growing_annuity(contrib = input$annual_c, rate = 0.01*input$return_r, growth = 0.01*input$growth_r, years = i)
    }
    
    modalities <- data.frame(
      year = c(0:input$years),
      nocontrib = A,
      fixed_contrib = B,
      growing_contrib = C,
      stringsAsFactors = FALSE
    )
  })

  output$freqs_plot <- renderPlot({
    if (input$facet == "No") {
      ggplot(data = modalities2(), aes(x = year, y = returns, group = variable)) +
        geom_point(aes(color = variable)) + geom_path(aes(color = variable)) +
        xlab("Year") +
        ylab("Return") +
        ggtitle("Three modes of investing")
    } else {
      ggplot(data = modalities2(), aes(x = year, y = returns, group = variable)) +
        geom_point(aes(color = variable)) + geom_path(aes(color = variable)) + geom_area(aes(fill = variable), alpha = 0.4) +
        xlab("Year") +
        ylab("Return") +
        ggtitle("Three modes of investing") +
        facet_grid(~ variable) + 
        theme_bw()
    }
  })
  
  output$summary_table <- renderTable({
    modalities()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)