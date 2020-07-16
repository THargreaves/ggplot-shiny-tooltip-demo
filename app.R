library(Cairo)
library(dplyr)
library(gapminder)  # year + country
library(ggplot2)
library(magrittr)

library(shiny)
library(shinyBS)
library(shinydashboard)

# Total price of diamonds for each color-cut combination
diamonds_total_prices <- diamonds %>%
  group_by(color, cut) %>%
  summarise(total_price = sum(price), .groups = 'drop') %>%
  ungroup()


ui <- dashboardPage(
  dashboardHeader(title = 'Tooltip Demo'),
  dashboardSidebar(
    selectInput(
      'plot_layout',
      'Plot Layout',
      choices = c('Stacked', 'Side-by-Side', 'Expanded')
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="icon", type="image/x-icon", href="favicon.ico"),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
      tags$script(src = 'main.js')
    ),
    fluidRow(
      box(
        id = 'plot_container',
        plotOutput('plot', hover = hoverOpts(
          id = 'plot_hover',
          delay = 100,
          delayType = 'throttle'
        )
      ),
        uiOutput('plot_tooltip'),
        width = 12)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$plot <- renderPlot({
      p <- ggplot(diamonds_total_prices,
                  aes(x = color, y = total_price, fill = cut))

      # Specify positioning and y-axis label
      if (input$plot_layout == 'Stacked') {
        p <- p +
          geom_col(position = 'stack', alpha = 0.7, col = 'black') +
          scale_y_continuous(name = 'Cummulative Total Price')
      } else if (input$plot_layout == 'Side-by-Side') {
        p <- p +
          geom_col(position = 'dodge', alpha = 0.7, col = 'black') +
          scale_y_continuous(name = 'Total Price')
      } else if (input$plot_layout == 'Expanded') {
        p <- p +
          geom_col(position = 'fill', alpha = 0.7, col = 'black') +
          scale_y_continuous(name = 'Proportional Total Price')
      }
      
      p <- p +
        scale_x_discrete("Colour") +
        scale_fill_discrete("Cut") +
        theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      p
    })

    # Find the selected colour if any
    color <- reactive({
      req(input$plot_hover)
      x_pos <- input$plot_hover$x
      valid_x <- 0.5 < x_pos &&
        x_pos < length(levels(diamonds$color)) + 0.5 &&
        abs(x_pos - round(x_pos)) < 0.45
      if (valid_x) {
        levels(diamonds$color)[round(input$plot_hover$x)]
      } else {
        NULL
      }
    })
    
    # Totals for each cut of selected colour
    totals <- reactive({
      req(color())
      diamonds_total_prices %>%
        filter(color == color()) %>%
        # Order by factor levels
        arrange(-as.numeric(cut)) %>%
        extract2('total_price')
    })

    # Index of selected cut
    clicked <- reactive({
      req(input$plot_hover)
      req(totals())
      req(color())
      if (input$plot_hover$y <= 0) {
        return(NULL)
      }
      
      if (input$plot_layout == 'Stacked') {
        boundaries <- which(cumsum(totals()) > input$plot_hover$y)
        if (!any(boundaries)) {
          NULL
        } else {
          which.max(boundaries)
        }
      } else if (input$plot_layout == 'Side-by-Side') {
        sub_x <- (input$plot_hover$x - (round(input$plot_hover$x) - 0.45)) / 0.9
        clicked <- ceiling(sub_x * length(levels(diamonds$cut)))

        if (totals()[length(totals()) + 1 - clicked] >= input$plot_hover$y) {
          clicked
        } else {
          NULL
        }
      } else if (input$plot_layout == 'Expanded') {
        boundaries <- which(cumsum(totals()) / sum(totals()) > input$plot_hover$y)
        if (!any(boundaries)) {
          NULL
        } else {
          which.max(boundaries)
        }
      }
    })

    clarity <- reactive({levels(diamonds$cut)[clicked()]})
    price <- reactive({rev(totals())[clicked()]})

    output$plot_tooltip <- renderUI({
      req(color)
      req(clicked())

      verbatimTextOutput("tooltip_text")
    })

    output$tooltip_text <- renderPrint({
      req(color())
      req(clicked())
      cat(paste(
          c('Colour:', 'Clarity:', 'Total Price:'),
          c(color(), clarity(),
            paste0('$', format(price(), big.mark = ','))),
          collapse = "\n"
      ))
    })
}

shinyApp(ui, server)
