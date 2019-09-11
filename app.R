#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)
theme_set(theme_bw() + theme(legend.position = "none", strip.text = element_text(size = 12), axis.text = element_text(size=14), axis.title = element_text(size=16)))

iris %>%
  rename(`Sepal Width` = Sepal.Width, 
         `Petal Width` = Petal.Width, 
         `Sepal Length` = Sepal.Length, 
         `Petal Length` = Petal.Length) %>%
  mutate(Species = str_to_title(Species)) -> iris_tib
  
ui <- dashboardPage(
  dashboardHeader(title = "Visualizing quantitative data", titleWidth = "300px"), 
  dashboardSidebar(
    selectInput("var", "Variable to plot:", choices = c("Sepal Width", "Sepal Length", "Petal Width", "Petal Length"), selected = "Sepal Width"),
    radioButtons("showmean", "Show mean on histogram and boxplot?", choices = c("No", "Yes"), selected = "No")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "About", 
          helpText("This website allows you to examine distributions of traits for three species of iris: Setosa, Versicolor, and Virginica (50 flowers from each species). Choose the trait you want to visualize on the left. You can also choose whether or not to view the ", tags$b("mean"), " of the trait distribution as part of the plot. You can use this feature to test your skills at identifying the mean of a distribution. Furthermore, you can compare how each type of plot (histogram, boxplot, and barplot) look when depicting the same distribution. This website was developed by Stephanie J. Spielman (spielman <at> rowan <dot> edu)."), 
          img(src='iris_pics.png', align = "center", width = "100%")),
      box(title = "Histogram", sliderInput("bins",
                                           "Number of histogram bins:",
                                           min = 1,
                                           max = 50,
                                           value = 30),
          plotOutput("histPlot")
      ),
      
      box(title = "Boxplot",
          plotOutput("boxPlot")
      ),
      box(title = "Barplot",
          plotOutput("barPlot")
      )
    ) # fluidrow
  )
)



# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Plotting quantitative data"),
#    
#    sidebarLayout(
#       sidebarPanel(
#         selectInput("var", "Variable to plot:", choices = c("Sepal Width", "Sepal Length", "Petal Width", "Petal Length"), selected = "Sepal Width"),
#         radioButtons("showmean", "Show mean on histogram and boxplot?", choices = c("No", "Yes"), selected = "No")
#       ),
#     
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#         h2("Histogram"),
#         sliderInput("bins",
#                     "Number of histogram bins:",
#                     min = 1,
#                     max = 50,
#                     value = 30),
#         plotOutput("histPlot"),
#         h2("Boxplot"),
#         plotOutput("boxPlot"),
#         h2("Bar plot"),
#         plotOutput("barPlot")
#       )
#    )
# )

server <- function(input, output) {
   
  
  iris_tib_sum <- reactive({
      iris_tib %>%
        group_by(Species) %>%
        summarize(meanvar = mean(!!(sym(input$var))), 
                  sdvar   = sd(!!(sym(input$var))))
  })
  
  output$histPlot <- renderPlot({

    iris_tib %>%
      ggplot(aes(x = !!(sym(input$var)), fill = Species)) + geom_histogram(bins = input$bins, color = "black") + facet_grid(~Species) + scale_y_continuous(expand = c(0,0))-> p
    
      if (input$showmean == "Yes") p <- p + geom_vline(data = iris_tib_sum(), aes(xintercept=meanvar), color = "black", size=1)
    p
   })

  output$boxPlot <- renderPlot({
    iris_tib %>%
      ggplot(aes(x = Species, y = !!(sym(input$var)), fill = Species)) + geom_boxplot() -> p
    
    if (input$showmean == "Yes") p <- p + geom_point(data = iris_tib_sum(), aes(x = Species, y = meanvar), fill = "black",color = "yellow", size = 5, pch = 23) 
    p
  })
  
  output$barPlot <- renderPlot({
    
    iris_tib %>%
      dplyr::select(Species, !!input$var) %>%
      ggplot(aes(x = Species, y = !!(sym(input$var)), fill = Species)) + 
      stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
      stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width=0.3, size=1)
    # iris_tib %>%
    #   dplyr::select(Species, !!input$var) %>%
    #   group_by(Species) %>%
    #   summarize(meanvar = mean(!!input$var), sdvar = sd(!!input$var))
    #   ggplot(aes(x = Species, y = input$var, fill = Species)) + geom_bar(stat="identity") +
    #   geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
    #                 width=.2,                    # Width of the error bars
    #                 position=position_dodge(.9))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


















