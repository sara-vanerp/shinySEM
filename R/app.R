# SHINY SEM
# Author: Sara van Erp

library(shiny)
library(lavaan)
library(tidySEM)
library(plotly)

source("./functions_shinySEM.R")

ui <- fluidPage(

    titlePanel("Shiny SEM"),

    sidebarLayout(
        sidebarPanel(
            # upload data
            fileInput("file", "Upload your data file (CSV), including variable names.",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                      ),
            radioButtons("sep", "Which character separates different fields?",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            tags$hr(),

            # specify model
            textAreaInput("model", p("Specify the model using", a("lavaan syntax.", href = "https://lavaan.ugent.be/index.html")),
                          value = "
                          # latent variable definitions
                            ind60 =~ x1 + x2 + x3
                            dem60 =~ y1 + y2 + y3 + y4
                            dem65 =~ y5 + y6 + y7 + y8
                          # regressions
                            dem60 ~ ind60
                            dem65 ~ ind60 + dem60
                          # residual correlations
                            y1 ~~ y5
                            y2 ~~ y4 + y6
                            y3 ~~ y7
                            y4 ~~ y8
                            y6 ~~ y8 ",
                           rows = 15),

            # plotting options
            selectInput("layout", "Which layout to use for the model plot?",
                         choices = c("Tree" = "layout_as_tree",
                                     "Star" = "layout_as_star",
                                     "Circle" = "layout_as_circle",
                                     "Nice" = "layout_nicely",
                                     "Grid" = "layout_on_grid",
                                     "Random" = "layout_randomly"),
                         selected = "layout_as_tree"),
        ),

        mainPanel(
            plotlyOutput("mod.plot"),

            textOutput("info")
        )
    )
)


server <- function(input, output) {

    # read data file
    dat <- reactive({
        req(input$file)

        read.csv(input$file$datapath,
                 sep = input$sep)
    })

    # add labels to the model and fit using lavaan
    fit <- reactive({
        req(input$model)

        mod.lbl <- label_syntax_fun(toString(input$model))
        #mod.fit <- sem(mod.lbl, data = dat())
        mod.fit <- sem(mod.lbl, data = PoliticalDemocracy) #TODO: use input data instead of placeholder
    })

    # plot the model
    # Note: custom layout not working yet
    output$mod.plot <- renderPlotly({
        ggplotly(plot_fun(fit(), layout = input$layout), tooltip = "text")
    })

    click_data <- reactive({
        click <- event_data("plotly_click")$customdata
    })

    output$clicks <- renderPrint({
        data.frame(click_data())
    })

    output$info <- renderText({
        if(is.null(click_data())){
            print("Please select a parameter in the model by clicking on one of the labels")
        } else if(grepl("l", click_data()) == TRUE){ # loadings
            print("This is a factor loading. A factor loading represents the presumed causal effect of the factor on the observed score. A factor loading can be reported in unstandardized or standardized form and interpreted similarly as a regression coefficient.")
        } else if(grepl("v", click_data()) == TRUE){ # latent variable variance
          print("This is a latent variable variance term. Just like observed variables, scores on latent variables vary between the units of observation and this variation is captured by the latent variable variance term. However, unlike observed variables, latent variables do not have a natural scale. Therefore, each latent variable must be assigned a scale in order to identify and thus be able to estimate the model. The scale of a latent variable can be set either by fixing the unstandardized factor loading for one indicator to 1 (unit loading identification) or by fixing the latent variable variance term to 1 (unit variance identification).")
        } else if(grepl("e", click_data()) == TRUE){ # measurement error
            print("This is a measurement error term. The variance for each observed score can be split into two components: a common and unique component. The common component refers to the part of the variance that is explained by the factors to which the indicator belongs. The unique variance, on the other hand, is represented by the measurement error and reflects all other sources of variation that are not explained by the model. This includes random error (score unreliability) and all sources of systematic variance not due to the factors.")
        } else if(grepl("r", click_data()) == TRUE){ # correlations
            print("This is a correlation. A correlation reflects the assumption that two variables have something in common that is not explicitly represented in the model. Correlations are often included between factors in a confirmatory factor model and sometimes between measurement errors to reflect that indicators are associated beyond the association that can be explained by the underlying factors. In case of correlated measurement errors, we speak of a nonstandard factor model.")
        } else if(grepl("b", click_data()) == TRUE){ # structural regression parameters
            print("This is a structural regression parameter. A structural regression parameter represents the presumed causal effect between two latent variables, or between a latent and observed variable. A structural regression parameter can be reported in unstandardized or standardized form and interpreted similarly as a regression coefficient.")
        }
    })

}

shinyApp(ui = ui, server = server)
