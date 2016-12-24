# Define UI
shinyUI(navbarPage("Santander product recommendation",
                   theme = shinytheme("cerulean"), #united
                   tabPanel("Predictions aligner",
                            sidebarPanel(
                              selectInput("targetProduct", "Studied product",
                                          selected = defaultProduct, multiple = FALSE,
                                          choices = productsSimple),
                              fluidRow(column(3, actionButton("prevProduct", "Previous",
                                                              icon = icon("arrow-left"))),
                                       column(3, actionButton("nextProduct", "Next",
                                                              icon = icon("arrow-right")),
                                              offset=1)
                              ),
                              br(),
                              numericInput("inspectRow", "inspect row", value = 1),
                              fluidRow(column(3, actionButton("prevRow", "Previous",
                                                              icon = icon("arrow-left"))),
                                       column(3, actionButton("nextRow", "Next",
                                                              icon = icon("arrow-right")),
                                              offset=1)
                              )
                            ),
                            # Visual inspection main panel
                            mainPanel(
                              plotlyOutput("countByLagPlotly")
                            )
                   )
))