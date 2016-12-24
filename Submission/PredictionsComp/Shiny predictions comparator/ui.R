# Define UI
shinyUI(navbarPage("Santander product recommendation",
                   theme = shinytheme("cerulean"), #united
                   tabPanel("Base predictions analysis",
                            sidebarPanel(
                              selectInput("targetVar", "Considered product", productsSimple,
                                          selected=standardProd),
                              fluidRow(column(5, actionButton("prevTarget", "Previous",
                                                              icon = icon("arrow-left"))),
                                       column(5, actionButton("nextTarget", "Next",
                                                              icon = icon("arrow-right")),
                                              offset=1)
                              ),
                              br(),
                              radioButtons("normalizeMode", "Normalize mode", choices = c("None", "Exponential"), selected = "Exponential", inline = TRUE),
                              h4(textOutput("nbPrevZerosDescription")),
                              h4(textOutput("nbTopProductsDescription")),
                              h4(textOutput("nbTopProductsPrevPosDescription")),
                              h5(textOutput("relMAPContributionDescription")),
                              h5(textOutput("probMultiplierDescription")),
                              br(),
                              sliderInput("lagRange", "Lag range", min=1, max=16, value=c(1, 16))
                            ),
                            
                            # Visual inspection main panel
                            mainPanel(
                              tabsetPanel(id = "mainPanelProdAnalysis",
                                          tabPanel("Mean base predictions",
                                                   br(),
                                                   plotlyOutput("meanByMonthPlotly")
                                          ),
                                          tabPanel("Predictions by month",
                                                   br(),
                                                   # plotlyOutput("predsByMonthPlotly")
                                                   plotOutput("predsByMonthPlot")
                                          ),
                                          tabPanel("Base model confidence",
                                                   br(),
                                                   plotlyOutput("confidenceByMonthPlotly")
                                          ),
                                          tabPanel("Base predictions correlations",
                                                   br(),
                                                   plotlyOutput("basePredCorsPlotly")
                                          ),
                                          tabPanel("Base lag scatter comparison",
                                                   br(),
                                                   fluidRow(column(4, selectInput("lag1Compare", "Lag 1", 1:16, 5), offset = 1),
                                                            column(4, selectInput("lag2Compare", "Lag 2", 1:16, 11), offset = 1)
                                                   ),
                                                   fluidRow(column(2, actionButton("prevLag1Compare", "Previous",
                                                                                   icon = icon("arrow-left")), offset=1),
                                                            column(2, actionButton("nextLag1Compare", "Next",
                                                                                   icon = icon("arrow-right")),
                                                                   offset=0),
                                                            column(2, actionButton("prevLag2Compare", "Previous",
                                                                                   icon = icon("arrow-left")), offset=1),
                                                            column(2, actionButton("nextLag2Compare", "Next",
                                                                                   icon = icon("arrow-right")),
                                                                   offset=0)
                                                   ),
                                                   fluidRow(column(8, radioButtons("rankMethod", "Rank calculation", choices = c("Weighted", "First lag", "Second lag"), inline = TRUE),
                                                                   offset = 3)),
                                                   plotlyOutput("lagComparePlotly")
                                          ),
                                          tabPanel("Predictions scatter comparison",
                                                   br(),
                                                   fluidRow(column(4, selectInput("otherTargetCompare", "Compared product", productsSimple,
                                                                                  selected="nom_pens"), offset = 1),
                                                            column(4, selectInput("lagOtherCompare", "Compare lag", 1:16, 5), offset = 1)
                                                   ),
                                                   plotlyOutput("predictionsComparePlotly")
                                          )
                              )
                            )
                   )
                   # ,
                   # # About tab
                   # tabPanel("About",
                   #          h4(HTML(aboutString))
                   # )
))