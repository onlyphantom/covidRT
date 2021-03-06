library(shiny)

ui <- navbarPage(title="Web Dashboard",
                 tabPanel("Global",
                          sidebarLayout(
                            mainPanel(
                              fluidRow(
                                column(4, 
                                       h3("Total Cases"), 
                                       wellPanel(textOutput("totalCases"))),
                                column(4, 
                                       h3("Total Recovered"),
                                       wellPanel()),
                                column(4,
                                       h3("Total Deaths"),
                                       wellPanel())
                              )
                            ),
                            sidebarPanel(
                              h2("Covid-19 Analytics"),
                              helpText(textOutput('dateToday'),
                                       "Data source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University. View the full list of data sources in Credits."),
                              selectInput("topN", 
                                          label="Countries to display",
                                          choices = c("Top 10 by cases", "Top 20 by cases", "Top 30 by cases"), selected = "Top 20 by cases"),
                              radioButtons("plotType", 
                                           label="Plot type",
                                           choices=c("Bars", "Points")
                              ),
                              selectInput("sortBy", 
                                          label="Order Criteria",
                                          choices = c("Default (Alphabetical)","Confirmed Cases", "Recovered", "Deaths")),
                              checkboxInput("medianLine",
                                            label = "Plot Median (Confirmed Cases) Line", 
                                            value=TRUE)
                            )
                          )
                 ),
                 tabPanel("All Countries (Latest)"),
                 tabPanel("Credits"),
                 tabPanel("Covid-19 Prevention Advice")
)

server <- function(input, output) {
  output$totalCases <- renderText({10000})
  output$dateToday <- renderText({paste("Date: ",as.character(Sys.Date()))})
}
shinyApp(ui = ui, server = server)