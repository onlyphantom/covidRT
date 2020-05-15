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
                              ),
                              h1(textOutput('projectTitle'))
                            ),
                            sidebarPanel(
                              h2("Covid-19 Analytics"),
                              helpText(textOutput('dateToday'),
                                "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur sit amet quam dui. Pellentesque feugiat, ligula vel tempor pellentesque, orci massa faucibus nisl, eu pulvinar arcu justo ac arcu. Mauris libero est, luctus sit amet luctus non, euismod vel mi. Donec pretium et lectus vel sagittis. Nam convallis justo eget malesuada luctus."),
                              textInput('userProjectTitle', label = "Project Title", placeholder = "My awesome visualization")
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
  output$projectTitle <- renderText({toupper(input$userProjectTitle)})
}
shinyApp(ui = ui, server = server)