library(shiny)
library(DT)
library(ggplot2)
source("reader.R")
source("preprocess.R")

corona <- read_github()
corona_latest <- subset(corona, date == max(corona$date))
melted_latest <- melt_all(corona_latest)

ui <- navbarPage(title="Web Dashboard",
                 tabPanel("Global",
                          sidebarLayout(
                            mainPanel(
                              fluidRow(
                                column(4, 
                                       h6("Total Cases"), 
                                       wellPanel(h3(textOutput("totalCases")),
                                                 p(textOutput("totalCasesInc"))
                                       )
                                ),
                                column(4, 
                                       h6("Total Recovered"), 
                                       wellPanel(h3(textOutput("totalRecovery")),
                                                 p(textOutput("totalRecoveryInc"))
                                       )
                                ),
                                column(4, 
                                       h6("Total Deaths"), 
                                       wellPanel(h3(textOutput("totalDeaths")),
                                                 p(textOutput("totalDeathsInc"))
                                       )
                                )
                              ), #end fluidRow
                              plotOutput("globalPlot")
                            ), # end mainPanel
                            sidebarPanel(
                              h2("Covid-19 Analytics"),
                              helpText(textOutput('dateUpdated'),
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
                 tabPanel("All Countries (Latest)",
                          dataTableOutput("todayTable")),
                 tabPanel("Credits"),
                 tabPanel("Covid-19 Prevention Advice")
)


server <- function(input, output) {
  output$totalCases <- renderText({
    prettyNum(sum(corona_latest$confirmed), big.mark=",")})
  output$totalDeaths <- renderText({
    prettyNum(sum(corona_latest$deaths), big.mark=",")})
  output$totalRecovery <- renderText({
    prettyNum(sum(corona_latest$recovered), big.mark=",")})
  output$totalCasesInc <- renderText({
    1000
  })
  output$totalRecoveryInc <- renderText({
    1000
  })
  output$totalDeathsInc <- renderText({
    1000
  })
  
  output$dateUpdated <- renderText({paste("Updated as of: ",max(corona$date))})
  
  output$globalPlot <- renderPlot({
    cntToPlot <- switch(input$topN, 
                        "Top 10 by cases"=corona_latest %>% top_n(10, confirmed) %>% pull(country), 
                        "Top 20 by cases"=corona_latest %>% top_n(20, confirmed) %>% pull(country),
                        "Top 30 by cases"=corona_latest %>% top_n(30, confirmed) %>% pull(country)
    )
    dat <- subset(melted_latest, country %in% cntToPlot)
    
    geoms <- switch(input$plotType,
                    "Bars"=geom_col(), 
                    "Points"=geom_point(aes(size=value)))
    
    sortByCrit <- switch(input$sortBy, 
                         "Default (Alphabetical)" = "default", 
                         "Confirmed Cases"="confirmed", 
                         "Recovered"="recovered",
                         "Deaths"="deaths"
    )
    
    if(sortByCrit != "default"){
      or <- dat$country[order(dat[dat$variable == sortByCrit, "value"])]            
    } else {
      or <- levels(sort(dat$country))
    }
    
    globalPO <- ggplot(data=dat, 
           aes(x=value, y=factor(country, levels=or), fill=variable, col=variable)) +
      geoms +
      guides(size="none") +
      scale_color_manual(values=c("goldenrod2", "firebrick3", "lightblue"), aesthetics = c("colour", "fill")) +
      labs(title="Covid-19 Pandemic, 2020", 
           subtitle="--- line: Average of confirmed cases",
           caption="Source: JHU CSSE, graph by Finetut.com", 
           x="", y="")
    
    if(input$medianLine){
      globalPO <- globalPO + geom_vline(xintercept=median(dat[dat$variable=="confirmed","value"]),
                                        linetype="dashed")
    }
    
    globalPO
    
  })
  
  
  output$todayTable <- renderDataTable({
    corona_latest
  })
}
shinyApp(ui = ui, server = server)