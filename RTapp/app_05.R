library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
source("reader.R")
source("preprocess.R")
source("computeFigure.R")

corona_latest <- subset(corona, date == max(corona$date))
melted_latest <- melt_all(corona_latest)

ui <- navbarPage(title="COVID-19",
                 theme=shinytheme("superhero"),
                 tabPanel("Global",
                          sidebarLayout(
                            mainPanel(
                              fluidRow(
                                column(4, 
                                       h6("Total Cases"), 
                                       wellPanel(h3(textOutput("totalCases")),
                                                 p(img(src='up.png'),textOutput("totalCasesInc",inline = TRUE))
                                       )
                                ),
                                column(4, 
                                       h6("Total Recovered"), 
                                       wellPanel(h3(textOutput("totalRecovery")),
                                                 p(img(src='up.png'),textOutput("totalRecoveryInc",inline = TRUE))
                                       )
                                ),
                                column(4, 
                                       h6("Total Deaths"), 
                                       wellPanel(h3(textOutput("totalDeaths")),
                                                 p(img(src='up.png'),textOutput("totalDeathsInc",inline = TRUE))
                                       )
                                )
                              ), #end fluidRow
                              plotOutput("globalPlot")
                            ), # end mainPanel
                            sidebarPanel(
                              h2("Covid-19 Analytics"),
                              img(src="logo.png", width="140px"),
                              hr(),
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

                          fluidRow(
                            column(12, dataTableOutput("todayTable"))
                          ),
                          fluidRow(
                            column(12, downloadButton("downloadTable", " Download", style="float:right;"))
                          )
                 ),
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
    prettyNum(as.numeric(inc["confirmed"]), big.mark = ",")
  })
  output$totalRecoveryInc <- renderText({
    prettyNum(as.numeric(inc["recovered"]), big.mark = ",")
  })
  output$totalDeathsInc <- renderText({
    prettyNum(as.numeric(inc["deaths"]), big.mark = ",")
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
           x="", y="") +
      theme(rect = element_rect(fill = "transparent"), 
            text = element_text(color = "white"),
            axis.text = element_text(color = "white"))
    
    if(input$medianLine){
      globalPO <- globalPO + geom_vline(xintercept=median(dat[dat$variable=="confirmed","value"]),
                                        linetype="dashed")
    }
    
    globalPO
    
  }, bg="transparent")
  
  
  output$todayTable <- renderDataTable({
    corona_latest
  },style="bootstrap4")
  
  output$downloadTable <- downloadHandler(
    filename="covid-19-finetut.csv",
    content = function(file){
      write.csv(corona_latest, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)