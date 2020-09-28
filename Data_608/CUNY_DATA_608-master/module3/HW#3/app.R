
if (!require('shiny')) install.packages('shiny')
if (!require('dplyr')) install.packages('dplyr')
if (!require('googleVis')) install.packages('googleVis')
if (!require('profvis')) install.packages('profvis')
if (!require('shinythemes')) install.packages('shinythemes')

# get data / helper functions
source('helper.R')

# Define the shinyapp structure
#############################
# ui - component -------
#
ui <- fluidPage(
  theme = shinythemes::shinytheme("lumen"),
  h1('Exploring the mortality rate using CDC WONDER Data'),
  h4("Salma Elshahawy, 28th October, 2020, Module #3"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "1st_quarter"',
        p("Select death cause to compare mortality rates."),
        selectInput('icd', 'ICD Chapter', sub$ICD.Chapter)
      ),
      conditionalPanel(
        'input.dataset === "2nd_quarter_I"',
        p("Select death cause and state to compare mortality rates based on national average."),
        selectInput('icd2', 'ICD Chapter', data$ICD.Chapter),
        selectInput('state', 'State', data$State)
      ),
      conditionalPanel(
        'input.dataset === "2nd_quarter_II"',
        p("Select states with the highest variability in mortality rate per disease in a time period."),
        selectInput('icd3', 'ICD Chapter', data$ICD.Chapter), 
        sliderInput("obs", "Number of States to Map",
                    min = 1, max = 50, value = 2)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("1st_quarter", 
                 br(),
                 htmlOutput('plot1')),
        tabPanel("2nd_quarter_I", 
                 br(),
                 htmlOutput('plot2')),
        tabPanel("2nd_quarter_II", 
                 br(),
                 htmlOutput('plot3'))
      )
    )
  ),
  fluidRow(
    column(12,
           h2("Data Source"),
           helpText(
             "For more information on this Data Source visit - reference: ",
             a(href="https://wonder.cdc.gov/wonder/help/ucd.html#", target="_blank", "CDC-WONDER")
           )
    )
  )
)

#############################
# Server component -------
server <- function(input, output, session) {
  
  selectedData1 <- reactive({
    sub[sub$ICD.Chapter == input$icd, ]
  })
  
  selectedData2 <- reactive({
    df <- sub2[sub2$ICD.Chapter == input$icd2 & sub2$State == input$state, ]
    df[3:5]
  })
  
  selectedData3 <- reactive({
    df <- sub2[sub2$ICD.Chapter == input$icd3, ]
    
    ## get states with most variance
    sub4 <- df %>%
      group_by(State) %>%
      summarise(std = sd(State.Avg)) %>%
      top_n(input$obs, std)
    l <- sub4$State
    
    final <- df[df$State %in% l, ]
    final
  })
  
  output$plot1 <- renderGvis({
    t1 <- paste0("Cause: ", input$icd)
    gvisColumnChart(selectedData1(), 
                    options=list(title=t1,
                                 legend="none"))
  })
  output$plot2 <- renderGvis({
    t2 <- paste0("Cause: ", input$icd2,  "  |  State: ", input$state)
    gvisAreaChart(selectedData2(), 
                  options=list(title=t2,
                               hAxis="{format:'####'}"))
  })
  output$plot3 <- renderGvis({
    State<-'
            {"yLambda":1,"showTrails":false,
            "colorOption":"_UNIQUE_COLOR",
            "iconKeySettings":[],
            "xZoomedDataMax":1388534400000,
            "dimensions":{"iconDimensions":["dim0"]},
            "sizeOption":"_UNISIZE","yZoomedIn":false,
            "uniColorForNonSelected":false,"time":"1999",
            "duration":{"timeUnit":"Y","multiplier":1}}
            '
    t3 <- paste0("Cause: ", input$icd3)
    gvisMotionChart(selectedData3(), 
                    idvar="State",
                    timevar="Year",
                    options=list(title=t3,
                                 showChartButtons=F,
                                 state=State))
  })
}

#############################
# concat to render --------
shinyApp(ui, server)



