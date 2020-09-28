
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
  h4("Salma Elshahawy, 28th October, 2020"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Q1"',
        selectInput('icd', 'ICD Chapter', sub$ICD.Chapter),
        p("Select cause of death to compare crude mortality rates across different US States.")
      ),
      conditionalPanel(
        'input.dataset === "Q2a"',
        selectInput('icd2', 'ICD Chapter', data$ICD.Chapter),
        selectInput('state', 'State', data$State),
        p("Select cause of death and state to compare crude mortality rates for that US state to the US national average.")
      ),
      conditionalPanel(
        'input.dataset === "Q2b"',
        selectInput('icd3', 'ICD Chapter', data$ICD.Chapter), 
        sliderInput("obs", "Number of States to Map",
                    min = 1, max = 50, value = 5),
        p("Selects states with the most variability in mortality rate
                  for the given disease across the dataset time period.")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Q1", 
                 br(),
                 htmlOutput('plot1')),
        tabPanel("Q2a", 
                 br(),
                 htmlOutput('plot2')),
        tabPanel("Q2b", 
                 br(),
                 htmlOutput('plot3'))
      )
    )
  ),
  fluidRow(
    column(12,
           h2("Data Source"),
           p("The Underlying Cause of Death data available on WONDER are 
                 county-level national mortality and population data spanning 
                 the years 1999-2015. Data are based on death certificates for 
                 U.S. residents. Each death certificate identifies a single 
                 underlying cause of death and demographic data. The number of 
                 deaths, crude death rates or age-adjusted death rates, and 95% 
                 confidence intervals and standard errors for death rates can 
                 be obtained by place of residence (total U.S., region, state 
                 and county), age group (single-year-of age, 5-year age groups, 
                 10-year age groups and infant age groups), race, Hispanic 
                 ethnicity, gender, year, cause-of-death (4-digit ICD-10 code or 
                 group of codes), injury intent and injury mechanism, drug/alcohol 
                 induced causes and urbanization categories. Data are also available 
                 for place of death, month and week day of death, and whether 
                 an autopsy was performed."),
           helpText(
             "For more information on this Data Source visit: ",
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



