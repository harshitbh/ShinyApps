shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Customer Base Analytics"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    useShinyjs(),#shiny java script functionalities.
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv')),#input csv file with text and csv as display.
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ',',inline=TRUE),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double'='"',
                   'Single'="'"),
                 '',inline=TRUE),
    tags$hr(),
    disabled(sliderInput("sample", "Sample Customers %:", min=5, max=100, value=20, step = 5)),#slider input from 5-100 with 20 as default.
    checkboxInput("scatterD3_transitions", "Use Transitions Effects", value = TRUE),#used in scatterd3 inputs
    checkboxInput("scatterD3_ellipses", "Confidence ellipses", value = TRUE)#used in scatterd3 inputs
  ),
  
  # Show a tabset that includes a plot, summary, and table view of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Input Data", dataTableOutput("table")),#render table wrt input data for 20%sample
      tabPanel("Visits Pattern", plotlyOutput("custvisits",height = "450px")),#visit pattern 
      tabPanel("Customer Base Score", dataTableOutput("custscore")),#customer base score
      tabPanel("Scatter Plot", scatterD3Output("scatter",  width = "100%"))#scatter d3 output
    )
  )
))
