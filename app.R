#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# The app visualizes transaction data from
# Erste Personal Accounts API
#
# API info: https://www.ersteapihub.com/docs/apis/bank.csas/v3%2Fnetbanking
#

library(shiny)
library(httr)
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(DT)
library(xml2)

# Credentials and access points
API_key="your-key"
cli_key="sandboxClientId"
cli_secret="sandboxClientSecret"
authorize_url = "https://webapi.ersteapihub.com/api/csas/sandbox/v1/sandbox-idp/token"
access_url = "https://webapi.ersteapihub.com/api/csas/sandbox/v3/netbanking"

# User defined function for extracting transaction list
GET_trans <- function(id, datestart, dateend, headers){
  trans <- GET(paste(access_url, '/my/accounts/',id,'/transactions?dateStart=',
                     datestart,
                     '&dateEnd=',
                     dateend,
                     sep=''),
               add_headers(.headers = headers))
  trans <- content(trans)$transactions
  trans_tbl <- data.table(
    ref=as.character(sapply(trans, function(x){x$id})),
    amt=sapply(trans, function(x){x$amount$value}),
    ccy=sapply(trans, function(x){x$amount$currency}),
    c_d=sapply(trans, function(x){x$transactionType}),
    dat=ymd_hms(sapply(trans, function(x){x$bookingDate}))
  )
  return(trans_tbl)
}
datescale <- data.frame(scale=c('year','month','week','day'), 
                        dformat=c('%Y', '%Y / %m', '%Y / %m / %d','%Y / %m / %d'),
                        stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Personal account API CS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p('This Shiny application demonstrates the Erste AISP API. It uses the Sandbox environment. For further details and API documentation please refer to '),
      a(href='https://www.ersteapihub.com', 'https://www.ersteapihub.com'),
      HTML('<br/><br/>'),
      actionButton("login", label='Log in via API', icon=icon('refresh')),
      HTML('<br/><br/>'),
      selectInput("choice", "Select an account:", list('')),
      p('Here you can filter transacionts by date.'),
      dateRangeInput("dateRange", "Select a period",
                     start = Sys.Date()-180, end = Sys.Date()),
      p('You can aggregate your transaction cash-flows by years, months, weeks or days. It will be displayed on the figure panel.'),
      selectInput("scale", "Select the level of aggregation",
                  list('year', 'month', 'week', 'day'), selected='month') 
    ),
    
    mainPanel(
      plotOutput(outputId = 'barPlot'),
      dataTableOutput(outputId = 'tbl')
    )
  )
)

# Define server logic required to visualize transaction data
server <- function(input, output, session) {
  
  observeEvent(input$login, {
  # Authorization >> get acces token
  
  auth <- POST(authorize_url, 
               body=list(
                 grant_type="authorization_code",
                 code="test-code",
                 client_id=cli_key,
                 client_secret=cli_secret
               ),
               encode='form',
               config=list(add_headers("Content-Type" = "application/x-www-form-urlencoded"))
  )
  token <- content(auth)$access_token
  token <- paste('Bearer ',token,sep='')
  # Credentials
  headers <- c(API_key, token)
  names(headers) <- c('WEB-API-key','Authorization')
  
  # Get account list
  acc <- GET(paste(access_url, '/my/accounts', sep=''),
             add_headers(.headers = headers))
  acc <- content(acc)$accounts
  acc_list <- data.table(
    id=sapply(acc, function(x){x$accountno$'cz-iban'}),
    product=sapply(acc, function(x){x$productI18N}),
    type=sapply(acc, function(x){x$type}),
    subtype=sapply(acc, function(x){x$subtype})
    )
  acc_list$name <- paste(acc_list$type, acc_list$subtype, sep=' - ')
  
  updateSelectInput(session=session, inputId = "choice", choices = acc_list$name)
  
  trans <- reactive({
      GET_trans(acc_list[acc_list$name==input$choice]$id, 
                input$dateRange[1],
                input$dateRange[2],
                headers)
  })

  output$barPlot <- renderPlot(if(input$choice != ""){
      t_plot <- trans()
      t_plot$dat_floor <- floor_date(t_plot$dat, input$scale)
      trans_aggr <- t_plot[, .(amt=sum(amt)), by=.(c_d, dat_floor)]
      gp <- ggplot(data=trans_aggr, aes(x=as.Date(dat_floor), y=amt, fill=c_d)) + geom_col(position='dodge', width = 10)
      gp <- gp + scale_x_date(labels = date_format(datescale[datescale$scale==input$scale,]$dformat),
                              breaks=date_breaks(input$scale),
                              limits= input$dateRange)
      gp <- gp + theme(axis.text.x = element_text(angle=90))
      gp <- gp + xlab('') + ylab('CZK')
      gp
   })
  
  output$tbl <- DT::renderDataTable(if(input$choice != ""){
    t <- trans()
    names(t) <- c('Transaction ID','Amount','CCY','Credit/debit','Booking date')
    t
  })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

