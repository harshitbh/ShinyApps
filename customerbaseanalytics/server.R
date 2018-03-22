options(shiny.maxRequestSize = 10*1024^2) #By default upload file size is 5 MB raise limit to 10 Mb
options(digits=2)#change the options digit to 2.

#shiny server input which have to take form UI.output which has to come as a result.
shinyServer(function(input, output,session) {
  
  #reactive block it is automatically called when the shiny-server app runs.    
  data <- reactive({ 
    req(input$file1) #require that the input is available
    inFile <- input$file1 #store the input in infile.Infile contains name,size,type and datapath
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote, check.names=FALSE,row.names = NULL) #stores the file in temp datapath with other inputs from users.
    enable("sample")#sample customer slide would be enable for it.
    return(df)#as a part of the reactive block the function will return df data frame
  })
  
  #create final dataset based on columns
  sampledata <- reactive({
    seldata <- data()#call result of data function called above.
    sampID <-  sample(seldata$cust,replace=F,size=input$sample*.01*nrow(seldata))#choose sample custID from input slider
    sampdata <- seldata[seldata$cust %in% sampID,]#select data containing the customer ID.
    sampdata$date <- as.Date(sampdata$date, "%Y-%m-%d")#change the date format to yy-mm-dd.
    sampdata$cust <- as.factor(sampdata$cust)#change the customer to factor.
    return(sampdata)  
  })
  
  #choose any 20 customers randomly- only to show irregularity
  #random20 customers would be shown in visit pattern plot.
  random20 <- reactive({
    seldata20 <- sampledata()
    sampID20 <- sample(seldata20$cust,replace=F,size=20)
    data20 <- seldata20[seldata20$cust %in% sampID20,]
    data20$date <- as.Date(data20$date, "%YY-%mm-%dd")
    data20$cust <- as.factor(data20$cust)
    return(data20)  
  })
  #reactive block to calculate the final score.
  finalscore <- reactive({
    cdnowElog <- sampledata()#select sample data with by default 20%.
    cdnowElog$date <- as.Date(cdnowElog$date, "%Y-%m-%d") #convert the date to ymd as per model required.
    cbs <- elog2cbs(cdnowElog,T.cal = max(cdnowElog$date),T.tot = max(cdnowElog$date))#Efficient implementation for the conversion of an event log into a customer-by-sufficient-statistic (CBS) data.frame, with a row for each customer
    params.bgnbd   <- BTYD::bgnbd.EstimateParameters(cbs) #Estimates parameters for the BG/NBD model.
    Palive <- round(bgnbd.PAlive(params.bgnbd, cbs$x, cbs$t.x, cbs$T.cal),digits = 2)#Uses BG/NBD model parameters and a customer's past transaction behavior to return the probability that they are still alive at the end of the calibration period.
    
    #Uses BG/NBD model parameters and a customer's past transaction behavior to return the number of transactions they are expected to make in a given time period.
    Exp.Txn.52weeks   <- round(bgnbd.ConditionalExpectedTransactions(params.bgnbd, T.star=52, cbs$x, cbs$t.x, cbs$T.cal),digits=2)
    
    #find the average spend and total transactions number per customer.
    cdnowSum <- ddply(cdnowElog, c("name","cust"), summarize, tot.trans=length(sales), ave.spend=mean(sales))
    cdnowSum$ave.spend <- round(cdnowSum$ave.spend, digits = 2)#round to 2 digit
    cdnowSum$cust <- as.factor(cdnowSum$cust)#change customer to factor type.
    spend.params <- spend.EstimateParameters(cdnowSum$ave.spend, cdnowSum$tot.trans)#estimate parameters for spend model.
    
    spend.exp.value <- spend.expected.value(spend.params, cdnowSum$ave.spend, cdnowSum$tot.trans)#conditional expected transaction value.
    spend.exp.value <- round(spend.exp.value, digits = 2)#round to two digits
    dfcluster <- as.data.frame(cbind(Palive,Exp.Txn.52weeks))#bind column for Palive and Exp transactions
    clus <- kmeans(dfcluster,3)#apply kmeans with 3 clusters
    cdnow.output <- as.data.frame(cbind(levels(cdnowSum$cust),spend.exp.value,Palive,Exp.Txn.52weeks,clus$cluster)) #result of cluster bind with earlier data set.
    colnames(cdnow.output) <- c("CustomerID","ExpSpend","PActive","ExpTrans52wks","Cluster")
    cdnow.output.merged <- merge(cdnowSum,cdnow.output,by.x = "cust",by.y = "CustomerID")#merge cluster output with model output
    colnames(cdnow.output.merged) <- c("CustomerID","Name","Total.Txns","Avg.Spend","ExpSpend","PActive","ExpTrans52wks","Cluster") #rename the names
    cdnow.output.merged <- unfactor(cdnow.output.merged)
    return(cdnow.output.merged) 
  })
  #visit pattern plot
  output$custvisits <- renderPlotly({
    g <- ggplot(random20(), aes(date, name)) + 
      geom_point(color="firebrick", size = 2,shape = 23, fill = "green", stroke = .5) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major = element_line(colour = "grey50", size = 0.5),
            panel.background = element_blank(),
            plot.margin = margin(2, 2, 2, 2)) #plot the random20 data with date and name as hover value using ggplot
    g <- ggplotly(g)#convert the ggplot graph to plotly charts                                
  })
  
  #scatter d3 charts
  #Render the text using tooltips for all the details over the scatter chart.
  output$scatter <-  renderScatterD3({
    df <- finalscore()
    tooltips <- paste("Customer <b>",df$CustomerID,"-",df$Name, "</b> had made <b>", df$Total.Txns, "</b> transactions with average spend of <b>$ ", df$Avg.Spend, "</b>.",
                      "He/She is predicted <b>", df$PActive*100, "</b> % active and in next 52 weeks expected to make <b>", df$ExpTrans52wks, "</b>transactions." )
    final <- cbind(df,tooltips)
    scatterD3( x   = df$PActive,
               y   = df$ExpTrans52wks,
               size_var = df$ExpSpend,
               col_var = df$Cluster,
               legend_width = 0,
               axes_font_size = 12,
               transitions = input$scatterD3_transitions,
               xlim=c(-0.1,1.1), 
               ylim=c(-2,max(df$ExpTrans52wks)+2),
               xlab = "Active Probability", ylab = "Expected transactions in next 52 weeks",
               caption = list(title = "Customer Base Analytics",
                              subtitle = "Probability of customer active score and expected number of transactions in next 52 weeks",
                              text = ""),
               size_range = c(10,1000),
               point_opacity = 0.7,
               ellipses = input$scatterD3_ellipses,
               hover_size = 4,
               hover_opacity = 1,
               left_margin = 70,
               lasso = TRUE,
               tooltip_text = tooltips 
    )
  })
  
  #render datatable to print the table over UI.
  output$custscore <- renderDataTable({
    finalscore()#final  score calculation over reactive block earlier.
  }, options = list(lengthMenu = c(5, 30, 50, 100), pageLength = 8,scrollX = TRUE))
  
  #render datatable to print the table over UI.  
  output$table <- renderDataTable({
    sampledata()#sample data form the uploaded data set.
  }, options = list(lengthMenu = c(5, 30, 50, 100), pageLength = 8,scrollX = TRUE))
})
