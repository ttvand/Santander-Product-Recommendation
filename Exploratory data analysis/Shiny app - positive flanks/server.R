# Flag for product by month initialization
firstFlag <<- TRUE

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

shinyServer(function(input, output, session) {
  # Logic to subset the positive flanks data based on the month selection
  positiveFlankDataMonth <- reactive({
    out <- data
    
    # Subset on the analysed month
    out <- data[monthFormat == input$targetMonth, ]
    
    out
  })
  
  # Logic to subset the positive flanks data based on the month and product
  # selection
  positiveFlankData <- reactive({
    out <- data
    
    # Subset on the analysed month and product
    out <- data[monthFormat == input$targetMonth &
                  newProdSimple == input$targetProduct, ]
    
    out
  })
  
  # Logic to subset the positive flanks data based on the month and product
  # selection
  positiveFlankDataExclude <- reactive({
    out <- positiveFlankData()
    
    # Subset on the excluded product
    if(input$excludeProduct != "None"){
      excludeProduct <- paste0("ind_", input$excludeProduct, "_ult1")
      if(input$combinedExcluded == "And"){
        keepRows <- which(out[[excludeProduct]]==1 & 
                            out[[paste0(excludeProduct, "Lag1")]]==0)
      } else{
        keepRows <- which(out[[excludeProduct]]==0 | 
                            out[[paste0(excludeProduct, "Lag1")]]==1)
      }
      out <- out[keepRows, ]
    }
    
    out
  })
  
  # Returns records of the selected month for users that contain both the
  # selected product but also other products
  otherPositiveFlankData <- reactive({
    positiveFlankData <- positiveFlankData()
    positiveFlankDataUsers <- unique(positiveFlankData$ncodpers)
    out <- data
    
    # Subset on the analysed month, product and users
    excludeSelfCombined <- input$excludeSelfCombined
    out <- data[monthFormat == input$targetMonth &
                  (!excludeSelfCombined |
                     newProdSimple != input$targetProduct) &
                  ncodpers %in% positiveFlankDataUsers, ]
    out
  })
  
  # Product by month summary data 
  prodByMonthData <- reactive({
    out <- data
    
    # Subset on the analysed month
    out <- data[newProdSimple %in% input$targetMonthProducts,
                .(Count = monthProdPosFlankCount[1]), .(fecha_dato, newProdSimple)]
    out
  })
  
  # Product by month correlation summary data 
  prodByMonthCorrData <- reactive({
    prodByMonthData <- prodByMonthData()
    if(nrow(prodByMonthData) <= 0) return()
    out <- dcast(prodByMonthData, fecha_dato~newProdSimple)
    out
  })
  
  
  # Calculate the number of analysed positive flanks for the selected month
  nbPosFlanksMonthGeneral <- reactive({
    nrow(positiveFlankDataMonth())
  })
  
  # Calculate the number of positive flanks for the analysed month
  nbPosFlanksMonth <- reactive({
    nrow(positiveFlankData())
  })
  
  # Calculate the number of analysed positive flanks
  nbPosFlanks <- reactive({
    nrow(positiveFlankData())
  })
  
  # Calculate the number of analysed positive flanks after exclusion
  nbPosFlanksExcluded <- reactive({
    nrow(positiveFlankDataExclude())
  })
  
  # Calculate the number of joined positive flanks
  nbJoinedPosFlanks <- reactive({
    sum(otherPositiveFlankData()$newProdSimple != input$targetProduct)
  })
  
  # Descriptive text of the number of positive flanks for the analysed month
  output$posFlanksDescriptionMonth <- renderText({
    nbPosFlanks <- nbPosFlanksMonthGeneral()
    paste0(nbPosFlanks, " month positive flank",
           ifelse(nbPosFlanks==1, "", "s"))
  })
  
  # Descriptive text of the number of positive flanks
  output$posFlanksDescription <- renderText({
    nbPosFlanks <- nbPosFlanks()
    paste0(nbPosFlanks, " product month positive flank",
           ifelse(nbPosFlanks==1, "", "s"))
  })
  
  # Descriptive text of the number of positive flanks after exclusion 
  output$posFlanksExcludedDescription <- renderText({
    nbPosFlanks <- nbPosFlanksExcluded()
    if(input$excludeProduct == "None"){
      out <- ""
    } else{
      out <- paste0(nbPosFlanks, " product month positive flank",
                    ifelse(nbPosFlanks==1, "", "s"), " after exclusion of product ",
                    input$excludeProduct)
    }
    out
  })
  
  # Descriptive text of the number of positive flanks after combination of
  # products
  output$posFlanksCombinedDescription <- renderText({
    nbPosFlanks <- nbPosFlanksExcluded()
    if(input$excludeProduct == "None"){
      out <- ""
    } else{
      out <- paste0(nbPosFlanks, " product month positive flank",
                    ifelse(nbPosFlanks==1, "", "s"),
                    " after combination with product ", input$excludeProduct)
    }
    out
  })
  
  # Descriptive text of the number of joined positive flanks
  output$joinedPosFlanksDescription <- renderText({
    nbJoinedPosFlanks <- nbJoinedPosFlanks()
    paste0(nbJoinedPosFlanks, " joined product month positive flank",
           ifelse(nbJoinedPosFlanks==1, "", "s"))
  })
  
  # Previous target month logic
  observeEvent(input$prevMonth,{
    currentId <- which(input$targetMonth == orderedDateFormat)
    newId <- max(c(1, currentId - 1))
    
    # Update target variable
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[newId])
  })
  
  # Next target variable logic
  observeEvent(input$nextMonth,{
    currentId <- which(input$targetMonth == orderedDateFormat)
    newId <- min(c(length(orderedDateFormat), currentId + 1))
    
    # Update target variable
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[newId])
  })
  
  # Set the month to June 2015
  observeEvent(input$goJun15,{
    # Update target month
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[5])
  })
  
  # Set the month to December 2015
  observeEvent(input$goDec15,{
    # Update target month
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[11])
  })
  
  # Previous target product logic
  observeEvent(input$prevProduct,{
    currentId <- which(input$targetProduct == productsSimple)
    newId <- max(c(1, currentId - 1))
    
    # Update target product
    updateSelectInput(session, "targetProduct",
                      selected = productsSimple[newId])
  })
  
  # Next target product logic
  observeEvent(input$nextProduct,{
    currentId <- which(input$targetProduct == productsSimple)
    newId <- min(c(length(productsSimple), currentId + 1))
    
    # Update target product
    updateSelectInput(session, "targetProduct",
                      selected = productsSimple[newId])
  })
  
  # Render the dynamic product UI
  output$productUI <- renderUI({
    defaultId <- ifelse(onlyAnalyseTop, 1, 3)
    targetProduct <- ifelse(is.null(input$targetProduct),
                            productsSimple[defaultId],
                            input$targetProduct)
    out <- list(
      selectInput("targetProduct", "Studied product",
                  selected = targetProduct, multiple = FALSE,
                  choices = productsSimple),
      fluidRow(column(5, actionButton("prevProduct", "Previous",
                                      icon = icon("arrow-left"))),
               column(5, actionButton("nextProduct", "Next",
                                      icon = icon("arrow-right")),
                      offset=1)
      ),
      h4(textOutput("posFlanksDescription")),
      conditionalPanel(condition="input.mainPanelProdAnalysis == 'Combined new products analysis'",
                       h4(textOutput("joinedPosFlanksDescription"))),
      br()
    )
    out
  })
  
  # Render the dynamic product by month UI
  output$productMonthUI <- renderUI({
    isolate({
      if(is.null(input$targetMonthProducts)){
        defaultId <- ifelse(onlyAnalyseTop, 1, 3)
        targetMonthProducts <- ifelse(firstFlag, productsSimple[defaultId], "")
      } else{
        targetMonthProducts <- input$targetMonthProducts
      }
      firstFlag <<- FALSE
      
      out <- list(
        selectizeInput("targetMonthProducts", "Studied product(s)",
                       selected = targetMonthProducts, multiple = TRUE,
                       choices = c("", productsSimple)),
        actionButton("loadMonthImportant", "Load major products")
      )
      out
    })
  })
  
  # Generate the product barplot for the monthly positive flank data
  output$productDistrPlotly <- renderPlotly({
    positiveFlankData <- positiveFlankDataMonth()
    nbPosFlanks <- nbPosFlanksMonthGeneral()
    positiveFlankCount <- positiveFlankData[, .N, .(newProdSimple)]
    
    # Add products with a zero count
    zeroProds <- productsSimple[!productsSimple %in%
                                  positiveFlankCount[, newProdSimple]]
    if(length(zeroProds)>0){
      positiveFlankCount <- rbind(positiveFlankCount,
                                  data.table(newProdSimple = zeroProds, N = 0))
    }
    names(positiveFlankCount)[2] <- "Count"
    positiveFlankCount$Fraction <- positiveFlankCount$Count/nbPosFlanks
    
    # Generate the ggplot based on the plot type selection
    p <- ggplot(positiveFlankCount, aes_string(x="newProdSimple",
                                               y=input$yAxisMonthly,
                                               fill="newProdSimple")) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Limit y to [0, 1/3] when considering fractions
    if(input$yAxisMonthly == "Fraction"){
      p <- p + ylim(c(0, 1/3))
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate the other product barplot for the positive flank data
  output$productDistrCombinedPlotly <- renderPlotly({
    positiveFlankData <- otherPositiveFlankData()
    nbPosFlanks <- nbPosFlanks()
    positiveFlankCount <- positiveFlankData[, .N, .(newProdSimple)]
    
    # Add products with a zero count
    zeroProds <- productsSimple[!productsSimple %in%
                                  positiveFlankCount[, newProdSimple]]
    if(length(zeroProds)>0){
      positiveFlankCount <- rbind(positiveFlankCount,
                                  data.table(newProdSimple = zeroProds, N = 0))
    }
    names(positiveFlankCount)[2] <- "Count"
    positiveFlankCount$Fraction <- positiveFlankCount$Count/nbPosFlanks
    positiveFlankCount <- as.data.frame(positiveFlankCount)
    
    # Generate the ggplot based on the plot type selection
    p <- ggplot(positiveFlankCount, aes_string(x="newProdSimple",
                                               y=input$yAxisCombined,
                                               fill="newProdSimple")) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Limit y to [0, 1] when considering fractions
    if(input$yAxisCombined == "Fraction"){
      p <- p + ylim(c(0, 1))
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate the previous product barplot for the positive flank data
  output$productPrevMonthPlotly <- renderPlotly({
    positiveFlankData <- positiveFlankData()
    lagData <- positiveFlankData[, paste0(products, "Lag1"), with=FALSE]
    lagSums <- colSums(lagData, na.rm = TRUE)
    positivePrevCount <- data.table(newProdSimple = productsSimple,
                                    Count = lagSums,
                                    Fraction = lagSums/nrow(positiveFlankData))
    positivePrevCount[, newProdSimple := factor(newProdSimple,
                                                levels = productsSimple)]
    
    # Generate the ggplot based on the plot type selection
    p <- ggplot(positivePrevCount, aes_string(x="newProdSimple",
                                              y= input$yAxisPrevious,
                                              fill="newProdSimple")) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Limit y to [0, 1] when considering fractions
    if(input$yAxisPrevious == "Fraction"){
      p <- p + ylim(c(0, 1))
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate a data table of the previous month counts
  output$prevTable <- renderDataTable({
    positiveFlankData <- positiveFlankData()
    lagData <- positiveFlankData[, paste0(products, "Lag1"), with=FALSE]
    lagSums <- colSums(lagData, na.rm = TRUE)
    positivePrevCount <- data.table(newProdSimple = productsSimple,
                                    Count = lagSums,
                                    Fraction = round(100*lagSums/
                                                       nrow(positiveFlankData),
                                                     2))
    names(positivePrevCount)[3] <- "Fraction (%)"
    positivePrevCount[, newProdSimple := factor(newProdSimple,
                                                levels = productsSimple)]
    out <- positivePrevCount[order(-Count), ]
    
    out
  }, options = list(searching = FALSE, paging = FALSE),
  rownames= FALSE)
  
  # Generate the density plot for the continuous plot data
  output$densityContPlotly <- renderPlotly({
    plotData <- positiveFlankDataExclude()
    
    # Generate the ggplot based on the plot type selection
    if(input$densityTypeCont == "Histogram"){
      p <- ggplot(plotData, aes_string(input$contVarY)) +
        geom_histogram(bins = input$nbDensityBinsCont)
    } else{
      p <- ggplot(plotData, aes_string(input$contVarY)) +
        geom_density()
    }
    
    # Set the x range
    contId <- which(contVars == input$contVarY)
    p <- p +
      xlim(contVarRanges[contId, ])
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Previous categorical variable logic
  observeEvent(input$prevCat,{
    currentId <- which(input$catVarSel == catVars)
    newId <- max(c(1, currentId - 1))
    
    # Update categorical variable
    updateSelectInput(session, "catVarSel",
                      selected = catVars[newId])
  })
  
  # Next categorical variable logic
  observeEvent(input$nextCat,{
    currentId <- which(input$catVarSel == catVars)
    newId <- min(c(length(catVars), currentId + 1))
    
    # Update categorical variable
    updateSelectInput(session, "catVarSel",
                      selected = catVars[newId])
  })
  
  # Descriptive text of the selected categorical variable
  output$catVarDescription <- renderText({ 
    nbLevels <- length(unique(positiveFlankData()[[input$catVarSel]]))
    paste0("Categorical variable with ", nbLevels, " level",
           ifelse(nbLevels==1, "", "s"), " for the analysed data")
  })
  
  # Select the categorical plot data by excluding records that have a 
  # minimum group count
  catPlotData <- reactive({
    out <- positiveFlankDataExclude()
    
    # Count the group frequencies
    freqs <- table(out[[input$catVarSel]])
    out <- out[out[[input$catVarSel]] %in%
                 names(freqs)[freqs >= input$minCatCount]]
    
    out
  })
  
  # Counted plot data
  catPlotDataCount <- reactive({
    catData <- catPlotData()[, c(input$catVarSel), with=FALSE]
    names(catData)[1] <- "y"
    catData[y=="", y:= "Unknown"]
    out <- catData[, .N, y]
    out <- out[order(-N), ]
    out[, Fraction := round(100*N/nrow(catData), 2)]
    names(out)[1] <- input$catVarSel
    names(out)[-1] <- c("Count", "Fraction (%)")
    out
  })
  
  # Descriptive text of the number of considered levels
  output$catSubsetDescription <- renderText({ 
    nbLevels <- length(unique(positiveFlankData()[[input$catVarSel]]))
    nbConsideredLevels <- length(unique(catPlotData()[[input$catVarSel]]))
    paste0(nbConsideredLevels, " categorical variable level",
           ifelse(nbConsideredLevels==1, "", "s"), " out of ",
           nbLevels, " contain", ifelse(nbConsideredLevels==1, "s", ""),
           " the minimum group count")
  })
  
  # Generate the categorical analysis plot
  output$catPlotly <- renderPlotly({
    plotData <- catPlotDataCount()
    
    p <- ggplot(plotData, aes_string(x=input$catVarSel,
                                     y="Count",
                                     fill=input$catVarSel)) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate a data table of the categorical counts
  output$catTable <- renderDataTable({
    out <- catPlotDataCount()
    
    out
  }, options = list(searching = FALSE, paging = FALSE),
  rownames= FALSE)
  
  # Plot the monthly product frequency for each of the selected products
  output$productByMonthPlotly <- renderPlotly({
    plotData <- prodByMonthData()
    if(nrow(plotData) == 0) return()
    
    p <- ggplot(plotData, aes(x=fecha_dato, y=Count, colour=newProdSimple)) +
      geom_line() +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE) +
      ylim(c(0, max(plotData$Count)))
    
    # Add a vertical line to one year back from the test data set
    p <- p +
      geom_vline(xintercept = as.numeric(as.Date("2015-06-28")), linetype=4)
    p <- p +
      geom_vline(xintercept = as.numeric(as.Date("2015-12-28")), linetype=2)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Plot the monthly product correlation matrix for the selected products
  output$productByMonthCorrPlotly <- renderPlotly({
    plotData <- prodByMonthCorrData()
    if(is.null(plotData) || ncol(plotData)<3 || nrow(plotData)<=0) return()
    
    correlations <- round(cor(plotData[, -1, drop=FALSE]), 2)
    cormat <- melt(correlations)
    
    # Reorder the correlation matrix
    cormat <- reorder_cormat(correlations)
    upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    # Create a ggheatmap
    p <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Load the most important products for the month by time plot
  observeEvent(input$loadMonthImportant,{
    updateSelectizeInput(session, "targetMonthProducts",
                         selected = productsSimple[importantProdIds])
  })
})