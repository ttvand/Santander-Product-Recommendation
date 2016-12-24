shinyServer(function(input, output, session) {
  # Calculate the number of previous zeros for the considered product
  productPrevZeros <- reactive({
    out <- basePredictions[, sum(get(paste0("ind_", input$targetVar,
                                            "_ult1_Weighted")) != 0)]
    out
  })
  
  # Calculate the rank extension based on the normalisation mode
  rankExtension <- reactive({
    if(input$normalizeMode== "Exponential"){
      out <- "_Norm_Exp"
    } else{
      out <- ""
    }
    out
  })
  
  # Calculate the number of top predictions for the considered product
  productTopPreds <- reactive({
    rankExtension <- rankExtension()
    out <- basePredictions[, sum(get(paste0(input$targetVar, rankExtension,
                                            "_Rank"))==1)]
    out
  })
  
  # Calculate the number of top predictions for the considered product,
  # considering only users that showed at least one historic positive product
  # flank
  productTopPredsPrevPos <- reactive({
    rankExtension <- rankExtension()
    out <- basePredictions[ncodpers %in% posFlankClients,
                           sum(get(paste0(input$targetVar, rankExtension,
                                          "_Rank"))==1)]
    out
  })
  
  # Extract the relative MAP contribution
  relMAPContribution <- reactive({
    out <- mapContributions[17, grep(input$targetVar,
                                     colnames(mapContributions))]
    out
  })
  
  # Extract the probability multiplier
  probMultiplier <- reactive({
    out <- probMultipliers[grepl(input$targetVar, names(probMultipliers))]
    out
  })
  
  # Descriptive text of the fraction of previous zeros
  output$nbPrevZerosDescription <- renderText({
    nbPrevZeros <- productPrevZeros()
    prevZerosPercent <- round(100*nbPrevZeros/nrow(basePredictions), 2)
    paste0(nbPrevZeros, " previously not owned product",
           ifelse(nbPrevZeros==1, "", "s"), " (", prevZerosPercent, "%)")
  })
  
  # Descriptive text of the number of predicted top products
  output$nbTopProductsDescription <- renderText({
    nbTopProducts <- productTopPreds()
    topProductPercent <- round(100*nbTopProducts/nrow(basePredictions), 2)
    paste0(nbTopProducts, " predicted top product",
           ifelse(nbTopProducts==1, "", "s"), " (", topProductPercent, "%)")
  })
  
  # Descriptive text of the number of predicted top products,
  # considering only users that showed at least one historic positive product
  # flank
  output$nbTopProductsPrevPosDescription <- renderText({
    nbTopProducts <- productTopPredsPrevPos()
    nbConsideredTopClients <- basePredictions[ncodpers %in% posFlankClients, .N]
    topProductPercent <- round(100*nbTopProducts/nbConsideredTopClients, 2)
    paste0(nbTopProducts, " predicted top product",
           ifelse(nbTopProducts==1, "", "s"), " (", topProductPercent,
           "%) prevPos")
  })
  
  # Descriptive text of the relative MAP contribution
  output$relMAPContributionDescription <- renderText({
    relMAPContribution <- relMAPContribution()
    relMAPContributionRound <- round(100*relMAPContribution, 2)
    paste0("Relative MAP contribution: ", relMAPContributionRound, "%")
  })
  
  # Descriptive text of the probability multiplier
  output$probMultiplierDescription <- renderText({
    probMultiplier <- probMultiplier()
    probMultiplierRound <- round(probMultiplier, 3)
    paste("Probability multiplier:", probMultiplierRound)
  })
  
  # Extract the mean predictions by lag for the selected product as well as
  # the extrapolated public leaderboard counts
  meanByMonthData <- reactive({
    # Extract the mean predictions
    meanPredsProd <- basePredSums[grepl(input$targetVar, names(basePredSums)) &
                                    !grepl("Weighted|Rank",
                                           names(basePredSums))]
    lags <- as.numeric(gsub(".*Lag_", "", names(meanPredsProd)))
    meanPreds <- data.table(Lag = lags, Count = meanPredsProd,
                            Type = "Test predictions sum")
    
    # Extract the train count
    trainCountsTarget <- trainCounts[grepl(input$targetVar,
                                           names(trainCounts))]
    trainCounts <- data.table(Lag = unique(meanPreds$Lag),
                              Count = trainCountsTarget, Type = "Train count")
    
    # Extract the test count
    testCount <- publicPosFlanks[match(input$targetVar, productsSimple)]
    testCounts <- data.table(Lag = unique(meanPreds$Lag), Count = testCount,
                             Type = "Test count")
    
    # Combine the series
    out <- rbind(trainCounts, meanPreds, testCounts)
    
    # Drop lags outside of the lag range
    out <- out[Lag >= input$lagRange[1] & Lag <= input$lagRange[2], ]
    
    # Reorder the target columns by converting the type to a factor
    out[, Type := factor(Type, levels = c("Test predictions sum",
                                          "Train count",
                                          "Test count"
    ))]
    
    out
  })
  
  # Extract the mean confidence by lag for the selected product
  confidenceByMonthData <- reactive({
    prodFoldPredRatios <-
      meanFoldPredRatios[targetVar == paste0("ind_", input$targetVar, "_ult1"),]
    prodLags <- prodFoldPredRatios[, lag]
    validRowLags <- prodLags >= input$lagRange[1] & prodLags <= input$lagRange[2]
    out <- prodFoldPredRatios[which(validRowLags),
                              c("lag", "meanFoldRatio"), with=FALSE]
    names(out) <- c("Lag", "Confidence")
    
    out
  })
  
  # Return the monthly predictions for the selected product
  prodMonthData <- reactive({
    out <- basePredictions[, c(1, which(
      grepl(input$targetVar, names(basePredictions)) &
        !grepl("Weighted|Rank", names(basePredictions)))),
      with=FALSE]
    
    # Only keep the lag information in the target column names
    colLags <- suppressWarnings(as.numeric(gsub(".*_Lag_", "", names(out))))
    validColLags <- colLags >= input$lagRange[1] & colLags <= input$lagRange[2]
    out <- out[, c(1, 1 + which(validColLags[-1])), with=FALSE]
    names(out)[-1] <- paste("Lag", colLags[-1][validColLags[-1]])
    
    out
  })
  
  # Extract the records where the product month predictions are 0, this 
  # indicates that the product was already owned in the previous month
  prevOwnedIds <- reactive({
    out <- rowSums(prodMonthData()[, -1, with=FALSE]) == 0
    out
  })
  
  # Extract the non-zero product month predictions
  prodMonthDataNZ <- reactive({
    out <- prodMonthData()
    out <- out[!prevOwnedIds(), ]
    out
  })
  
  # Extract the non-zero product month predictions for the considered lags
  prodLagCompareNZ <- reactive({
    out <- prodMonthDataNZ()
    colLags <- suppressWarnings(as.numeric(gsub("Lag ", "", colnames(out))))
    out <- out[, c(1, which(colLags == input$lag1Compare),
                   which(colLags == input$lag2Compare)), with=FALSE]
    
    # Subset based on the minimum scatter plot threshold (time saving)
    out <- out[out[[2]]>minScatterThreshold | out[[3]]>minScatterThreshold, ]
    
    out
  })
  
  # Extract the non-zero product lag predictions for the considered products
  prodOtherTargetCompareNZ <- reactive({
    out <- basePredictions[, 
                           (grepl(input$targetVar, names(basePredictions)) |
                              grepl(input$otherTargetCompare, names(basePredictions))) &
                             !grepl("Weighted|Rank", names(basePredictions)) &
                             grepl(paste0("Lag_", input$lagOtherCompare), names(basePredictions)),
                           with=FALSE]
    names(out) <- c("first", "second")
    out <- out[pmin(first, second) > 0 &
                 pmax(first, second) > minScatterThreshold, ]
    out
  })
  
  # Previous target variable logic
  observeEvent(input$prevTarget,{
    currentId <- which(input$targetVar == productsSimple)
    newId <- max(c(1, currentId - 1))
    
    # Update target variable
    updateSelectInput(session, "targetVar",
                      selected = productsSimple[newId])
  })
  
  # Next target variable logic
  observeEvent(input$nextTarget,{
    currentId <- which(input$targetVar == productsSimple)
    newId <- min(c(length(productsSimple), currentId + 1))
    
    # Update target variable
    updateSelectInput(session, "targetVar",
                      selected = productsSimple[newId])
  })
  
  # Previous lag 1 compare scatter logic
  observeEvent(input$prevLag1Compare,{
    currentId <- as.numeric(input$lag1Compare)
    newLag <- max(c(1, currentId - 1))
    
    # Update the lag
    updateSelectInput(session, "lag1Compare",
                      selected = newLag)
  })
  
  # Next lag 1 compare scatter logic
  observeEvent(input$nextLag1Compare,{
    currentId <- as.numeric(input$lag1Compare)
    newLag <- min(c(16, currentId + 1))
    
    # Update the lag
    updateSelectInput(session, "lag1Compare",
                      selected = newLag)
  })
  
  # Previous lag 2 compare scatter logic
  observeEvent(input$prevLag2Compare,{
    currentId <- as.numeric(input$lag2Compare)
    newLag <- max(c(1, currentId - 1))
    
    # Update the lag
    updateSelectInput(session, "lag2Compare",
                      selected = newLag)
  })
  
  # Next lag 2 compare scatter logic
  observeEvent(input$nextLag2Compare,{
    currentId <- as.numeric(input$lag2Compare)
    newLag <- min(c(16, currentId + 1))
    
    # Update the lag
    updateSelectInput(session, "lag2Compare",
                      selected = newLag)
  })
  
  # Plot the monthly prediction summary of the selected product
  output$meanByMonthPlotly <- renderPlotly({
    plotData <- meanByMonthData()
    if(nrow(plotData) == 0) return()
    
    p <- ggplot(plotData, aes(x=Lag, y=Count, colour=Type)) +
      geom_line() +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE) +
      ylim(c(0, max(plotData$Count)))
    
    # Add a vertical line to one year back from the test data set
    p <- p +
      geom_vline(xintercept = 5, linetype=4)
    p <- p + geom_vline(xintercept = 11, linetype=2)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Plot the monthly prediction confidence of the selected product
  output$confidenceByMonthPlotly <- renderPlotly({
    plotData <- confidenceByMonthData()
    if(nrow(plotData) == 0) return()
    
    p <- ggplot(plotData, aes(x=Lag, y=Confidence)) +
      geom_line(colour = "#9999CC") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE) +
      ylim(c(0, max(plotData$Confidence)))
    
    # Add a vertical line to one year back from the test data set
    p <- p +
      geom_vline(xintercept = 5, linetype=4)
    p <- p + geom_vline(xintercept = 11, linetype=2)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Plot the monthly base model correlation matrix for the selected product
  output$basePredCorsPlotly <- renderPlotly({
    plotData <- prodMonthDataNZ()
    if(nrow(plotData) == 0) return()
    
    correlations <- round(cor(as.matrix(plotData[, -1, with=FALSE])), 2)
    correlations[lower.tri(correlations)] <- NA
    melted_cormat <- melt(correlations)
    
    # Create a ggheatmap
    p <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed()
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Plot the distribution of the predictions by lag
  output$predsByMonthPlot <- renderPlot({
    
    plotData <- prodMonthDataNZ()
    if(is.null(plotData) || nrow(plotData)<=0) return()
    
    # Subset the data so that the boxplots can be rendered in a reasonable time
    if(nrow(plotData) > maxRowsPredsByMonthPlot){
      plotData <- plotData[sample(1:nrow(plotData), maxRowsPredsByMonthPlot)]
    }
    
    plotDataMelt <- suppressWarnings(melt(plotData[, -1, with=FALSE]))
    plotDataMelt[[2]] <- log(plotDataMelt[[2]])
    p <- ggplot(plotDataMelt, aes(x=variable, y=value, fill=variable)) +
      geom_boxplot() +
      xlab("Lag") +
      ylab("Log(prediction)") +
      theme(legend.position="none")
    
    p
  })
  
  # Plot the monthly base model lag comparison scatter plot
  output$lagComparePlotly <- renderPlotly({
    plotData <- prodLagCompareNZ()
    if(nrow(plotData) == 0) return()
    colnames(plotData) <- gsub(" ", "_", colnames(plotData))
    
    # Extract and add the product rank to the plot data
    rankExtension <- rankExtension()
    if(input$rankMethod == "Weighted"){
      targetRank <- basePredictions[ncodpers %in% plotData$ncodpers,
                                    get(paste0(input$targetVar, rankExtension,
                                               "_Rank"))]
    } else{
      targetVals <- plotData[[2 + as.numeric(input$rankMethod == "Second lag")]]
      weightedCols <-
        basePredictions[ncodpers %in% plotData$ncodpers,
                        !grepl(input$targetVar, names(basePredictions)) &
                          grepl(paste0("Weighted", rankExtension, "$"),
                                names(basePredictions)), with=FALSE]
      targetRank <- 1 + rowSums(weightedCols > targetVals)
    }
    plotData$Rank <- NA
    for(i in 1:showTopRanksScatter){
      plotData$Rank[targetRank == i] <- i
    }
    plotData[, Rank := factor(Rank)]
    plotData[, TopRank :=
               basePredictions[match(plotData$ncodpers, ncodpers),
                               get(paste0(paste0("Weighted", rankExtension),
                                         "_TopRank"))]]
    
    # Create a ggscatter
    p <- ggplot(plotData, aes_string(x = colnames(plotData)[2],
                                     y = colnames(plotData)[3],
                                     colour = "Rank"))+
      geom_point(aes(text = paste("Top rank:", TopRank)))
    
    # Add the equality line
    lineRange <- range(c(0, as.matrix(plotData[, 2:3, with=FALSE])))
    lineDf <- data.frame(x=lineRange, y=lineRange, Rank = NA, TopRank = NA)
    names(lineDf) <- names(plotData)[-1]
    p <- p + 
      geom_path(data=lineDf, colour="darkcyan", size=1)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Compare the predicted probabilities of multiple base models for different 
  # target product and the same lags
  output$predictionsComparePlotly <- renderPlotly({
    plotData <- prodOtherTargetCompareNZ()
    if(nrow(plotData) == 0) return()
    colnames(plotData) <- c(input$targetVar, input$otherTargetCompare)
    
    # Create a ggscatterplot
    p <- ggplot(plotData, aes_string(x = colnames(plotData)[1],
                                     y = colnames(plotData)[2])) +
      geom_point()
    
    # Add the equality line
    lineRange <- range(c(0, as.matrix(plotData)))
    lineDf <- data.frame(x=lineRange, y=lineRange)
    names(lineDf) <- names(plotData)
    p <- p + 
      geom_path(data=lineDf, colour="darkcyan", size=1)
    
    # Convert to plotly format
    ggplotly(p)
  })
})