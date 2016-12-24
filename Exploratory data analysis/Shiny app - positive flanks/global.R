# # Set working directory
# setwd("C:/Users/Tom/Documents/Kaggle/Santander/")

# Load required libraries
library(shiny)
library(data.table)
library(DT)
library(shinythemes)
library(plotly)
library(reshape2)

# Option to only analyze top products - fixed throughout interactive analysis
onlyAnalyseTop <- TRUE

# Target date
targetDate <- "12-11-2016"

# Read the positive flanks data
# dataFn <- file.path(getwd(), "Feature engineering", targetDate,
                    # "product positive flanks.rds")
dataFn <- "product positive flanks.rds"
data <- readRDS(dataFn)

# Extract the products from the data columns
products <- grep("_ult1$", names(data), value = TRUE)
productsSimple <- gsub("^ind_|_ult1$", "", products)

# List the most important product ids
importantProdIds <- c(3, 5, 7, 13, 18, 19, 22, 23, 24)

# Option to limit the analysis to the top products
if(onlyAnalyseTop){
  products <- products[importantProdIds]
  productsSimple <- productsSimple[importantProdIds]
  importantProdIds <- 1:length(products)
}

# Extract the months range
orderedDateFormat <- data$monthFormat[match(sort(unique(data$fecha_dato)),
                                            data$fecha_dato)]

# Convert newProdSimple to a factor
data[, newProdSimple := factor(newProdSimple, levels = productsSimple)]

# Drop negative antiguedad values
data[antiguedad<0, antiguedad := NA]

# Take a log of the income
data[, logRenta := log(renta)]

# List the considered continuous variables
contVars <- c("age", "antiguedad", "renta", "logRenta") #ncodpers
contVarRanges <- matrix(NA, nrow = length(contVars), ncol = 2, dimnames = 
                          list(contVars, c("min", "max")))
contVarRanges[1, ] <- c(0, 100)
contVarRanges[2, ] <- c(0, 260)
contVarRanges[3, ] <- c(0, 1e6)
contVarRanges[4, ] <- c(8, 16)

# List the considered categorical variables
catVars <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo",
             "indrel", "indrel_1mes", "tiprel_1mes", "indresi", "indext",
             "conyuemp", "canal_entrada", "indfall", "tipodom", "nomprov",
             "ind_actividad_cliente", "segmento")

# Convert categorical "numeric" variables to factors
dataCatVar <- data[, catVars, with=FALSE]
toCatCols <- names(which(sapply(dataCatVar, class) != "character"))
data[, toCatCols] <- lapply(data[, toCatCols, with=FALSE], as.factor)

# Text to display in the about section
aboutString <- "This app was developed by Tom Van de Wiele and relates to the exploratory analysis of the 'Santander product recommendation' Kaggle competition <br/><br/>"