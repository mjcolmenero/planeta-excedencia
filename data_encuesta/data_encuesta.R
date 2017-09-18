library(dplyr)
library(stringr)

setwd('~/Documents/_projects/2017/visualizar/planeta-excedencia/data_encuesta/')

### -- -- -- read the data -- -- -- ### 
rawData <- read.csv('Excedencia-report.csv', stringsAsFactors = FALSE)
data <- data.frame() # new data frame to store the clean data


### -- -- -- set direct columns -- -- -- ###
# columns 2: number of kids
data <- data.frame('num_hijos' = as.vector(rawData[ ,2]))

# column 18: do you have help
data$help <-  as.vector(rawData[ ,18])

# column 20: ask for excedencia for all the children
data$excedencia_all <- as.vector(rawData[ ,20])
data <- data %>% 
  mutate(excedencia_all = ifelse(excedencia_all == 0, 'no', 'si'))

# column 21:23 child number and excedencia duration (will be cleaned by hand)
data$duration_child_1 <- as.vector(rawData[ ,21])
data$duration_child_2 <- as.vector(rawData[ ,22])
data$duration_child_3 <- as.vector(rawData[ ,23])

# column 24: main reason
data$main_reason <- as.vector(rawData[ ,24])

# column 34: diff reason
data$diff_reasons <- as.vector(rawData[ ,34])

# column 35: other parent ask for permison?
data$other_ask <- as.vector(rawData[ ,35])
data <- data %>% 
  mutate(other_ask = ifelse(other_ask == 0, 'no', 'si'))

# column 36: why other not?
data$other_not_reason <- as.vector(rawData[ ,36])


### -- -- -- set others -- -- -- ###
for (i in 1:nrow(rawData)) {
  # 3rd column is the 'otros' for the 2nd question
  if (rawData[i, 3] != "") data$num_hijos[i] <- paste('otros: ', rawData[i, 3], sep = '')
  
  # columns 19: 'otros' for 'help'
  if (rawData[i, 19] != "") data$help[i] <-  paste('otros: ', rawData[i, 19], sep = '')
  
  # columns 25: 'otros' for 'main reason'
  if (rawData[i, 25] != "") data$main_reason[i] <-  paste('otros: ', rawData[i, 25], sep = '')
  
  # columns 37: 'otros' for 'other not reason'
  if (rawData[i, 37] != "") data$other_not_reason[i] <-  paste('otros: ', rawData[i, 37], sep = '')
}


### -- -- -- grouped answers -- -- -- ###

# columns 4:17 are the children years of birth
prevCols <- ncol(data)
for (i in 1:nrow(rawData)) {
  years <- c()
  for (j in 4:17) {
    if (!is.na(rawData[i, j]) && rawData[i, j] != "") {
      if (is.numeric(rawData[i, j])) {
        years <- c(years, rawData[i, j])
      } else {
        aux <- unlist(str_extract_all(rawData[i, j], '([0-9]{4})'))
        for (y in aux) {
          years <- c(years, as.numeric(y))
        }
      }
    }
  }
  years <- sort(years)
  for (y in 1:length(years)) {
    data[i, y + prevCols] <- years[y]
    names(data)[y + prevCols] <- paste('birth_child_', y, sep = '')
  }
}

# columns 26:33 secondary reasons
prevCols <- ncol(data)
for (i in 1:nrow(rawData)) {
  reasons <- c()
  for (j in 26:33) {
    if (rawData[i, j] != "") {
      reasons <- c(reasons, rawData[i, j])
    }
  }
  if (length(reasons > 0)) {
    reasons <- sort(reasons)
    for (r in 1:length(reasons)) {
      data[i, r + prevCols] <- reasons[r]
      names(data)[r + prevCols] <- paste('secondary_reason_', r, sep = '')
    }
  }
}




# filter by date: get rid off the ones before the 16 - 09

