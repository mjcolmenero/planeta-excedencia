library(dplyr)
library(stringr)

setwd('~/Documents/_projects/2017/visualizar/planeta-excedencia/data_encuesta/')

### -- -- -- read the data -- -- -- ### 
rawData <- read.csv('Excedencia-report.csv', stringsAsFactors = FALSE, header = TRUE)
rawData <- rawData[6:nrow(rawData), ]

data <- data.frame() # new data frame to store the clean data


### -- -- -- set direct columns -- -- -- ###
# columns 2: number of kids
data <- data.frame('q01_num_hijos' = as.vector(rawData[ ,2]))

# column 18: do you have help
data$q03_help <-  as.vector(rawData[ ,18])

# column 20: ask for excedencia for all the children
data$q04_excedencia_all <- as.vector(rawData[ ,20])
data <- data %>% 
  mutate(q04_excedencia_all = ifelse(q04_excedencia_all == 0, 'no', 'si'))

# column 21:23 child number and excedencia duration (will be cleaned by hand)
data$q05_duration_child <- as.vector(rawData[ ,21])
data$q06_duration_child <- as.vector(rawData[ ,22])
data$q07_duration_child <- as.vector(rawData[ ,23])

# column 24: main reason
data$q08_main_reason <- as.vector(rawData[ ,24])

# column 34: diff reason
data$q10_diff_reasons <- as.vector(rawData[ ,34])

# column 35: other parent ask for permison?
data$q11_other_ask <- as.vector(rawData[ ,35])
data <- data %>% 
  mutate(q11_other_ask = ifelse(q11_other_ask == 0, 'no', 'si'))

# column 36: why other not?
data$q12_other_not_reason <- as.vector(rawData[ ,36])

# column 43: company reaction
data$q14_company_reaction <- as.vector(rawData[ ,43])

# column 45: mates reaction
data$q15_mates_reaction <- as.vector(rawData[ ,45])

# column 47: formation time
data$q16_formation_time <- as.vector(rawData[ ,47])

# column 49: work during the leave 
data$q17_work_during <- as.vector(rawData[ ,49])

# column 51: back to work 
data$q18_back_work <- as.vector(rawData[ ,51])

# column 53: responsability after
data$q19_tasks_after <- as.vector(rawData[ ,53])

# column 53: current job
data$q20_current_tasks <- as.vector(rawData[ ,55])

# column 57: overall evaluation
data$q21_excedencia_general_evaluation <- as.vector(rawData[ ,57])

# column 58: excedencia improvements
data$q22_excedencia_needs_improvements <- as.vector(rawData[ ,58])

# column 60: excedencia improvements
data$q23_excedencia_specify_improvements <- as.vector(rawData[ ,60])

# column 61: conciliation overall evaluation
data$q24_conciliation_general_evaluation <- as.vector(rawData[ ,61])

# column 62: conciliation improvements
data$q25_conciliation_needs_improvements <- as.vector(rawData[ ,62])

# column 64: conciliation improvements
data$q26_conciliation_specify_improvements <- as.vector(rawData[ ,64])

# column 65: three word experience
data$q27_experience_description <- as.vector(rawData[ ,65])

# column 66: anythng to add
data$q28_comments <- as.vector(rawData[ ,66])

# column 67: genre
data$q29_genre <- as.vector(rawData[ ,67])

# column 68: age
data$q29_age <- as.vector(rawData[ ,68])


# column 69: civil status
data$q31_civil_status <- as.vector(rawData[ ,69])

# column 70: education
data$q32_education <- as.vector(rawData[ ,70])

# column 71: job
data$q33_job <- as.vector(rawData[ ,71])

# column 73: contract
data$q34_contract <- as.vector(rawData[ ,73])

# column 73: salary
data$q35_salary <- as.vector(rawData[ ,75])


### -- -- -- set others -- -- -- ###
others <- function(input_column, output_column) {
  for (i in 1:nrow(rawData)) {
    if (rawData[i, input_column] != "") data[output_column][i, ] <- paste('otros: ', rawData[i, input_column], sep = '')
  }
  return(data)
}

data <- others(3, 'q01_num_hijos')
data <- others(19, 'q03_help')
data <- others(25, 'q08_main_reason')
data <- others(37, 'q12_other_not_reason')
data <- others(44, 'q14_company_reaction')
data <- others(46, 'q15_mates_reaction')
data <- others(48, 'q16_formation_time')
data <- others(50, 'q17_work_during')
data <- others(52, 'q18_back_work')
data <- others(54, 'q19_tasks_after')
data <- others(56, 'q20_current_tasks')
data <- others(59, 'q22_excedencia_needs_improvements')
data <- others(63, 'q25_conciliation_needs_improvements')
data <- others(72, 'q33_job')
data <- others(74, 'q34_contract')



### -- -- -- multiple answers -- -- -- ###

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
    names(data)[y + prevCols] <- paste('q02', letters[y], '_birth_child', sep = '')
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
      names(data)[r + prevCols] <- paste('q09', letters[r], '_secondary_reason', sep = '')
    }
  }
}

# columns 38:42 q13 - conciliation options offered by the company
prevCols <- ncol(data)
for (i in 1:nrow(rawData)) {
  reasons <- c()
  for (j in 38:42) {
    if (rawData[i, j] != "") {
      if (i != 42) {
        reasons <- c(reasons, rawData[i, j])
      } else {
        reasons <- c(reasons, paste('otros: ', rawData[i, j], sep = ''))
      }
    }
  }
  if (length(reasons > 0)) {
    reasons <- sort(reasons)
    for (r in 1:length(reasons)) {
      data[i, r + prevCols] <- reasons[r]
      names(data)[r + prevCols] <- paste('q13', letters[r], '_conciliation_opts', sep = '')
    }
  }
}

### -- -- -- sort columns -- -- -- ###
for (i in 1:nrow(data)) {
  data$q00_id[i] <- i
}

data <- data[,order(colnames(data))]

write.csv(data, 'encuesta_output.csv', row.names = F)


# filter by date: get rid off the ones before the 16 - 09

