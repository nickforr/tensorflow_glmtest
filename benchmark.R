

# Replicating, roughly, this: 
# http://deeper.solutions/blog/2017/03/06/r-vs-python-vs-scala-vs-spark-vs-tensorflow-the-quantitative-answer/

# Download data
# dataUrl <- "http://stat-computing.org/dataexpo/2009/2008.csv.bz2"
# download.file(dataUrl, "raw_data_2008.csv.bz2")

library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(tensorflow)


if (!file.exists("tidyData.RDS")) {
rawData <- readr::read_csv("raw_data_2008.csv.bz2")

#drop rows where delay column is na
tidyData <- 
  rawData[1:4000000, ] %>% 
  tidyr::drop_na(ArrDelay) %>%
  dplyr::mutate(IsArrDelayed = as.numeric(.data$ArrDelay > 0)) %>%
  dplyr::mutate_if(is.character, as_factor)
saveRDS(tidyData, "tidyData.RDS")
rm(rawData)
} else {
  tidyData <- readRDS("tidyData.RDS")
}


# tidyData["Origin"] <- model.matrix(~Origin, data = tidyData)
# tidyData["Dest"] <- model.matrix(~Dest, data = tidyData)

#split the dataset in two parts
trainIndex = 
  sample(1:nrow(tidyData), size = round(0.8 * nrow(tidyData)), replace = FALSE)
train = tidyData[trainIndex, ]
test  = tidyData[-trainIndex, ]

# R attempt
bench::mark({
  model <- 
    glm(IsArrDelayed ~ Year + Month + DayofMonth + DayOfWeek + DepTime + 
          AirTime + Origin + Dest + Distance,
        data = train, family = binomial)
}, iterations = 1)













