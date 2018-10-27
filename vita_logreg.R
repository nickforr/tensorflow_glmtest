



library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)

vita <- readRDS("vita_mock.RDS")

cleanVita <- 
  as_data_frame(vita) %>%
  mutate_if(is.character, as_factor) %>%
  mutate(DeadInYOE = as.integer(DeadInYOE))
rm(vita)

wts <- cleanVita$partialETRinitial
cleanVita$partialETRinitial <- NULL

bench::mark({
  model <- 
    glm(DeadInYOE ~ ., data = cleanVita, family = binomial, weights = wts)
}, iterations = 1)











