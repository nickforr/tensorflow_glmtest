



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
cleanVita$Memberkey <- NULL

bench::mark({
  model <- 
    glm(DeadInYOE ~ ., data = cleanVita, family = binomial, weights = wts)
}, iterations = 1)

bench::mark({
  model <- 
    glm(DeadInYOE ~ I(ageNearInYOE^(-1)) + I(ageNearInYOE^(-2)) + I(ageNearInYOE^(-3)) + I(ageNearInYOE^(-4)) + YOE.C + I(ageNearInYOE^(-4)):MortGroupRGPCV18 + I(ageNearInYOE^(-4)):salBandRGPCV18, 
    data = cleanVita, family = binomial, weights = wts)
}, iterations = 1)











