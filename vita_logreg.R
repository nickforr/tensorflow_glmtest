



library(dplyr)
library(tidyr) # for drop_na
library(forcats) # for as_factor
library(h2o)
#library(h2o4gpu)
library(speedglm)

vita <- readRDS("vita_mock.RDS")

wts <- vita$partialETRinitial
vita$partialETRinitial <- NULL
vita$Memberkey <- NULL

cleanVita <- 
  as_data_frame(vita) %>%
  mutate_if(is.character, funs(as.numeric(as_factor(.)))) %>%
  mutate(DeadInYOE = as.integer(DeadInYOE))
rm(vita)

# simplistic for baseline
bench::mark({
  model_base <- 
    glm(DeadInYOE ~ ., data = cleanVita, family = binomial, weights = wts)
}, iterations = 5)
summary(model_base)
rm(model_base)

# simple h2o
# See https://www.h2o.ai/wp-content/uploads/2018/01/RBooklet.pdf
# And https://www.r-bloggers.com/glm-with-h2o-in-r/
# https://stackoverflow.com/questions/45426642/h2o-glm-interact-only-certain-predictors
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/interaction_pairs.html
h2o.init(max_mem_size = "20g", nthreads = -1)
hCleanVita <- cleanVita
hCleanVita$wts <- wts
hCleanVita <- as.h2o(hCleanVita)
bench::mark({
  model_h2oBase <- 
    h2o.glm(
      x = names(cleanVita)[!(names(cleanVita) %in% c("wts", "DeadInYOE"))], 
      y = "DeadInYOE", 
      training_frame = hCleanVita, family = "binomial", weights_column = "wts")
}, iterations = 5)
summary(model_h2oBase)
rm(model_h2oBase, hCleanVita)
h2o.shutdown()

# h2o4gpu - not sure of method...
x <- cleanVita[, -5]
y <- cleanVita$DeadInYOE
bench::mark({
  model_h2o4gpu <-
    h2o4gpu::h2o4gpu.gradient_boosting_classifier(n_estimators = 10) %>%
    h2o4gpu::fit(x = x, y = y)
}, iterations = 5)
summary(model_h2o4gpu)
rm(model_h2o4gpu, x, y)




# actual vita method
bench::mark({
  model_vita <- 
    glm(DeadInYOE ~ I(ageNearInYOE^(-1)) + I(ageNearInYOE^(-2)) + I(ageNearInYOE^(-3)) + 
          I(ageNearInYOE^(-4)) + YOE.C + I(ageNearInYOE^(-4)):MortGroupRGPCV18 + 
          I(ageNearInYOE^(-4)):salBandRGPCV18, 
    data = cleanVita, family = binomial, weights = wts)
}, iterations = 5)
summary(model_vita)
rm(model_vita)

# speedglm for actual vita method
bench::mark({
  model_speedvita <- 
    speedglm::speedglm(
      DeadInYOE ~ I(ageNearInYOE^(-1)) + I(ageNearInYOE^(-2)) + I(ageNearInYOE^(-3)) + 
        I(ageNearInYOE^(-4)) + YOE.C + I(ageNearInYOE^(-4)):MortGroupRGPCV18 + 
        I(ageNearInYOE^(-4)):salBandRGPCV18, 
        data = cleanVita, family = binomial(), weights = wts)
}, iterations = 5)
summary(model_speedvita)
rm(model_speedvita)

# add h2o with actual formula here (can use interaction_pairs)
h2o.init(max_mem_size = "20g", nthreads = -1)
bench::mark({
  hCleanVita <- 
    model.matrix(
      ~I(ageNearInYOE^(-1)) + I(ageNearInYOE^(-2)) + I(ageNearInYOE^(-3)) + 
        I(ageNearInYOE^(-4)) + YOE.C + I(ageNearInYOE^(-4)):MortGroupRGPCV18 + 
        I(ageNearInYOE^(-4)):salBandRGPCV18 - 1, 
      data = cleanVita) %>%
    as_data_frame()
}, iterations = 5)
hCleanVita$DeadInYOE <- cleanVita$DeadInYOE
hCleanVita$wts <- wts
hCleanVita <- as.h2o(hCleanVita)
bench::mark({
  model_h2oBase <- 
    h2o.glm(
      x = names(hCleanVita)[!(names(hCleanVita) %in% c("wts", "DeadInYOE"))], 
      y = "DeadInYOE", 
      training_frame = hCleanVita, family = "binomial", weights_column = "wts")
}, iterations = 5)
summary(model_h2oBase)
rm(model_h2oBase, hCleanVita)
h2o.shutdown()





# Note could try sparse.model.matrix?? (probably for speedglm?)



# actual vita method with model.matrix

bench::mark({
  model_vitaMatrix <- 
    glm.fit(x = x, y = y, family = binomial(), weights = wts)
}, iterations = 5)
summary(model_vitaMatrix, x, y)
rm(model_vitaMatrix)








# try poly for vita method??
# bench::mark({
#   model <- 
#     glm(DeadInYOE ~ poly(I(1 / ageNearInYOE), 4) + YOE.C + I(ageNearInYOE^(-4)):MortGroupRGPCV18 + I(ageNearInYOE^(-4)):salBandRGPCV18, 
#         data = cleanVita, family = binomial, weights = wts)
# }, iterations = 1)
# rm(model)



