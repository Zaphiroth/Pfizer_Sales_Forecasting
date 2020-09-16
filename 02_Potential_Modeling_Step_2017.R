# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Pfizer County Hospitals Potential Project
# Purpose:      Potential Modeling Step
# programmer:   Xin Huang
# Date:         09-05-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##------------------------------------------------------------------------------
##--                 loading the required packages
##------------------------------------------------------------------------------

options(java.parameters = "-Xmx2048m")
suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(dplyr)
  library(plm)
  library(dynlm)
  library(randomForest)
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
  library(caret)
  library(progress)
})

##------------------------------------------------------------------------------
##--                 construct the potential projection model
##------------------------------------------------------------------------------

## -- setup the formula for the randomForest
colnames(universe_data_js) <- tolower(colnames(universe_data_js))

chk_m <- filter(universe_data_js,
                is.na(est_drugincome_rmb))

independent_vars <-
  c("distance",
    "hosp_level",
    # "province",
    "city",
    # "prefecture",
    "specialty_1",
    "specialty_2",
    "医生数",
    "床位数",
    # "全科床位数",
    # "内科床位数",
    # "外科床位数",
    # "眼科床位数",
    "年诊疗人次",
    # "内科诊次",
    # "外科诊次",
    # "入院人数",
    # "住院病人手术人次数",
    "医疗收入",
    # "门诊收入",
    # "门诊治疗收入",
    # "门诊手术收入",
    # "住院收入",
    # "住院床位收入",
    # "住院治疗收入",
    # "住院手术收入" ,
    # "药品收入",
    # "门诊药品收入",
    # "门诊西药收入",
    # "住院药品收入",
    # "住院西药收入",
    "est_drugincome_rmb",
    "gdp_value_100mil",
    "gdp_gr",
    "permanent_residents_10thousand",
    "urban_permanent_residents_10thousand",
    "rural_permanent_residents_10thousand",
    "urban_disposal_income",
    "urban_disposal_income_gr",
    "rural_disposal_income",
    "rural_disposal_income_gr",
    "social_consumption_100mil",
    "retailotc_10thousand"
  )

dependent_var <- "value_2017"
formula <- as.formula(paste(dependent_var, " ~ ",
                            paste(independent_vars, collapse = " + ")))


##-- do the data format for modeling
universe_data_js_m <- universe_data_js %>%
  mutate(
    hosp_level = factor(hosp_level),
    # region = factor(region),
    province = factor(province),
    city = factor(city),
    district = factor(district),
    specialty_1 = factor(specialty_1),
    specialty_2 = factor(specialty_2),
    specialty.3 = factor(specialty.3),
    hospital_name  = factor(hospital_name)
  ) %>%
  select(-pha,
         -value_2016,
         -unit_2016)

##-- do the roughly miss values impute
universe_data_js_m1 <- na.roughfix(universe_data_js_m)

##-- filter out the training data
training_data <- universe_data_js_m1 %>%
  filter(flag == 1)

training_data$value_2017 <- (training_data$value_2017)^(1/1.8)
# training_data$chk <- (training_data$value_2017)^2

##-- here is the optimization of the parameters for randomForest
ntrees <- seq(50, 1000, 50)
mtry <- seq(2, length(independent_vars))
seed <- 123456
# Manual Search
# control <-
#   trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "grid")
control <- trainControl(
  method = "repeatedcv",
  number = 5,
  savePredictions = TRUE,
  repeats = 1,
  verboseIter = T,
  returnResamp = "all"
)
# tunegrid <- expand.grid(.mtry = c(sqrt(ncol(x))))
tunegrid <- expand.grid(.mtry = mtry)
modellist <- list()
pb <- progress_bar$new(total = length(ntrees))
for (ntree in ntrees) {
  pb$tick()
  set.seed(seed)
  fit <- train(
    formula,
    data = training_data,
    method = "rf",
    metric = "RMSE",
    tuneGrid = tunegrid,
    trControl = control,
    ntree = ntree
  )
  key <- toString(ntree)
  modellist[[key]] <- fit
}

##-- compare results
# results <- resamples(modellist)
# summary(results)
# dotplot(results)
best_tune <- sapply(modellist, function(x)
  x$bestTune)
best_result <- lapply(modellist, function(x)
  x$results)
best_result_m <-
  map2(best_tune, best_result, function(x, y)
    y[y$mtry == x,])
best_result_m <- bind_rows(best_result_m)
index <- which.min(best_result_m$RMSE)
best_para_combo <- c(ntrees[index], best_tune[[index]])

#-- test on the CV
mod_fit <- modellist[[toString(best_para_combo[1])]]
cv_performance <- mod_fit$resample %>%
  filter(mtry == best_para_combo[2])
cv_pred <- mod_fit$pred %>%
  filter(mtry == best_para_combo[2]) %>%
  arrange(rowIndex)
# %>%
# group_by(Resample) %>%
# summarise(r_square = 1 - sum((pred - obs) ^ 2) / sum((obs - mean(obs)) ^ 2))
# write.xlsx(cv_pred, "03_Outputs/rf_concentration_test.xlsx")


##-- choose the best parameters combos to run the full random forest model

set.seed(123456)
potential_model <-
  randomForest(
    formula,
    data = training_data,
    na.action = na.roughfix,
    mtry = best_para_combo[2],
    ntree = best_para_combo[1],
    importance = TRUE
  )

importance(potential_model, type = 2)
importance_info <- data.frame(variable_names = rownames(importance(potential_model, type = 1)),
                              "%IncMSE" = as.numeric(importance(potential_model, type = 1)),
                              stringsAsFactors = F)
varImpPlot(potential_model, type = 1, sort = TRUE)

training_data$fitted_value <- predict(potential_model)

1 - sum((training_data$value_2017 - training_data$fitted_value) ^ 2) /
  sum((training_data$value_2017 - mean(training_data$value_2017)) ^ 2)

##------------------------------------------------------------------------------
##--                 predict the potential in the testing data
##------------------------------------------------------------------------------

testing_data_m <- universe_data_js_m1 %>%
  filter(flag == 0)
potential_pred <- predict(potential_model, newdata = testing_data_m)
testing_data_m$value_2017 <- (potential_pred) ^1.8
# testing_data_m$fitted_value <- (potential_pred) ^ 4


##------------------------------------------------------------------------------
##--                 summary the potential of the universe
##------------------------------------------------------------------------------

# final_data <- bind_rows(training_data, testing_data_m)
# final_data_chk <- final_data %>%
#   group_by(city) %>%
#   summarise(fitted_value = sum(fitted_value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   arrange(desc(fitted_value)) %>%
#   mutate(county_hosp_rank = row_number())

sample_data_m <- bind_rows(htn_county_sample_data_js_m1,
            testing_data) %>%
  # filter(hospital_name != "南京市妇幼保健院") %>%
  left_join(htn_biding_data_2017_js, by = "hospital_name") %>%
  filter(!is.na(value_2017)) %>%
  mutate(flag = 1)

# sample_data_m <- sample_data_m %>%
#   left_join(demograph_data_m, by = c("district" = "county")) %>%
#   mutate(flag = 1,
#          gdp_gr = as.numeric(gdp_gr)) %>%
#   select(-PHA)

colnames(sample_data_m) <- tolower(colnames(sample_data_m))

final_data <- bind_rows(sample_data_m, testing_data_m) %>%
  mutate(value_prj_2017 = value_2017 / 0.8)

final_data_chk1 <- final_data %>%
  group_by(district, gdp_value_100mil, permanent_residents_10thousand,
           urban_disposal_income, rural_disposal_income, social_consumption_100mil,
           retailotc_10thousand) %>%
  summarise(value_2017 = sum(value_2017, na.rm = TRUE),
            value_prj_2017 = sum(value_prj_2017, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(value_2017)) %>%
  mutate(county_rank = row_number())

correlation <- cor(final_data_chk1[, -c(1, 10)])

final_data_chk2 <- final_data %>%
  group_by(district, gdp_value_100mil, permanent_residents_10thousand,
           urban_disposal_income, rural_disposal_income, social_consumption_100mil,
           retailotc_10thousand) %>%
  summarise(est_drugincome_rmb = sum(est_drugincome_rmb, na.rm = T),
            医疗收入 = sum(医疗收入, na.rm = T),
            医生数 = sum(医生数, na.rm = T),
            年诊疗人次 = sum(年诊疗人次, na.rm = T),
            床位数 = sum(床位数, na.rm = T),
            distance = sum(distance, na.rm = T),
            value_2017 = sum(value_2017),
            value_prj_2017 = sum(value_prj_2017)) %>%
  # select(district, gdp_value_100mil, permanent_residents_10thousand,
  #        urban_disposal_income, rural_disposal_income, social_consumption_100mil,
  #        retailotc_10thousand, value_2017,value_prj_2017) %>%
  # mutate(value_2017 = sum(value_2017, na.rm = TRUE),
  #        value_prj_2017 = sum(value_prj_2017, na.rm = TRUE)) %>%
  # ungroup() %>%
  arrange(desc(value_2017)) %>%
  mutate(county_rank = row_number())



correlation <- cor(final_data_chk2[, -c(1, 16)])

wb <- createWorkbook()
addWorksheet(wb, "model_performance_cv1")
addWorksheet(wb, "model_performance_cv2")
addWorksheet(wb, "model_performance")
addWorksheet(wb, "variable_importance")
addWorksheet(wb, "final_data")
addWorksheet(wb, "correlation_chk")

writeData(wb, "model_performance_cv1", cv_performance)
writeData(wb, "model_performance_cv2", cv_pred)
writeData(wb, "model_performance", training_data)
writeData(wb, "variable_importance", importance_info)
writeData(wb, "final_data", final_data)
writeData(wb, "correlation_chk", correlation, rowNames = T)
saveWorkbook(wb, "03_Outputs/county_hospitals_potential_projection_2017_1008.xlsx",
             overwrite = TRUE)


# write.xlsx(final_data,
#            "03_Outputs/county_hospitals_potential_projection_0914.xlsx")


results_2016 <- read.xlsx("03_Outputs/county_hospitals_potential_projection_by_matching_0920.xlsx")
results_2016_m <- results_2016 %>%
  select(hospital_name,
         value_)








##------------------------------------------------------------------------------
##--                 Try the boost model
##------------------------------------------------------------------------------
# universe_data_js_m2 <-
#   universe_data_js_m1[, c("新版名称", "value", independent_vars, "flag")]
# universe_data_js_m3 <-
#   dummyVars( ~ hosp_level + city + specialty_1, data = universe_data_js_m2)
# 
# universe_data_js_m4 <-
#   as.data.frame(predict(universe_data_js_m3, newdata = universe_data_js_m2))
# 
# universe_data_js_m5 <- universe_data_js_m2 %>%
#   select(-c(hosp_level, city, specialty_1)) %>%
#   bind_cols(universe_data_js_m4)
# 
# training_data_m <- universe_data_js_m5 %>%
#   filter(flag == 1)
# 
# independent_vars_m <-
#   setdiff(colnames(training_data_m),
#           c("新版名称", "value", "flag"))
# 
# 
# sample <- sample.int(
#   n = nrow(training_data_m),
#   size = floor(.8 * nrow(training_data_m)),
#   replace = F
# )
# 
# train_t <- training_data_m[sample,] #just the samples
# valid  <- training_data_m[-sample,] #everything but the samples
# 
# train_y <-  train_t[, 'value']
# train_x = train_t[, independent_vars_m]
# 
# valid_y = valid[, 'value']
# valid_x = valid[, independent_vars_m]
# 
# train_y[1:10]
# 
# gb_train = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
# gb_valid = xgb.DMatrix(data = as.matrix(valid_x), label = valid_y)
# 
# # train xgb, evaluating against the validation
# watchlist = list(train = gb_train, valid = gb_valid)
# 
# metrics <- data.frame()
# for (depth in seq(1, 10, 1)) {
#   for (eta in seq(0.01, 1, 0.01)) {
#     bst_slow = xgb.train(
#       data = gb_train,
#       max.depth = 4,
#       eta = 0.01,
#       nthread = 2,
#       nround = 10000,
#       watchlist = watchlist,
#       objective = "reg:linear",
#       early_stopping_rounds = 100,
#       print_every_n = 500
#     )
#     
#     # validatation data r square
#     valid_pred <- predict(bst_slow, gb_valid)
#     r_square_valid <-
#       1 - sum((valid_y - valid_pred) ^ 2) / sum((valid_y - mean(valid_y)) ^
#                                                   2)
#     
#     # training data r square
#     
#     train_pred <- predict(bst_slow, gb_train)
#     r_square_train <-
#       1 - sum((train_y - train_pred) ^ 2) / sum((train_y - mean(train_y)) ^
#                                                   2)
#     
#     bst_slow = xgb.train(
#       data = gb_train,
#       max.depth = depth,
#       eta = eta,
#       nthread = 2,
#       nround = 10000,
#       watchlist = watchlist,
#       objective = "reg:linear",
#       early_stopping_rounds = 100,
#       print_every_n = 500
#     )
#     
#     # validatation data r square
#     valid_pred <- predict(bst_slow, gb_valid)
#     r_square_valid <-
#       1 - sum((valid_y - valid_pred) ^ 2) / sum((valid_y - mean(valid_y)) ^
#                                                   2)
#     
#     # training data r square
#     
#     train_pred <- predict(bst_slow, gb_train)
#     r_square_train <-
#       1 - sum((train_y - train_pred) ^ 2) / sum((train_y - mean(train_y)) ^
#                                                   2)
#     
#     
#     metrics <- bind_rows(
#       metrics,
#       data.frame(
#         depth = depth,
#         eta = eta,
#         r_square_valid = r_square_valid,
#         r_square_train = r_square_train
#       )
#     )
#     
#   }
# }
# 
# ##-- run the full model
# best_para_combo_boost <-
#   metrics[which.max(metrics$r_square_valid), c(1, 2)]
# 
# full_training_data <-
#   xgb.DMatrix(data = as.matrix(training_data_m[, independent_vars_m]),
#               label = as.matrix(training_data_m[, "value"]))
# 
# full_watch_list <- list(train = full_training_data)
# full_bst_model = xgb.train(
#   data = full_training_data,
#   max.depth = best_para_combo_boost[1],
#   eta = best_para_combo_boost[2],
#   nthread = 2,
#   nround = 10000,
#   watchlist = full_watch_list,
#   objective = "reg:linear",
#   early_stopping_rounds = 100,
#   print_every_n = 500
# )
# 
# full_train_pred <- predict(full_bst_model, full_training_data)
# full_train_y <- training_data_m[, "value"]
# r_square_full_train <-
#   1 - sum((full_train_y - full_train_pred) ^ 2) /
#   sum((full_train_y - mean(full_train_y)) ^ 2)
# 
# testing_data_m <- universe_data_js_m5 %>%
#   filter(flag == 0) %>%
#   select(-c(新版名称, value, flag))
# dtest <- xgb.DMatrix(data = as.matrix(testing_data_m))
# boost_pred <- predict(bst_slow, dtest)
# 
# ##-- test on the CV
# set.seed(123456)
# sequence <- NULL
# for (i in 1:5) {
#   if (i < 5) {
#     sequence <- c(sequence, rep(i, 24))
#   }
#   else {
#     sequence <- c(sequence, rep(i, 22))
#   }
# }
# 
# foldid <- sample(sequence, size = length(y), replace = FALSE)
# 
# best_para_combo_boost <-
#   metrics[which.max(metrics$r_square_valid), c(1, 2)]
# 
# r_square_cv_boost <- list()
# for (i in 1:5) {
#   idx1 <- which(foldid != i)
#   idx <- which(foldid == i)
#   
#   
#   
#   cv_training_data <-
#     xgb.DMatrix(data = as.matrix(training_data_m[idx1, independent_vars_m]),
#                 label = as.matrix(training_data_m[idx1, "value"]))
#   
#   cv_data <-
#     xgb.DMatrix(data = as.matrix(training_data_m[idx, independent_vars_m]))
#   
#   cv_watch_list <- list(train = cv_training_data)
#   cv_bst_model = xgb.train(
#     data = cv_training_data,
#     max.depth = best_para_combo_boost[1],
#     eta = best_para_combo_boost[2],
#     nthread = 2,
#     nround = 10000,
#     watchlist = cv_watch_list,
#     objective = "reg:linear",
#     early_stopping_rounds = 100,
#     print_every_n = 500
#   )
#   
#   cv_pred <- predict(cv_bst_model, cv_data)
#   cv_y <- training_data_m[idx, "value"]
#   
#   toString(i)
#   r_square_cv_boost[[toString(i)]] <-
#     1 - sum((cv_y - cv_pred) ^ 2) /  sum((cv_y - mean(cv_y)) ^ 2)
#   
# }
# 
# 
# ##------------------------------------------------------------------------------
# ##--                 Try the simple linear model
# ##------------------------------------------------------------------------------
# universe_data_js_m2 <-
#   universe_data_js_m1[, c("新版名称", "value", independent_vars, "flag")]
# universe_data_js_m3 <-
#   dummyVars( ~ hosp_level + city + specialty_1, data = universe_data_js_m2)
# 
# universe_data_js_m4 <-
#   as.data.frame(predict(universe_data_js_m3, newdata = universe_data_js_m2))
# 
# universe_data_js_m5 <- universe_data_js_m2 %>%
#   select(-c(hosp_level, city, specialty_1)) %>%
#   bind_cols(universe_data_js_m4)
# 
# training_data_m <- universe_data_js_m5 %>%
#   filter(flag == 1)
# 
# independent_vars_m <-
#   setdiff(colnames(training_data_m),
#           c("新版名称", "value", "flag"))
# 
# 
# sample <- sample.int(
#   n = nrow(training_data_m),
#   size = floor(.8 * nrow(training_data_m)),
#   replace = F
# )
# 
# train_t <- training_data_m[sample,] #just the samples
# valid  <- training_data_m[-sample,] #everything but the samples
# 
# train_y <-  train_t[, 'value']
# train_x = train_t[, independent_vars_m]
# 
# valid_y = valid[, 'value']
# valid_x = valid[, independent_vars_m]
# 
# training_data_l <- data.frame(value = train_y, train_x)
# valid_data_l <- data.frame(value = valid_y, valid_x)
# 
# colnames(training_data_l) <-
#   gsub("\\.", "_", colnames(training_data_l))
# colnames(valid_data_l) <- gsub("\\.", "_", colnames(valid_data_l))
# 
# 
# lm_model <- lm(as.formula(paste(
#   "value", " ~ ",
#   paste(setdiff(colnames(training_data_l), "value"),
#         collapse = " + ")
# )),
# data = training_data_l)
# summary(lm_model)
# 
# lm_predict <- predict(lm_model, valid_data_l)
# lm_y <- valid_data_l[, "value"]
# r_square_lm <-
#   1 - sum((lm_y - lm_predict) ^ 2) / sum((lm_y - mean(lm_y)) ^ 2)
# 
# 
# ##------------------------------------------------------------------------------
# ##--                 Try the glmnet
# ##------------------------------------------------------------------------------
# 
# universe_data_js_m2 <-
#   universe_data_js_m1[, c("新版名称", "value", independent_vars, "flag")]
# 
# model_data_js <- universe_data_js_m2 %>%
#   select(-c(新版名称))
# 
# model_data_js_m <- model.matrix(value ~ ., data = model_data_js) %>%
#   as.data.frame()
# 
# x <- model_data_js_m %>%
#   filter(flag == 1) %>%
#   select(-c(flag))
# x <- as.matrix(x)
# 
# y <- model_data_js %>%
#   filter(flag == 1) %>%
#   select(value)
# y <- unlist(y)
# 
# x_new <- model_data_js_m %>%
#   filter(flag == 0) %>%
#   select(-c(flag))
# x_new <- as.matrix(x_new)
# 
# 
# set.seed(123456)
# sequence <- NULL
# for (i in 1:5) {
#   if (i < 5) {
#     sequence <- c(sequence, rep(i, 24))
#   }
#   else {
#     sequence <- c(sequence, rep(i, 22))
#   }
# }
# 
# foldid <- sample(sequence, size = length(y), replace = FALSE)
# 
# r_square_glmnet <- list()
# for (i in 1:5) {
#   idx <- which(foldid == i)
#   cv_x <- x[idx,]
#   cv_y <- y[idx]
#   idx1 <- which(foldid != i)
#   training_x <- x[idx1,]
#   training_y <- y[idx1]
#   
#   fit <- cv.glmnet(x = training_x, y = training_y)
#   pred <- predict(fit, cv_x, s = "lambda.min")
#   
#   r_square_glmnet[[toString(i)]] <-
#     1 - sum((cv_y - pred) ^ 2) / sum((cv_y - mean(cv_y)) ^ 2)
#   
# }
# 
# cvfit = cv.glmnet(x, y, foldid = foldid)
# plot(cvfit)
# r2 <-
#   cvfit$glmnet.fit$dev.ratio[which(cvfit$glmnet.fit$lambda == cvfit$lambda.min)]
# glmnet_pred <- predict(cvfit, newx = x, s = "lambda.min")
# 
# r_square_glmnet <-
#   1 - sum((y - glmnet_pred) ^ 2) / sum((y - mean(y)) ^ 2)
