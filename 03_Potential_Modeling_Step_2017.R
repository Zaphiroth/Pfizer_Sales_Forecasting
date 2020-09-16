# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bayer County Potential
# Purpose:      Potential Modeling Step
# programmer:   Xin Huang
# Date:         26-11-2018
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
  library(randomForestSRC)
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
colnames(universe_data_js_model) <- tolower(colnames(universe_data_js_model))

chk_m <- filter(universe_data_js_model,
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

dependent_var <- setdiff(unique(universe_data_js$brandName), NA)
formula <- as.formula(paste("Multivar(",paste(dependent_var,collapse = ","), ") ~ ",
                            paste(independent_vars, collapse = " + ")))


##-- do the data format for modeling
universe_data_js_model_m <- universe_data_js_model %>%
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
  select(-pha)

##-- do the roughly miss values impute
universe_data_js_model_m1 <- na.roughfix(universe_data_js_model_m)

##-- filter out the training data
training_data <- universe_data_js_model_m1 %>%
  filter(flag == 1)

model_output <- rfsrc(formula,
                      data = training_data,
                      block.size=1, importance = TRUE)
