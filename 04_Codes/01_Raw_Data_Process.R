# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sales forecasting
# Purpose:      Prediction
# programmer:   Zhe Liu
# Date:         11-26-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##-- loading the required packages
options(java.parameters = "-Xmx2048m")

suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(dplyr)
  library(plm)
  library(dynlm)
  library(tidyr)
  library(data.table)
  library(stringi)
  library(stringr)
  library(lubridate)
  library(randomForest)
  library(randomForestSRC)
  library(VIM)
})

##-- readin the raw data
county_sample_data_js <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx")

biding_data_2017 <- read.xlsx("02_Inputs/js项目辉瑞41个品种-201809.xlsx")
biding_data_2016 <- fread("02_Inputs/biding_data_2016.csv", encoding="UTF-8")
biding_data_2016 <- setDF(biding_data_2016)

demograph_data <-
  read.xlsx("02_Inputs/data_for_clustering.xlsx", sheet = "county")
colnames(demograph_data) <-
  c("province", "city", "county", "county_code_2015", "gdp_value_100mil",
    "gdp_gr", "permanent_residents_10thousand", 
    "urban_permanent_residents_10thousand",
    "rural_permanent_residents_10thousand",
    "permanent_residents_birth_rate",
    "new_born", "urban_disposal_income", "urban_disposal_income_gr",
    "rural_disposal_income", "rural_disposal_income_gr",
    "social_consumption_100mil",
    "retailotc_10thousand", "sample_vaccine_data")


##-- data preprocessing
htn_biding_data_2016 <- biding_data_2016 %>%
  filter(ATCII %in% c("β-受体阻滞剂", "钙通道阻滞剂", 
                      "抗高血压药","利尿剂", "作用于肾素-血管紧张素系统的药物"))

htn_biding_data_2016_js <-
  htn_biding_data_2016 %>%
  group_by(province, city, district, Hospital_Code, hospital_name,
           Hospital_Categroy_I, Hospital_Categroy_II,
           Hospital_Categroy_III, Hospital_Categroy_IV) %>%
  summarise(value_2016 = sum(value, na.rm = TRUE),
            unit_2016 = sum(unit, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(province == "江苏省")
htn_biding_data_2016_js[htn_biding_data_2016_js$district == "海安县",]$district <- "海安市"

htn_biding_data_2017 <- biding_data_2017 %>%
  select(省份, 城市, 区县, 医院名称, 商品名, 采购数量, 采购金额, 医院区域类型) %>%
  mutate(商品名 = ifelse(is.na(商品名), "其他", 商品名))
colnames(htn_biding_data_2017) <- c("province", "city", "district",
                                    "hospital_name", "trade_name", "unit", "value",
                                    "Hospital_Categroy")

htn_biding_data_2017_js <- htn_biding_data_2017 %>%
  filter(province == "江苏省") %>%
  group_by(hospital_name, trade_name) %>%
  summarise(amount = sum(value, na.rm = TRUE)) %>%
  mutate(amount = sqrt(amount)) %>%                              # sqrt
  spread(trade_name, amount, fill = 0) %>%
  mutate(flag = 1)

demograph_data_m <- demograph_data %>%
  select(-c(permanent_residents_birth_rate,
            new_born, sample_vaccine_data))

county_sample_data_js_tmp <- county_sample_data_js %>%
  mutate(PHA_tmp = ifelse(is.na(PHA), hospital_name, PHA))

county_hospitals_universe_js <- read.xlsx("02_Inputs/江苏-医院&县医院-HX0930.xlsx", sheet = "Sheet4") %>%
  left_join(demograph_data_m, by = c("Prefecture" = "county")) %>%
  mutate(flag = ifelse(`新版名称` %in% unique(county_sample_data_js_tmp$PHA_tmp), 
                       1, 0),
         医生数 = as.numeric(医生数),
         gdp_gr = as.numeric(gdp_gr)) %>%
  select(-c(Type, Province, City, county.HP, City.Tier.2010, City.Tier.2018,
            Region, PHA.ID, PHAHospname, 重复)) %>%
  rename(hospital_name = 新版名称,
         district = Prefecture)

htn_county_sample_data_js_m <- county_sample_data_js %>%
  dplyr::select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
                   county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) %>%
  mutate(医生数 = as.numeric(医生数)) %>%
  dplyr::select(-c(Province, City, Hospital_Code, Hospital_Categroy_I, province, city,
                   Hospital_Categroy_II,Hospital_Categroy_III, Hospital_Categroy_IV,
                   `Re-Speialty`)) %>%
  group_by(hospital_name) %>%
  arrange(-Est_DrugIncome_RMB) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  left_join(demograph_data_m, by = c("district" = "county")) %>%
  mutate(gdp_gr = as.numeric(gdp_gr))

testing_data <- county_hospitals_universe_js %>%
  filter(flag == 0) %>%
  select(-flag)

universe_data_js <- bind_rows(htn_county_sample_data_js_m, testing_data) %>%
  left_join(htn_biding_data_2017_js, by = "hospital_name") %>%
  mutate(flag = ifelse(is.na(flag), 0, flag)) %>%
  select(-c(PHA, Specialty.3, value_2016, unit_2016, province, county_code_2015)) %>%
  mutate(hospital_name = as.factor(hospital_name),
         Hosp_level = as.factor(Hosp_level),
         Specialty_1 = as.factor(Specialty_1),
         Specialty_2 = as.factor(Specialty_2),
         Est_DrugIncome_RMB = as.numeric(Est_DrugIncome_RMB),
         医生数 = as.numeric(医生数),
         床位数 = as.numeric(床位数),
         全科床位数 = as.numeric(全科床位数),
         内科床位数 = as.numeric(内科床位数),
         外科床位数 = as.numeric(外科床位数),
         眼科床位数 = as.numeric(眼科床位数),
         年诊疗人次 = as.numeric(年诊疗人次),
         门诊诊次 = as.numeric(门诊诊次),
         内科诊次 = as.numeric(内科诊次),
         外科诊次 = as.numeric(外科诊次),
         入院人数 = as.numeric(入院人数),
         住院病人手术人次数 = as.numeric(住院病人手术人次数),
         医疗收入 = as.numeric(医疗收入),
         门诊收入 = as.numeric(门诊收入),
         门诊治疗收入 = as.numeric(门诊治疗收入),
         门诊手术收入 = as.numeric(门诊手术收入),
         住院收入 = as.numeric(住院收入),
         住院床位收入 = as.numeric(住院床位收入),
         住院治疗收入 = as.numeric(住院治疗收入),
         住院手术收入 = as.numeric(住院手术收入),
         药品收入 = as.numeric(药品收入),
         门诊药品收入 = as.numeric(门诊药品收入),
         门诊西药收入 = as.numeric(门诊西药收入),
         住院药品收入 = as.numeric(住院药品收入),
         住院西药收入 = as.numeric(住院西药收入),
         district = as.factor(district),
         city = as.factor(city),
         gdp_value_100mil = as.numeric(gdp_value_100mil),
         gdp_gr = as.numeric(gdp_gr),
         permanent_residents_10thousand = as.numeric(permanent_residents_10thousand),
         urban_permanent_residents_10thousand = as.numeric(urban_permanent_residents_10thousand),
         rural_permanent_residents_10thousand = as.numeric(rural_permanent_residents_10thousand),
         urban_disposal_income = as.numeric(urban_disposal_income),
         urban_disposal_income_gr = as.numeric(urban_disposal_income_gr),
         rural_disposal_income = as.numeric(rural_disposal_income),
         rural_disposal_income_gr = as.numeric(rural_disposal_income_gr),
         social_consumption_100mil = as.numeric(social_consumption_100mil),
         retailotc_10thousand = as.numeric(retailotc_10thousand))

htn_biding_data_2017_js_m <- htn_biding_data_2017 %>%
  filter(province == "江苏省") %>%
  group_by(hospital_name, trade_name) %>%
  summarise(amount = sum(value, na.rm = TRUE)) %>%
  spread(trade_name, amount, fill = 0) %>%
  mutate(flag = 1)

universe_data_js_m <- bind_rows(htn_county_sample_data_js_m, testing_data) %>%
  left_join(htn_biding_data_2017_js_m, by = "hospital_name") %>%
  mutate(flag = ifelse(is.na(flag), 0, flag)) %>%
  select(-c(PHA, Specialty.3, value_2016, unit_2016, province, county_code_2015)) %>%
  mutate(hospital_name = as.factor(hospital_name),
         Hosp_level = as.factor(Hosp_level),
         Specialty_1 = as.factor(Specialty_1),
         Specialty_2 = as.factor(Specialty_2),
         Est_DrugIncome_RMB = as.numeric(Est_DrugIncome_RMB),
         医生数 = as.numeric(医生数),
         床位数 = as.numeric(床位数),
         全科床位数 = as.numeric(全科床位数),
         内科床位数 = as.numeric(内科床位数),
         外科床位数 = as.numeric(外科床位数),
         眼科床位数 = as.numeric(眼科床位数),
         年诊疗人次 = as.numeric(年诊疗人次),
         门诊诊次 = as.numeric(门诊诊次),
         内科诊次 = as.numeric(内科诊次),
         外科诊次 = as.numeric(外科诊次),
         入院人数 = as.numeric(入院人数),
         住院病人手术人次数 = as.numeric(住院病人手术人次数),
         医疗收入 = as.numeric(医疗收入),
         门诊收入 = as.numeric(门诊收入),
         门诊治疗收入 = as.numeric(门诊治疗收入),
         门诊手术收入 = as.numeric(门诊手术收入),
         住院收入 = as.numeric(住院收入),
         住院床位收入 = as.numeric(住院床位收入),
         住院治疗收入 = as.numeric(住院治疗收入),
         住院手术收入 = as.numeric(住院手术收入),
         药品收入 = as.numeric(药品收入),
         门诊药品收入 = as.numeric(门诊药品收入),
         门诊西药收入 = as.numeric(门诊西药收入),
         住院药品收入 = as.numeric(住院药品收入),
         住院西药收入 = as.numeric(住院西药收入),
         district = as.factor(district),
         city = as.factor(city),
         gdp_value_100mil = as.numeric(gdp_value_100mil),
         gdp_gr = as.numeric(gdp_gr),
         permanent_residents_10thousand = as.numeric(permanent_residents_10thousand),
         urban_permanent_residents_10thousand = as.numeric(urban_permanent_residents_10thousand),
         rural_permanent_residents_10thousand = as.numeric(rural_permanent_residents_10thousand),
         urban_disposal_income = as.numeric(urban_disposal_income),
         urban_disposal_income_gr = as.numeric(urban_disposal_income_gr),
         rural_disposal_income = as.numeric(rural_disposal_income),
         rural_disposal_income_gr = as.numeric(rural_disposal_income_gr),
         social_consumption_100mil = as.numeric(social_consumption_100mil),
         retailotc_10thousand = as.numeric(retailotc_10thousand))












