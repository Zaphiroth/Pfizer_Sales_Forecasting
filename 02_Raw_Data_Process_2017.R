# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bayer CountyPotential
# Purpose:      Raw Data Process
# programmer:   Xin Huang
# Date:         26-11-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# loading the required packages
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
  library(tidyr)
})


##-- readin the raw sample data
county_sample_data_js <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx")
table(county_sample_data_js$city)
# 常州市   淮安市 连云港市   南通市   苏州市   泰州市   无锡市   宿迁市   徐州市   盐城市   扬州市 
# 2       12        8      129       21       20       13       58       17       24        9 
# 镇江市 
# 10 



##-- read in biding data of 2016

biding_data_2017 <- read.xlsx("02_Inputs/js项目辉瑞41个品种-201809.xlsx")
biding_data_2016 <- fread("02_Inputs/biding_data_2016.csv", encoding="UTF-8")
biding_data_2016 <- setDF(biding_data_2016)


htn_biding_data_2017 <- biding_data_2017 %>%
  mutate(brandName = ifelse(is.na(商品名),
                            "others",
                            商品名)) %>%
  select(年, 省份, 城市, 区县,
          医院名称, brandName,
          采购数量, 采购金额, 
          医院区域类型) 

nrow(htn_biding_data_2017)

colnames(htn_biding_data_2017) <- c("Year", "province", "city", "district",
                                    "hospital_name", "brandName", "unit", "value",
                                    "Hospital_Categroy")

htn_biding_data_2017_js <- htn_biding_data_2017 %>%
  filter(province == "江苏省") %>%
  group_by(hospital_name,
           brandName) %>%
  summarise(value_2017 = sum(value, na.rm = T))

nrow(htn_biding_data_2017_js)


htn_biding_data_2016 <- biding_data_2016 %>%
  filter(ATCII %in% c("β-受体阻滞剂", "钙通道阻滞剂", 
                      "抗高血压药","利尿剂", "作用于肾素-血管紧张素系统的药物") 
         # |
         #   ATCIV == "HMG-CoA还原酶抑制剂和其他调节血脂药的复方"
         ) #葆至能

##-- sample continuty check
continuty_chk <- htn_biding_data_2016 %>%
  filter(hospital_name %in% unique(county_sample_data_js$hospital_name)) %>%
  group_by(hospital_name, month) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(hospital_name, month) %>%
  group_by(hospital_name) %>%
  summarise(month_cnt = n()) %>%
  filter(month_cnt >= 8)


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


##-- do some QC
value_chk <- htn_biding_data_2016_js %>%
  filter(Hospital_Categroy_IV == "县级等级") 
sum(value_chk$value_2016)

htn_county_sample_data_js_m <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
             county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) 


htn_county_sample_data_js_m <- htn_county_sample_data_js_m %>%
  # filter(hospital_name %in% continuty_chk$hospital_name) %>%
  mutate(医生数 = as.numeric(医生数)) %>%
  select(-c(Province, City, Hospital_Code, Hospital_Categroy_I, province, city,
            Hospital_Categroy_II,Hospital_Categroy_III, Hospital_Categroy_IV,
            `Re-Speialty`)) %>%
  group_by(hospital_name) %>%
  arrange(-Est_DrugIncome_RMB) %>%
  filter(row_number() == 1) %>%
  ungroup()


# htn_county_sample_data_js_exclude_m <- htn_county_sample_data_js_m %>%
#   filter(is.na(Est_DrugIncome_RMB))

# htn_county_sample_data_js_m <- htn_county_sample_data_js_m %>%
#   filter(!is.na(Est_DrugIncome_RMB))

##-- add more information for the counties
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

demograph_data_m <- demograph_data %>%
  select(-c(permanent_residents_birth_rate,
            new_born, sample_vaccine_data))

demograph_data_js <- demograph_data %>% 
  filter(province == "江苏省") %>%
  select(-c(province, city,
            permanent_residents_birth_rate,
            new_born, sample_vaccine_data))


##-- generate the model data based on the sample county hospitals in JS
htn_county_sample_data_js_m1 <- htn_county_sample_data_js_m %>%
  ungroup() %>% 
  left_join(demograph_data_m, by = c("district" = "county")) %>%
  mutate(gdp_gr = as.numeric(gdp_gr),
         flag = 1)


# htn_county_sample_data_js_m2 <- htn_county_sample_data_js_m1 %>%
#   left_join(sample_jw_m, by = "hospital_name")


table(htn_county_sample_data_js_m1$city)
# 常州   淮安 连云港   南京   南通   苏州   泰州   无锡   宿迁   徐州   盐城   扬州   镇江 
# 2      4      4      1     19     14     13      6      6      8     14      6      9 


##-- clean the universe data
county_sample_data_js_tmp <- county_sample_data_js %>%
  mutate(PHA_tmp = ifelse(is.na(PHA), hospital_name, PHA))

county_hospitals_universe_js <-
  read.xlsx("02_Inputs/江苏-医院&县医院-HX0930.xlsx", sheet = "Sheet4") %>%
  left_join(demograph_data_m, by = c("Prefecture" = "county")) %>%
  mutate(flag = ifelse(`新版名称` %in% unique(county_sample_data_js_tmp$PHA_tmp), 
         1, 0),
         医生数 = as.numeric(医生数),
         gdp_gr = as.numeric(gdp_gr)) %>%
  select(-c(Type, Province, City, county.HP, City.Tier.2010, City.Tier.2018,
            Region, PHA.ID, PHAHospname, 重复)) %>%
  rename(hospital_name = 新版名称,
         district = Prefecture)

table(county_hospitals_universe_js$flag)

# do some check
chk <- county_hospitals_universe_js %>%
  filter(is.na(rural_disposal_income)) %>%
  select(district) %>%
  distinct()

chk1 <- county_sample_data_js_tmp %>%
  filter(!(PHA_tmp %in% county_hospitals_universe_js$hospital_name))

chk2 <- county_hospitals_universe_js %>%
  filter(flag == 0)

##-- filter out the sample data from universe
setdiff(colnames(htn_county_sample_data_js_m1), 
        colnames(county_hospitals_universe_js))

testing_data <- county_hospitals_universe_js %>%
  # left_join(universe_jw_m, by = "hospital_name") %>%
  filter(flag == 0)


universe_data_js <- 
  bind_rows(htn_county_sample_data_js_m1,
            testing_data) %>%
  select(-unit_2016, -value_2016) %>%
  left_join(htn_biding_data_2017_js, by = "hospital_name") %>%
  mutate(flag = ifelse(is.na(value_2017),
                       0,
                       1)) 
nrow(universe_data_js) #3803

universe_data_js_test <- universe_data_js %>%
  filter(flag == 0) %>%
  select(-brandName)

universe_data_js_train <- universe_data_js %>%
  filter(flag == 1) %>%
  # filter(hospital_name%in%continuty_chk$hospital_name) %>%
  spread(brandName, value_2017, fill = 0) %>%
  filter(!is.na(Est_DrugIncome_RMB)) 


universe_data_js_model <- bind_rows(universe_data_js_test,
                                    universe_data_js_train)

nrow(universe_data_js_model) # 2627

length(unique(universe_data_js_model$brandName))
sum(is.na(universe_data_js_model$brandName))


  


## -- check ytd growth 
sample_data_2016_m <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
            county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) %>%
  group_by(hospital_name) %>%
  arrange(-Est_DrugIncome_RMB) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(hospital_name,
         province,
         city,
         district,
         value_2016) 

comparison_year_growth <- sample_data_2016_m %>%
  filter(hospital_name!= "南京市妇幼保健院") %>%
  left_join(htn_biding_data_2017_js, by = c("hospital_name")) %>%
  filter(!is.na(value_2017)) 

comparison_year_growth_by_hosp <- comparison_year_growth

comparison_year_growth_m <- comparison_year_growth %>%
  group_by(district) %>%
  summarise(value_2016 = sum(value_2016, na.rm = T),
            value_2017 = sum(value_2017, na.rm = T)) %>%
  mutate(increase = value_2017/value_2016)

write.xlsx(comparison_year_growth_m, 
           "03_Outputs/year_growth_of_sample_data_by_district.xlsx")

comparison_year_growth_m1 <- comparison_year_growth %>%
  mutate(increase = value_2017/value_2016)

write.xlsx(comparison_year_growth_m1, 
           "03_Outputs/year_growth_of_sample_data_by_hosp.xlsx")



## -- matching testing data with sample data 
write.xlsx(universe_data_js,
           "03_Outputs/testing_data_and_sample_data_chked_by_est_drupincome.xlsx")





  

  














