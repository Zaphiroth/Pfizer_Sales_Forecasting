# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Pfizer County Hospitals Potential Project
# Purpose:      Raw Data Process
# programmer:   Xin Huang
# Date:         09-05-2018
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
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
})


##-- readin the raw sample data
# county_sample_data_js <- read.xlsx("02_Inputs/pfizer_江苏_sample_hx.xlsx")
county_sample_data_js <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx")
table(county_sample_data_js$city)
# 常州市   淮安市 连云港市   南通市   苏州市   泰州市   无锡市   宿迁市   徐州市   盐城市   扬州市 
# 2       12        8      129       21       20       13       58       17       24        9 
# 镇江市 
# 10 

# candidates_of_county_hosp <- read.xlsx("02_Inputs/20180824最新MOH新增修改和补充_补充江苏.xlsx",
#                                        sheet = "MOH")
# add_in_nj_hosp <- candidates_of_county_hosp %>%
#   filter(新版名称 == "南京市妇幼保健院") %>%
#   rename(city = City,
#          hospital_name = 新版名称) %>%
#   select(-是否新增, -PHAHospname, -PHA.ID,
#          -重复 )
# 
# setdiff(colnames(county_sample_data_js_org),
#         colnames(add_in_nj_hosp))

# county_sample_data_js <- bind_rows(county_sample_data_js_org,
#                                    add_in_nj_hosp)
  # %>%
  # mutate(doc_cnt = as.numeric(`医生数`),
  #        bed_cnt = `床位数`) %>%
  # select(-c(`医生数`, `床位数`))

##-- read in biding data of 2016

biding_data_2017 <- read.xlsx("02_Inputs/js项目辉瑞41个品种-201809.xlsx")
biding_data_2016 <- fread("02_Inputs/biding_data_2016.csv", encoding="UTF-8")
biding_data_2016 <- setDF(biding_data_2016)

##-- according to the htn mkt definition as following to filter the data
# C02 ANTIHYPERTENSIVES
# C03 DIURETICS
# C07 BETA BLOCKING AGENTS
# C08 CALCIUM ANTAGONISTS
# C09 RENIN-ANGIOTEN SYST AGENT
# C11A LIPREG.CV.MULT-TH.COMBS

htn_biding_data_2017 <- biding_data_2017 %>%
  select(年, 省份, 城市, 区县,
        医院名称, 采购数量,
        采购金额, 医院区域类型)

colnames(htn_biding_data_2017) <- c("Year", "province", "city", "district",
                                    "hospital_name", "unit", "value",
                                    "Hospital_Categroy")

htn_biding_data_2017_js <- htn_biding_data_2017 %>%
  filter(province == "江苏省") %>%
  group_by(hospital_name) %>%
  summarise(value_2017 = sum(value, na.rm = T))


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

# htn_biding_data_2016_js_sample <- htn_biding_data_2016_js %>%
#   filter(hospital_name %in% unique(county_sample_data_js$新版名称_m))
# setdiff(unique(county_sample_data_js$新版名称_m),
#         unique(htn_biding_data_2016_js_sample$hospital_name))


htn_county_sample_data_js_m <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
             county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) 

# htn_county_sample_data_js_exclude <- htn_county_sample_data_js_m %>%
#   filter(!hospital_name %in% continuty_chk$hospital_name)

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
  # group_by(新版名称_m,  Hosp_level, Province, City, Prefecture,
  #               Specialty_1, Specialty_2, Est_DrugIncome_RMB, sale,
  #               doc_cnt, bed_cnt, Hospital_Code, Hospital_Categroy_I, 
  #               Hospital_Categroy_II, Hospital_Categroy_III, 
  #               Hospital_Categroy_IV) %>%
  # summarise(value = sum(value, na.rm = TRUE),
  #           unit = sum(unit, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(demograph_data_m, by = c("district" = "county")) %>%
  # select(-c(sale, district, Hospital_Code, Hospital_Categroy_I, 
  #               Hospital_Categroy_II, Hospital_Categroy_III, 
  #               Hospital_Categroy_IV)) %>%
  mutate(gdp_gr = as.numeric(gdp_gr),
         # 新版名称 = 新版名称_m,
         flag = 1)


htn_county_sample_data_js_m2 <- htn_county_sample_data_js_m1 %>%
  left_join(sample_jw_m, by = "hospital_name")


table(htn_county_sample_data_js_m1$city)
# 常州   淮安 连云港   南京   南通   苏州   泰州   无锡   宿迁   徐州   盐城   扬州   镇江 
# 2      4      4      1     19     14     13      6      6      8     14      6      9 


##-- clean the universe data
county_sample_data_js_tmp <- county_sample_data_js %>%
  mutate(PHA_tmp = ifelse(is.na(PHA), hospital_name, PHA))



# orgin <- read.xlsx("02_Inputs/Universe_County_Hospitals_of_JS.xlsx")
# new <- read.xlsx("02_Inputs/江苏-医院&县医院-HX new.xlsx")
# setdiff(colnames(new),
#         colnames(orgin))


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

# wb <- createWorkbook()
# addWorksheet(wb, "matched_sample")
# addWorksheet(wb, "universe_except_ms")
# 
# writeData(wb, "matched_sample", chk1)
# writeData(wb, "universe_except_ms", chk2)
# 
# saveWorkbook(wb, "03_Outputs/qc_sample_universe.xlsx", overwrite = TRUE)

sample_data_not_in_universe <- chk1

##-- filter out the sample data from universe
setdiff(colnames(htn_county_sample_data_js_m1), 
        colnames(county_hospitals_universe_js))

testing_data <- county_hospitals_universe_js %>%
  left_join(universe_jw_m, by = "hospital_name") %>%
  filter(flag == 0)


universe_data_js <- 
  bind_rows(htn_county_sample_data_js_m2,
            testing_data) %>%
  left_join(htn_biding_data_2017_js, by = "hospital_name") %>%
  mutate(flag = ifelse(is.na(value_2017),
                       0,
                       1)) 

universe_data_js_test <- universe_data_js %>%
  filter(flag == 0)

universe_data_js_train <- universe_data_js %>%
  filter(flag == 1) %>%
  filter(hospital_name%in%continuty_chk$hospital_name) %>%
  filter(!is.na(Est_DrugIncome_RMB))

universe_data_js <- bind_rows(universe_data_js_test,
                              universe_data_js_train)


  # %>%
  # select(-c(province, city))

# ## -- check 2016 sample data unsatisfy continuity in 
# chk_num_of_exclude_out_of_2017_biding_data <- htn_county_sample_data_js_exclude %>%
#   filter(!hospital_name %in% biding_data_2017$医院名称)
# 
# ## -- check 2016 sample data without est_drugincome_rmb in 
# chk_num_of_exclude_m_out_of_2017_biding_data <- htn_county_sample_data_js_exclude_m %>%
#   filter(!hospital_name %in% biding_data_2017$医院名称)
# 
# ## -- check training data 
# chk_num_of_train_in_2017_biding_data <- htn_county_sample_data_js_m %>%
#   filter(hospital_name %in% biding_data_2017$医院名称)
# 
# write.xlsx(bind_rows(chk_num_of_exclude_out_of_2017_biding_data,
#                      chk_num_of_exclude_m_out_of_2017_biding_data),
#            "03_Outputs/sample_out_of_2017_biding_data.xlsx")
# 
# 
# total_value_of_sample <- htn_county_sample_data_js_m %>%
#   filter(hospital_name!= "南京市妇幼保健院") %>%
#   group_by(hospital_name) %>%
#   arrange(-Est_DrugIncome_RMB) %>%
#   filter(row_number() == 1) %>%
#   ungroup() %>%
#   mutate(chk = ifelse(hospital_name %in% biding_data_2017$医院名称,
#                       "in",
#                       "out")) %>%
#   group_by(chk) %>%
#   summarise(value = sum(value, na.rm = T),
#             count = n()) %>%
#   mutate(total_value = sum(value, na.rm = T),
#          proportion = value/total_value)


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





  

  














