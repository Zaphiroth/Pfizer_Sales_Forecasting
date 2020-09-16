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
  library(plyr)
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

##-- data
train_data <- universe_data_js %>%
  filter(flag == 1) %>%
  select(-hospital_name, -flag) %>%
  data.frame()

test_data <- universe_data_js %>%
  filter(flag == 0) %>%
  select(-hospital_name, -flag) %>%
  data.frame()

##-- trial model
response <- colnames(train_data)[43:145]
cat(response, sep = ", ")

trial_model <- rfsrc(Multivar(阿尔马尔, 艾克朗, 爱络, 安博诺, 安博维, 安来, 安利博,
                              安内强, 安内真, 安泰达, 傲坦, 奥一心, 百普乐, 拜新同,
                              邦坦, 倍他乐克, 博苏, 博欣舒, 代文, 得高宁, 迪赛平,
                              伏络清, 复代文, 合贝爽, 合心爽, 吉加, 佳菲, 捷平, 金络,
                              康忻, 科苏, 科素亚, 可多华, 可苹, 兰迪, 兰普, 乐息平,
                              力斯得, 立文, 丽泉, 联环尔定, 联环笑定, 罗浩, 洛格乐,
                              洛汀新, 络德, 络活喜, 美卡素, 蒙诺, 那妥, 纳催离,
                              纳斯力妥, 纳欣同, 伲福达, 宁立平, 诺金平, 欧美宁, 佩尔,
                              平克亚欣, 平能, 平欣, 浦美特, 其他, 勤可息, 瑞素坦,
                              瑞泰, 沙泰齐, 沙汀宁, 圣畅, 施慧达, 枢衡, 舒尼亚, 双将平,
                              司乐平, 苏适, 穗悦, 特苏尼, 天禾恒, 恬尔新, 拓赛, 西络宁,
                              希尔达, 缬克, 欣然, 玄宁, 压氏达, 雅荣, 雅施达, 亚邦恒贝,
                              亚宁定, 伊达力, 伊粒平, 伊迈格, 依苏, 裕优定, 愈畅,
                              毓乐宁, 元治, 悦南珊, 悦宁定, 再畅, 再宁平, 泽通) ~ .,
                     data = train_data, ntree = 100, na.action = "na.impute", importance = TRUE)

trial_p <- predict(trial_model, test_data[, 1:42], na.action = "na.impute")

##-- evaluation
ntree <- rep(seq(50, 500, by = 50), times = 42)
mtry <- rep(1:42, each = 10)
d <- cbind(ntree, mtry)

eval.fun <- function(ntree, mtry) {
  model <- rfsrc(Multivar(阿尔马尔, 艾克朗, 爱络, 安博诺, 安博维, 安来, 安利博,
                              安内强, 安内真, 安泰达, 傲坦, 奥一心, 百普乐, 拜新同,
                              邦坦, 倍他乐克, 博苏, 博欣舒, 代文, 得高宁, 迪赛平,
                              伏络清, 复代文, 合贝爽, 合心爽, 吉加, 佳菲, 捷平, 金络,
                              康忻, 科苏, 科素亚, 可多华, 可苹, 兰迪, 兰普, 乐息平,
                              力斯得, 立文, 丽泉, 联环尔定, 联环笑定, 罗浩, 洛格乐,
                              洛汀新, 络德, 络活喜, 美卡素, 蒙诺, 那妥, 纳催离,
                              纳斯力妥, 纳欣同, 伲福达, 宁立平, 诺金平, 欧美宁, 佩尔,
                              平克亚欣, 平能, 平欣, 浦美特, 其他, 勤可息, 瑞素坦,
                              瑞泰, 沙泰齐, 沙汀宁, 圣畅, 施慧达, 枢衡, 舒尼亚, 双将平,
                              司乐平, 苏适, 穗悦, 特苏尼, 天禾恒, 恬尔新, 拓赛, 西络宁,
                              希尔达, 缬克, 欣然, 玄宁, 压氏达, 雅荣, 雅施达, 亚邦恒贝,
                              亚宁定, 伊达力, 伊粒平, 伊迈格, 依苏, 裕优定, 愈畅,
                              毓乐宁, 元治, 悦南珊, 悦宁定, 再畅, 再宁平, 泽通) ~ .,
                 data = train_data, ntree = ntree, mtry = mtry, na.action = "na.impute")
  error <- get.mv.error(model)
  temp <- data.frame(mse = mean(error))
}

model.eval <- data.frame()
system.time(model.eval <- mdply(d, eval.fun))
# best_hyperpara : ntree = 400, mtry = 14

##-- model
rfsrc.model <- rfsrc(Multivar(阿尔马尔, 艾克朗, 爱络, 安博诺, 安博维, 安来, 安利博,
                                  安内强, 安内真, 安泰达, 傲坦, 奥一心, 百普乐, 拜新同,
                                  邦坦, 倍他乐克, 博苏, 博欣舒, 代文, 得高宁, 迪赛平,
                                  伏络清, 复代文, 合贝爽, 合心爽, 吉加, 佳菲, 捷平, 金络,
                                  康忻, 科苏, 科素亚, 可多华, 可苹, 兰迪, 兰普, 乐息平,
                                  力斯得, 立文, 丽泉, 联环尔定, 联环笑定, 罗浩, 洛格乐,
                                  洛汀新, 络德, 络活喜, 美卡素, 蒙诺, 那妥, 纳催离,
                                  纳斯力妥, 纳欣同, 伲福达, 宁立平, 诺金平, 欧美宁, 佩尔,
                                  平克亚欣, 平能, 平欣, 浦美特, 其他, 勤可息, 瑞素坦,
                                  瑞泰, 沙泰齐, 沙汀宁, 圣畅, 施慧达, 枢衡, 舒尼亚, 双将平,
                                  司乐平, 苏适, 穗悦, 特苏尼, 天禾恒, 恬尔新, 拓赛, 西络宁,
                                  希尔达, 缬克, 欣然, 玄宁, 压氏达, 雅荣, 雅施达, 亚邦恒贝,
                                  亚宁定, 伊达力, 伊粒平, 伊迈格, 依苏, 裕优定, 愈畅,
                                  毓乐宁, 元治, 悦南珊, 悦宁定, 再畅, 再宁平, 泽通) ~ .,
                     data = train_data, ntree = 400, mtry = 14, na.action = "na.impute")

##-- prediction
pred <- predict(rfsrc.model, test_data[, 1:42], na.action = "na.impute")

pred.value <- get.mv.predicted(pred) %>%
  data.frame()

##-- result
result <- universe_data_js %>%
  filter(flag == 0) %>%
  .[, 1:43] %>%
  bind_cols(pred.value) %>%
  gather("trade_name", "amount.sqrt", 阿尔马尔, 艾克朗, 爱络, 安博诺, 安博维, 安来, 安利博,
         安内强, 安内真, 安泰达, 傲坦, 奥一心, 百普乐, 拜新同,
         邦坦, 倍他乐克, 博苏, 博欣舒, 代文, 得高宁, 迪赛平,
         伏络清, 复代文, 合贝爽, 合心爽, 吉加, 佳菲, 捷平, 金络,
         康忻, 科苏, 科素亚, 可多华, 可苹, 兰迪, 兰普, 乐息平,
         力斯得, 立文, 丽泉, 联环尔定, 联环笑定, 罗浩, 洛格乐,
         洛汀新, 络德, 络活喜, 美卡素, 蒙诺, 那妥, 纳催离,
         纳斯力妥, 纳欣同, 伲福达, 宁立平, 诺金平, 欧美宁, 佩尔,
         平克亚欣, 平能, 平欣, 浦美特, 其他, 勤可息, 瑞素坦,
         瑞泰, 沙泰齐, 沙汀宁, 圣畅, 施慧达, 枢衡, 舒尼亚, 双将平,
         司乐平, 苏适, 穗悦, 特苏尼, 天禾恒, 恬尔新, 拓赛, 西络宁,
         希尔达, 缬克, 欣然, 玄宁, 压氏达, 雅荣, 雅施达, 亚邦恒贝,
         亚宁定, 伊达力, 伊粒平, 伊迈格, 依苏, 裕优定, 愈畅,
         毓乐宁, 元治, 悦南珊, 悦宁定, 再畅, 再宁平, 泽通) %>%
  mutate(amount = round(amount.sqrt ^ 2, 2)) %>%
  select(-amount.sqrt)

train.result <- universe_data_js_m %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  gather("trade_name", "amount", 阿尔马尔, 艾克朗, 爱络, 安博诺, 安博维, 安来, 安利博,
         安内强, 安内真, 安泰达, 傲坦, 奥一心, 百普乐, 拜新同,
         邦坦, 倍他乐克, 博苏, 博欣舒, 代文, 得高宁, 迪赛平,
         伏络清, 复代文, 合贝爽, 合心爽, 吉加, 佳菲, 捷平, 金络,
         康忻, 科苏, 科素亚, 可多华, 可苹, 兰迪, 兰普, 乐息平,
         力斯得, 立文, 丽泉, 联环尔定, 联环笑定, 罗浩, 洛格乐,
         洛汀新, 络德, 络活喜, 美卡素, 蒙诺, 那妥, 纳催离,
         纳斯力妥, 纳欣同, 伲福达, 宁立平, 诺金平, 欧美宁, 佩尔,
         平克亚欣, 平能, 平欣, 浦美特, 其他, 勤可息, 瑞素坦,
         瑞泰, 沙泰齐, 沙汀宁, 圣畅, 施慧达, 枢衡, 舒尼亚, 双将平,
         司乐平, 苏适, 穗悦, 特苏尼, 天禾恒, 恬尔新, 拓赛, 西络宁,
         希尔达, 缬克, 欣然, 玄宁, 压氏达, 雅荣, 雅施达, 亚邦恒贝,
         亚宁定, 伊达力, 伊粒平, 伊迈格, 依苏, 裕优定, 愈畅,
         毓乐宁, 元治, 悦南珊, 悦宁定, 再畅, 再宁平, 泽通)

chk <- result %>%
  group_by(trade_name) %>%
  summarise(amount = sum(amount))

chk1 <- bind_rows(result, train.result) %>%
  group_by(trade_name) %>%
  summarise(amount = sum(amount)) %>%
  arrange(desc(amount))

wb <- createWorkbook()
addWorksheet(wb, "train_data")
addWorksheet(wb, "test_data")
addWorksheet(wb, "amount_by_trade_name")
writeData(wb, "train_data", train.result)
writeData(wb, "test_data", result)
writeData(wb, "amount_by_trade_name", chk1)
saveWorkbook(wb, "03_Outputs/Sales_Forecasting_1128.xlsx", overwrite = TRUE)



