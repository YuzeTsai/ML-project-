# Data manipulation 

library(ggplot2)
library(tidyverse)
library(data.table)
library(readr)
install.packages("arrow")
library(arrow)
install.packages("earth", type = "binary")
install.packages('Formula')
install.packages('plotmo')


library(earth)

gc()

df_elec <- fread("~/Desktop/untitled folder/歷年各縣市再生能源裝置容量(101-11401).csv")
rm(df, data1)
df <- fread("~/Desktop/untitled folder/q15019-2140091076.csv")
df2 <- fread("~/Desktop/Result.csv")


df_elec[, year:= 年別+1911]
df_elec[, 1] = NULL
df[, year:= as.numeric(gsub("[^0-9]", "", 統計期)) + 1911]
df[, 1]= NULL
df[, total_income := as.numeric(家庭戶數)*as.numeric(所得收入)]
df_elec <- df_elec[-(287:308), ]
df_elec[, 縣市 := gsub("台北市", "臺北市", 縣市)]
df_elec[, 縣市 := gsub("台中市", "臺中市", 縣市)]
df_elec[, 縣市 := gsub("台南市", "臺南市", 縣市)]
df_elec[, 縣市 := gsub("台東縣", "臺東縣", 縣市)]

result <- merge(df_elec, df, by.x = c("year", "縣市"), by.y = c("year", "地區別"), all = TRUE)
colnames(df2) <- c('year','縣市', '垃圾焚化量(公噸)', 
                   '垃圾衛生掩埋量(公噸)',
                   '執行機關資源回收量(公噸)',
                   '一般廢棄物產生量(公噸)',
                   '一般廢棄物回收率(%)',
                   '地方環保單位歲出決算數(千元)') 
df2 <- df2[-(1), ]
df_final <- merge(result, df2, by = c("year", "縣市"), all = T)
rm(df, df_elec, df2)
gc()
df_final[, t_capacity_of_renewable_e := rowSums(.SD, na.rm = TRUE), .SDcols = c("風力", "太陽光電", "其他(含水力)")]

results[, c(9,10,11,12,13,14)] <- results[, as.numeric(c(9,10,11,12,13,14))]


people <- fread("~/Desktop/people.csv")
elec <- fread("~/Desktop/untitled folder/elec.csv")
df_transit <- merge(people, elec, by = c("year", "縣市"), all = T)
df_transit <- df_transit[, 1:4]
rm(people, elec)
df_final <- merge(df_final, df_transit, by = c("year", "縣市"), all = T)
df_final <- df_final[-(1:44), ]
df_final <- df_final[-(241:262), ]
write_csv(df_final, "~/Desktop/untitled folder/output.csv")


results <- fread("~/Desktop/untitled folder/output.csv")
results$`一般廢棄物回收率(%)` <- as.numeric(gsub(",", ".", results$`一般廢棄物回收率(%)`))
results$`戶籍登記人口數(人)` <- as.numeric(gsub("\\.", "", results$`戶籍登記人口數(人)`)) # get rid of all points
results$`地方環保單位歲出決算數(千元)` <- as.numeric(gsub("\\.", "", results$`地方環保單位歲出決算數(千元)`))
results$`一般廢棄物產生量(公噸)` <- as.numeric(gsub("\\.", "", results$`一般廢棄物產生量(公噸)`))
results$`執行機關資源回收量(公噸)` <- as.numeric(gsub("\\.", "", results$`執行機關資源回收量(公噸)`))
results$`垃圾焚化量(公噸)` <- as.numeric(gsub("\\.", "", results$`垃圾焚化量(公噸)`))
results[, 9:15] <- lapply(results[, 9:15], as.numeric)
results$`售電統計（度）`
str(results)

results[, per_capita_electricity_usage(du) := `售電統計（度）`/`戶籍登記人口數(人)`]

see <- results %>% select(-c("year", "縣市", "戶籍登記人口數(人)", "售電統計（度）", "家庭戶數", "所得收入", "風力", "太陽光電", "其他(含水力)"))


sum <- summary(see)
sum


output <- fread("~/Desktop/untitled folder/output.csv")
candf <- fread("~/Desktop/untitled folder/cost and founds.csv")
str(output)
str(candf)
candf[, 5:29]=NULL
# 把第2列當作新的欄位名稱
new_names <- as.character(candf[2])
setnames(candf, new_names)

# 移除前兩列（原本的標題和現在已成欄名的列）
candf <- candf[-c(1,2)]
candf$year <- as.integer(candf$year)
setnames(candf, old = names(candf)[1], new = "縣市")
view(candf)
# 統一型別
output[, year := as.character(year)]
candf[, year := as.character(year)]
candf <- candf[-c(221:999),]


# 合併
final <- merge(output, candf, by = c("year", "縣市"), all = TRUE)
view(final)
final<- final[!(縣市 %in% c("總計", "臺灣地區"))]
final <- final[,-c(3,4,5,6,8)]
final <- final[, -c(9)]
str(final)
final <-final[, number_of_people := as.integer(gsub("\\.", "", `戶籍登記人口數(人)`))]
final <- final[, -c(10)]
final$`一般廢棄物回收率(%)` <- as.numeric(gsub(",", ".", final$`一般廢棄物回收率(%)`))
final$`一般廢棄物產生量(公噸)` <- as.numeric(gsub("\\.", "", final$`一般廢棄物產生量(公噸)`))
final$`垃圾衛生掩埋量(公噸)` <- as.numeric(gsub("\\.", "", final$`垃圾衛生掩埋量(公噸)`))
final$`執行機關資源回收量(公噸)` <- as.numeric(gsub("\\.", "", final$`執行機關資源回收量(公噸)`))
final$`垃圾焚化量(公噸)` <- as.numeric(gsub("\\.", "", final$`垃圾焚化量(公噸)`))
final <- final[, per_capita_electricity_usage := `售電統計（度）`/number_of_people ]
final <- final[, per_capita_incinerated_waste_kg := `垃圾焚化量(公噸)`/number_of_people*1000]
final <- final[, per_capita_landfilled_waste_kg := `垃圾衛生掩埋量(公噸)`/number_of_people*1000]
final <- final[, per_capita_resource_recovery_by_local_authorities_kg:= `執行機關資源回收量(公噸)`/number_of_people*1000]
final <- final[, per_capita_generation_of_general_waste_kg := `一般廢棄物產生量(公噸)`/number_of_people*1000]
final <- final[,-c(4,5,6,7)]
final <- final[,-c(6)]
final <- final[, per_capita_t_capacity_of_renewable_energy := t_capacity_of_renewable_e/number_of_people]
final <- final[,-c(5)]
final$`環保罰鍰佔比(%)` <- as.numeric(gsub("\\.", "", final$`環保罰鍰佔比(%)`))
final$`環保罰鍰佔比(%)` <- final$`環保罰鍰佔比(%)`/1000
final$`歲出政事別結構比-社區發展及環境保護支出(％)` <- as.numeric(final$`歲出政事別結構比-社區發展及環境保護支出(％)`)

vancancy <- fread("~/Desktop/untitled folder/vancancy_rate - 工作表1.csv")
str(vancancy)

final$year <- as.integer(final$year)
final <- merge(final, vancancy, by=c("year","縣市"), all =T)
write_csv(final, "~/Desktop/untitled folder/final.csv")

final <- fread("~/Desktop/untitled folder/final.csv")
food <- fread("~/Desktop/untitled folder/food_w.csv")
view(food)
food <- food[, -c(4,5,6,7)]
str(food)
view(final)
str(final)
final <- merge(final, food, by = c("year", "city/county"), all = TRUE)
final <- final[, per_capita_food_waste_kg := per_capita_food_waste*1000/number_of_people]
final <- final[, -15]
setnames(final, old = names(final)[2], new = "city/county")
setnames(final, old = names(final)[3], new = "Household_income")
setnames(final, old = names(final)[4], new = "General_waste_recycling_rate")
setnames(final, old = names(final)[5], new = "Budget_structure_ratio_for_environmental")
setnames(final, old = names(final)[6], new = "Proportion_of_environmental_fines")
