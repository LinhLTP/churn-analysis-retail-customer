######################################################################
# Churn analysis - retail customer 


library(tidyverse)
library(readxl)
library(knitr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)

Data <- read.csv("data1.csv", header = TRUE)
df1 <- Data

unique(df1$CUST_THANHPHO)

#################################
# A - Data Processing #
#################################

## Subset Ho Chi Minh City 
df2 <- subset(df1, CUST_THANHPHO == "HO CHI MINH")

## Detect PROID without starting with G 
df3 <- df2 %>% filter(!str_detect(df2$PRODID, "^G")) # Xoa nhung PROID bat dau chu G de biet chu k phai G la chu gi 

## Xoa PROID khong bat dau bang chu G 
df4 <- df2 %>% filter(!str_detect(df2$PRODID, "^P"))

## Lam sach du lieu
df4 %>% filter(!is.na(TOTALAMOUNT))
df4 %>% filter(!TOTALAMOUNT <= 0)

df4[!(is.na(df4$CHANNEL_NAME) | df4$CHANNEL_NAME==""), ] #remove the NAs and blanks
df4[!(is.na(df4$CARDTYPE) | df4$CARDTYPE ==""), ]
df4[!(is.na(df4$GENDER) & df4$GENDER ==""), ]
df4[!(is.na(df4$AGE) | df4$AGE ==""), ]

#################################
# B - RFM ANALYSIS #
#################################
dsRFM <- df4 %>% group_by(CUSTCODE) %>% summarise(recency = as.numeric(Sys.Date() - max(as.Date(NGAYHD))), frequency = n(), monetary = sum(as.numeric(TOTALAMOUNT)))

# (boxplot(dsRFM$recency))
# (boxplot(dsRFM$frequency))
# (boxplot(dsRFM$monetary))
# 
hist(log(dsRFM$monetary))
hist(dsRFM$monetary)
hist(dsRFM$recency, breaks = 120)


##------------------------------------------------------------------------------
# Xem set phan bo cua recency qua MIN - MAX - QUANTILE - MODE - HISTOGRAM 
dsRFM2 <- dsRFM
dsRFM2$recency <- dsRFM2$recency/31 #Recency tinh theo thang 

ggplot(dsRFM2) +
    geom_histogram(aes(x = dsRFM2$recency),
                   binwidth = 0.9, fill = "light blue", color = "grey",breaks=seq(2, 60, by=1)) + geom_vline(aes(xintercept = mean(dsRFM2$recency)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(dsRFM2$recency)),col='Blue',size=0.9)

quantile(dsRFM2$recency)
mean(dsRFM2$recency)
median(dsRFM2$recency)
# Mode 
names(table(dsRFM2$recency))[table(dsRFM2$recency)==max(table(dsRFM2$recency))]
table(dsRFM2$recency)
##------------------------------------------------------------------------------


# hist(log10(dsRFM$recency))
# hist(dsRFM$recency)
# hist(log10(dsRFM$frequency))

write_xlsx(x = dsRFM, path = "dsRFM.xlsx", col_names = TRUE) # Xuat ra file excel 

#################################
# C - K-MEANS CLUSTER #
#################################

## Data processing for Cluster 
require(knitr)
kable(head(dsRFM))
summary(dsRFM)

dsRFM <- na.omit(dsRFM)
summary(dsRFM)

# write_xlsx(x = dsRFM, path = "dsRFM-TKMT.xlsx", col_names = TRUE) # Xuat ra file excel 
# write_xlsx(x = l, path = "L.xlsx", col_names = TRUE) # Xuat ra file excel 

#Log tramsformation 
hist(log10(dsRFM$monetary))
dsData <- dsRFM
dsData$monetary <- log10(dsData$monetary)

#Normalize
row.names(dsData) <- dsRFM$CUSTCODE
dsData <- scale(dsData[,2:4])
summary(dsData)


# STEP 2: PHAN CUM VOI K = 4
set.seed(123)
km.res <- kmeans(dsData, 4, nstart = 25)
km.res

# STEP 3: Tính toán trung  bình các cụm (hay chính là các centroid): 
a <- as.data.frame(km.res$centers)

write_xlsx(x = a, path = "a.xlsx", col_names = TRUE)

# STEP 4: SO QUAN SAT O MOI CUM

### INSIGHT
length(km.res$cluster) #Coi so luong observation 

nrow(dsRFM)

dsRFM$clusterk <- as.factor(km.res$cluster)

kable(head(dsRFM))

### Merge Cluster & Transaction
df5 <- inner_join(df4, dsRFM, by = 'CUSTCODE')

tt <- df5 %>% filter(is.na(GENDER))

#################################
# D - CHURN ANALYSIS #
#################################

###########################################################
### I. TOÀN BỘ TẬP KHÁCH HÀNG HCM - T mean T max 
##########################################################

NHOM_txns <- df5 %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)

NHOM_time_between <- NHOM_txns %>% 
  arrange(CUSTCODE, NGAYHD) %>% 
  group_by(CUSTCODE) %>% 
  mutate(dt = as.numeric(as.Date(NGAYHD) - lag(as.Date(NGAYHD)), unit= 'days')) %>% 
  ungroup() %>% 
  na.omit()

NHOM_time_between_max <- NHOM_time_between %>% 
  group_by(CUSTCODE) %>% 
  summarise(lagMax = max(dt)) %>%
  ungroup() %>% 
  filter(lagMax>0)

##------------------------------------------------------------------------------
# Xem set phan bo cua recency qua MIN - MAX - QUANTILE - MODE - HISTOGRAM 
NHOM_time_between_Test <- NHOM_time_between
NHOM_time_between_Test$dt <- NHOM_time_between_Test$dt/31 #Recency tinh theo thang 

ggplot(NHOM_time_between_Test) +
    geom_histogram(aes(x = NHOM_time_between_Test$dt),
                   binwidth = 0.9, fill = "lightpink1", color = "grey",breaks=seq(2, 60, by=1)) + geom_vline(aes(xintercept = mean(NHOM_time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(NHOM_time_between_Test$dt)),col='Blue',size=0.9)

quantile(NHOM_time_between_Test$dt)
mean(NHOM_time_between_Test$dt)
median(NHOM_time_between_Test$dt)
# Mode 
NHOM_time_between_Test <- NHOM_time_between
names(table(NHOM_time_between_Test$dt))[table(NHOM_time_between_Test$dt)==max(table(NHOM_time_between_Test$dt))]
table(NHOM_time_between_Test$dt)
##------------------------------------------------------------------------------


###############################################################
### II. TOÀN BỘ TẬP KHÁCH HÀNG LOSING TREASURE - T mean T max 
###############################################################

# Tách nhóm Losing Treasure - Clusterk 2 
df_Losing <- df5 %>% subset(clusterk == '2')

Loss_txns <- df_Losing %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)

Loss_time_between <- Loss_txns %>% 
  arrange(CUSTCODE, NGAYHD) %>% 
  group_by(CUSTCODE) %>% 
  mutate(dt = as.numeric(as.Date(NGAYHD) - lag(as.Date(NGAYHD)), unit= 'days')) %>% 
  ungroup() %>% 
  na.omit()


#################################################
### III. TACH NHOM LOSING TREASURE - Clusterk 2 
#################################################
df_Losing <- df5 %>% subset(clusterk == '2')

##############################################################################
### NHOM I - Frequency > 1, Ngay HD > 1
#############################################################################

##################################
### CẤP 1 - SO SANH VOI CHINH NO 
##################################

#### Dieu kien 1: Frequency > 1 #####
FreqOther <- subset(df_Losing, frequency > "1") #592098
#### Dieu kien 2: Mua nhieu ngay #####
# Tong chi tieu trong ngay 
txns3 <- FreqOther %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)
# Lay nhung KH mua nhieu ngay
LINH <- txns3 %>% group_by(CUSTCODE,NGAYHD) %>% summarise(N = n()) 
LINH3 <- LINH %>% group_by(CUSTCODE) %>% summarise(N = n()) %>% filter(!(N=="1")) # Remove khach hang mua nhieu lan (tach hoa don) trong 1 ngay
LINH5 <- merge(LINH3, txns3, by = "CUSTCODE")
a <- LINH5[!duplicated(LINH5$CUSTCODE), ] #80,496 khach hang 

# Khoang cach giua cac lan mua 
time_between <- LINH5 %>% 
  arrange(CUSTCODE, NGAYHD) %>% 
  group_by(CUSTCODE) %>% 
  mutate(dt = as.numeric(as.Date(NGAYHD) - lag(as.Date(NGAYHD)), unit= 'days')) %>% 
  ungroup() %>% 
  na.omit()

# Khoang cach MAX & MEAN giua cac lan mua 
time_between_max <- time_between %>% 
  group_by(CUSTCODE) %>% 
  summarise(lagMax = max(dt)) %>%
  ungroup() %>% 
  filter(lagMax>0)
time_between_mean <- time_between %>% 
  group_by(CUSTCODE) %>% 
  summarise(lagMean = mean(dt)) %>%
  ungroup() %>% 
  filter(lagMean>0)

##------------------------------------------------------------------------------
# Xem set phan bo cua recency qua MIN - MAX - QUANTILE - MODE - HISTOGRAM 
time_between_Test <- time_between
time_between_Test$dt <- time_between_Test$dt/31 #Recency tinh theo thang 

ggplot(time_between_Test) +
    geom_histogram(aes(x = time_between_Test$dt),
                   binwidth = 0.9, fill = "gold1", color = "grey",breaks=seq(2, 70, by=1)) + geom_vline(aes(xintercept = mean(time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(time_between_Test$dt)),col='Blue',size=0.9)


max(time_between_Test$dt)
quantile(time_between_Test$dt)
mean(time_between_Test$dt)
median(time_between_Test$dt)
# Mode 
time_between_Test <- time_between
time_between_Test2 <- time_between #Recency tinh theo ngay 
names(table(time_between_Test2$dt))[table(time_between_Test2$dt)==max(table(time_between_Test2$dt))]
table(time_between_Test2$dt)
##------------------------------------------------------------------------------


# Gan nhan Churn hoac Non-churn 
df_Losing_Nhom21 <- inner_join(df_Losing, time_between_max, by = 'CUSTCODE')
df_Losing_Nhom22 <- inner_join(df_Losing_Nhom21, time_between_mean, by = 'CUSTCODE')
Check <- df_Losing_Nhom22[!duplicated(df_Losing_Nhom22$CUSTCODE), ] #80,496 khach hang 

df_Losing_Nhom22$CHURN <- ifelse(df_Losing_Nhom22$recency < df_Losing_Nhom22$lagMean, "Not Churn", ifelse(df_Losing_Nhom22$recency >= df_Losing_Nhom22$lagMean & df_Losing_Nhom22$recency < df_Losing_Nhom22$lagMax, "Churn-L1", ifelse(df_Losing_Nhom22$recency >= df_Losing_Nhom22$lagMax,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
df_Losing_Nhom22_Customer <- df_Losing_Nhom22[!duplicated(df_Losing_Nhom22$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(df_Losing_Nhom22_Customer$CHURN, sort.group = "decreasing", cum.percent = TRUE) #Thong ke so luong Churn va Non-churn theo KHACH HANG, khong phai theo GIAO DICH 


###########################################################
### CẤP 2 - SO SANH VOI TOAN BO TAP KHACH HANG HCM
##########################################################
NHOM_mean <- mean(NHOM_time_between$dt)
NHOM_max <- max(NHOM_time_between$dt)
NHOM_median <- median(NHOM_time_between$dt)

df_Losing_Nhom22$CHURN_2 <- ifelse(df_Losing_Nhom22$recency < NHOM_mean, "Not Churn", ifelse(df_Losing_Nhom22$recency >= NHOM_mean & df_Losing_Nhom22$recency < NHOM_max, "Churn-L1", ifelse(df_Losing_Nhom22$recency >= NHOM_max,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
df_Losing_Nhom22_Customer <- df_Losing_Nhom22[!duplicated(df_Losing_Nhom22$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(df_Losing_Nhom22_Customer$CHURN_2, sort.group = "decreasing", cum.percent = TRUE) #Thong ke so luong Churn va Non-churn theo KHACH HANG, khong phai theo GIAO DICH 


#############################################################################
### NHOM II - Frequency >=1, Ngay HD = 1
#############################################################################
FreqOther_8 <- subset(df_Losing, frequency > "1" | frequency == "1")

txns_8 <- FreqOther_8 %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)
LINH_8 <- txns_8 %>% group_by(CUSTCODE,NGAYHD) %>% summarise(N = n()) 
LINH_8 <- LINH_8 %>% group_by(CUSTCODE) %>% summarise(N = n()) %>% filter(N=="1")
a8 <- LINH_8[!duplicated(LINH_8$CUSTCODE), ] #14,244 khach hang 

# LINH_8 <- merge(LINH_8, txns_8, by = "CUSTCODE")
LINH_9 <- merge(LINH_8, df_Losing, by = "CUSTCODE")
a9 <- LINH_9[!duplicated(LINH_9$CUSTCODE), ] #14,244 khach hang 


##################################
### CẤP 1 - SO SANH VOI CHINH NO 
##################################

# Gan nhan Churn hoac Non-churn: Muon gia tri cua NHOM I lam benchmarking 
Mean_Benchmark <- mean(time_between$dt)
Max_Benchmark <- max(time_between$dt)

LINH_9$CHURN <- ifelse(LINH_9$recency < Mean_Benchmark, "Not Churn",
                       ifelse(LINH_9$recency >= Mean_Benchmark & LINH_9$recency < Max_Benchmark, "Churn-L1",
                              ifelse(LINH_9$recency >= Max_Benchmark,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
LINH_9 <- LINH_9[!duplicated(LINH_9$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(LINH_9$CHURN, sort.group = "decreasing", cum.percent = TRUE)


###########################################################
### CẤP 2 - SO SANH VOI TOAN BO TAP KHACH HANG HCM
##########################################################
NHOM_mean_a <- mean(NHOM_time_between$dt)
NHOM_max_a <- max(NHOM_time_between$dt)
NHOM_median_a <- median(NHOM_time_between$dt)

LINH_9$CHURN_2 <- ifelse(LINH_9$recency < NHOM_mean_a, "Not Churn", ifelse(LINH_9$recency >= NHOM_mean_a & LINH_9$recency < NHOM_max_a, "Churn-L1", ifelse(LINH_9$recency >= NHOM_max_a,"Churn-L2","Not Churn")))

library(epiDisplay)
LINH_9 <- LINH_9[!duplicated(LINH_9$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(LINH_9$CHURN_2, sort.group = "decreasing", cum.percent = TRUE)

table(LINH_9$GENDER)


###############################################################
### II. TOÀN BỘ TẬP KHÁCH HÀNG HIGH POTENTIAL - T mean T max 
###############################################################

# Tách nhóm High Potential - Cluster 4 
df_HP <- df5 %>% subset(clusterk == '4')
df_HP <- df_HP[!duplicated(df_HP$CUSTCODE), ] #134,716 Khach hang 

HP_txns <- df_HP %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)

HP_time_between <- HP_txns %>% 
  arrange(CUSTCODE, NGAYHD) %>% 
  group_by(CUSTCODE) %>% 
  mutate(dt = as.numeric(as.Date(NGAYHD) - lag(as.Date(NGAYHD)), unit= 'days')) %>% 
  ungroup() %>% 
  na.omit()



#################################################
### III. TACH NHOM HIGH POTENTIAL - Clusterk 4 (-0.607422)
#################################################
df_HP <- df5 %>% subset(clusterk == '4')

##############################################################################
### NHOM I - Frequency > 1, Ngay HD > 1
#############################################################################

##################################
### CẤP 1 - SO SANH VOI CHINH NO 
##################################

#### Dieu kien 1: Frequency > 1 #####
df_HP1 <- subset(df_HP, frequency > "1") #592098
#### Dieu kien 2: Mua nhieu ngay #####
# Tong chi tieu trong ngay 
txns_HP1 <- df_HP1 %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)
# Lay nhung KH mua nhieu ngay
THANH <- txns_HP1 %>% group_by(CUSTCODE,NGAYHD) %>% summarise(N = n()) 
THANH2 <- THANH %>% group_by(CUSTCODE) %>% summarise(N = n()) %>% filter(!(N=="1")) # Remove khach hang mua nhieu lan (tach hoa don) trong 1 ngay
THANH3 <- merge(THANH2, txns_HP1, by = "CUSTCODE")
d <- THANH3[!duplicated(THANH3$CUSTCODE), ] #38,316 khach hang 

# Khoang cach giua cac lan mua 
time_between_HP1 <- THANH3 %>% 
  arrange(CUSTCODE, NGAYHD) %>% 
  group_by(CUSTCODE) %>% 
  mutate(dt = as.numeric(as.Date(NGAYHD) - lag(as.Date(NGAYHD)), unit= 'days')) %>% 
  ungroup() %>% 
  na.omit()

# Khoang cach MAX & MEAN giua cac lan mua 
time_between_max_HP <- time_between_HP1 %>% 
  group_by(CUSTCODE) %>% 
  summarise(lagMax = max(dt)) %>%
  ungroup() %>% 
  filter(lagMax>0)
time_between_mean_HP <- time_between_HP1 %>% 
  group_by(CUSTCODE) %>% 
  summarise(lagMean = mean(dt)) %>%
  ungroup() %>% 
  filter(lagMean>0)

##------------------------------------------------------------------------------
# Xem set phan bo cua recency qua MIN - MAX - QUANTILE - MODE - HISTOGRAM 
HP1_time_between_Test <- time_between_HP1
HP1_time_between_Test$dt <- HP1_time_between_Test$dt/31 #Recency tinh theo thang 

ggplot(HP1_time_between_Test) + geom_histogram(aes(x = HP1_time_between_Test$dt),binwidth = 0.9, fill = "yellow", color = "grey",breaks=seq(2, 60, by=1)) + geom_vline(aes(xintercept = mean(HP1_time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(HP1_time_between_Test$dt)),col='Blue',size=0.9)

quantile(HP1_time_between_Test$dt)
mean(HP1_time_between_Test$dt)
median(HP1_time_between_Test$dt)
# Mode 
HP1_time_between_Test2 <- time_between_HP1 #Recency tinh theo ngay 
names(table(HP1_time_between_Test2$dt))[table(HP1_time_between_Test2$dt)==max(table(HP1_time_between_Test2$dt))]
# table(HP1_time_between_Test2$dt)

#### DSRFM 
dsRFM_HP <- dsRFM %>% subset(clusterk == '4')
dsRFM_HP$recency <- dsRFM_HP$recency/31 #Recency tinh theo thang 
quantile(dsRFM_HP$recency)
mean(dsRFM_HP$recency)
median(dsRFM_HP$recency)
# Mode 
names(table(dsRFM_HP$recency))[table(dsRFM_HP$recency)==max(table(dsRFM_HP$recency))]
##------------------------------------------------------------------------------


# Gan nhan Churn hoac Non-churn 
df_HP1_1 <- inner_join(df_HP1, time_between_max_HP, by = 'CUSTCODE')
df_HP1_2 <- inner_join(df_HP1_1, time_between_mean_HP, by = 'CUSTCODE')
Check_HP1 <- df_HP1_2[!duplicated(df_HP1_2$CUSTCODE), ] #38,316 khach hang 

df_HP1_2$CHURN <- ifelse(df_HP1_2$recency < df_HP1_2$lagMean, "Not Churn", ifelse(df_HP1_2$recency >= df_HP1_2$lagMean & df_HP1_2$recency < df_HP1_2$lagMax, "Churn-L1", ifelse(df_HP1_2$recency >= df_HP1_2$lagMax,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
df_HP1_2_Customer <- df_HP1_2[!duplicated(df_HP1_2$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(df_HP1_2_Customer$CHURN, sort.group = "decreasing", cum.percent = TRUE) #Thong ke so luong Churn va Non-churn theo KHACH HANG, khong phai theo GIAO DICH 


###########################################################
### CẤP 2 - SO SANH VOI TOAN BO TAP KHACH HANG HCM
##########################################################
NHOM_mean_HP1 <- mean(NHOM_time_between$dt)
NHOM_max_HP1 <- max(NHOM_time_between$dt)
NHOM_median_HP1 <- median(NHOM_time_between$dt)

df_HP1_2$CHURN_2 <- ifelse(df_HP1_2$recency < NHOM_mean_HP1, "Not Churn", ifelse(df_HP1_2$recency >= NHOM_mean_HP1 & df_HP1_2$recency < NHOM_max_HP1, "Churn-L1", ifelse(df_HP1_2$recency >= NHOM_max_HP1,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
df_HP1_2_Customer2 <- df_HP1_2[!duplicated(df_HP1_2$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(df_HP1_2_Customer2$CHURN_2, sort.group = "decreasing", cum.percent = TRUE) #Thong ke so luong Churn va Non-churn theo KHACH HANG, khong phai theo GIAO DICH 


#############################################################################
### NHOM II - Frequency >=1, Ngay HD = 1
#############################################################################
df_HP2 <- subset(df_HP, frequency > "1" | frequency == "1") # 250,408
# c <- df_HP2[!duplicated(df_HP2$CUSTCODE), ] #134,716

txns_HP2 <- df_HP2 %>% 
  mutate(CUSTCODE = as.factor(CUSTCODE),
         NGAYHD = NGAYHD) %>%
  group_by(CUSTCODE, NGAYHD) %>% 
  summarise(Spend = sum(as.numeric(TOTALAMOUNT))) %>%
  ungroup() %>% 
  filter(Spend>0)
PHUONG <- txns_HP2 %>% group_by(CUSTCODE,NGAYHD) %>% summarise(N = n()) 
PHUONG2 <- PHUONG %>% group_by(CUSTCODE) %>% summarise(N = n()) %>% filter(N=="1")
e <- PHUONG2[!duplicated(PHUONG2$CUSTCODE), ] #96,400 khach hang 

# LINH_8 <- merge(LINH_8, txns_8, by = "CUSTCODE")
PHUONG4 <- merge(PHUONG2, df_HP2, by = "CUSTCODE")
f <- PHUONG4[!duplicated(PHUONG4$CUSTCODE), ] #96,400 khach hang 

##################################
### CẤP 1 - SO SANH VOI CHINH NO 
##################################

# Gan nhan Churn hoac Non-churn: Muon gia tri cua NHOM I lam benchmarking 
Mean_Benchmark_HP2 <- mean(time_between_HP1$dt)
Max_Benchmark_HP2 <- max(time_between_HP1$dt)

PHUONG4$CHURN <- ifelse(PHUONG4$recency < Mean_Benchmark_HP2, "Not Churn",
                       ifelse(PHUONG4$recency >= Mean_Benchmark_HP2 & PHUONG4$recency < Max_Benchmark_HP2, "Churn-L1",ifelse(PHUONG4$recency >= Max_Benchmark_HP2,"Churn-L2","Not Churn")))

library(epiDisplay)
# freqency $ %
PHUONG4_Customer <- PHUONG4[!duplicated(PHUONG4$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(PHUONG4_Customer$CHURN, sort.group = "decreasing", cum.percent = TRUE)


###########################################################
### CẤP 2 - SO SANH VOI TOAN BO TAP KHACH HANG HCM
##########################################################
NHOM_mean_HP2 <- mean(NHOM_time_between$dt)
NHOM_max_HP2 <- max(NHOM_time_between$dt)
NHOM_median_HP2 <- median(NHOM_time_between$dt)

PHUONG4$CHURN_2 <- ifelse(PHUONG4$recency < NHOM_mean_HP2, "Not Churn", ifelse(PHUONG4$recency >= NHOM_mean_HP2 & PHUONG4$recency < NHOM_max_HP2, "Churn-L1", ifelse(PHUONG4$recency >= NHOM_max_HP2,"Churn-L2","Not Churn")))

library(epiDisplay)
PHUONG4_Customer_2 <- PHUONG4[!duplicated(PHUONG4$CUSTCODE), ] # Xoa duplicate cac giao dich
tab1(PHUONG4_Customer_2$CHURN_2, sort.group = "decreasing", cum.percent = TRUE)


#################################
# E - VE DO THI #
#################################

###########################################################
### 1. GIA TRI TRUNG BINH, MIN, MAX, HISTOGRAM
##########################################################

require(gridExtra)

w1 <- ggplot(NHOM_time_between_Test) +
    geom_histogram(aes(x = NHOM_time_between_Test$dt),
                   binwidth = 0.9, fill = "lightpink1", color = "grey",breaks=seq(2, 60, by=1)) + geom_vline(aes(xintercept = mean(NHOM_time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(NHOM_time_between_Test$dt)),col='Blue',size=0.9)

w2 <- ggplot(time_between_Test) +
    geom_histogram(aes(x = time_between_Test$dt),
                   binwidth = 0.9, fill = "green", color = "grey",breaks=seq(2, 70, by=1)) + geom_vline(aes(xintercept = mean(time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(time_between_Test$dt)),col='Blue',size=0.9)

w3 <- ggplot(HP1_time_between_Test) + geom_histogram(aes(x = HP1_time_between_Test$dt),binwidth = 0.9, fill = "yellow", color = "grey",breaks=seq(2, 60, by=1)) + geom_vline(aes(xintercept = mean(HP1_time_between_Test$dt)),col='Black',size=0.9) + geom_vline(aes(xintercept = median(HP1_time_between_Test$dt)),col='Blue',size=0.9)

grid.arrange(w1, w2, w3, ncol=3)


###########################################################
### 2. TY LE CHURN CHUNG 
##########################################################

##### POTENTIAL - F > 1, HD > 1 ##### 
## Percentage
library(ggplot2)
library(scales)
a1 <- ggplot(df_HP1_2_Customer1, aes(x = CHURN)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="light blue") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 3) +
  labs(title = "1.1 High Potential - Nhom 1 - So sanh voi chinh no", y = "Percent", x = "") + theme_minimal()
a1


a2 <- ggplot(df_HP1_2_Customer2, aes(x = CHURN_2)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="lightpink1") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "1.2 High Potential - Nhom 1 - So sanh voi Tap HCM", y = "Percent", x = "")
a2

##### POTENTIAL - F >= 1, HD = 1 ##### 
a3 <- ggplot(PHUONG4_Customer, aes(x = CHURN)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="light blue") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "2.1 High Potential - Nhom 2 - So sanh voi benchmarking", y = "Percent", x = "")
a3

a4 <- ggplot(PHUONG4_Customer_2, aes(x = CHURN_2)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="lightpink1") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "2.2 High Potential - Nhom 2 - So sanh voi Tap HCM", y = "Percent", x = "")
a4

grid.arrange(a1, a2, a3, a4, ncol=2, nrow=2)

##### LOSING TREASURE - F > 1, HD > 1 ##### 
a5 <- ggplot(df_Losing_Nhom22_Customer, aes(x = CHURN)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="gold1") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "1.1 Losing Treasure - Nhom 1 - So sanh voi chinh no", y = "Percent", x = "")
a5

a6 <- ggplot(df_Losing_Nhom22_Customer, aes(x = CHURN_2)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="palegreen") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "1.2 Losing Treasure - Nhom 1 - So sanh voi Tap HCM", y = "Percent", x = "")
a6

##### LOSING TREASURE - F >= 1, HD = 1 ##### 
a7 <- ggplot(LINH_9, aes(x = CHURN)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="gold1") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "2.1 Losing Treasure - Nhom 2 - So sanh voi benchmarking", y = "Percent", x = "")
a7

a8 <- ggplot(LINH_9, aes(x = CHURN_2)) +  
        geom_bar(aes(y = (..count..)/sum(..count..)),color="white", fill="palegreen") + 
        scale_y_continuous(labels = percent) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "2.2 Losing Treasure - Nhom 2 - So sanh voi Tap HCM", y = "Percent", x = "")
a8

grid.arrange(a5, a6, a7, a8, ncol=2, nrow=2)


###########################################################
### 3. TY LE CHURN THEO KENH PHAN PHOI  
##########################################################

###### HIGH POTENTIAL F > 1, HD > 1 ##### 
library(readxl)
# write_xlsx(x = df_HP1_2_Customer1, path = "k.xlsx", col_names = TRUE) # Xuat ra file excel 

v <- read_excel("k.xlsx", sheet = "K")
v0 <- v %>% group_by(CHANNEL_NAME,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))
v1 <- ggplot(data = v0, aes(y=n, x=CHANNEL_NAME, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 1 - So sanh voi chinh no")
v1


# v00 <- df_HP1_2_Customer2
# write_xlsx(x = v00, path = "k1.xlsx", col_names = TRUE) # Xuat ra file excel 
v2 <- read_excel("k1.xlsx", sheet = "K")
v01 <- v2 %>% group_by(CHANNEL_NAME,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

v12 <- ggplot(data = v01, aes(y=n, x=CHANNEL_NAME, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 1 - So sanh Tap KH HCM")
v12

###### HIGH POTENTIAL F>1, HD = 1 ##### 

# i <- PHUONG4_Customer
# write_xlsx(x = i, path = "k2.xlsx", col_names = TRUE) # Xuat ra file excel 
i1 <- read_excel("k2.xlsx", sheet = "K")
i2 <- i1 %>% group_by(CHANNEL_NAME,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

i3 <- ggplot(data = i2, aes(y=n, x=CHANNEL_NAME, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh voi benchmarking ")
i3

# ii <- PHUONG4_Customer_2
# write_xlsx(x = ii, path = "k4.xlsx", col_names = TRUE) # Xuat ra file excel 
ii1 <- read_excel("k4.xlsx", sheet = "K")
ii2 <- ii1 %>% group_by(CHANNEL_NAME,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

ii3 <- ggplot(data = ii2, aes(y=n, x=CHANNEL_NAME, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh Tap kH HCM")
ii3

grid.arrange(v1, v12, i3, ii3, ncol=2, nrow=2)

###### LOSING TREASURE F>1, HD > 1 ##### 

# n <- df_Losing_Nhom22_Customer
# write_xlsx(x = n, path = "k5.xlsx", col_names = TRUE) # Xuat ra file excel 
n1 <- read_excel("k5.xlsx", sheet = "K")
n2 <- n1 %>% group_by(CHANNEL_NAME,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

n3 <- ggplot(data = n2, aes(y=n, x=CHANNEL_NAME, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh chinh no")
n3


# m <- df_Losing_Nhom22_Customer
# write_xlsx(x = m, path = "k6.xlsx", col_names = TRUE) # Xuat ra file excel 
m1 <- read_excel("k6.xlsx", sheet = "K")
m2 <- m1 %>% group_by(CHANNEL_NAME,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

m3 <- ggplot(data = m2, aes(y=n, x=CHANNEL_NAME, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh Tap KH HCM")
m3

###### HIGH POTENTIAL F>1, HD = 1 ##### 

z1 <- LINH_9 # Vi k co missing data nen khong can xuat ra file excel
# write_xlsx(x = z, path = "k7.xlsx", col_names = TRUE) # Xuat ra file excel 
# z1 <- read_excel("k7.xlsx", sheet = "K")
z2 <- z1 %>% group_by(CHANNEL_NAME,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

z3 <- ggplot(data = z2, aes(y=n, x=CHANNEL_NAME, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh voi benchmarking")
z3


r1 <- LINH_9 # Vi k co missing data nen khong can xuat ra file excel
# write_xlsx(x = r, path = "k8.xlsx", col_names = TRUE) # Xuat ra file excel 
# r1 <- read_excel("k8.xlsx", sheet = "K")
r2 <- r1 %>% group_by(CHANNEL_NAME,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CHANNEL_NAME, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

r3 <- ggplot(data = r2, aes(y=n, x=CHANNEL_NAME, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CHANNEL_NAME, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh voi Tap KH HCM")
r3


grid.arrange(n3, m3, z3, r3, ncol=2, nrow=2)


###########################################################
### 4. TY LE CHURN THEO LOAI THE 
##########################################################

#### LOSING TREASURE, F > 1, HD > 1 #### 
aq <- df_Losing_Nhom22_Customer
aq1 <- aq %>% group_by(CARDTYPE,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

aq2 <- ggplot(data = aq1, aes(y=n, x=CARDTYPE, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh chinh no")
aq2

#--
aq11 <- aq %>% group_by(CARDTYPE,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

aq22 <- ggplot(data = aq11, aes(y=n, x=CARDTYPE, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh voi Tap KH HCM")
aq22

#### LOSING TREASURE, F > 1, HD = 1 #### 

aq3 <- LINH_9
aq33 <- aq3 %>% group_by(CARDTYPE,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

aq4 <- ggplot(data = aq33, aes(y=n, x=CARDTYPE, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh voi benchmarking")
aq4

#--
aq5 <- aq %>% group_by(CARDTYPE,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

aq6 <- ggplot(data = aq5, aes(y=n, x=CARDTYPE, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh voi tap KH HCM")
aq6


grid.arrange(aq2, aq22, aq4, aq6, ncol=2, nrow=2)

#### HIGH POTENTIAL, F > 1, HD > 1 #### 

av <- df_HP1_2_Customer1
av1 <- av %>% group_by(CARDTYPE,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

av2 <- ggplot(data = av1, aes(y=n, x=CARDTYPE, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh voi chinh no")
av2

#--
av3 <-df_HP1_2_Customer2

av33 <- av3 %>% group_by(CARDTYPE,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

av333 <- ggplot(data = av33, aes(y=n, x=CARDTYPE, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh voi Tap KH HCM")
av333

#--

av4 <- PHUONG4_Customer

av44 <- av4 %>% group_by(CARDTYPE,CHURN) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

av45 <- ggplot(data = av44, aes(y=n, x=CARDTYPE, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh voi benchmark")
av45


#--
av6 <- PHUONG4_Customer_2

av61 <- av6 %>% group_by(CARDTYPE,CHURN_2) %>% summarise(n = n()) %>% ungroup() %>% mutate('relative'=unlist(by(data = n, INDICES = CARDTYPE, 
                              FUN = function(x) round(x/sum(x)*100, digits = 1))))

av611 <- ggplot(data = av61, aes(y=n, x=CARDTYPE, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('So luong KH') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = CARDTYPE, label = paste0(relative,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh voi tap KH HCM")
av611


grid.arrange(av2, av333, av45, av611, ncol=2, nrow=2)


###########################################################
### 5. TY LE CHURN THEO GIOI TINH  
##########################################################

#### LOSING TREASURE, F > 1, HD > 1 #### 
C <- df_Losing_Nhom22_Customer
C$GENDER_recode <- ifelse(C$GENDER == "1","Nam","Nu")
table(C$GENDER_recode)
table(C$CHURN)

b1 <- ggplot(C, aes(x= CHURN,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "LT - Nhom 1 - So sanh voi chinh no")
b1

#--
D <- df_Losing_Nhom22_Customer
D$GENDER_recode <- ifelse(D$GENDER == "1","Nam","Nu")
table(D$GENDER_recode)

b2 <- ggplot(D, aes(x= CHURN_2,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "LT - Nhom 1 - So sanh voi Tap KH HCM")
b2

#### LOSING TREASURE, F > 1, HD = 1 #### 
table(LINH_9$GENDER)
C1 <- LINH_9
C1 <- C1 %>% filter(!is.na(GENDER))
C1$GENDER_recode <- ifelse(C1$GENDER == "1","Nam","Nu")
table(C1$GENDER_recode)
table(C1$CHURN)

b3 <- ggplot(C1, aes(x= CHURN,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "LT - Nhom 2 - So sanh voi benchmarking")
b3

# c13 <- C1 %>% filter(is.na(GENDER_recode)) # Lay gia tri NA ra xem 

#--

table(LINH_9$GENDER)
C12 <- LINH_9
C12 <- C12 %>% filter(!is.na(GENDER))
C12$GENDER_recode <- ifelse(C12$GENDER == "1","Nam","Nu")
table(C12$GENDER_recode)

b4 <- ggplot(C12, aes(x= CHURN_2,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "LT - Nhom 2 - So sanh voi Tap HCM")
b4

grid.arrange(b1, b2, b3, b4, ncol=2, nrow=2)


#### HIGH POTENTIAL, F > 1, HD > 1 #### 

C <- df_HP1_2_Customer
table(df_HP1_2_Customer1$GENDER)
C <- C %>% filter(!is.na(GENDER))

C$GENDER_recode <- ifelse(C$GENDER == "1","Nam","Nu")
table(C$GENDER_recode)

b1 <- ggplot(C, aes(x= CHURN,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "HP - Nhom 1 - So sanh voi chinh no")
b1

#--
D <- df_HP1_2_Customer2
D <- D %>% filter(!is.na(GENDER))
D$GENDER_recode <- ifelse(D$GENDER == "1","Nam","Nu")
table(D$GENDER_recode)

b2 <- ggplot(D, aes(x= CHURN_2,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "HP - Nhom 1 - So sanh voi Tap KH HCM")
b2

#### HIGH POTENTIAL, F > 1, HD = 1 #### 

C1 <- PHUONG4_Customer
C1 <- C1 %>% filter(!is.na(GENDER))
C1$GENDER_recode <- ifelse(C1$GENDER == "1","Nam","Nu")
table(C1$GENDER_recode)

b3 <- ggplot(C1, aes(x= CHURN,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "HP - Nhom 2 - So sanh voi benchmarking")
b3

#--

C12 <- PHUONG4_Customer_2
C12 <- C12 %>% filter(!is.na(GENDER))
C12$GENDER_recode <- ifelse(C12$GENDER == "1","Nam","Nu")
table(C12$GENDER_recode)

b4 <- ggplot(C12, aes(x= CHURN_2,  group=GENDER_recode)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent", fill="Churn") +
    facet_grid(~GENDER_recode) +
    scale_y_continuous(labels = scales::percent) + labs(title = "HP - Nhom 2 - So sanh voi Tap HCM")
b4

grid.arrange(b1, b2, b3, b4, ncol=2, nrow=2)

###########################################################
### 7. TY LE CHURN THEO DO TUOI 
##########################################################

#### LOSING TREASURE, F > 1, HD > 1 #### 
AG <- df_Losing_Nhom22_Customer
table(AG$AGE)

AG$AGE_recode <- ifelse(AG$AGE <= 24,"Duoi 24T", ifelse(AG$AGE >= 25 & AG$AGE <= 34, "Tu 25-34T",ifelse(AG$AGE >= 35 & AG$AGE <= 44, "Tu 34-44T", ifelse(AG$AGE >= 45 & AG$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

t <- AG %>% filter(is.na(AGE_recode)) # Xem so luong missing value
AG %>% filter(!is.na(AGE_recode)) # remove so luong missing value

AG[!(is.na(AG$AGE_recode) | AG$AGE_recode==""), ]

table(AG$AGE_recode)

AG1 <- AG %>% group_by(AGE_recode,CHURN) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG2 <- ggplot(data = AG1, aes(y=perc, x=AGE_recode, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh chinh no")
AG2


# #--
AG11 <- AG %>% group_by(AGE_recode,CHURN_2) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG22 <- ggplot(data = AG11, aes(y=perc, x=AGE_recode, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 1 - So sanh voi Tap HCM")
AG22

#### LOSING TREASURE, F > 1, HD = 1 #### 
AG3 <- LINH_9
table(AG$AGE)

AG3$AGE_recode <- ifelse(AG3$AGE <= 24,"Duoi 24T", ifelse(AG3$AGE >= 25 & AG3$AGE <= 34, "Tu 25-34T",ifelse(AG3$AGE >= 35 & AG3$AGE <= 44, "Tu 34-44T", ifelse(AG3$AGE >= 45 & AG3$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

table(AG3$AGE_recode)

AG31 <- AG3 %>% group_by(AGE_recode,CHURN) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG32 <- ggplot(data = AG31, aes(y=perc, x=AGE_recode, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh Benchmarking")
AG32


#--
AG311 <- AG3 %>% group_by(AGE_recode,CHURN_2) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG321 <- ggplot(data = AG311, aes(y=perc, x=AGE_recode, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "LT - Nhom 2 - So sanh Tap HCM")
AG321

grid.arrange(AG2, AG22, AG32, AG321, ncol=2, nrow=2)


#### HIGH POTENTIAL, F > 1, HD > 1 #### 

AG <- df_HP1_2_Customer1
table(AG$AGE)

AG$AGE_recode <- ifelse(AG$AGE <= 24,"Duoi 24T", ifelse(AG$AGE >= 25 & AG$AGE <= 34, "Tu 25-34T",ifelse(AG$AGE >= 35 & AG$AGE <= 44, "Tu 34-44T", ifelse(AG$AGE >= 45 & AG$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

table(AG$AGE_recode)

AG1 <- AG %>% group_by(AGE_recode,CHURN) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG2 <- ggplot(data = AG1, aes(y=perc, x=AGE_recode, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 1 - So sanh voi chinh no")
AG2

# #--
AGG <- df_HP1_2_Customer2
table(AGG$AGE)

AGG$AGE_recode <- ifelse(AGG$AGE <= 24,"Duoi 24T", ifelse(AGG$AGE >= 25 & AGG$AGE <= 34, "Tu 25-34T",ifelse(AGG$AGE >= 35 & AGG$AGE <= 44, "Tu 34-44T", ifelse(AGG$AGE >= 45 & AGG$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

table(AGG$AGE_recode)

AG111 <- AGG %>% group_by(AGE_recode,CHURN_2) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG222 <- ggplot(data = AG111, aes(y=perc, x=AGE_recode, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 1 - So sanh voi Tap HCM")
AG222


#### HIGH POTENTIAL, F > 1, HD = 1 #### 

AG33 <- PHUONG4_Customer

AG33$AGE_recode <- ifelse(AG33$AGE <= 24,"Duoi 24T", ifelse(AG33$AGE >= 25 & AG33$AGE <= 34, "Tu 25-34T",ifelse(AG33$AGE >= 35 & AG33$AGE <= 44, "Tu 34-44T", ifelse(AG33$AGE >= 45 & AG33$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

table(AG33$AGE_recode)

AG313 <- AG33 %>% group_by(AGE_recode,CHURN) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG323 <- ggplot(data = AG313, aes(y=perc, x=AGE_recode, fill=CHURN)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh Benchmarking")
AG323


##--

AG334 <- PHUONG4_Customer_2

AG334$AGE_recode <- ifelse(AG334$AGE <= 24,"Duoi 24T", ifelse(AG334$AGE >= 25 & AG334$AGE <= 34, "Tu 25-34T",ifelse(AG334$AGE >= 35 & AG334$AGE <= 44, "Tu 34-44T", ifelse(AG334$AGE >= 45 & AG334$AGE <= 54, "Tu 45-54T","Tu 55T tro len"))))

table(AG334$AGE_recode)

AG3114 <- AG334 %>% group_by(AGE_recode,CHURN_2) %>% summarise(n = n()) %>% mutate(perc=round(n/sum(n),digits = 2))

AG3214 <- ggplot(data = AG3114, aes(y=perc, x=AGE_recode, fill=CHURN_2)) + 
  geom_bar(stat="identity", width = 0.5) + 
  xlab('') + ylab('') +
  #use JOELS great solution for the label position 
  #and add percentage based on variable 'relative', otherwise use 'number'
  geom_text(aes(x = AGE_recode, label = paste0(perc,'%')),
            colour = 'white', position=position_stack(vjust=0.5)) + 
  labs(fill='Churn', title = "HP - Nhom 2 - So sanh Tap HCM")
AG3214


grid.arrange(AG2, AG222, AG323, AG3214, ncol=2, nrow=2)


