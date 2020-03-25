---
title: "PumpITup"
author: "Manu"
date: "21/3/2020"
output:
  html_document:
    keep_html: true
    df_print: paged
    toc_depth: 3
    number_sections: true 
    theme: yeti
    highlight: tango
    code_folding: hide
    fig_width: 9
    fig_height: 7
    toc: true
    toc_float:
      collapsed: true
      smooth_scroll: false
---

```{r global options, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

Libraries:

```{r}
library(data.table) 
library(inspectdf)
library(dplyr)     
library(caret)
library(ggmosaic)
library(ggpubr)
library(corrplot)
library(rpart)
library(ggfortify)
library(rattle)
library(lubridate)
library(plyr)
library(DataExplorer)
library(patchwork)
library(stringr)
library(forcats)
library(fastDummies)
library(stringdist)
library(recipes)
library(ggthemes)
```

# 1. Data

```{r}
# Target
train_label <- fread("datos/varObj.csv", nThread = 4, data.table = F )

# input
train_raw <- fread("datos/data_train.csv", nThread = 4, data.table = F )

# test 
test_raw <- fread('datos/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv', nThread = 4, data.table = F)
```

## 1.1 Duplicated data

There are some duplicated observations with diferent ID.

```{r}
duplicated(select(train_raw, -id)) %>% sum()

train_raw <- train_raw[!duplicated(select(train_raw, -id)),]

# Checking
duplicated(select(train_raw, -id)) %>% sum()
```

## 1.2 Train + Test

Needed to do Feature Engineering.

```{r}
train_raw$type <-  "train"
test_raw$type <-  "test"

train <- inner_join(train_label, train_raw, by = "id")

data <- rbind.fill(train, test_raw)
```



## 1.3 EDA

Quick EDA.

```{r}
#categorical plot
x <- inspect_cat(data)
show_plot(x)

# correlations in numeric columns
x <- inspect_cor(data)
show_plot(x)

# feature imbalance bar plot
x <- inspect_imb(data)
show_plot(x)

# missingness barplot
x <- inspect_na(data)
show_plot(x)

# histograms for numeric columns
x <- inspect_num(data)
show_plot(x)

# barplot of column types
x <- inspect_types(data)
show_plot(x)


#summary(Filter(is.numeric, data))
```

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f1.png)

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f2.png)

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f3.png)

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f4.png)

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f5.png)
![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f6.png)


Some interesting graphs.



```{r}
# violin chart
data %>% 
  filter(construction_year>0& type=="train") %>%
  ggviolin(., x = "status_group", y = "construction_year", fill = "status_group", alpha=.5,
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              add = "boxplot", add.params = list(fill = "white"),
              ggtheme = theme_gray())

# NA lot lat. Tanzania Map.
data %>%
  filter(type=="train") %>%
  ggplot(.,aes(longitude, latitude, color=status_group)) + geom_point(alpha=.5) + geom_jitter(width = .4,height = .4)

# Mosaic chart: unknown cateogory behaves like never pay.
data %>%
  filter(type=="train") %>%
  ggplot(.) + geom_mosaic(aes(product(payment), fill=factor(status_group))) + coord_flip()
```

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f9.png)
![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f7.png)
![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f8.png)


Some insights from the EDA and the graph:

  * gps_height: (-90) Tanzania has no points below sea level, the lowest would be 0.
  * Lon/Lat: Tanzania cannot have logitude 0, nor maximum latitude 0. (see if it matches 0.0)
  * num_private is not known what it is and with a median of 0 and a mean of 0.46 it seems that it should be discarded.
  * Cat. vars: There are many with many levels. Some of them are redundant or duplicated.


# 2. Cleaning

## 2.1 Numerical

When inspecting the data some variables have inconsistent values. For example, latitude and longitude do had 0.


```{r}
data <- data %>% 
  mutate(construction_year = ifelse(construction_year==0, NA, construction_year)) %>% 
  mutate(gps_height = ifelse(gps_height <= 0, NA, gps_height)) %>% 
  mutate(latitude = ifelse(latitude > - 0.1, NA, latitude)) %>% 
  mutate(longitude = ifelse(longitude == 0, NA, longitude)) 
```

The following variables are way too much skewed. Options: $log; \sqrt{var}; BoxCox, YeoJohnson_{when_{neg}}$

* amount_tsh: It has way too many 0s:

  a) Binary - 0 ,1
  b) log(+1). $log(0)= \epsilon$ while $log(1) = 0$... So it does not change the fact that they are 0. 
  
* population: The same.

```{r}
data$amount_tsh_fe <- ifelse(data$amount_tsh == 0, 0, 1)
data$amount_tsh <- log(data$amount_tsh + 1)

data$population_fe <- ifelse(data$population <2, 0, 1)
data$population <- log(data$population + 1)
```


## 2.2 Categorical - Text Mining

There are variables like subvillage, installer, funder that have many levels. The strings are upper, lower, upcase with blanks... very messy. So text mining is required.

  * To lower:
  
```{r}
# Not interest in modify the target variable so -> to factor and search with character.
data$status_group <- factor(data$status_group)

# Here to check if levels are reduced: 2411
unique(data$installer) %>% length() %>% sum()

# With stringr -> all to lowercase.
data[,sapply(data, class)=="character"] = lapply(Filter(is.character, select(data, - status_group)), str_to_lower)

# Now: 2163
unique(data$installer) %>% length() %>% sum()
``` 

  * Blanks:

```{r}
data[,sapply(data, class)=="character"] <- lapply(Filter(is.character, select(data, -status_group)), str_replace_all, " ", "")

# Now: 2102
unique(data$installer) %>% length() %>% sum()
```

  * Text Mining: Own function to join similar words based on stringR and strindist (calculate the distance between words). For instance: "acra" = "accra" from installer.
  
    - If you want to use it: lower and eliminate blanks in your variables before hand, so that the adist function performns faster.

```{r}
inspect_str <- function(variable, verbose = FALSE, distance = 1) {
  
  for (i in letters) {
    
    start_letter <- unique(variable[str_starts(variable, i)])
    
    ad <- adist(start_letter,useBytes = T)
    
    words <- start_letter[which(ad <= distance & upper.tri(ad), arr.ind = TRUE)]
    
    comparison <- as.data.frame(matrix(words, ncol = 2))
    
    rows <- nrow(comparison)
    
    if (rows> 0) {
    
      for (j in 1:rows){
         
          old <- comparison[j,1]
          new <- comparison[j,2]
    
          variable <- ifelse(variable == old, new, variable)
          
        }
      
    }
    
    if (verbose == TRUE) {
      
      print(i)
      
    }
      
  }
  
  variable
}
```

It took 4:50 mins. 5 vars * 75k. Remind that subvillage had 22k levels. 

For a single var like installer (2100k categories) lats 10 seg.

```{r}
# large_factors <- c("funder", "installer",  "subvillage", "ward", "scheme_name")
# data[,large_factors] <- lapply(data[,large_factors], inspect_str, TRUE, 1)
# saveRDS(data, file = "ApuntesNADM/PIU/data.RDS")
data <- readRDS("ApuntesNADM/PIU/data.RDS")

# Finally: 1726 from 2411
unique(data$installer) %>% length() %>% sum()
```



## 2.3 NearZeroVariance

Remove num_private and recorded_by (which is a constant).

```{r}
# index_zero <-  nearZeroVar(data, freqCut = 95/5, uniqueCut = 10)
# data[,index_zero] %>% head()

data$num_private <- NULL
data$recorded_by <- NULL
```


# 3. Feature Engineering + Deeper clean.

## 3.1 Duplicated variables

Elimination of duplicated variables. In the first attemps, variables with low and balanced categories were selected. However, with frequency / target encoding, info. can be extracted from variables with high amount of categories.

```{r}
data$extraction_type_group <- NULL
data$extraction_type_class <- NULL
data$management_group <- NULL
data$payment_type <- NULL
data$quality_group <- NULL
data$quantity_group <- NULL
data$source_class <- NULL
data$source_type <- NULL
data$waterpoint_type_group <- NULL
data$permit <- NULL
```

Just in case:

```{r}
data[data$scheme_management=="",]$scheme_management <- "unknown"
data[data$scheme_name=="",]$scheme_name <- "unknown"
```

Public Meeting: Just divide it in 3 categories.

```{r}
data <- data %>% 
  mutate(public_meeting=ifelse(is.na(public_meeting), "unknown", ifelse(public_meeting==TRUE, "public", "private")))
```


## 3.2 FE: Dates

Thanks to lubridate, seasonality can be captured if any. Also it is always worth to try week day, maybe people do not check well the pumps at the weekend.

```{r}
data$age <- as.numeric(ymd("2020-03-20") - ymd(data$date_recorded))

data$month <- month(ymd(data$date_recorded),label = TRUE, abbr = FALSE)

data$year <- year(ymd(data$date_recorded))

data$wday <- wday(ymd(data$date_recorded),label = TRUE, abbr = FALSE)

data$date_recorded <- NULL
```

## 3.3 NA: Long/Lat/Gps/Construction year + FE

The region code and district code are used to impute GPS + LONG + LAT.

  * Idea from: https://github.com/dipetkov/DrivenData-PumpItUp/blob/master/transform-data.R

```{r}
data$region_code <- factor(data$region_code)
data$district_code <- factor(data$district_code)

# Compute averages in districts within regions
data <-  data %>% 
  group_by(region,district_code) %>%
  mutate(district.long = mean(longitude, na.rm = TRUE)) %>%
  mutate(district.lat = mean(latitude, na.rm = TRUE)) %>%
  ungroup()

# Compute averages in regions (just in case the above is also NA)
data <-  data %>%
  group_by(region) %>%
  mutate(region.long = mean(longitude, na.rm = TRUE)) %>%
  mutate(region.lat = mean(latitude, na.rm = TRUE)) %>%
  ungroup()

# "Impute" missing longitude/latitude values
data <-  data %>%
  mutate(longitude = ifelse(!is.na(longitude), longitude,
                            ifelse(!is.na(district.long), district.long, region.long))) %>%
  mutate(latitude = ifelse(!is.na(latitude), latitude,
                           ifelse(!is.na(district.lat), district.lat, region.lat)))

data <- data %>% 
  select(-district.long,-district.lat,-region.lat,-region.long)

data <-  data %>%
  group_by(region) %>%
  mutate(region.gps = mean(gps_height, na.rm = TRUE)) %>%
  ungroup()

# imputar gps.
data <-  data %>%
  mutate(gps_height = ifelse(!is.na(gps_height), gps_height, region.gps))

data <- data %>% 
  select(-region.gps)
                      

data$region_code <- NULL
data$district_code <- NULL
```

Here feature engineering. Finding the distance of the pumps based on a point, supposed to be 0,0: "geochecking".

```{r}
# Geo
data$lonlat_fe <- sqrt(data$longitude^2 + data$latitude^2)
```

*Construction* imputing. Methodology:

  a) NA: Identify NA with a categorical variable.
  b) Ranger: Random forest with all variables. Thanks to that only the best variables were chosen.
  c) missRanger with 160 trees (16 var * 10) and 4 variables entering into the trees sqrt(k); k = variables.
  d) Plot an histogram to see how well it has imputed.
  
The final model:

```{r}
data$construction_year_na <- ifelse(is.na(data$construction_year), "na", "ok") # Keep it to create a binary (was NA or Not).

library(missRanger)

data <- missRanger(data, construction_year  ~ source + region + extraction_type + payment + basin + month + age +
    waterpoint_type + quantity +management + scheme_management + gps_height+  longitude +latitude + lonlat_fe + wday, 
    pmm.k = 4,
    num.trees = 160,
    seed = 123456)

data %>% 
  filter(type=="train") %>% 
  ggplot(., aes(construction_year, fill=construction_year_na)) + geom_histogram()
```

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f10.png)


## 3.4 Dummies

Only for variables with < 18 categories. When creating dummies it is advisable to remove the most frequent category. 

```{r}
data_oh <- dummy_cols(data, select_columns =c("basin","construction_year_na", "region", "scheme_management", "extraction_type", "management", "payment", "water_quality", "quantity", "source", "waterpoint_type", "month", "wday", "public_meeting"), remove_most_frequent_dummy = TRUE ,remove_selected_columns = TRUE)
```


## 3.5 Lumping: Vast category variables

Like funder, installer, wpt_name, subvillage, ward, scheme_name:

  1) Approach: Lumping to at least gather INFO of top 50 categories.
  2) Approach: Frequency encoding.
  3) Approach: Target encoding.
  
```{r}
data_oh_lum <- data_oh %>%
  mutate(funder = fct_lump(funder, n = 50, other_level = "alternative")) %>%
  mutate(installer = fct_lump(installer, n = 50, other_level = "alternative")) %>%
  mutate(wpt_name = fct_lump(wpt_name, n = 50, other_level = "alternative")) %>%
  mutate(subvillage = fct_lump(subvillage, n = 50, other_level = "alternative")) %>%
  mutate(ward = fct_lump(ward, n = 50, other_level = "alternative")) %>%
  mutate(scheme_name = fct_lump(scheme_name, n = 50, other_level = "alternative")) %>%
  mutate(lga = fct_lump(lga, n=50), other_level = "alternative")

data_oh_lum <- dummy_cols(data_oh_lum, select_columns =c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga"), remove_most_frequent_dummy = TRUE ,remove_selected_columns = TRUE)
```

## 3.6 Target encoding.

Target encoding is the process of replacing a categorical value with the mean (regression) or proportion (classification) of the target variable: https://bradleyboehmke.github.io/HOML/engineering.html#alternatives-1

Problem for competitions:

  * The target variable is not available for test data.
  * To solve this, first compute target encoding for train, and then match the test data category with its value in train. This can cause data leak.
  * I created a function for this. 
  
  ** In this case target encoding caused overfit. 

```{r}
# library(dataPreparation)
# 
# #  Preparation:
# data_oh$status_group_aux <- as.integer(data_oh$status_group)-1 # to compute the mean.
# # just train because status_group in test is NA.
# data_oh_train = data_oh %>% 
#   filter(type == "train") %>% 
#   select(funder, installer, wpt_name, subvillage, ward, scheme_name, lga, status_group_aux)
#   
#   
# # building the target encoding
# target_encoding <- build_target_encoding(
#   data_oh_train,
#   cols_to_encode = c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga"),
#   target_col = "status_group_aux",
#   functions = "mean",
#   verbose = FALSE
# )
# 
# # To factor:
# data_oh[, c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga")] = lapply(data_oh[,c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga")], factor)
# 
# # applying the tatget encoding to train data.
# data_oh <- as.data.frame(data_oh) 
# 
# for (i in 1:length(target_encoding)) {
#   for (j in 1:nrow(target_encoding[[i]])) {
#     
#     levels(data_oh[,which(names(data_oh) == names(target_encoding)[i])])[levels(data_oh[,which(names(data_oh) == names(target_encoding)[i])])==target_encoding[[i]][[j,1]]] = target_encoding[[i]][[j,2]]
#     
#   }
#   
#   
# }
  
```

Some cat in test that were not present in TRAIN.

```{r}
# data_oh <- readRDS(file = "ApuntesNADM/PIU/target_encoding")
# data_oh[, c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga")] = apply(data_oh[, c("funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga")], 2, function(x) as.numeric(as.character(x)))
# data_oh[, c(names(target_encoding))] = apply(data_oh[, c(names(target_encoding))], 2, function(x) ifelse(is.na(x), 0, x))
# 
# data_oh$status_group_aux <- NULL

```


Function for next competitions!

```{r}
# use build_target_encoding from dataPreparation library first.
# Make sure that the variables are factors.
target_encode_test <- function(target_encoding, data, factor = FALSE) {
  
  data <- as.data.frame(data)
  
  if (factor == TRUE) {
    
    data[, names(target_encoding)] <- lapply(data[, names(target_encoding)], factor)
    
  }
  
  for (i in 1:length(target_encoding)) {
    
      levels(data_oh[,which(names(data) == names(target_encoding)[i])])[levels(data[,which(names(data) == names(target_encoding)[i])])==target_encoding[[i]][[j,1]]] = target_encoding[[i]][[j,2]]
      
  }
  
  data[, names(target_encoding)] = apply(data[, names(target_encoding)], 2, function(x) as.numeric(as.character(x)))
  data[, names(target_encoding)] = apply(data[, names(target_encoding)], 2, function(x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))
  
  data
  
}

```

## 3.7 Frequency encoding


```{r}
# library(cattonum)
# freq <- catto_freq(select(data_oh, "funder", "installer", "wpt_name", "subvillage", "ward", "scheme_name", "lga"))   
# 
# data_oh$funder <- freq$funder
# data_oh$wpt_name <- freq$wpt_name
# data_oh$scheme_name <- freq$scheme_name
# data_oh$lga <- freq$lga
# data_oh$ward <- freq$ward
# data_oh$installer <- freq$installer
# data_oh$subvillage <- freq$subvillage
```


  
Ideas that were a failure in previous attemps:

  * Using Support Vector Machines (too many parameters to tune and they were slower that XGB and DRF). Outcome in Driven Data 0.8209. 
  * Oversampling: It did not work AT ALL ~ 0.8154.
  * Also some feature engineering.


# 4. Train/Test + XGB

Split the data for the competition.

```{r}
input_train <- data_oh_lum %>% 
  filter(type=="train")
input_test <-data_oh_lum %>% 
  filter(type=="test")


input_train <- input_train %>% 
  select(-type, -other_level)
input_test <- input_test %>% 
  select(-type,-status_group, -other_level)
```



Caret.

```{r}
set.seed(7)

index <- createDataPartition(input_train$status_group, p=.8, list=FALSE)

train <- input_train[index,]
test <- input_train[-index,]
```

## 4.1 XGBOOST Tunning

* Following: https://rpubs.com/dalekube/XGBoost-Iris-Classification-Example-in-R

* To understand the parameters: https://xgboost.readthedocs.io/en/latest/parameter.html

* It is so picky but really worth it. I usually prefer interpretable models such as lasso/elastic net logistic regression, naive bayes... Even so, doublessly XGboost is the model for competitions.


```{r}
# To 0 func, 1 fnr, 2 nf:
# levels(train$status_group) 

target <- input_train$status_group

target <- as.integer(target)-1

# Only DMatrix:


xx <- xgb.DMatrix(data.matrix(input_train[,-2]), label=target)
```


```{r}
# default parameters
params <- list(booster = "gbtree", objective = "multi:softprob", num_class=3,eta=0.02,  max_depth=17, min_child_weight=1, subsample=0.8, colsample_bytree=0.5)
```

Using the inbuilt xgb.cv function, let's calculate the best nround for this model. In addition, this function also returns CV error, which is an estimate of test error.

  * Other parameters were calculated in H2O with Rstudio Cloud (h2o.xgboost only available in linux and Mac) and h2o.grid() function. 

```{r}
# cv
xgbcv <- xgb.cv( params = params, data = xx, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgbcv$evaluation_log$test_merror_mean %>% 
  which.min()

xgbcv$evaluation_log %>% 
  ggplot(., aes(iter,test_merror_mean,color=test_merror_mean)) +
    geom_line(size=.8) +
    scale_color_gradient(low="green",high = "red") +
    geom_vline(xintercept = 471, linetype = 5,color="dodgerblue", size=.8) + 
    geom_text(aes(x=469, label="471", y=.21), colour="white",size=4) +
    labs(y = "Error", x= "Trees", title = "Cross-Validation XGB", color="Error") +
    theme_solarized_2(light = F)
  
```

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f11.png)


# 5. Test Model

```{r}

library(xgboost)

# To 0 func, 1 fnr, 2 nf:
# levels(train$status_group) 

target <- train$status_group

target <- as.integer(target)-1

# Only DMatrix:

# Removing y from train like in python... 

xx <- xgb.DMatrix(data.matrix(train[,-2]), label=target)

# Model train

bb_tr <- xgboost(data = xx, max_depth = 17, eta = 0.02,
                 subsample = 0.8, colsample_bytree = 0.5,
                 min_child_weight = 1, nrounds = 471, num_class = 3,
                 objective = "multi:softprob", maximize = FALSE)


# Predictions

predictors_test <- data.matrix(test[,-2])

bb_pred_tr <- predict(bb_tr, predictors_test, reshape = T) # It returns Probs.
  
colnames(bb_pred_tr) <- c("functional", "functional needs repair", "non functional")

# Labeling:
bb_pred_tr$prediction = apply(bb_pred_tr, 1, function(x) colnames(bb_pred_tr)[which.max(x)])

# Actual Test:
xx <- test$status_group

# Acc: 0.8161 (lumping) || 0.924 (target encoding) || 0.8153 (freq)
result = sum(bb_pred_tr$prediction==xx)/length(bb_pred_tr$prediction)
result

# Confusion Matrix:
table(bb_pred_tr$prediction, xx)
```




# 6. Full Model

```{r echo=FALSE}
# To 0 func, 1 fnr, 2 nf:
# levels(train$status_group) 

target <- input_train$status_group

target <- as.integer(target)-1

# Only DMatrix:


xx <- xgb.DMatrix(data.matrix(input_train[,-2]), label=target)
  

# it is not needed but with this one 0.8244 in DrivenData

bb <- xgboost(data = xx, max_depth = 17, eta = 0.02,
                 subsample = 0.8, colsample_bytree = 0.5,
                 min_child_weight = 1, nrounds = 471, num_class = 3,
                 objective = "multi:softprob", maximize = FALSE, verbose = TRUE)



######### test ###########
predictors_test <- data.matrix(input_test)

# 0.8244 (lumping) || # 0.75 (target encoding) || # 0.8240 (freq)
bb_pred <- predict(bb, predictors_test, reshape = T)
colnames(bb_pred) <- c("functional", "functional needs repair", "non functional")
  
bb_pred$prediction = apply(bb_pred, 1 , function(x) colnames(bb_pred)[which.max(x)])

```


Var Importance:

```{r}
importance <- xgb.importance(feature_names = names(input_train[,-2]), model = bb)
head(importance,20)

xgb.ggplot.importance(importance[1:21])
```

![plot of chunk eda](C:/Users/Manu/Desktop/UCM/3machinelearning/DrivenData-PumpItUp/Figures/f12.png)


# 7. Submission

```{r}
my_sub <- data.frame(
                     id = test_raw$id,
                     status_group = bb_pred$prediction
                     )
fwrite(my_sub, file= "sub_v02_base_.csv", sep=",")
```

