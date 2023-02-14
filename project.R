library(dplyr)
library(tm)

#Process the raw dataset.csv
#1. Add column names
nyra <- read.csv("C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/big-data-derby-2022/nyra_2019_complete.csv", header=FALSE)
nyra <- setNames(nyra, c("track_id","race_date","race_number","program_number",
                 "trakus_index","latitude","longitude","distance_id",
                 "course_type","track_condition","run_up_distance","race_type",
                 "purse","post_time","weight_carried","jockey","odds",
                 "position_at_finish"))

#2. Correct wrong value
nyra[1,1] <- c("AQU")

#3. Simplify the number of rows (based on unique track_id & program_number)
nyra_new <- nyra[!duplicated(nyra[c(1,2,3,4)]), ]
nyra_new <- nyra_new[order(nyra_new$track_id, nyra_new$race_date, nyra_new$race_number, nyra_new$program_number),]

#4. export a new basic dataset.csv (nyra_CS699.csv)
write.csv(nyra_new, "C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/big-data-derby-2022/nyra_CS699.csv", row.names=FALSE)

#5. Add horse id & name
nyra_new <- read.csv("C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/big-data-derby-2022/nyra_CS699.csv")
merge_key1 <- paste0(nyra_new$track_id, nyra_new$race_date, nyra_new$race_number, nyra_new$program_number)
nyra_new$merge_key <- merge_key1
for (i in (1:ncol(nyra_new))){
  nyra_new[,i] <- trimws(nyra_new[,i], which=c("both")) #Strip leading and Trailing Space in Column
}
head(nyra_new$merge_key)

horse_ids <- read.csv("C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/big-data-derby-2022/horse_ids.csv")
horse_ids <- subset(horse_ids, select=-c(X))
horse_names <- read.csv("C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/big-data-derby-2022/horse_names.csv")
horse_names <- subset(horse_names, select=-c(X))
horse_info <- merge(horse_ids, horse_names, by="horse_id")
horse_info$horse_id <- as.numeric(horse_info$horse_id) + 1
merge_key2 <- paste0(horse_info$track_id, horse_info$race_date, horse_info$race, horse_info$program_number)
horse_info$merge_key <- merge_key2
for (i in (1:ncol(horse_info))){
  horse_info[,i] <- trimws(horse_info[,i], which=c("both")) #Strip leading and Trailing Space in Column
}
head(horse_info$merge_key)
horse_info <- subset(horse_info, select=c(horse_id, horse_name, merge_key))

nyra_new2 <- merge(nyra_new, horse_info, by="merge_key")
nyra_new2 <- subset(nyra_new2, select=-c(merge_key))

#6. Add origin(1)&weighted(2) rank of weight_carried&odds of each race
rank_key <- paste0(nyra_new2$track_id, nyra_new2$race_date, nyra_new2$race_number)
nyra_new2$rank_key <- rank_key
nyra_new2$weight_carried <- as.numeric(nyra_new2$weight_carried)
nyra_new2$odds <- as.numeric(nyra_new2$odds)
nyra_new2$odds[nyra_new2$odds == 0] <- max(nyra_new2$odds)

nyra_new2 <- as.data.frame(nyra_new2 %>% group_by(rank_key) %>% mutate(weight_carried_rank1 = rank(weight_carried, ties.method = "min")))
nyra_new2 <- as.data.frame(nyra_new2 %>% group_by(rank_key) %>% mutate(weight_carried_rank2 = round(weight_carried/min(weight_carried), digits = 2)))
nyra_new2 <- as.data.frame(nyra_new2 %>% group_by(rank_key) %>% mutate(odds_rank1 = rank(odds, ties.method = "min")))
nyra_new2 <- as.data.frame(nyra_new2 %>% group_by(rank_key) %>% mutate(odds_rank2 = ceiling(odds/min(odds))))

nyra_new2 <- subset(nyra_new2, select=-c(rank_key))

#7. Add race_winner & race_top3 columns for further classification use
race_winner <- c(nyra_new2$position_at_finish==1)
nyra_new2$race_winner <- race_winner #Conventionally, the winner takes about 60% purse

race_top3 <- c(nyra_new2$position_at_finish<4)
nyra_new2$race_top3 <- race_top3 #Conventionally, the top3 take about 90% purse

#8. export this final-version dataset.csv (nyra_CS699.csv)
write.csv(nyra_new2, "C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/nyra_CS699.csv", row.names=FALSE)

#9. Overview of our new dataset.csv
nyra_new2 <- read.csv("C:/Users/User/Desktop/BU Graduate/CS699/Homework/Problems/project/nyra_CS699.csv")
plot(nyra_new2$weight_carried_rank2, nyra_new2$position_at_finish)
nrow(nyra_new2)
length(unique(nyra_new2$jockey))
length(unique(nyra_new2$horse_name))

#10. Attribute selection
library(caTools)
set.seed(0626)
sample <- sample.split(nyra_new2$track_id, SplitRatio=0.2)
nyra_new_AS <- subset(nyra_new2, sample == TRUE)
nyra_new_AS <- subset(nyra_new_AS, select=-c(position_at_finish, race_winner))
#10.1. Boruta
library(Boruta)
set.seed(0626)
boruta_output <- Boruta(race_top3~., data=na.omit(nyra_new_AS), doTrace=0)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
#10.2. Variable Importance - rpart model
library(caret)
set.seed(0626)
rPartMod <- train(as.factor(race_top3)~., data=nyra_new_AS, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)
#10.3. Lasso Regression
library(glmnet)
set.seed(0626)
x <- as.matrix(nyra_new_AS[,-24]) # all X vars
for (i in (1:ncol(x))){
  x[,i] <- trimws(x[,i], which=c("both")) #Strip leading and Trailing Space in Column
}
y <- as.double(as.matrix(ifelse(nyra_new_AS[, 24]=="TRUE", 0, 1))) # Only Class
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')
plot(cv.lasso)
#Or: 10.3. Variable Importance - RRF model
set.seed(0626)
rPartMod <- train(as.factor(race_top3)~., data=nyra_new_AS, method="RRF")
rpartImp <- varImp(rPartMod)
print(rpartImp)
#10.4. Relative Importance - Linear Regression
library(relaimpo)
set.seed(0626)
lmMod <- lm(as.factor(race_top3)~., data=nyra_new_AS)
relImportance <- calc.relimp(lmMod, type = "lmg", rela = F)  
cat('Relative Importances: \n')
sort(round(relImportance$lmg, 3), decreasing=TRUE)
#10.5. DALEX
library(randomForest)
library(DALEX)
rf_mod <- randomForest(as.factor(race_top3)~., data=nyra_new_AS, ntree=100)
explained_rf <- explain(rf_mod, data=nyra_new_AS, y=nyra_new_AS$race_top3)
varimps = variable_dropout(explained_rf, type='raw')
print(varimps)
