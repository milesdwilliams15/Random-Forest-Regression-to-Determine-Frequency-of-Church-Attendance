

## Let's use a random forest model to predict church attendance:
relig2<-relig[,200:309]

form<-as.factor(attendanceNum) ~ beliefNum + certaintyNum + personalGodNum + 
  heavenNum + hellNum + scriptureByGodNum + scriptureLiteralNum + 
  exclusiveNum + prayNum + ageNum + 
  educationNum + incomeNum + Republican + Democrat + Independent + 
  ideologyNum + Male + white + black + Hispanic + other

library(randomForest)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(relig2))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(relig2)), size = smp_size)

train <- relig2[train_ind, ]
test <- relig2[-train_ind, ]

rf<-randomForest(form,data=train,na.action=na.omit)
pred <- predict(rf, newdata = test,na.action="na.exclude")
pred <- as.numeric(pred) - 1
table(pred, test$attendanceNum)
## pred   0   1   2   3   4   5
#     0   1   3   1   0   0   0
#     1  10  20  18   8   9   3
#     2  18  57 111  52  51   4
#     3  10  21  52  55  53  24
#     4  34 145 264 308 711 388
#     5   6  21  42  61 189 227
(1 + 20 + 111 + 55 + 711 + 227) / (length(pred) - sum(is.na(pred))) # sum of the diagonal
                                             # elements of the matrix
                                             # divided by the 
## only ~37.8%% accuracy with the above model
sqrt(sum((log(pred+1)-log(test$attendanceNum+1))^2,na.rm=TRUE)/(length(pred) - sum(is.na(pred))))
## Root-Mean-Square Error = ~0.41

relig2$attendanceNum2 <- NA
relig2$attendanceNum2[relig2$attendanceNum > 3] <- 2
relig2$attendanceNum2[relig2$attendanceNum == 3] <- 1
relig2$attendanceNum2[relig2$attendanceNum == 2] <- 1
relig2$attendanceNum2[relig2$attendanceNum < 2] <- 0

form2<-as.factor(attendanceNum2) ~ beliefNum + certaintyNum + personalGodNum + 
  heavenNum + hellNum + scriptureByGodNum + scriptureLiteralNum + 
  exclusiveNum + prayNum + ageNum + 
  educationNum + incomeNum + Republican + Democrat + Independent + 
  ideologyNum + Male + white + black + Hispanic + other

rf2<-randomForest(form2,data=train,na.action=na.omit)
pred2 <- predict(rf2, newdata = test,na.action="na.exclude")
pred2 <- as.numeric(pred2) - 1
table(pred2, test$attendanceNum2)
## pred2    0    1    2
#      0   27   21    6
#      1  140  389  207
#      2  179  562 1446
(27 + 389 + 1446)/(length(pred2) - sum(is.na(pred2)))
## Better: ~62.5% accuracy
sqrt(sum((log(pred2+1)-log(test$attendanceNum2+1))^2,na.rm=TRUE)/(length(pred2) - sum(is.na(pred2))))
## Root-Mean-Square Error = ~0.38 (better than above)

relig2$attendanceNum3 <- NA
relig2$attendanceNum3[relig2$attendanceNum >= 3] <- 1
relig2$attendanceNum3[relig2$attendanceNum < 3] <- 0

form3<-as.factor(attendanceNum3) ~ beliefNum + certaintyNum + personalGodNum + 
  heavenNum + hellNum + scriptureByGodNum + scriptureLiteralNum + 
  exclusiveNum + prayNum + ageNum + 
  educationNum + incomeNum + Republican + Democrat + Independent + 
  ideologyNum + Male + white + black + Hispanic + other

rf3<-randomForest(form3,data=train,na.action=na.omit)
pred3 <- predict(rf3, newdata = test,na.action="na.exclude")
pred3 <- as.numeric(pred3) - 1
table(pred3, test$attendanceNum3)
## pred3    0    1
#      0  211   98
#      1  623 2045
(211 + 2045)/(length(pred3) - sum(is.na(pred3)))
## Better yet: ~75.7% accuracy
sqrt(sum((log(pred3+1)-log(test$attendanceNum3+1))^2,na.rm=TRUE)/(length(pred3) - sum(is.na(pred3))))
## Root-Mean-Square Error = ~0.34 (better)

# Get importance
importance    <- importance(rf3)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
library(dplyr)
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
library(ggplot2)
windows()
ggplot(rankImportance%>%filter(Rank!="#20"), aes(x = reorder(Rank, -Importance), 
                           y = Importance, fill = Importance)) +
  geom_col() + theme_classic() +
  xlab("") + theme(legend.position = "none") +
  scale_x_discrete(labels = c("Frequency of Prayer",
                              "Age",
                              "Income",
                              "Education",
                              "Political Ideology",
                              "Certainty of Belief",
                              "Exclusvity of Salvation",
                              "Sex",
                              "Belief in a Personal God",
                              "Literal Interpretation of Scripture",
                              "Belief in Hell",
                              "Political Independent",
                              "White",
                              "Democrat",
                              "Republican",
                              "Hispanic",
                              "Belief in Heaven",   
                              "Black",
                              "Other Race/Ethnicity")) +
  theme(text=element_text(family="serif")) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0)) +
  ggtitle("Predictors Ranked by Importance")