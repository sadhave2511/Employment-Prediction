summary(train)
dim(train)

#Missing Value detection
install.packages("VIM")
library(VIM)
mice_plot <- aggr(train, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(train), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


#QUESTION 1
table(train$Year)
t_2012 <- subset(train, train$Year == 2012)
t_2012 <- t_2012[,-c(1:9)]
t_2012 <- t_2012[,-c(16)]
dim(t_2012)
na <- names(t_2012)
for (i in 1:ncol(t_2012)){
  hist(t_2012[,i],      main = na[i])
}

summary(t_2012)


#QUESTION 3
library(ggplot2)
#EFFECT ON BABY CARE BASED ON EDUCATION, WORKING HOURS AND INCOME
chi <- subset(train, train$Children > 0)
chi <- select(chi, Caring.for.Children, Gender, Education.Level, Children, Employment.Status, Weekly.Earnings, Weekly.Hours.Worked) 
head(chi)
table(chi$Education.Level)
table(chi$Children)
chi$per_child <- chi$Caring.for.Children/chi$Children
chi$pct_child <- (chi$per_child*100)/(24*60)
summary(chi$pct_child)
chi$Education.Level <- as.character(chi$Education.Level)
chi$Education.Level[chi$Education.Level == '10th grade'] <- '< High School'
chi$Education.Level[chi$Education.Level == '11th grade'] <- '< High School'
chi$Education.Level[chi$Education.Level == '12th grade'] <- '< High School'
chi$Education.Level[chi$Education.Level == '9th grade'] <- '< High School'
chi$Education.Level[chi$Education.Level == 'High School'] <- '< High School'
chi$Education.Level[chi$Education.Level == 'Associate Degree'] <- 'HS/GED/Voc School'
chi$Education.Level[chi$Education.Level == 'Some College'] <- 'HS/GED/Voc School'
chi$Education.Level[chi$Education.Level == 'Bachelor'] <- 'College Graduate'
chi$Education.Level[chi$Education.Level == 'Master'] <- 'Masters/PHD/Prof'
chi$Education.Level[chi$Education.Level == 'Prof. Degree'] <- 'Masters/PHD/Prof'
chi$Education.Level[chi$Education.Level == 'Doctoral Degree'] <- 'Masters/PHD/Prof'

table(chi$Education.Level)
chi$Education.Level <- as.factor(chi$Education.Level)
chi$Education.Level <- ordered(chi$Education.Level, labels = c('< High School', 'HS/GED/Voc School', 'College Graduate',  'Masters/PHD/Prof'))

head(train)

#male female 
res <- wilcox.test(per_child ~ Gender, data = chi, exact = FALSE)
boxplot(chi$per_child ~ chi$Gender, main = 'Child Care by Gender', 
        xlab='Gender', ylab='Percent of Childcare in a day', 
        col=rainbow(7), notch = FALSE)
by_Gender <- group_by(chi, Gender)
summ <- as.data.frame(summarise(by_Gender, child_care = mean(pct_child, na.rm = TRUE)))
plot(summ, main = 'Child Care by Gender', 
     xlab='Gender', ylab='Percent of Childcare in a day', ylim=c(20,90))

#Education.Level
by_educ <- group_by(chi,Education.Level)
summ <- as.data.frame(summarise(by_educ, child_care = mean(pct_child, na.rm = TRUE)))
summary(chi)
plot(summ, main = 'Child Care by Gender', 
     xlab='Gender', ylab='Percent of Childcare in a day')
#Kruskal Wallis Test
fit2 <- kruskal.test(pct_child ~ Education.Level, data = chi) 

#right skewed
qqnorm(chi$pct_child)
qqline(chi$pct_child, col = 2)
chi$ln_child_care <- log(chi$pct_child)
summary(chi$pct_child)
ls <- subset(chi, chi$pct_child > 0)
chi$ln_child_care <- log(chi$pct_child)
hist(ls$ln_child_care)
table(ls$Education.Level)
plot(aggregate(ln_child_care ~ Education.Level, data =ls, FUN = mean))

#ANOVA
boxplot(ls$ln_child_care ~ ls$Education.Level, main = 'Child Care by Education Level - mean is black dot', 
        xlab='Education Level', ylab='Log of Child care', 
        col=rainbow(7), notch = TRUE)
fit <- aov(ln_child_care ~ Education.Level, data= ls)
plot(fit)
summary(fit)


#significant different
tukey <- TukeyHSD(fit)
plot(tukey)


psig=as.numeric(apply(tukey$Education.Level,1,prod)>=0)+1
str(psig)
op=par(mar=c(4.2,9,3.8,2))
plot(tukey,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tukey$Education.Level)[length(psig)-j+1],
       las=1,cex.axis=0.8,col.axis=psig[length(psig)-j+1])
}
par(op)


#####WORKING HOURS
str(ls)
?head
hist(ls$Weekly.Hours.Worked)
table(ls$Weekly.Hours.Worked)
table(ls$wk_bin)
ls$wk_bin[ls$Weekly.Hours.Worked == 0 ] <- '= 0'
ls$wk_bin[ls$Weekly.Hours.Worked > 0 &  ls$Weekly.Hours.Worked <= 20] <- '< 20'
ls$wk_bin[ls$Weekly.Hours.Worked > 20 & ls$Weekly.Hours.Worked <= 30 ] <- '< 30'
ls$wk_bin[ls$Weekly.Hours.Worked > 30 & ls$Weekly.Hours.Worked <= 40 ] <- '< 40'
ls$wk_bin[ls$Weekly.Hours.Worked > 40 & ls$Weekly.Hours.Worked <= 50 ] <- '< 50'
ls$wk_bin[ls$Weekly.Hours.Worked > 50]  <- '50+'
ls$wk_bin <- as.factor(ls$wk_bin)
plot(table(ls$wk_bin))
ls$wk_bin <- ordered(ls$wk_bin, levels = c('= 0', '< 20', '< 30', '< 40', '< 50',  '50+'))


?as.factor
boxplot(ls$ln_child_care ~ ls$wk_bin, main = 'Child Care by hours worked weekly - mean is black dot', 
        xlab='Weekly work hours', ylab='Log of Child care', 
        col=rainbow(7), notch = FALSE)
plot(aggregate(ln_child_care ~ wk_bin, data =ls, FUN = mean))
?plot
?aggregate

by_wk_hrs <- group_by(ls, wk_bin)
summ <- as.data.frame(summarise(by_wk_hrs, wk_hrs = mean(pct_child, na.rm = TRUE)))

fit <- aov(ln_child_care ~ wk_bin, data= ls)
fit2 <- kruskal.test(ln_child_care ~ wk_bin, data = ls) 

summary(fit)
tukey <- TukeyHSD(fit)
plot(tukey)
psig=as.numeric(apply(tukey$wk_bin,1,prod)>=0)+1
str(psig)
op=par(mar=c(4.2,9,3.8,2))
plot(tukey,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tukey$wk_bin)[length(psig)-j+1],
       las=1,cex.axis=0.8,col.axis=psig[length(psig)-j+1])
}
par(op)


ls$wk_hr[ls$Weekly.Hours.Worked > 0]  <- 'Work'
ls$wk_hr[ls$Weekly.Hours.Worked <= 0]  <- 'No Work'
boxplot(ls$ln_child_care ~ ls$wk_hr, main = 'Child Care by hours worked weekly - mean is black dot', 
        xlab='Weekly work hours', ylab='Log of Child care', 
        col=rainbow(7), notch = FALSE)
res <- wilcox.test(ln_child_care ~ wk_hr, data = ls, exact = FALSE)
by_wk_hrs <- group_by(ls, wk_hr, Gender)
summ <- as.data.frame(summarise(by_wk_hrs, wk_hrs = mean(pct_child, na.rm = TRUE)))


#####INCOME
summary(ls)
ls_e <- subset(ls, ls$Employment.Status == 'Employed')
hist(ls_e$Weekly.Earnings)
ls_e$ln_earn <- log(ls_e$Weekly.Earnings)
table(ls_e$inc_range)
ls_e$inc_range[ls_e$Weekly.Earnings >= 1000] <- '1000+'
ls_e$inc_range[ls_e$Weekly.Earnings > 500 & ls_e$Weekly.Earnings <= 1000] <- '< 1000'
ls_e$inc_range[ls_e$Weekly.Earnings > 100 & ls_e$Weekly.Earnings <= 500] <- '< 500'
ls_e$inc_range[ls_e$Weekly.Earnings > 0 & ls_e$Weekly.Earnings <= 100] <- '< 100'
ls_e$inc_range[ls_e$Weekly.Earnings <= 0] <- '= 0'
ls_e$inc_range <- as.factor(ls_e$inc_range)
ls_e$inc_range <- ordered(ls_e$inc_range, labels = c('= 0', '< 100', '< 500', '< 1000','1000+'))
boxplot(ls_e$ln_child_care ~ ls_e$inc_range, main = 'Child Care by income - mean is black dot', 
        xlab='Income', ylab='Log of Child care', 
        col=rainbow(7), notch = FALSE)
plot(aggregate(ln_child_care~inc_range, data = ls_e, FUN = mean))
fit <- aov(ln_child_care ~ inc_range, data= ls_e)
summary(fit)

by_inc <- group_by(ls_e,inc_range)
summ <- as.data.frame(summarise(by_inc, mean(pct_child, na.rm = TRUE)))
fit2 <- kruskal.test(ln_child_care ~ inc_range, data = ls_e) 

#regression for question 3:
summary(ls_e)
mod <- lm(ln_child_care~Gender+Education.Level+inc_range+wk_bin, data = ls_e)
summary(mod)
?lm 
#QUESTION 4 

str(train)
str(as)
as <- train[,c(4,8,16,18,20,21,22,23,24)]

as <- subset(as, as$Weekly.Earnings >0)
#library(classInt)
#classIntervals(as$Weekly.Earnings, 5, style = 'quantile')
as$inc_range[as$Weekly.Earnings <= 0] <- '= 0'
as$inc_range[as$Weekly.Earnings > 0 & as$Weekly.Earnings <= 346] <- '< 346'
as$inc_range[as$Weekly.Earnings > 346 & as$Weekly.Earnings <= 577] <- '< 577'
as$inc_range[as$Weekly.Earnings > 577 & as$Weekly.Earnings <= 844] <- '< 844'
as$inc_range[as$Weekly.Earnings > 844 & as$Weekly.Earnings <= 1250] <- '< 1250'
as$inc_range[as$Weekly.Earnings > 1250] <- '1250+'
as$inc_range <- as.factor(as$inc_range)
as$inc_range <- ordered(as$inc_range, labels = c('= 0', '< 346', '< 577', '< 844', '< 1250','1250+'))
table(as$inc_range)
str(as)

#Earnings
as <- train[,c(4,8,16,18,20,21,22,23,24)]
as$Age.Range <- NULL
as <- subset(as, as$Weekly.Earnings > 0)
str(as)
hist(as$Weekly.Earnings)
summary(as)
as$lei_Time <- as$Playing.with.Children + as$Shopping + as$Socializing...Relaxing + as$Television + as$Golfing + as$Running + as$Volunteering
inc_lim <- quantile(as$Weekly.Earnings, probs = seq(0,1,0.33))
as$inc_range[as$Weekly.Earnings <= inc_lim[2]] <- "$0-$346 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[2] & as$Weekly.Earnings <= inc_lim[3]] <- "$347-$577 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[3] & as$Weekly.Earnings <= inc_lim[4]] <- "$578-$844 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[4] & as$Weekly.Earnings <= inc_lim[5]] <- "$845-$1250 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[5] & as$Weekly.Earnings <= inc_lim[6]] <- "$1251-$2885 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[6]] <- "> $2885 per week"

as$inc_range[as$Weekly.Earnings <= inc_lim[2]] <- "$0-$495 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[2] & as$Weekly.Earnings <= inc_lim[3]] <- "$496-$956 per week"
as$inc_range[as$Weekly.Earnings > inc_lim[3]] <- "> $957 per week"
table(as$inc_range)
as$Weekly.Earnings <- NULL
plot(aggregate(lei_Time~inc_range, data = as, FUN = mean))
aggregate(.~inc_range, data = as, FUN = mean)

#Age range
as <- train[,c(3,8,16,18,20,21,22,23,24)]
str(as)
as$Age.Range[as$Age <= 25] <- "Young"
as$Age.Range[as$Age > 25 & as$Age <= 55] <- "Middle-Aged"
as$Age.Range[as$Age > 55] <- "Old"
as$lei_Time <- as$Playing.with.Children + as$Shopping + as$Socializing...Relaxing + as$Television + as$Golfing + as$Running + as$Volunteering
table(as$Age.Range)
as$Age <- NULL
table(as$Age.Range)
aggregate(lei_Time~Age.Range, data = as, FUN = mean)

#Overall
as <- train[,c(4,8,16,18,20,21,22,23,24)]
as$Weekly.Earnings <- NULL
as$Age.Range <- NULL
overall <- as.data.frame(apply(as, 2, mean))
library(data.table)
setDT(overall, keep.rownames = TRUE)[]
names(overall) <- c("Activity", "Mean_time_spent")
ggplot(data = overall) + 
  geom_bar(mapping = aes(x = Activity, y = Mean_time_spent, fill = Activity), stat = "identity", width = 1) + theme_bw()
  coord_polar()+


############ QUESTION 5
unique(train$Year)
str(train)
train$Year <- as.factor(train$Year)

library(dplyr)
train <- mutate(train, Emp.categ = ifelse(Employment.Status == "Employed", 1,0))
class(train)
by_year <- group_by(train, Year)
summ <- as.data.frame(summarise(by_year, Employment = sum(Emp.categ), child = sum(Children), Earn = mean(Weekly.Earnings, na.rm = TRUE),Wk_hrs = mean(Weekly.Hours.Worked, na.rm = TRUE),
          Sleep = mean(Sleeping, na.rm = TRUE), Groom =mean(Grooming, na.rm = TRUE), child_care = mean(Caring.for.Children, na.rm = TRUE),
          Job_search = mean(Job.Searching, na.rm = TRUE), shop = mean(Shopping, na.rm = TRUE), relax = mean(Socializing...Relaxing, na.rm = TRUE),
          TV = mean(Television, na.rm = TRUE), volunt = mean(Volunteering, na.rm = TRUE)))
head(summ)
nam <- names(summ)
for(i in (2:ncol(summ))){
ggplot(summ)+geom_bar(aes(x = summ[,1], y = summ[,i]), stat = "identity")+ ggtitle(nam[i])+ labs(y=nam[i], x= "Year")
}


hist(mtcars$mpg)
x <- mtcars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
  
asd <- train
str(asd)
asd$yr[asd$Year <= 2007] <- "2005-2007"
asd$yr[asd$Year >= 2010] <- "2010-2012"
asd$yr[asd$Year >= 2008 & asd$Year <= 2009] <- "2008-2009"
#unemp and job search
asd_unemp <- subset(asd, asd$Employment.Status == "Unemployed")
plot(density(asd_unemp$Job.Searching, bw = 0.05))



############### QUESTION 6

age_dat <- train[,c(4,10:24)]
str(train)
str(age_dat)
by_age <- group_by(train, Age.Range)
summ <- as.data.frame(summarise(by_age, Wk_hrs = mean(Weekly.Hours.Worked*60, na.rm = TRUE),
                                Sleep = mean(Sleeping, na.rm = TRUE), Groom =mean(Grooming, na.rm = TRUE), Hse_wk =mean(Housework, na.rm = TRUE), food_prep =mean(Food...Drink.Prep, na.rm = TRUE), child_care = mean(Caring.for.Children, na.rm = TRUE),
                                Play_child= mean(Playing.with.Children, na.rm = TRUE), Job_search = mean(Job.Searching, na.rm = TRUE), shop = mean(Shopping, na.rm = TRUE), relax = mean(Socializing...Relaxing, na.rm = TRUE), eat_drink = mean(Eating.and.Drinking, na.rm = TRUE),
                                TV = mean(Television, na.rm = TRUE), volunt = mean(Volunteering, na.rm = TRUE), Golf = mean(Golfing, na.rm = TRUE), Run = mean(Running, na.rm = TRUE)))
apply(summ, 1,which.max)


############### QUESTION 7

tr <- train
head(tr)
tr$Id <- NULL
tr$ln_child_care <- NULL
tr$Emp.categ <- NULL 
tr$Total <- NULL
tr$Year <- NULL
tr$Age.Range <-NULL
tr$Education.Level <- as.character(tr$Education.Level)
tr$Education.Level[tr$Education.Level == '10th grade'] <- '< High School'
tr$Education.Level[tr$Education.Level == '11th grade'] <- '< High School'
tr$Education.Level[tr$Education.Level == '12th grade'] <- '< High School'
tr$Education.Level[tr$Education.Level == '9th grade'] <- '< High School'
tr$Education.Level[tr$Education.Level == 'High School'] <- '< High School'
tr$Education.Level[tr$Education.Level == 'Associate Degree'] <- 'HS/GED/Voc School'
tr$Education.Level[tr$Education.Level == 'Some College'] <- 'HS/GED/Voc School'
tr$Education.Level[tr$Education.Level == 'Bachelor'] <- 'College Graduate'
tr$Education.Level[tr$Education.Level == 'Master'] <- 'Masters/PHD/Prof'
tr$Education.Level[tr$Education.Level == 'Prof. Degree'] <- 'Masters/PHD/Prof'
tr$Education.Level[tr$Education.Level == 'Doctoral Degree'] <- 'Masters/PHD/Prof'
tr$Education.Level <- as.factor(tr$Education.Level)
chk <- subset(tr, tr$Weekly.Earnings == 0 & tr$Weekly.Hours.Worked == 0 )
summary(chk$Employment.Status)
chk$Weekly.Earnings <- NULL
chk$Weekly.Hours.Worked <- NULL




#correlation
num <- sapply(chk, is.numeric)
tr_num <- chk[,num]
cor_data <- cor(tr_num, method = "spearman")
library(reshape2)
melted <- melt(cor_data)
head(melted)
ggplot(melted, aes(x=Var1, y = Var2, fill = value))+ geom_tile() +
scale_fill_gradient2(low = "Green", high = "Orange", mid = "white", 
                     midpoint = 0, limit = c(-1,1), space = "Lab", 
                     name="Spearman\nCorrelation") +theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                                                                     size = 12, hjust = 1)) + coord_fixed()

chk$Television <- NULL
chk$Playing.with.Children <- NULL
chk$Weekly.Earnings <- NULL
chk$Weekly.Hours.Worked <- NULL



library(caTools)
sample = sample.split(chk$Employment.Status, SplitRatio = .75)
mod_train <- subset(chk, sample == TRUE)
mod_test <- subset(chk, sample == FALSE)


#Random forest
library(randomForest)
fit <- randomForest(Employment.Status ~ ., data = mod_train, importance = TRUE)
varImpPlot(fit)
importance(fit)
summary(mod_train$Weekly.Earnings)

test_orig <- mod_test$Employment.Status
mod_test$Employment.Status <- NULL
pred <-predict(fit,mod_test, type = "response")
head(pred)
table(test_orig, pred)
prop.table(table(test_orig, pred))

#GBM
fit3 <- gbm(Employment.Status ~ .,
            distribution = "multinomial",
            data = mod_train,
            n.trees = 2500,
            shrinkage = .01,
            n.minobsinnode = 20)
summary(fit3)
fit3
str(tr)

#Train
pred2 = data.frame(predict(object = fit3,
                              newdata = mod_train,
                              n.trees = 1500,
                              type = "response"))
pred2$op <- ifelse(pred2$Not.in.labor.force.1500 > pred2$Employed.1500 & pred2$Not.in.labor.force.1500 > pred2$Unemployed.1500, "Not in labor", 
                   ifelse(pred2$Employed.1500 > pred2$Unemployed.1500, "Emploed", "Unemployed"))
prop.table(table(mod_train$Employment.Status, pred2$op))

#Test
pred2 = data.frame(predict(object = fit3,
                           newdata = mod_test,
                           n.trees = 1500,
                           type = "response"))
pred2$op <- ifelse(pred2$Not.in.labor.force.1500 > pred2$Employed.1500 & pred2$Not.in.labor.force.1500 > pred2$Unemployed.1500, "Not in labor", 
                   ifelse(pred2$Employed.1500 > pred2$Unemployed.1500, "Emploed", "Unemployed"))
prop.table(table(test_orig, pred2$op))


###############################################################################################
#QUESTION 7 : WITH ONLY EMPLOYMENT AND UNEMPLOYMENT DATA
###############################################################################################

chk <- subset(tr, tr$Weekly.Earnings == 0 & tr$Weekly.Hours.Worked == 0 & as.character(tr$Employment.Status) != "Not in labor force")
summary(chk$Employment.Status)
chk$Employment.Status <- as.character(chk$Employment.Status)
chk$Employment.Status <- as.factor(chk$Employment.Status)

chk$Weekly.Earnings <- NULL
chk$Weekly.Hours.Worked <- NULL




#correlation
num <- sapply(chk, is.numeric)
tr_num <- chk[,num]
cor_data <- cor(tr_num, method = "spearman")
library(reshape2)
melted <- melt(cor_data)
head(melted)
ggplot(melted, aes(x=Var1, y = Var2, fill = value))+ geom_tile() +
  scale_fill_gradient2(low = "Green", high = "Orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                                                                                        size = 12, hjust = 1)) + coord_fixed()

chk$Television <- NULL
chk$Playing.with.Children <- NULL
chk$Weekly.Earnings <- NULL
chk$Weekly.Hours.Worked <- NULL



library(caTools)
sample = sample.split(chk$Employment.Status, SplitRatio = .75)
mod_train <- subset(chk, sample == TRUE)
mod_test <- subset(chk, sample == FALSE)


#Random forest
library(randomForest)
fit <- randomForest(Employment.Status ~ ., data = mod_train, importance = TRUE)
varImpPlot(fit)
importance(fit)
summary(mod_train$Weekly.Earnings)

test_orig <- mod_test$Employment.Status
mod_test$Employment.Status <- NULL
pred <-predict(fit,mod_test, type = "response")
head(pred)
table(test_orig, pred)
prop.table(table(test_orig, pred))




str(mod_train)
str(Test.Dataset)
test <- subset(Test.Dataset, Test.Dataset$Weekly.Earnings == 0 & Test.Dataset$Weekly.Hours.Worked == 0 )
dim(test)
dim(Test.Dataset)
test$Education.Level <- as.character(test$Education.Level)
test$Education.Level[test$Education.Level == '10th grade'] <- '< High School'
test$Education.Level[test$Education.Level == '11th grade'] <- '< High School'
test$Education.Level[test$Education.Level == '12th grade'] <- '< High School'
test$Education.Level[test$Education.Level == '9th grade'] <- '< High School'
test$Education.Level[test$Education.Level == 'High School'] <- '< High School'
test$Education.Level[test$Education.Level == 'Associate Degree'] <- 'HS/GED/Voc School'
test$Education.Level[test$Education.Level == 'Some College'] <- 'HS/GED/Voc School'
test$Education.Level[test$Education.Level == 'Bachelor'] <- 'College Graduate'
test$Education.Level[test$Education.Level == 'Master'] <- 'Masters/PHD/Prof'
test$Education.Level[test$Education.Level == 'Prof. Degree'] <- 'Masters/PHD/Prof'
test$Education.Level[test$Education.Level == 'Doctoral Degree'] <- 'Masters/PHD/Prof'

table(test$Education.Level)
test$Education.Level <- as.factor(test$Education.Level)


test$Pred <- predict(fit, test, type = "response")
summary(test)
table(test$Pred)

#GBM
fit3 <- gbm(Employment.Status ~ .,
            distribution = "multinomial",
            data = mod_train,
            n.trees = 2500,
            shrinkage = .01,
            n.minobsinnode = 20)
summary(fit3)
fit3
str(tr)

#Train
pred2 = data.frame(predict(object = fit3,
                           newdata = mod_train,
                           n.trees = 1500,
                           type = "response"))
pred2$op <- ifelse(pred2$Employed.1500 > pred2$Unemployed.1500, "Employed", "Unemployed")
prop.table(table(mod_train$Employment.Status, pred2$op))

#Test
pred2 = data.frame(predict(object = fit3,
                           newdata = mod_test,
                           n.trees = 1500,
                           type = "response"))
pred2$op <- ifelse(pred2$Employed.1500 > pred2$Unemployed.1500, "Employed", "Unemployed")
prop.table(table(test_orig, pred2$op))


Pred_GBM = data.frame(predict(object = fit3,
                           newdata = test,
                           n.trees = 1500,
                           type = "response"))
test$Pred_GBM <- ifelse(Pred_GBM$Employed.1500 > Pred_GBM$Unemployed.1500, "Employed", "Unemployed")
summary(test)
table(test$Pred_GBM)
head(test)
dim(test)
write.table(test, "C:/Education/IAS challenge/mydata.txt", sep="\t")

#Gains chart
#Random Forest
library(rpart)
library(ROCR)
v <- c(pred = pred)
head(pred)
head(test_orig)
predictionnn <- prediction(v, test_orig)
gain <- performance(predictionnn,"tpr","fpr")
plot(gain, col="orange", lwd=2)
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)



#Gains chart
#GBM
pred2$op <- as.factor(pred2$op)
v2 <- c(pred = pred2$op)
head(v)
predictionnn2 <- prediction(v2, mod_train$Employment.Status)
gain2 <- performance(predictionnn2,"tpr","fpr")

gain2.x = unlist(slot(gain2, 'x.values'))
gain2.y = unlist(slot(gain2, 'y.values'))

lines(x=gain2.x, y=gain2.y, col="dark green", lwd=2)

