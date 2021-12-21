# Maximus
#PSI CA1 - Student results Statistics

#Start project
#install these libraries
install.packages("readr") #Importing & export csv
install.packages("pastecs") #descriptive statistic summaries
install.packages("ggplot2") #histograms with more detail
install.packages("semTools") #skewness and kurtosis
install.packages("coin")
install.packages("rstatix")
install.packages("effectsize")
install.packages("sjstats")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("FSA")
install.packages("psych")
install.packages("car")
install.packages("hrbrthemes")
install.packages("viridis")

#load these libraries
library(readr)
library(pastecs) 
library(ggplot2) 
library(semTools) 
library(coin)
library(rstatix)
library(effectsize)
library(sjstats)
library(tidyverse)
library(tidyr)
library(dplyr)
library(FSA)
library(psych)
library(car)
library(hrbrthemes)
library(viridis)

#Import the Dataset
df <- read_csv("C:/Users/maxno/OneDrive/Desktop/PSI_Project/dataset.csv")
View(df)

#check the dataset for NAs
apply(is.na(df),2,sum)

#check for difference between guardian.m & guardian.p which should be the same
df$new_col <- ifelse(df$guardian.m == df$guardian.p, 'A', ifelse(df$guardian.p != df$guardian.m, 'B','C'))
table(df$new_col)
#check for difference between traveltime.m & traveltime.p which should be the same
df$new_col <- ifelse(df$traveltime.m == df$traveltime.p, 'A', ifelse(df$traveltime.p != df$traveltime.m, 'B','C'))
table(df$new_col)
#check for difference between studytime.m & studytime.p which should be the same
df$new_col <- ifelse(df$studytime.m == df$studytime.p, 'A', ifelse(df$studytime.p != df$studytime.m, 'B','C'))
table(df$new_col)
#check for difference between schoolsup.m & schoolsup.p which should be the same
df$new_col <- ifelse(df$schoolsup.m == df$schoolsup.p, 'A', ifelse(df$schoolsup.p != df$schoolsup.m, 'B','C'))
table(df$new_col)
#check for difference between activities.m & activities.p which should be the same
df$new_col <- ifelse(df$activities.m == df$activities.p, 'A', ifelse(df$activities.p != df$activities.m, 'B','C'))
table(df$new_col)
#check for difference between higher.m & higher.p which should be the same
df$new_col <- ifelse(df$higher.m == df$higher.p, 'A', ifelse(df$higher.p != df$higher.m, 'B','C'))
table(df$new_col)
#check for difference between romantic.m & romantic.p which should be the same
df$new_col <- ifelse(df$romantic.m == df$romantic.p, 'A', ifelse(df$romantic.p != df$romantic.m, 'B','C'))
table(df$new_col)
#check for difference between famrel.m & famrel.p which should be the same
df$new_col <- ifelse(df$famrel.m == df$famrel.p, 'A', ifelse(df$famrel.p != df$famrel.m, 'B','C'))
table(df$new_col)
#check for difference between freetime.m & freetime.p which should be the same
df$new_col <- ifelse(df$freetime.m == df$freetime.p, 'A', ifelse(df$freetime.p != df$freetime.m, 'B','C'))
table(df$new_col)
#check for difference between goout.m & goout.p which should be the same
df$new_col <- ifelse(df$goout.m == df$goout.p, 'A', ifelse(df$goout.p != df$goout.m, 'B','C'))
table(df$new_col)
#check for difference between Dalc.m & Dalc.p which should be the same
df$new_col <- ifelse(df$Dalc.m == df$Dalc.p, 'A', ifelse(df$Dalc.p != df$Dalc.m, 'B','C'))
table(df$new_col)
#check for difference between Walc.m & Walc.p which should be the same
df$new_col <- ifelse(df$Walc.m == df$Walc.p, 'A', ifelse(df$Walc.p != df$Walc.m, 'B','C'))
table(df$new_col)
#check for difference between health.m & health.p which should be the same
df$new_col <- ifelse(df$health.m == df$health.p, 'A', ifelse(df$health.p != df$health.m, 'B','C'))
table(df$new_col)
#check for difference between famsup.m & famsup.p which should be the same
df$new_col <- ifelse(df$famsup.m == df$famsup.p, 'A', ifelse(df$famsup.p != df$famsup.m, 'B','C'))
table(df$new_col)

#drop columns after cleaning
df <-select(df, c(-traveltime.p,-guardian.p,-studytime.p,-schoolsup.p,-activities.p,-higher.p,-romantic.p,-famrel.p,-freetime.p,-goout.p,-Dalc.p,-Walc.p,-health.p,-famsup.p,-new_col))
#Renaming of columns
df <- df %>% rename(traveltime=traveltime.m,guardian=guardian.m,studytime=studytime.m,schoolsup=schoolsup.m,activities=activities.m,higher=higher.m,romantic=romantic.m,famrel=famrel.m,freetime=freetime.m,goout=goout.m,dalc=Dalc.m,walc=Walc.m,health=health.m, famsup=famsup.m) 
View(df)

#DONE IN EXPLORATION PHASE - removing all student win 0 value in pG3 & mG3. Shrinking the dataset from 382 -> 340 students.
#NB: DO NOT INCLUDE NEXT TWO LINES TO CARRY OUT THE PREPERATION STEP - Do keep for exploration onwards
tempdf<-df[df$mG3 != 0, ]
df<-tempdf[tempdf$pG3 != 0, ]
str(df)
#Display frequency of mG3 pre dealing with missing 0 values
local({
  .Table <- with(df, table(mG3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#mean of final grade in Maths and Portugese for each student
df$resultmean <- rowMeans(df[,c('mG3', 'pG3')], na.rm=TRUE)

#examine structure of dataset with str(df)
str(df)

#Checking for Mean of new meanresult attribute(Maths & Portugese combined)
x = mean(df$resultmean)
x

#Frequency for Schoolsup
local({
  .Table <- with(df, table(schoolsup))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#explore variables stats
pastecs::stat.desc(df$resultmean, basic=F)
pastecs::stat.desc(df$mG3, basic=F)
pastecs::stat.desc(df$pG3, basic=F)

gg <- ggplot(df, aes(x=mG3))
#Change the label of the x axis
gg <- gg + labs(x="3rd Final Math Result")
#binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#1) adding a normal curve 2)pass the mean and standard deviation 3) use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG3, na.rm=TRUE), sd=sd(df$mG3,na.rm=TRUE)))
gg

#Display frequency of mG3 pre dealing with missing 0 values
local({
  .Table <- with(df, table(mG3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#new combined mean of final grade in Maths and Portugese for each student
df$resultmean <- rowMeans(df[,c('mG3', 'pG3')], na.rm=TRUE)

#Display frequency of mG3 post dealing with missing 0 values
local({
  .Table <- with(df, table(mG3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

#New mG3 summary statistics
pastecs::stat.desc(df$mG3, basic=F)
#Skew and Kurtosis
tpskew<-semTools::skew(df$mG3)
tpkurt<-semTools::kurtosis(df$mG3)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
# scale is a function that creates z scores
amG3<- abs(scale(df$mG3))
#calculating the percentage of standardised scores that are outside acceptable range - "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt")
FSA::perc(as.numeric(amG3), 1.96, "gt")
FSA::perc(as.numeric(amG3), 3.29, "gt")

#Histogram mg3
gg1 <- ggplot(df, aes(x=mG3))
gg1 <- gg1 + labs(x="Fully Cleaned mG3 Results")
gg1 <- gg1 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg1 <- gg1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg1 <- gg1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG3, na.rm=TRUE), sd=sd(df$mG3, na.rm=TRUE)))
gg1

#mG3 qqplot
qqnorm(df$mG3)
qqline(df$mG3, col = 2)

#New pG3 summary statistics
pastecs::stat.desc(df$pG3, basic=F)
#Skew and Kurtosis
tpskew<-semTools::skew(df$pG3)
tpkurt<-semTools::kurtosis(df$pG3)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
# scale is a function that creates z scores
apG3<- abs(scale(df$pG3))
#calculating the percentage of standardised scores that are outside acceptable range - "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt")
FSA::perc(as.numeric(apG3), 1.96, "gt")
FSA::perc(as.numeric(apG3), 3.29, "gt")

#Histogram pg3
gg1 <- ggplot(df, aes(x=pG3))
gg1 <- gg1 + labs(x="pG3 Results Histogram")
gg1 <- gg1 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg1 <- gg1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg1 <- gg1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$pG3, na.rm=TRUE), sd=sd(df$mG3, na.rm=TRUE)))
gg1

#pG3 qqplot
qqnorm(df$pG3)
qqline(df$pG3, col = 2)

#New resultmean summary statistics
pastecs::stat.desc(df$resultmean, basic=F)
#Skew and Kurtosis
tpskew<-semTools::skew(df$resultmean)
tpkurt<-semTools::kurtosis(df$resultmean)
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]
# scale is a function that creates z scores
aresultmean<- abs(scale(df$resultmean))
#calculating the percentage of standardised scores that are outside acceptable range - "gt", greater than or equal "geq", "gt", less than or equal "leq",  or less than "lt")
FSA::perc(as.numeric(aresultmean), 1.96, "gt")
FSA::perc(as.numeric(aresultmean), 3.29, "gt")

#Histogram resultmean
gg1 <- ggplot(df, aes(x=resultmean))
gg1 <- gg1 + labs(x="resultmean Results Histogram")
gg1 <- gg1 + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg1 <- gg1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg1 <- gg1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$resultmean, na.rm=TRUE), sd=sd(df$resultmean, na.rm=TRUE)))
gg1

#resultmean qqplot
qqnorm(df$resultmean)
qqline(df$resultmean, col = 2)

#Pearson's co-orelation test for normal distribution
scatter <- ggplot(df, aes(df$mG3, df$pG3))
#Add regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "mG3", y = "pG3")
stats::cor.test(df$mG3, df$pG3, method='pearson')

#Independent two sample t-test - descriptive statitics by group mG3 & schoolsup
psych::describeBy(df$mG3, df$schoolsup, mat=TRUE)

#Conduct Levene's test for homogeneity of variance in library car - 
##the null hypothesis is that variances in groups are equal so to assume homogeneity we would expect probability to not be statistically significant.
car::leveneTest(mG3 ~ schoolsup, data=df)

#t-test using stats
stats::t.test(mG3~ schoolsup,var.equal=TRUE,data=df)

temp <- stats::t.test(mG3~ schoolsup,var.equal=TRUE,data=df)
#Calculate Cohen's d. artithmetically
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
#Using function from effectsize package
effectsize::t_to_d(t = temp$statistic, temp$parameter)
#Eta squared calculation
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group schoolsup and mG3 difference
df %>%
  ggplot( aes(x=df$schoolsup, y=df$mG3, fill=df$schoolsup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("School Support") +
  ylab("Maths final results") +
  ggtitle("schoolsup and mG3 difference")

#Independent two sample t-test - by group pG3 & schoolsup
psych::describeBy(df$pG3, df$schoolsup, mat=TRUE)
car::leveneTest(pG3 ~ schoolsup, data=df)
stats::t.test(pG3~ schoolsup,var.equal=TRUE,data=df)
temp <- stats::t.test(pG3~ schoolsup,var.equal=TRUE,data=df)
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
effectsize::t_to_d(t = temp$statistic, temp$parameter)
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group schoolsup and mG3 difference
df %>%
  ggplot( aes(x=df$schoolsup, y=df$pG3, fill=df$schoolsup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("School Support") +
  ylab("Portuguese final results") +
  ggtitle("schoolsup and pG3 difference")

#Independent two sample t-test - by group mG3 & paid.m
psych::describeBy(df$mG3, df$paid.m, mat=TRUE)
car::leveneTest(mG3 ~ paid.m, data=df)
stats::t.test(mG3~ paid.m,var.equal=TRUE,data=df)
temp <- stats::t.test(mG3~ paid.m,var.equal=TRUE,data=df)
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
effectsize::t_to_d(t = temp$statistic, temp$parameter)
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group schoolsup and mG3 difference
df %>%
  ggplot( aes(x=df$paid.m, y=df$mG3, fill=df$paid.m)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("Paid Grinds") +
  ylab("Math final results") +
  ggtitle("paid.m and mG3 difference")

#Independent two sample t-test - by group pG3 & paid.p
psych::describeBy(df$pG3, df$paid.p, mat=TRUE)
car::leveneTest(pG3 ~ paid.p, data=df)
stats::t.test(pG3~ paid.p,var.equal=TRUE,data=df)
temp <- stats::t.test(pG3~ paid.p,var.equal=TRUE,data=df)
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
effectsize::t_to_d(t = temp$statistic, temp$parameter)
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group paid grinds and pG3 difference
df %>%
  ggplot( aes(x=df$paid.p, y=df$pG3, fill=df$paid.p)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("Paid Grinds") +
  ylab("Portuguese final results") +
  ggtitle("paid.p and pG3 difference")

#Independent two sample t-test - by group mG3 & higher
psych::describeBy(df$mG3, df$higher, mat=TRUE)
car::leveneTest(mG3 ~ higher, data=df)
stats::t.test(mG3~ higher,var.equal=TRUE,data=df)
temp <- stats::t.test(mG3~ higher,var.equal=TRUE,data=df)
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
effectsize::t_to_d(t = temp$statistic, temp$parameter)
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group higher and mG3 difference
df %>%
  ggplot( aes(x=df$higher, y=df$mG3, fill=df$higher)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("higher education goal") +
  ylab("Math final results") +
  ggtitle("higher and mG3 difference")

#Independent two sample t-test - by group pG3 & higher
psych::describeBy(df$pG3, df$higher, mat=TRUE)
car::leveneTest(pG3 ~ higher, data=df)
stats::t.test(pG3~ higher,var.equal=TRUE,data=df)
temp <- stats::t.test(pG3~ higher,var.equal=TRUE,data=df)
effcd=round((2*temp$statistic)/sqrt(temp$parameter),2)
effectsize::t_to_d(t = temp$statistic, temp$parameter)
effes=round((temp$statistic*temp$statistic)/((temp$statistic*temp$statistic)+(temp$parameter)),3)
effes

#Creating Boxplot for visual display of group higher and pG3 difference
df %>%
  ggplot( aes(x=df$higher, y=df$pG3, fill=df$higher)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(color = "blue", size = 12, hjust=0.5),
    axis.title.y = element_text(color = "blue", size = 12, hjust=0.5),
    legend.position="centre",
    plot.title = element_text(size=11,hjust = 0.5)) +
  xlab("higher education goal") +
  ylab("Portuguese final results") +
  ggtitle("higher and pG3 difference")
