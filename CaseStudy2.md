---
title: "CaseStudy2"
author: "Anand R and Shantanu G"
date: "December 2, 2018"
output: 
  html_document:
    keep_md: yes
---
####1. Setting environment and parameters


### Background
We are from DDSAnalytics a talent management company specializing in talent management solutions. We do solutions for workforce planning, employee training programs, identifying high-potential employees and reducing/preventing voluntary employee turnover (attrition).


### About our client

Our client Anheuser-Busch is one of the top beer producers in the world with the following vitals to boast. Head quartered in St. Louis MO, the company brews more than 100 brands of beers with flagship brands Budweiser and Bud Light.
Budweiser has an issue with employee attrition and improving employee morale to gain competitive edge in its industry. We are conducting an analysis using the existing employee data.


### Pitfalls of High attrition
1. Losing talented employee with the company's business knowledge
2. Replacing with new employee costs
3. Replaced employee needs to be trained in business
4. Recruiting costs

### Analysis
The goal is to ascertain the HR problem of employee attrition and how we can predict the rate through Machine Learning model.
We were supplied with data set of 1170 rows of data with 34 columns, which we will use to
do the exploratory data analysis and predic the top 3 attributes that would affect the attrition.

  The following steps will be done in that order.

1. Conduct exploratory data analysis
2. Create training and test data sets
3. Build knn classification model to train the training data provided.
4. Minimise the top 10 columns that comprise the model to predict with accuracy
5. Run it thru again to check if they can give top 3 variables that affect attrition
6. Evaluation of the model - we will do several iterations to come up with changing k values
7. Conclusion - draw inferences from the model

### 1. Exploratory Data Analysis


```r
setwd("C:/Anands/DS_HW/GIT/ExampleProject/MSDS6306-CaseStudy2")

training <- read.csv("DataFiles/CaseStudy2-data.csv")

#Exploratory data Analysis

# Split training and test sets for our analysis and model
samplesize_attrition = nrow(training)

train_perc = .75
train_indices = sample(seq(1,samplesize_attrition,length = samplesize_attrition),train_perc*samplesize_attrition)
dfTrain = training[train_indices,]
dfVal  = training[-train_indices,]

#Write CSV file to destination folder

write.csv(dfTrain, file ="dfTrain.csv")
write.csv(dfVal, file="dfVal.csv")

str(dfTrain)
```

```
## 'data.frame':	877 obs. of  37 variables:
##  $ ID                      : int  174 1123 788 920 936 123 588 732 990 671 ...
##  $ Age                     : int  38 36 30 29 41 54 34 38 55 33 ...
##  $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 1 3 3 1 3 3 2 3 3 ...
##  $ DailyRate               : int  201 1229 1358 1389 256 397 404 653 1117 392 ...
##  $ Department              : Factor w/ 3 levels "Human Resources",..: 2 3 3 2 3 1 2 2 3 3 ...
##  $ DistanceFromHome        : int  10 8 16 21 10 19 2 29 18 2 ...
##  $ Education               : int  3 4 1 4 2 4 4 5 5 4 ...
##  $ EducationField          : Factor w/ 6 levels "Human Resources",..: 4 6 2 2 4 4 6 2 2 4 ...
##  $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ EmployeeNumber          : int  2015 990 1479 20 1329 698 1383 79 597 1670 ...
##  $ EnvironmentSatisfaction : int  2 1 4 2 3 3 3 4 1 4 ...
##  $ Gender                  : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 1 1 1 2 ...
##  $ HourlyRate              : int  99 84 96 51 40 88 98 50 83 93 ...
##  $ JobInvolvement          : int  1 3 3 4 1 3 3 3 3 3 ...
##  $ JobLevel                : int  3 2 2 3 2 3 2 2 4 2 ...
##  $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 6 8 8 5 8 2 1 3 4 8 ...
##  $ JobSatisfaction         : int  3 4 3 1 2 2 4 4 2 4 ...
##  $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 2 1 2 1 3 2 3 3 3 1 ...
##  $ MonthlyIncome           : int  13206 5079 5301 9980 6151 10725 6687 2406 16835 5505 ...
##  $ MonthlyRate             : int  3376 25952 2939 10195 22074 6729 6163 5456 9873 3921 ...
##  $ NumCompaniesWorked      : int  3 4 8 1 1 2 1 1 3 1 ...
##  $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ PercentSalaryHike       : int  12 13 15 11 13 15 11 11 23 14 ...
##  $ PerformanceRating       : int  3 3 3 3 3 3 3 3 4 3 ...
##  $ RelationshipSatisfaction: int  1 4 3 3 1 3 4 4 4 3 ...
##  $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
##  $ StockOptionLevel        : int  1 2 2 1 0 1 0 0 0 2 ...
##  $ TotalWorkingYears       : int  20 12 4 10 19 16 14 10 37 6 ...
##  $ TrainingTimesLastYear   : int  3 3 2 1 4 1 2 2 2 5 ...
##  $ WorkLifeBalance         : int  3 3 2 3 3 4 4 3 3 3 ...
##  $ YearsAtCompany          : int  18 7 2 10 19 9 14 10 10 6 ...
##  $ YearsInCurrentRole      : int  16 7 1 9 2 7 11 3 9 2 ...
##  $ YearsSinceLastPromotion : int  1 0 2 8 11 7 4 9 7 0 ...
##  $ YearsWithCurrManager    : int  11 7 2 8 9 1 11 9 7 4 ...
##  $ Rand                    : num  0.407 -0.777 0.856 -0.15 0.217 ...
```

```r
summary(dfTrain)
```

```
##        ID              Age        Attrition           BusinessTravel
##  Min.   :   1.0   Min.   :18.00   No :738   Non-Travel       : 90   
##  1st Qu.: 294.0   1st Qu.:30.00   Yes:139   Travel_Frequently:176   
##  Median : 592.0   Median :35.00             Travel_Rarely    :611   
##  Mean   : 584.2   Mean   :36.62                                     
##  3rd Qu.: 870.0   3rd Qu.:42.00                                     
##  Max.   :1170.0   Max.   :60.00                                     
##                                                                     
##    DailyRate                     Department  DistanceFromHome
##  Min.   : 103   Human Resources       : 31   Min.   : 1.000  
##  1st Qu.: 470   Research & Development:577   1st Qu.: 2.000  
##  Median : 819   Sales                 :269   Median : 7.000  
##  Mean   : 816                                Mean   : 9.327  
##  3rd Qu.:1171                                3rd Qu.:15.000  
##  Max.   :1499                                Max.   :29.000  
##                                                              
##    Education              EducationField EmployeeCount EmployeeNumber
##  Min.   :1.000   Human Resources : 11    Min.   :1     Min.   :   2  
##  1st Qu.:2.000   Life Sciences   :372    1st Qu.:1     1st Qu.: 492  
##  Median :3.000   Marketing       : 95    Median :1     Median :1029  
##  Mean   :2.897   Medical         :274    Mean   :1     Mean   :1028  
##  3rd Qu.:4.000   Other           : 48    3rd Qu.:1     3rd Qu.:1554  
##  Max.   :5.000   Technical Degree: 77    Max.   :1     Max.   :2065  
##                                                                      
##  EnvironmentSatisfaction    Gender      HourlyRate     JobInvolvement 
##  Min.   :1.000           Female:340   Min.   : 30.00   Min.   :1.000  
##  1st Qu.:2.000           Male  :537   1st Qu.: 48.00   1st Qu.:2.000  
##  Median :3.000                        Median : 66.00   Median :3.000  
##  Mean   :2.717                        Mean   : 65.93   Mean   :2.724  
##  3rd Qu.:4.000                        3rd Qu.: 83.00   3rd Qu.:3.000  
##  Max.   :4.000                        Max.   :100.00   Max.   :4.000  
##                                                                       
##     JobLevel                          JobRole    JobSatisfaction
##  Min.   :1.000   Sales Executive          :199   Min.   :1.000  
##  1st Qu.:1.000   Research Scientist       :187   1st Qu.:2.000  
##  Median :2.000   Laboratory Technician    :159   Median :3.000  
##  Mean   :2.027   Manufacturing Director   : 88   Mean   :2.717  
##  3rd Qu.:3.000   Healthcare Representative: 69   3rd Qu.:4.000  
##  Max.   :5.000   Manager                  : 55   Max.   :4.000  
##                  (Other)                  :120                  
##   MaritalStatus MonthlyIncome    MonthlyRate    NumCompaniesWorked Over18 
##  Divorced:201   Min.   : 1081   Min.   : 2094   Min.   :0.000      Y:877  
##  Married :395   1st Qu.: 2926   1st Qu.: 8045   1st Qu.:1.000             
##  Single  :281   Median : 4998   Median :14377   Median :2.000             
##                 Mean   : 6359   Mean   :14260   Mean   :2.675             
##                 3rd Qu.: 7988   3rd Qu.:20232   3rd Qu.:4.000             
##                 Max.   :19999   Max.   :26968   Max.   :9.000             
##                                                                           
##  OverTime  PercentSalaryHike PerformanceRating RelationshipSatisfaction
##  No :622   Min.   :11.00     Min.   :3.000     Min.   :1.000           
##  Yes:255   1st Qu.:12.00     1st Qu.:3.000     1st Qu.:2.000           
##            Median :14.00     Median :3.000     Median :3.000           
##            Mean   :15.15     Mean   :3.153     Mean   :2.729           
##            3rd Qu.:18.00     3rd Qu.:3.000     3rd Qu.:4.000           
##            Max.   :25.00     Max.   :4.000     Max.   :4.000           
##                                                                        
##  StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear
##  Min.   :80    Min.   :0.0000   Min.   : 0.00     Min.   :0.000        
##  1st Qu.:80    1st Qu.:0.0000   1st Qu.: 6.00     1st Qu.:2.000        
##  Median :80    Median :1.0000   Median :10.00     Median :3.000        
##  Mean   :80    Mean   :0.8358   Mean   :11.03     Mean   :2.766        
##  3rd Qu.:80    3rd Qu.:1.0000   3rd Qu.:14.00     3rd Qu.:3.000        
##  Max.   :80    Max.   :3.0000   Max.   :40.00     Max.   :6.000        
##                                                                        
##  WorkLifeBalance YearsAtCompany   YearsInCurrentRole
##  Min.   :1.000   Min.   : 0.000   Min.   : 0.00     
##  1st Qu.:2.000   1st Qu.: 3.000   1st Qu.: 2.00     
##  Median :3.000   Median : 5.000   Median : 3.00     
##  Mean   :2.781   Mean   : 6.917   Mean   : 4.26     
##  3rd Qu.:3.000   3rd Qu.: 9.000   3rd Qu.: 7.00     
##  Max.   :4.000   Max.   :40.000   Max.   :18.00     
##                                                     
##  YearsSinceLastPromotion YearsWithCurrManager      Rand          
##  Min.   : 0.000          Min.   : 0.000       Min.   :-2.631626  
##  1st Qu.: 0.000          1st Qu.: 2.000       1st Qu.:-0.690335  
##  Median : 1.000          Median : 3.000       Median : 0.001517  
##  Mean   : 2.146          Mean   : 4.119       Mean   :-0.033544  
##  3rd Qu.: 3.000          3rd Qu.: 7.000       3rd Qu.: 0.611882  
##  Max.   :15.000          Max.   :17.000       Max.   : 3.024861  
## 
```

```r
dim(dfVal)
```

```
## [1] 293  37
```

```r
dim(dfTrain)
```

```
## [1] 877  37
```

```r
# Removing columns which do not vary as well as the random column

dfTrain$Over18 <- NULL
dfTrain$EmployeeCount <- NULL
dfTrain$StandardHours <- NULL
dfTrain$EmployeeNumber <- NULL
dfTrain$Rand <- NULL

# Convert foloowing columns as factors
dfTrain$Education <- as.factor(dfTrain$Education)
dfTrain$EnvironmentSatisfaction <- as.factor(dfTrain$EnvironmentSatisfaction)
dfTrain$JobInvolvement <- as.factor(dfTrain$JobInvolvement)
dfTrain$JobLevel <- as.factor(dfTrain$JobLevel)
dfTrain$JobSatisfaction <- as.factor(dfTrain$JobSatisfaction)
dfTrain$PerformanceRating <- as.factor(dfTrain$PerformanceRating)
dfTrain$RelationshipSatisfaction <- as.factor(dfTrain$RelationshipSatisfaction)
dfTrain$StockOptionLevel <- as.factor(dfTrain$StockOptionLevel)
dfTrain$TrainingTimesLastYear <- as.factor(dfTrain$TrainingTimesLastYear)
dfTrain$WorkLifeBalance <- as.factor(dfTrain$WorkLifeBalance)

# finally assigning the data to a data frame which has clean data for model.

# This optimizes the training model 
trainc <- trainControl(method="repeatedcv", number=5, repeats=5)

#Creates a model for the learning machine which can be used for predictions on new data.

fitknn=train(Attrition~., dfTrain, method="knn", trControl=trainc)

impvars=varImp(fitknn, scale=FALSE)

impvars
```

```
## ROC curve variable importance
## 
##   only 20 most important variables shown (out of 31)
## 
##                         Importance
## OverTime                    0.6649
## MaritalStatus               0.6614
## StockOptionLevel            0.6461
## TotalWorkingYears           0.6393
## YearsAtCompany              0.6349
## Age                         0.6329
## YearsInCurrentRole          0.6295
## MonthlyIncome               0.6220
## YearsWithCurrManager        0.6214
## JobLevel                    0.6176
## JobInvolvement              0.5825
## EnvironmentSatisfaction     0.5677
## DailyRate                   0.5667
## WorkLifeBalance             0.5573
## DistanceFromHome            0.5546
## JobSatisfaction             0.5529
## JobRole                     0.5479
## Department                  0.5454
## HourlyRate                  0.5400
## Gender                      0.5337
```

#### Elimination of columns not affecting the outcome for training set

We eliminate the columns which have no variance.
a. Over18 has no variability, all are Y.
b. EmployeeCount has no variability - all are 1.
c. StandardHours has no variability - all are 80.
d. Employee Number is an identifier.
e. Rand is a column generated and unused in the model

#### Data Exploration

#### Report column NA's

* No NA columns found in data

* Scatter plots on finding correlation between attributes

####1.  Correlation between Monthly Income, Monthly Rate and Hourly Rate

####2.  Correlation between Education, EnvironmentSatisfacton, JobInvolvement


```r
#This function searches through a correlation matrix and returns a vector of 
corr_matrix=cor(dfTrain[sapply(dfTrain, is.numeric)])

#integers corresponding to columns to remove to reduce pair-wise correlations.

highCorr = findCorrelation(corr_matrix, cutoff=0.50)

highCorr
```

```
## [1] 11 10 14 12
```

```r
#Using Pairs comparison to find correlation
#Correlation diagram

pairs (~ MonthlyIncome + MonthlyRate + HourlyRate ,
       data = dfTrain ,col=4, main="fig1: Correlation-Income, MonthlyRate and Hourly rate")
```

![](CaseStudy2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
pairs (~EducationField + EnvironmentSatisfaction #+ Gender,
      ,data = dfTrain ,col=6, main="fig2: Correlation-EducationField and Environment")
```

![](CaseStudy2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

* The Correlation diagrams do not show any significant correlation in the scatter diagram and are good candidates for model.

#### Finding the core columns for the final knn classification model.

```r
# This optimizes the training model 
trainc <- trainControl(method="repeatedcv", number=5, repeats=5)

#Creates a model for the learning machine which can be used for predictions on new data.

fitknn=train(Attrition~., dfTrain, method="knn", trControl=trainc)

impvars=varImp(fitknn, scale=FALSE)

impvars
```

```
## ROC curve variable importance
## 
##   only 20 most important variables shown (out of 31)
## 
##                         Importance
## OverTime                    0.6649
## MaritalStatus               0.6614
## StockOptionLevel            0.6461
## TotalWorkingYears           0.6393
## YearsAtCompany              0.6349
## Age                         0.6329
## YearsInCurrentRole          0.6295
## MonthlyIncome               0.6220
## YearsWithCurrManager        0.6214
## JobLevel                    0.6176
## JobInvolvement              0.5825
## EnvironmentSatisfaction     0.5677
## DailyRate                   0.5667
## WorkLifeBalance             0.5573
## DistanceFromHome            0.5546
## JobSatisfaction             0.5529
## JobRole                     0.5479
## Department                  0.5454
## HourlyRate                  0.5400
## Gender                      0.5337
```

#### Scatter plots association with Attrition
1. fig 3: Scatter plot on Income and Work Life Balance with Attrition
2. fig 4: Scatter plot on Education and Department with Attrition


```r
gg1 <- ggplot(dfTrain, aes(x=WorkLifeBalance,                                     y=MonthlyIncome, color = Attrition)) + 
       geom_point() + 
         labs(subtitle="fig3:Income vs Work Life Balance", 
          y="Income", 
          x="WorkLife Balance", 
          title="Scatterplot")

plot(gg1)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Attrition per Education & department 
gg2 <- ggplot(dfTrain, aes(x=Education, y=Department, color = Attrition)) + 
  geom_point() + 
    labs(subtitle="fig4:Education and Department", 
       y="Department", 
       x="Education", 
       title="Scatterplot")

plot(gg2)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

#### Plots to show the Attrition based on monthly income and Job levels.
Following are violin plots that show attrition
1. Monthly income
2. Monthly income and Job level.


```r
#GG violin between monthly income, and attrition

gg3 <- ggplot(dfTrain,aes(Attrition,MonthlyIncome, fill=Attrition, color=Attrition))+
   geom_violin()+ 
     labs(subtitle="fig5:Attrition and Monthly Income",
       y="Attrition",       x="MonthlyIncome",
       title="Violin")
plot(gg3)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#scatter plot between monthly income, JobLevel and attrition
gg4 <- ggplot(training, aes(x=training$MonthlyIncome, y=training$JobLevel, color = Attrition)) + 
  geom_point() + 
    labs(subtitle="fig6:Monthly Income and Joblevel",
       y="JobLevel",
       x="Monthly Income",
       title="Scatterplot")
plot(gg4)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-2.png)<!-- -->
#Various plots to show Attrition and explanatory variables such as Income, Age, Years of service, Marital status and Years in service with company
1. Bar plot on Years at company and Income level
2. Box plot on Age and attrition
3. Scatter plot on Years at company and Age
4. Box plot on Years Since Last promotion and Attrition
5. Jitter plot on Marital Status and Attrition

```r
# Plot to show Years at company and Income attrition.

grad <- scales::seq_gradient_pal("blue", "blue")(seq(0,1,length.out=100))

ggplot(dfTrain, aes(x=reorder(dfTrain$YearsAtCompany, -dfTrain$YearsAtCompany), y=dfTrain$MonthlyIncome, fill=Attrition)) +
  geom_bar(stat='identity', position='dodge') +
  labs(title=" fig7: Years at company and income level", x="Years in service", y="Monthly Income") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x=element_text(angle=90, size=7), 
        legend.position="none") +     
  scale_fill_manual(values=grad)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#boxplot between Age and attrition
gg5 <- ggplot(training, aes(x=Attrition, y=Age, color = Attrition)) + 
  geom_boxplot() + 
    labs(subtitle="fig8:Age and Attrition",
       y="Age",
       x="Attrition",
       title="Box Plot")
plot(gg5)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# Scatter plot on Years at company and Age
ggplot(dfTrain, aes(x=YearsAtCompany, y=Age, color=YearsAtCompany)) + 
  geom_point(size=1.3, na.rm=TRUE) + 
 # geom_smooth(method=lm, na.rm=TRUE, se=FALSE, color="blue") +
  labs(title="fig9:Years at company and Age", x="Years at Company", y="Age of Employee") +
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  scale_color_gradient(low = "#ffbf00", high = "blue")
```

![](CaseStudy2_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
# Years Since Last promotion and Attrition
gg6 <- ggplot(dfTrain, aes(y=YearsSinceLastPromotion, x=Attrition, color = Attrition)) + 
  geom_boxplot() + 
    labs(subtitle="fig10:Years Since Last promotion and Attrition",
       y="Years Since last promotion",
       x="Attrition",
       title="Box Plot")
plot(gg6)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
# Jitter plot on Marital Status and Attrition

gg7 <- ggplot(dfTrain, aes(x=MaritalStatus, y=Attrition, color = Attrition)) + 
  geom_jitter() + 
    labs(subtitle="fig11:Marital Status and Attrition",
       y="Attrition",
       x="Marital Status",
       title="Jitter Plot")
plot(gg7)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

###Final model for prediction
The model is built based on outcome from ROC which predicts the columns that are correlated with attrition history provided.


```r
# Now the actual model and prediction

kval <- 5
# defining model data frames Training and Test

dftr <- data.frame(dfTrain[, c(1,2,3,14,17,18,21,25,26,29,30,32)]) %>% droplevels()

dfts <- data.frame(dfVal[, c(1,2,3,16,19,20,24,29,30,33,34,36)]) %>% droplevels()

dftr$JobLevel <- as.integer(dftr$JobLevel)
dftr$MaritalStatus <- as.integer(dftr$MaritalStatus)
dftr$OverTime <- as.integer(dftr$OverTime)
dftr$StockOptionLevel <- as.integer(dftr$StockOptionLevel)

dfts$JobLevel <- as.integer(dfts$JobLevel)
dfts$MaritalStatus <- as.integer(dfts$MaritalStatus)
dfts$OverTime <- as.integer(dfts$OverTime)
dfts$StockOptionLevel <- as.integer(dfts$StockOptionLevel)


# Appropriate model after getting the results
results = class::knn(dftr[,c(2,4,5,6,7,8,9,10,11,12)], dfts[,c(2,4,5,6,7,8,9,10,11,12)], dftr$Attrition, k=kval)
# Predict the data thus trained with test set
dfts$PredictedAttrition <- results
  
table(dfts$Attrition,dfts$PredictedAttrition)
```

```
##      
##        No Yes
##   No  244   0
##   Yes  47   2
```

```r
#Confushion Matrix creation
cm <- caret::confusionMatrix(table(dfts$Attrition,dfts$PredictedAttrition))

cmhd<-head(cm$table)

kable(cmhd, row.names=FALSE, caption="Table 1: Confushion Matrix showing prediction") %>% kable_styling(bootstrap_options="bordered")
```

<table class="table table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>Table 1: Confushion Matrix showing prediction</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> No </th>
   <th style="text-align:right;"> Yes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 244 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>

```r
cm
```

```
## Confusion Matrix and Statistics
## 
##      
##        No Yes
##   No  244   0
##   Yes  47   2
##                                           
##                Accuracy : 0.8396          
##                  95% CI : (0.7925, 0.8797)
##     No Information Rate : 0.9932          
##     P-Value [Acc > NIR] : 1               
##                                           
##                   Kappa : 0.0662          
##  Mcnemar's Test P-Value : 1.949e-11       
##                                           
##             Sensitivity : 0.83849         
##             Specificity : 1.00000         
##          Pos Pred Value : 1.00000         
##          Neg Pred Value : 0.04082         
##              Prevalence : 0.99317         
##          Detection Rate : 0.83276         
##    Detection Prevalence : 0.83276         
##       Balanced Accuracy : 0.91924         
##                                           
##        'Positive' Class : No              
## 
```

```r
model_accuracy_knn=sum(dfts$PredictedAttrition == dfts$Attrition)/nrow(dfts)

model_accuracy_knn
```

```
## [1] 0.8395904
```

```r
#Around 83 percent

dfPreds <- data.frame(dfts$ID,dfts$PredictedAttrition)
colnames(dfPreds) <- c("ID","PredictedAttrition")
write.csv(dfPreds, file="dfPreds.csv")
```

###Conclusion:

Model accuracy is .833 ie 83.3% of the times the data will predict attrition of employee correctly.
Further it is only good with the current data set and trends within the data. If the data composition changes over time, the program would compute a different set that would predict the outcome differently.

=======
###Prepared by Anand Rajan and Shantanu Godbole from DDSAnalytics
=======
