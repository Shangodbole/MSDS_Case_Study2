---
title: "CaseStudy2"
author: "Anand R and Shantanu G"
date: "November 22, 2018"
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
##  $ ID                      : int  491 74 678 895 87 855 66 268 119 1065 ...
##  $ Age                     : int  29 35 45 25 35 34 52 56 34 41 ...
##  $ Attrition               : Factor w/ 2 levels "No","Yes": 1 1 1 2 1 1 1 1 1 1 ...
##  $ BusinessTravel          : Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 3 3 3 3 2 3 1 3 3 ...
##  $ DailyRate               : int  1090 890 1005 383 809 829 621 667 937 337 ...
##  $ Department              : Factor w/ 3 levels "Human Resources",..: 3 3 2 3 2 2 3 2 3 3 ...
##  $ DistanceFromHome        : int  10 2 28 9 16 15 3 1 1 8 ...
##  $ Education               : int  3 3 2 2 3 3 4 4 3 3 ...
##  $ EducationField          : Factor w/ 6 levels "Human Resources",..: 3 3 6 2 4 4 3 2 3 3 ...
##  $ EmployeeCount           : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ EmployeeNumber          : int  766 49 1719 1439 14 1485 776 2026 1950 1909 ...
##  $ EnvironmentSatisfaction : int  4 4 4 1 1 2 3 3 1 3 ...
##  $ Gender                  : Factor w/ 2 levels "Female","Male": 2 1 1 2 2 2 2 2 2 1 ...
##  $ HourlyRate              : int  83 97 48 68 84 71 31 57 32 54 ...
##  $ JobInvolvement          : int  3 3 2 2 4 3 2 3 3 3 ...
##  $ JobLevel                : int  1 1 4 1 1 4 4 2 3 2 ...
##  $ JobRole                 : Factor w/ 9 levels "Healthcare Representative",..: 9 9 6 9 3 6 4 1 8 8 ...
##  $ JobSatisfaction         : int  2 4 2 1 2 1 1 3 4 2 ...
##  $ MaritalStatus           : Factor w/ 3 levels "Divorced","Married",..: 1 2 3 2 2 1 2 1 3 2 ...
##  $ MonthlyIncome           : int  2297 2014 16704 4400 2426 17007 16856 6306 9888 4393 ...
##  $ MonthlyRate             : int  17967 9687 17119 15182 16479 11929 10084 26236 6770 26841 ...
##  $ NumCompaniesWorked      : int  1 1 1 3 0 7 1 1 1 5 ...
##  $ Over18                  : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ OverTime                : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ PercentSalaryHike       : int  14 13 11 12 13 14 11 21 21 21 ...
##  $ PerformanceRating       : int  3 3 3 3 3 3 3 4 4 4 ...
##  $ RelationshipSatisfaction: int  4 1 3 1 3 4 1 1 1 3 ...
##  $ StandardHours           : int  80 80 80 80 80 80 80 80 80 80 ...
##  $ StockOptionLevel        : int  2 0 0 0 1 2 0 1 0 1 ...
##  $ TotalWorkingYears       : int  2 2 21 6 6 16 34 13 14 14 ...
##  $ TrainingTimesLastYear   : int  2 3 2 2 5 3 3 2 3 3 ...
##  $ WorkLifeBalance         : int  3 3 3 3 3 2 4 2 2 3 ...
##  $ YearsAtCompany          : int  2 2 21 3 5 14 34 13 14 5 ...
##  $ YearsInCurrentRole      : int  2 2 6 2 4 8 6 12 8 4 ...
##  $ YearsSinceLastPromotion : int  2 2 8 2 0 6 1 1 2 1 ...
##  $ YearsWithCurrManager    : int  2 2 6 2 3 9 16 9 1 4 ...
##  $ Rand                    : num  1.431 -0.845 1.242 -0.813 -0.744 ...
```

```r
summary(dfTrain)
```

```
##        ID              Age        Attrition           BusinessTravel
##  Min.   :   2.0   Min.   :18.00   No :731   Non-Travel       : 80   
##  1st Qu.: 283.0   1st Qu.:31.00   Yes:146   Travel_Frequently:164   
##  Median : 573.0   Median :35.00             Travel_Rarely    :633   
##  Mean   : 577.5   Mean   :37.11                                     
##  3rd Qu.: 876.0   3rd Qu.:43.00                                     
##  Max.   :1170.0   Max.   :60.00                                     
##                                                                     
##    DailyRate                       Department  DistanceFromHome
##  Min.   : 103.0   Human Resources       : 32   Min.   : 1.000  
##  1st Qu.: 470.0   Research & Development:577   1st Qu.: 2.000  
##  Median : 807.0   Sales                 :268   Median : 7.000  
##  Mean   : 805.6                                Mean   : 9.291  
##  3rd Qu.:1157.0                                3rd Qu.:15.000  
##  Max.   :1498.0                                Max.   :29.000  
##                                                                
##    Education              EducationField EmployeeCount EmployeeNumber
##  Min.   :1.000   Human Resources :  9    Min.   :1     Min.   :  11  
##  1st Qu.:2.000   Life Sciences   :385    1st Qu.:1     1st Qu.: 474  
##  Median :3.000   Marketing       : 92    Median :1     Median : 996  
##  Mean   :2.914   Medical         :276    Mean   :1     Mean   :1014  
##  3rd Qu.:4.000   Other           : 42    3rd Qu.:1     3rd Qu.:1541  
##  Max.   :5.000   Technical Degree: 73    Max.   :1     Max.   :2064  
##                                                                      
##  EnvironmentSatisfaction    Gender      HourlyRate     JobInvolvement 
##  Min.   :1.000           Female:350   Min.   : 30.00   Min.   :1.000  
##  1st Qu.:2.000           Male  :527   1st Qu.: 49.00   1st Qu.:2.000  
##  Median :3.000                        Median : 66.00   Median :3.000  
##  Mean   :2.723                        Mean   : 66.29   Mean   :2.717  
##  3rd Qu.:4.000                        3rd Qu.: 84.00   3rd Qu.:3.000  
##  Max.   :4.000                        Max.   :100.00   Max.   :4.000  
##                                                                       
##     JobLevel                          JobRole    JobSatisfaction
##  Min.   :1.000   Sales Executive          :203   Min.   :1.000  
##  1st Qu.:1.000   Research Scientist       :177   1st Qu.:2.000  
##  Median :2.000   Laboratory Technician    :157   Median :3.000  
##  Mean   :2.062   Manufacturing Director   : 90   Mean   :2.734  
##  3rd Qu.:3.000   Healthcare Representative: 77   3rd Qu.:4.000  
##  Max.   :5.000   Manager                  : 56   Max.   :4.000  
##                  (Other)                  :117                  
##   MaritalStatus MonthlyIncome    MonthlyRate    NumCompaniesWorked Over18 
##  Divorced:204   Min.   : 1081   Min.   : 2094   Min.   :0.000      Y:877  
##  Married :395   1st Qu.: 2904   1st Qu.: 8039   1st Qu.:1.000             
##  Single  :278   Median : 4998   Median :14242   Median :2.000             
##                 Mean   : 6454   Mean   :14268   Mean   :2.749             
##                 3rd Qu.: 8020   3rd Qu.:20366   3rd Qu.:4.000             
##                 Max.   :19999   Max.   :26968   Max.   :9.000             
##                                                                           
##  OverTime  PercentSalaryHike PerformanceRating RelationshipSatisfaction
##  No :627   Min.   :11.00     Min.   :3.000     Min.   :1.000           
##  Yes:250   1st Qu.:12.00     1st Qu.:3.000     1st Qu.:2.000           
##            Median :14.00     Median :3.000     Median :3.000           
##            Mean   :15.13     Mean   :3.154     Mean   :2.721           
##            3rd Qu.:18.00     3rd Qu.:3.000     3rd Qu.:4.000           
##            Max.   :25.00     Max.   :4.000     Max.   :4.000           
##                                                                        
##  StandardHours StockOptionLevel TotalWorkingYears TrainingTimesLastYear
##  Min.   :80    Min.   :0.0000   Min.   : 0.00     Min.   :0.000        
##  1st Qu.:80    1st Qu.:0.0000   1st Qu.: 6.00     1st Qu.:2.000        
##  Median :80    Median :1.0000   Median :10.00     Median :3.000        
##  Mean   :80    Mean   :0.8198   Mean   :11.26     Mean   :2.747        
##  3rd Qu.:80    3rd Qu.:1.0000   3rd Qu.:15.00     3rd Qu.:3.000        
##  Max.   :80    Max.   :3.0000   Max.   :40.00     Max.   :6.000        
##                                                                        
##  WorkLifeBalance YearsAtCompany   YearsInCurrentRole
##  Min.   :1.000   Min.   : 0.000   Min.   : 0.000    
##  1st Qu.:2.000   1st Qu.: 3.000   1st Qu.: 2.000    
##  Median :3.000   Median : 5.000   Median : 3.000    
##  Mean   :2.758   Mean   : 7.074   Mean   : 4.281    
##  3rd Qu.:3.000   3rd Qu.:10.000   3rd Qu.: 7.000    
##  Max.   :4.000   Max.   :40.000   Max.   :18.000    
##                                                     
##  YearsSinceLastPromotion YearsWithCurrManager      Rand         
##  Min.   : 0.000          Min.   : 0.000       Min.   :-3.01938  
##  1st Qu.: 0.000          1st Qu.: 2.000       1st Qu.:-0.64384  
##  Median : 1.000          Median : 3.000       Median : 0.01784  
##  Mean   : 2.219          Mean   : 4.146       Mean   : 0.02330  
##  3rd Qu.: 3.000          3rd Qu.: 7.000       3rd Qu.: 0.65087  
##  Max.   :15.000          Max.   :17.000       Max.   : 3.02486  
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

corr_matrix=cor(dfTrain[sapply(dfTrain, is.numeric)])

#This function searches through a correlation matrix and returns a vector of 
#integers corresponding to columns to remove to reduce pair-wise correlations.

highCorr = findCorrelation(corr_matrix, cutoff=0.50)

highCorr
```

```
## [1] 11 10 12 14
```

```r
#10 11 and 12

#Correlation diagram

pairs (~ MonthlyIncome + MonthlyRate + HourlyRate ,
       data = dfTrain ,col=4)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
pairs (~EducationField + EnvironmentSatisfaction #+ Gender,
      ,data = dfTrain ,col=6)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

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
## OverTime                    0.6454
## MonthlyIncome               0.6356
## StockOptionLevel            0.6310
## JobLevel                    0.6291
## MaritalStatus               0.6287
## YearsAtCompany              0.6279
## TotalWorkingYears           0.6264
## YearsWithCurrManager        0.6228
## YearsInCurrentRole          0.6205
## Age                         0.6031
## EnvironmentSatisfaction     0.5959
## JobInvolvement              0.5893
## DistanceFromHome            0.5604
## TrainingTimesLastYear       0.5602
## JobSatisfaction             0.5577
## NumCompaniesWorked          0.5570
## WorkLifeBalance             0.5413
## JobRole                     0.5399
## BusinessTravel              0.5376
## Department                  0.5341
```

```r
head(dfTrain)
```

```
##      ID Age Attrition    BusinessTravel DailyRate             Department
## 491 491  29        No     Travel_Rarely      1090                  Sales
## 74   74  35        No     Travel_Rarely       890                  Sales
## 678 678  45        No     Travel_Rarely      1005 Research & Development
## 895 895  25       Yes     Travel_Rarely       383                  Sales
## 87   87  35        No     Travel_Rarely       809 Research & Development
## 855 855  34        No Travel_Frequently       829 Research & Development
##     DistanceFromHome Education   EducationField EnvironmentSatisfaction
## 491               10         3        Marketing                       4
## 74                 2         3        Marketing                       4
## 678               28         2 Technical Degree                       4
## 895                9         2    Life Sciences                       1
## 87                16         3          Medical                       1
## 855               15         3          Medical                       2
##     Gender HourlyRate JobInvolvement JobLevel               JobRole
## 491   Male         83              3        1  Sales Representative
## 74  Female         97              3        1  Sales Representative
## 678 Female         48              2        4     Research Director
## 895   Male         68              2        1  Sales Representative
## 87    Male         84              4        1 Laboratory Technician
## 855   Male         71              3        4     Research Director
##     JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate
## 491               2      Divorced          2297       17967
## 74                4       Married          2014        9687
## 678               2        Single         16704       17119
## 895               1       Married          4400       15182
## 87                2       Married          2426       16479
## 855               1      Divorced         17007       11929
##     NumCompaniesWorked OverTime PercentSalaryHike PerformanceRating
## 491                  1       No                14                 3
## 74                   1       No                13                 3
## 678                  1       No                11                 3
## 895                  3       No                12                 3
## 87                   0       No                13                 3
## 855                  7       No                14                 3
##     RelationshipSatisfaction StockOptionLevel TotalWorkingYears
## 491                        4                2                 2
## 74                         1                0                 2
## 678                        3                0                21
## 895                        1                0                 6
## 87                         3                1                 6
## 855                        4                2                16
##     TrainingTimesLastYear WorkLifeBalance YearsAtCompany
## 491                     2               3              2
## 74                      3               3              2
## 678                     2               3             21
## 895                     2               3              3
## 87                      5               3              5
## 855                     3               2             14
##     YearsInCurrentRole YearsSinceLastPromotion YearsWithCurrManager
## 491                  2                       2                    2
## 74                   2                       2                    2
## 678                  6                       8                    6
## 895                  2                       2                    2
## 87                   4                       0                    3
## 855                  8                       6                    9
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



```r
#Using Pairs comparison to find correlation
pairs (~ MonthlyIncome + MonthlyRate + HourlyRate ,
       data = dfTrain)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
pairs (~Education +EnvironmentSatisfaction + JobInvolvement,
       data = dfTrain)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->
* Scatter plots on finding correlation between attributes

####1.  Correlation between Monthly Income, Monthly Rate and Hourly Rate

####2.  Correlation between Education, EnvironmentSatisfacton, JobInvolvement

#### GG Scatter plot for Income and Worklife balance

```r
gg1 <- ggplot(dfTrain, aes(x=WorkLifeBalance,                                     y=MonthlyIncome, color = Attrition)) + 
       geom_point() + 
         labs(subtitle="Income vs Work Life Balance", 
          y="Income", 
          x="WorkLife Balance", 
          title="Scatterplot")

plot(gg1)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Attrition per Education & department 
gg2 <- ggplot(dfTrain, aes(x=Education, y=Department, color = Attrition)) + 
  geom_point() + 
    labs(subtitle="Education and Department", 
       y="Department", 
       x="Education", 
       title="Scatterplot")

plot(gg2)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

#### Plots to show the Attrition based on monthly income and Job levels.
Following are violin plots that show attrition
1. Monthly income
2. Monthly income and Job level.


```r
#GG violin between monthly income, and attrition

gg3 <- ggplot(dfTrain,aes(Attrition,MonthlyIncome, fill=Attrition, color=Attrition))+
   geom_violin()+ 
     labs(subtitle="Attrition and Monthly Income",
       y="Attrition",       x="MonthlyIncome",
       title="Violin")
plot(gg3)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#scatter plot between monthly income, JobLevel and attrition
gg4 <- ggplot(training, aes(x=training$MonthlyIncome, y=training$JobLevel, color = Attrition)) + 
  geom_point() + 
    labs(subtitle="Monthly Income and Joblevel",
       y="JobLevel",
       x="Monthly Income",
       title="Scatterplot")
plot(gg4)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

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
  labs(title=" Years at company and income level", x="Years in service", y="Monthly Income") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x=element_text(angle=90, size=7), 
        legend.position="none") +     
  scale_fill_manual(values=grad)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#boxplot between Age and attrition
gg5 <- ggplot(training, aes(x=Attrition, y=Age, color = Attrition)) + 
  geom_boxplot() + 
    labs(subtitle="Age and Attrition",
       y="Age",
       x="Attrition",
       title="Box Plot")
plot(gg5)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# Scatter plot on Years at company and Age
ggplot(dfTrain, aes(x=YearsAtCompany, y=Age, color=YearsAtCompany)) + 
  geom_point(size=1.3, na.rm=TRUE) + 
 # geom_smooth(method=lm, na.rm=TRUE, se=FALSE, color="blue") +
  labs(title="Years at company and Age", x="Years at Company", y="Age of Employee") +
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  scale_color_gradient(low = "#ffbf00", high = "blue")
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
# Years Since Last promotion and Attrition
gg6 <- ggplot(dfTrain, aes(y=YearsSinceLastPromotion, x=Attrition, color = Attrition)) + 
  geom_boxplot() + 
    labs(subtitle="Years Since Last promotion and Attrition",
       y="Years Since last promotion",
       x="Attrition",
       title="Box Plot")
plot(gg6)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
# Jitter plot on Marital Status and Attrition

gg7 <- ggplot(dfTrain, aes(x=MaritalStatus, y=Attrition, color = Attrition)) + 
  geom_jitter() + 
    labs(subtitle="Marital Status and Attrition",
       y="Attrition",
       x="Marital Status",
       title="Jitter Plot")
plot(gg7)
```

![](CaseStudy2_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

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
dfts$predictedAttrition <- results
  
table(dfts$Attrition,dfts$predictedAttrition)
```

```
##      
##        No Yes
##   No  249   2
##   Yes  40   2
```

```r
#Confushion Matrix creation
cm <- caret::confusionMatrix(table(dfts$Attrition,dfts$predictedAttrition))
cm
```

```
## Confusion Matrix and Statistics
## 
##      
##        No Yes
##   No  249   2
##   Yes  40   2
##                                           
##                Accuracy : 0.8567          
##                  95% CI : (0.8112, 0.8947)
##     No Information Rate : 0.9863          
##     P-Value [Acc > NIR] : 1               
##                                           
##                   Kappa : 0.0636          
##  Mcnemar's Test P-Value : 1.135e-08       
##                                           
##             Sensitivity : 0.86159         
##             Specificity : 0.50000         
##          Pos Pred Value : 0.99203         
##          Neg Pred Value : 0.04762         
##              Prevalence : 0.98635         
##          Detection Rate : 0.84983         
##    Detection Prevalence : 0.85666         
##       Balanced Accuracy : 0.68080         
##                                           
##        'Positive' Class : No              
## 
```

```r
model_accuracy_knn=sum(dfts$predictedAttrition == dfts$Attrition)/nrow(dfts)

model_accuracy_knn
```

```
## [1] 0.8566553
```

```r
#Around 83 percent

dfPreds <- data.frame(dfts$ID,dfts$predictedAttrition)
write.csv(dfPreds, file="dfPreds.csv")
```

###Conclusion:

Model accuracy is .86 ie 86% of the times the data will predict attrition of employee correctly.
Further it is only good with the current data set and trends within the data. If the data composition changes over time, the program would compute a different set that would predict the outcome differently.

=======
###Prepared by Anand Rajan and Shantanu Godbole from DDSAnalytics
=======
