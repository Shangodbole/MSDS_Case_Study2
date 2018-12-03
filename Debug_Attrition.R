
# import datasets
# Breweries = read.csv(text = getURL("https://raw.githubusercontent.com/Shangodbole/MSDS_6306_CaseStudy1/master/Breweries.csv"), head = TRUE, sep = ",", row.names = NULL)


#Reading Training and test data


Attrition.data <- read.csv('C:/Anands/DS_HW/Attrition_data.csv',header = TRUE,sep = ",")
AttrValid.data <- read.csv('C:/Anands/DS_HW/CaseStudy2Validation.csv',header = TRUE,sep = ",")
#Explore the data

head(Attrition.data)

x = c(1,2,3,4,5,6,7,8)

#number of columns initialized
col.Attrition <- ncol(Attrition.data)

columns <- seq(2,col.Attrition-1,1)

# model combination columns chosen as 4 at a time

cols.model <- 3
kval <- 5

# Getting all combination columns in to the model

column.combitations <- combn(columns,cols.model) 

# assign dummy columns

Accuracy.table <- as.data.frame(c(1,2),c("A","B"))

Accuracy.table[1,2] <- paste(cols.model,collapse = " ")

combinations <- ncol(column.combitations)

# Create sample size of attrition data
samplesize_attrition = nrow(Attrition.data)

samplesize_attrition

#Separate training and test sets
train_perc = .75
train_indices = sample(seq(1,samplesize_attrition,length = samplesize_attrition),train_perc*samplesize_attrition)
Train.Attrition.data = Attrition.data[train_indices,]
Test.Attrition.data  = Attrition.data[-train_indices,]

# Counts on data
nrow(Train.Attrition.data)
nrow(Test.Attrition.data)

# Looping through all the possible combinations to find the accuracy rate

for (i in 1:combinations)
  { 
  
  classify.Attrition.data <- Train.Attrition.data
  test.classify.Attrition.data <- Test.Attrition.data

  # Run the numerical values for classification with column numbers
  for (j in 1:cols.model){
    
    column.class <- sapply(classify.Attrition.data[1,column.combitations[j,i],1],class)
    if (column.class == "factor"){
      
      column.numeric <- is.numeric(classify.Attrition.data[,column.combitations[j,i]])
      
      # Conversion of data for test
      
      if(column.numeric == FALSE){
        classify.Attrition.data[,column.combitations[j,i]] <- as.integer(as.factor(classify.Attrition.data[,column.combitations[j,i]]))
        test.classify.Attrition.data[,column.combitations[j,i]] <- as.integer(as.factor(test.classify.Attrition.data[,column.combitations[j,i]]))
        
      }
      
    }
    
  }
  
  cols.knn <- column.combitations[,i]
  
  # results = class::knn(classify.Attrition.data[,c(column.combitations[1,i],column.combitations[2,i],column.combitations[3,i])],
  #                      test.classify.Attrition.data[,c(column.combitations[1,i],column.combitations[2,i],column.combitations[3,i])],
  #                      classify.Attrition.data$Attrition,
  #                      k=5)
  
  # Train the data set with KNN classification
  
  results = class::knn(classify.Attrition.data[,cols.knn],
                       test.classify.Attrition.data[,cols.knn],
                       classify.Attrition.data$Attrition, k=kval)
  # Predict the data thus trained with test set
  test.classify.Attrition.data$predictedAttrition <- results
  
  table(test.classify.Attrition.data$Attrition,test.classify.Attrition.data$predictedAttrition)
  
  #Confushion Matrix creation
  cm <- caret::confusionMatrix(table(test.classify.Attrition.data$Attrition,test.classify.Attrition.data$predictedAttrition))
  
  Accuracy.table[i,1] <- cm$overall[1]
  Accuracy.table[i,2] <- paste(cols.knn,collapse = " ")
  
  if (is.integer(i/100) == TRUE){
    Sys.sleep(1)
  }
  print(i)
}

colnames(Accuracy.table) <- c("Accuracy","Column Num")

Accuracy.table<-arrange(Accuracy.table,desc(Accuracy.table$Accuracy))


head(Accuracy.table,10)
head(Accuracy.table$`Column Num`,10)


# 12 columns chosen from this. 2,14,16,17, 22,27,32,33,34,31,24,23

