# DataMiningAndVisualization

---
title: "Student Exam Scores"
output: 
#please create a dashboard
  flexdashboard::flex_dashboard:
    vertical_layout: scroll
    source_code: embed
---

```{r setup, include=FALSE, warning=FALSE}
#include=FALSE will not include r code in output
#warning=FALSE will remove any warnings from output
library(flexdashboard)
library(tidyverse)
library(GGally)
library(caret) #for logistic regression
library(broom) #for tidy() function
library(conflicted)
library(rpart)
library(rpart.plot)
```

```{r load_data}
#only reads if the file is located in the same folder as the RMD file
df <- read_csv("JMPProjectData.csv")
```

Introduction {data-orientation=rows}
=======================================================================

Row {data-height=250}
-----------------------------------------------------------------------

### Overview 

For this project, we will follow the DCOVAC process. The process is listed below:

DCOVAC – THE DATA MODELING FRAMEWORK

* DEFINE the Problem
* COLLECT the Data from Appropriate Sources
* ORGANIZE the Data Collected
* VISUALIZE the Data by Developing Charts
* ANALYZE the data with Appropriate Statistical Methods
* COMMUNICATE your Results

Row {data-height=650}
-----------------------------------------------------------------------

### The Problem & Data Collection

#### The Problem
The Student Exam Score data used in the analysis includes data on students from different backgrounds. The analysis will determine whether the variables in the data set help to determine the scores of the students from a fictional public school.


#### The Data
The data set consists of 30,641 rows and 13 columns, containing various data types. The character columns include Gender, ParentEduc, LunchType, TestPrep, ParentMaritalStatus, PracticeSport, and IsFirstChild. The numeric columns include NrSiblings, MathScore, ReadingScore, and WritingScore. The objective is to analyze the data set and investigate the potential relationships between these variables, focusing on factors that may impact academic performance.


####  Data Sources
This data set came from Kaggle, a platform for sharing data sets.


### The Data
VARIABLES TO PREDICT WITH

* *Gender*: Gender of the student
* *EthnicGroup*:Ethnic group of the student
* *ParentEduc*: Level of education of the student's parent
* *LunchType*:  Type of lunch received by the student 
* *TestPrep*: Whether the student completed test preparation
* *ReadingScore*: Score obtained by the student in the Reading exam between 0 and 100
* *WritingScore*: Score obtained by the student in the Writing exam between 0 and 100


VARIABLES WE WANT TO PREDICT

* *MathScore*: Score obtained by the student in the Math exam between 0 and 100
* *MathScoreClass*: Classification of the student math score > 80 (coded as 1 or 0)

Data
=======================================================================


Column {data-width=650} #experiment with the col size
-----------------------------------------------------------------------
### Organize the Data
Organizing data can also include summarizing data values in simple one-way and two-way tables.

```{r, cache=TRUE}
#the cache=TRUE can be removed. This will allow you to rerun your code without it having to run EVERYTHING from scratch every time. If the output seems to not reflect new updates, you can choose Knit, Clear Knitr cache to fix.

#Clean data by replacing spaces with decimals
colnames(df) <- make.names(colnames(df))
#View data
summary(df)
#remove RAD due to it being an index so not a real continuous number
#df <- select(df)
```
From this data we can see that the variables have a variety of different values based on their types. The scores have differing minimum values where only the MathScore had a minimum value of 0. Each exam has a wide range. We can note that the minimum value for siblings is 0, indicating that some students may not have any siblings.

Column {data-width=350}
-----------------------------------------------------------------------
### Transform Variables
The MathScore.Class column is 1 if the score is above 80 and 0 if not so this should be a character data type. In this data, MathScore.Class is just a categorical variable that is 1 if score value (MathScore) is high and 0 if low. We will convert our categorical variables to be factors.
```{r, cache=TRUE}
df <- mutate(df, Gender = as.factor(Gender),
             ParentEduc = as.factor(ParentEduc),
             LunchType = as.factor(LunchType),
             TestPrep = as.factor(TestPrep),
             ParentMaritalStatus = as.factor(ParentMaritalStatus),
             PracticeSport = as.factor(PracticeSport),
             IsFirstChild = as.factor(IsFirstChild),
             TransportMeans = as.factor(TransportMeans),
             MathScore.Class = as.factor(MathScore.Class))

```
#### TestPrep (was there any test prep?)
```{r, cache=TRUE}
as_tibble(select(df,TestPrep) %>%
  table()) 

```
#### MathScore.Class (High or Low Score)

<!--Instructions to import .jpg or .png images
use getwd() to see current path structure 
copy file into same place as .Rmd file
put the path to this file in the link
format: ![Alt text](book.jpg) -->

![](StudentExamScoreMathScoreClass.png)


Data Viz #1
=======================================================================


Column {data-width=500}
-----------------------------------------------------------------------
### Response Variables
#### MathScore.Class High(1)/Low(0)
```{r, cache=TRUE}
df %>%
  count(MathScore.Class) %>%
  ggplot(aes(x = MathScore.Class, y = n)) +
  geom_bar(stat = "identity")

```

We can see we have about 20% of the data as high Math score (>80). Looking at the potential predictors related to  MathScore.Class.


Column {data-width=500}
-----------------------------------------------------------------------

### Transform Variables

```{r, cache=TRUE}
ggpairs(select(df,MathScore, MathScore.Class, ReadingScore, WritingScore, WklyStudyHours, NrSiblings))
```


Data Viz #2
=======================================================================


Column {data-width=500}
-----------------------------------------------------------------------
### Response Variables

#### MathScore
```{r, cache=TRUE}
ggplot(df, aes(MathScore)) + geom_histogram(bins=20)
```

We see that the largest concentration of Math Score values is around 65. Looking at the potential predictors related to MathScore, the strongest relationships occur between WritingScore and ReadingScore. The data is also skewed to the left.


Column {data-width=500}
-----------------------------------------------------------------------

### Transform Variables

```{r, cache=TRUE}
ggpairs(select(df,MathScore, MathScore.Class, ReadingScore, WritingScore, WklyStudyHours, NrSiblings))
```

Data Viz #3
=======================================================================


Column {data-width=500}
-----------------------------------------------------------------------
### Response Variables

#### MathScore
```{r, cache=TRUE}
#creates a boxplot
ggplot(df, aes(y = MathScore)) + geom_boxplot()
```
We can see that in the MathScore data, the majority of the data is above the score of 50. The median value is almost a score of 65. 

Column {data-width=500}
-----------------------------------------------------------------------

### Transform Variables
```{r, cache=TRUE}
ggpairs(select(df,MathScore, MathScore.Class, ReadingScore, WritingScore, WklyStudyHours, NrSiblings))
```


MathScore Analysis {data-orientation=rows}
=======================================================================

Row
-----------------------------------------------------------------------

### Predict Score Value
For this analysis we will use a Linear Regression Model and a Partition Tree Model.

```{r, include=FALSE, cache=TRUE}
#runs a linear regression model
#the include=FALSE hides the output - remove to see
student_scores_lm <- lm(MathScore ~ . -MathScore.Class, data = df)
summary(student_scores_lm)

```

```{r, include=FALSE, cache=TRUE}
#the include=FALSE hides the output - remove to see
tidy(student_scores_lm)
```
```{r, include=FALSE, cache=TRUE}
#fit a partition tree
partition_tree <- rpart(MathScore ~ . -MathScore.Class, data = df)
summary(partition_tree)
rpart.plot(partition_tree)
```

### Adjusted R-Squared - Linear Regression

```{r, cache=TRUE}
ARSq<-round(summary(student_scores_lm)$adj.r.squared,2)
valueBox(paste(ARSq*100,'%'), icon = "fa-thumbs-up")
```
### Adjusted R-Square - Partition
```{r, cache=TRUE}
# Calculate R-squared value
predicted_scores <- predict(partition_tree, newdata = df)
ssr <- sum((predicted_scores - mean(df$MathScore))^2)
sst <- sum((df$MathScore - mean(df$MathScore))^2)
rsquared <- 1 - (ssr / sst)

valueBox(paste(round(rsquared, 2)*100, '%'), icon = "fa-thumbs-up")
```

### RMSE - Linear Regression

```{r, cache=TRUE}
Sig<-round(summary(student_scores_lm)$sigma,2)
valueBox(Sig, icon = "fa-thumbs-up")
```


#### RMSE - Partition Tree
```{r, cache = TRUE}
mse <- mean((predicted_scores - df$MathScore)^2)
rmse <- sqrt(mse)
valueBox(paste(round(rmse, 2)), icon = "fa-thumbs-up")
```


Row
-----------------------------------------------------------------------

### Regression Output

```{r,include=FALSE, cache=TRUE}
#knitr::kable(summary(MEDV_lm)$coef, digits = 3) #pretty table output
summary(student_scores_lm)$coef
```

```{r, cache=TRUE}
# this version sorts the p-values (it is using an index to reorder the coefficients)
idx <- order(coef(summary(student_scores_lm))[,4])  
out <- coef(summary(student_scores_lm))[idx,] 
knitr::kable(out, digits = 3) #pretty table output
```

### Partition Output
```{r}
summary_table <- data.frame(
  "Variable" = names(partition_tree$variable.importance),
  "Importance" = partition_tree$variable.importance
)

# Sort the table by importance
summary_table <- summary_table[order(summary_table$Importance, decreasing = TRUE), ]

# Display the summary table
knitr::kable(summary_table, digits = 3)
```
Row
-----------------------------------------------------------------------
### Tree Assumptions Explorations 

```{r}
# Plot the partition tree
rpart.plot(partition_tree)
```

### Residual Assumptions Explorations

```{r, cache=TRUE}
plot(student_scores_lm, which=c(1,2)) #which tells which plots to show (1-6 different plots)
```

```{r}
# Plot the partition tree
#rpart.plot(partition_tree)
```


Row
-----------------------------------------------------------------------

### Analysis Summary
After examining both models, we determine that there are some predictors that are not relevant to predicting the Math score value, so we will prune the linear regression model to remove the predictors that are not significant. We will also run another Partition Tree Model. 

Row
-----------------------------------------------------------------------

### Predict Score Value Final Version
For this analysis we will use a pruned Linear Regression Model. We removed ParentMaritalStatus (marital status of the student), NrSiblings (number if siblings the student has), TransportMeans (type of transportation to school), and WklyStudyHours (number of hours studied/week). We will also run another Partition Tree however, the partition tree does not use remove variables that do not help with prediction.

```{r, include=FALSE, cache=TRUE}
#the include=FALSE hides the output - remove to see
student_scores_lm <- lm(MathScore ~ Gender + ParentEduc + LunchType + TestPrep + PracticeSport + IsFirstChild + ReadingScore + WritingScore, data = df)
summary(student_scores_lm)

```

```{r, include=FALSE, cache=TRUE}
#the include=FALSE hides the output - remove to see
tidy(student_scores_lm)
```

```{r}
# Fit a partition tree with selected variables
partition_tree_ <- rpart(MathScore ~ Gender + ParentEduc + LunchType + TestPrep + PracticeSport + IsFirstChild + ReadingScore + WritingScore, data = df)

# Plot the partition tree
#rpart.plot(partition_tree)
```

Row
-----------------------------------------------------------------------

### Adjusted R-Squared - Linear Regression

```{r, cache=TRUE}
ARSq<-round(summary(student_scores_lm)$adj.r.squared,2)
valueBox(paste(ARSq*100,'%'), icon = "fa-thumbs-up")
```
### Adjusted R-Squared - Partition Tree

```{r}
# Calculate R-squared value
predicted_scores <- predict(partition_tree_, newdata = df)
ssr <- sum((predicted_scores - mean(df$MathScore))^2)
sst <- sum((df$MathScore - mean(df$MathScore))^2)
rsquared <- 1 - (ssr / sst)

valueBox(paste(round(rsquared, 2)*100, '%'), icon = "fa-thumbs-up")
```


### RMSE - Linear Regression

```{r, cache=TRUE}
Sig<-round(summary(student_scores_lm)$sigma,2)
valueBox(Sig, icon = "fa-thumbs-up")
```
### RMSE - Partition Tree

```{r}
mse <- mean((predicted_scores - df$MathScore)^2)
rmse <- sqrt(mse)
valueBox(paste(round(rmse, 2)), icon = "fa-thumbs-up")
```


Row
-----------------------------------------------------------------------

### Regression Output

```{r, include=FALSE, cache=TRUE}
knitr::kable(summary(student_scores_lm)$coef, digits = 3) #pretty table output
```

```{r, cache=TRUE}
# this version sorts the p-values (it is using an index to reorder the coefficients)
idx <- order(coef(summary(student_scores_lm))[,4])  
out <- coef(summary(student_scores_lm))[idx,] 
knitr::kable(out, digits = 3) #pretty table output
```

### Partition Output

```{r}
summary_table <- data.frame(
  "Variable" = names(partition_tree_$variable.importance),
  "Importance" = partition_tree_$variable.importance
)

# Sort the table by importance
summary_table <- summary_table[order(summary_table$Importance, decreasing = TRUE), ]

# Display the summary table
knitr::kable(summary_table, digits = 3)
```


### Residual Assumptions Explorations

```{r, cache=TRUE}
plot(student_scores_lm, which=c(1,2)) #which tells which plots to show (1-6 different plots)
```

### Tree Assumptions Explorations
```{r}
# Plot the partition tree
rpart.plot(partition_tree_)
```


Row
-----------------------------------------------------------------------

### Analysis Summary
Overall, the data did not lead to distorted residuals and affect the overall model fit. In the Residuals vs Fitted plot, there was no clear visible curve. Therefore, the scores were evenly predicted throughout.

Reducing the predictors that did not help with the prediction of the median value did not have a big impact on our fit statistics, specifically the R-square and RMSE (root mean squared error). This suggests that these predictors may not have strong associations with the Score Value and can be omitted without significantly affecting the model's performance.

The pruned version of the partition tree helped simplify the model without significantly impacting the model performance.

From the following table, we can see the effect on Score Value by the predictor variables.

```{r, cache=TRUE}
#create table summary of predictor changes
predchang <- data.frame(
  Variable = c('MathScore', 'ReadingScore', 'WritingScore'),
  Direction = c('Decrease', 'Decrease', 'Decrease')
)
knitr::kable(predchang)  # pretty table output

```




MathScore.Class Analysis {data-orientation=rows}
=======================================================================

Row {data-height=900}
-----------------------------------------------------------------------

### Predict Score Value
![](ExamScoreLogResults.png)
### Predict Score Value
![](ExamScorePartResults.png)

Conclusion
=======================================================================
### Summary

In Conclusion, we can see that our predictors do help to predict the score value, either the high/low score value (with cutoff at 80) or the actual score values.

Combining the results of both types of predictor models and only reporting where agreement was found, we can see that as these variables increase they:
```{r}
#final table summary of predictor changes
predchangfnl <- data.frame(
  Variable = c("Gender",
               "EthnicGroup",
               "ParentEduc",
               "LunchType", 
               "TestPrep", 
               "PracticeSport", 
               "ReadingScore", 
               "WritingScore"),
  Direction = c("Decrease", "Decrease", "Decrease", "Decrease", "Decrease", "Decrease", "Decrease", "Decrease")
)

knitr::kable(predchangfnl) #pretty table output

```








