Project 3
================
Rachel Fellman & Sabrina Dahl 

# Introduction

The “diabetes” dataset (as read in below) consists of 253,680 responses
to 21 variables of the Behavioral Risk Factor Surveillance System survey
conducted by the CDC in 2015. We will be focusing on the Diabetes_binary
as our response variable where 0 indicates no diabetes, and 1 indicates
either prediabetes or diabetes.  
We will be exploring how Body Mass Index (BMI), Age, Income, healthcare
(AnyHealthcare), and Mental Health (MentHlth) affects our response
variable and any trends between them. Age is scaled from 1-13 (1=18-24,
9=60-64, and 13= 80 or older). Income is scaled from 1-8 (1 is less than
10,000 dollars, 5 less than 35,000, and 8 is 75,000 or more). Healthcare
is an indication of whether they have any form of healthcare with 0 is
no and 1 is yes. Mental Health is the days of poor mental health on a
scale from 1-30.  
We will also be exploring the HvyAlcoholConsump, Sex, NoDocbcCost, and
PhysHlth variables. Heavy Alcohol Consumption would be greater than or
equal to 14 drinks a week for a man and greater than or equal to seven
drinks for a woman with 0 indicating no, and 1 indicating yes. For Sex,
0 indicates female and 1 indicates male. NoDocbcCost indicates whether
the individual needed to go to the doctor at any time in the past 12
months but did not go because of cost with 0 being no and 1 being yes.
PhysHlth is the days with physical illness or injury in the past 30 days
on a scale from 1-30.  
Exploratory Data Analysis (EDA) is important to ensure we understand the
data so results are created with intent. Checking for missing data and
outliers is important so that any skew to the data can be avoided, and
potentially different approaches can be used for more representative
results. We will then fit multiple model types to the training set and
determine the best model for predicting the Diabetes_binary variable
within each model type. Those *best* models will then each be applied to
the test set to determine which model type is best at predicting the
Diabetes_binary variable. This will be done for each level of education,
so that a model will be selected that best predicts whether an
individual has diabetes/prediabetes or not within their education level.
The education levels will be split as follows: 1= never attended-Grade
8, 3=Grade 9-11, 4=Grade 12-High School Graduate, 5=College Years 1-3,
and 6=College year 4-College Graduate.

# Data

We will start by reading in the data with the `read_csv` function with a
relative path.

``` r
diabetes <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
```

Next we will combine the 1 and 2 groups in the `Education` variable into
a new group. We will do this using the `mutate` and `ifelse` functions.
The new value for the 1 and 2 groups will just be 1, so `Education` can
equal 1, 3, 4, 5, or 6.

``` r
diabetes.1 <- diabetes %>% 
  mutate(Education = ifelse(Education < 3, 1, Education))
```

# Summarizations

We will start by doing some exploratory data analysis on the full data
set.

Since we will be creating models for different education levels, we will
split up the data into smaller data set for each of the 5 education
levels to do EDA. We will do this using our parameters

``` r
#create subsetted data set for each level of education using the params.
diabetes.ed <- diabetes.1 %>% 
  filter(Education == params$Ed)
```

First We will look at the graph of diabetes proportion vs Physical
Health. We will use `ggplot` with `geom_point` to create this scatter
plot.

``` r
#group by PhysHlth to create graph of proportion of people with diabetes at each number of days with illness/injury
physHlth.sum <- diabetes.ed %>% 
  group_by(PhysHlth) %>% 
  summarise(proportion.diabetes = mean(Diabetes_binary), n = n())
# create plot
ggplot(physHlth.sum, aes(x = PhysHlth, y = proportion.diabetes, size = n)) +
  geom_point(stat = "identity")+ 
#add title and labels
  xlab("Physical Illness/Injury (# of days)")+
  labs(title = "Proportion of People with Diabetes", subtitle = "vs the number of days they had a Physical illness/injury the last 30 days")
```

![](project3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> In this
graph it is helpful to look for patterns and trends in the data. Is
there a positive or negative relationship between the number of days
with physical illness/injury and the proportion of people with diabetes.
We can also examine the size of the dots on our plot to see if there are
more people with a lot days of physical injuries or more people with
fewer days of physical injuries.

Next we will look at variable concening alcohol consumption and its
possible relation to diabetes.

``` r
#Create a table with the proportions of people with/without diabetes vs whether they heavily drink or not
tab.Alc<- table(diabetes.ed$Diabetes_binary, diabetes.ed$HvyAlcoholConsump)
#Add Column and row names
colnames(tab.Alc)=c("No Heavy Alcohol Consumption", "Heavy Alcohol Consumption")
rownames(tab.Alc)=c("No Diabetes", "Pre-/Diabetes")
tab.Alc #printing
```

    ##                
    ##                 No Heavy Alcohol Consumption Heavy Alcohol Consumption
    ##   No Diabetes                           6852                       330
    ##   Pre-/Diabetes                         2256                        40

In this table we can see the counts of observations in each category. We
can look for things such as if there are higher counts of diabetes in
people with heavy alcohol consumption vs those with no heavy alcohol
consumption.

Next we will create a plot wit the same variables in the table we just
examined above but also splitting by gender.

``` r
#create a new factor variable of the Sex variable
Gender <- factor(ifelse(diabetes.ed$Sex>0,"Female","Male"))
#group by HvyAlcoholConsump to create graph of proportion of people with diabetes vs whether they drink heavily or not and their sex
Alc.sum <- diabetes.ed %>% 
  mutate(proportion.diabetes = mean(Diabetes_binary), Gender) %>%
  group_by(HvyAlcoholConsump)
#create graph
ggplot(Alc.sum, aes(x=HvyAlcoholConsump, y=proportion.diabetes))+
  geom_bar(stat="identity", aes(fill=Gender), position="stack")
```

![](project3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> With this
bar graph we can not only see if there are higher numbers of diabetes
among those with heavy alcohol consumption but we can also see if there
is a difference in alcohol consumption between men and women and if that
appears to have any impact on our proportion of people with diabetes.
Look for differences in the proportions between the groups.

Next we will look at a table of the proportion of people with diabetes
and if people were able to visit a doctor due to cost.

``` r
#Create a table with the proportions of people with/without diabetes vs whether they had to avoid the doctor the last 30 days due to costs
tab.Doc<- prop.table(table(diabetes.ed$Diabetes_binary, diabetes.ed$NoDocbcCost))
#Add Column and row names
colnames(tab.Doc)=c("Not Avoid", "Avoid Doc")
rownames(tab.Doc)=c("No Diabetes", "Pre-/Diabetes")
tab.Doc #printing
```

    ##                
    ##                  Not Avoid  Avoid Doc
    ##   No Diabetes   0.63410002 0.12365478
    ##   Pre-/Diabetes 0.20310192 0.03914328

Here we need to remember that the numbers we see are proportions. Is
there a higher proportion of diabetes in those that could not afford to
go to the doctor? Do we see lower proportions of diabetes in people who
were able to visit a doctor?

We will now make a graph of the the proportion of people who were unable
to go to the doctor because of the cost split up by sex. First we will
`group_by` the `NoDocbcCost` variable and then we will use `ggplot` with
`geom_bar`.

``` r
#group by NoDocbcCost to create graph of proportion of people with diabetes vs whether they avoided the doctor or not and their sex
Doc.sum <- diabetes.ed %>% 
  mutate(proportion.diabetes = mean(Diabetes_binary), Gender) %>%
  group_by(NoDocbcCost)
#create graph
ggplot(Doc.sum, aes(x=NoDocbcCost, y=proportion.diabetes))+
  geom_bar(stat="identity", aes(fill=Gender),position="stack")
```

![](project3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> In this bar
graph do we see a difference between men and women when examining the
proportion of people with diabetes who could not afford a doctor visit?
Do we see any proportions that are unexpected? The `NoDocbcCost`
variable indicates if the person avoided going to the doctor in the past
30 days because of the cost.

We will also look at a scatter plot of the proportion of people with
diabetes across BMI levels. First we will create the proportion variable
and then we will use `ggplot` with `geom_point` to create the graph.

``` r
# group by BMI to create graph of proportion of people with diabetes at each BMI.
bmi.sum <- diabetes.ed %>% 
  group_by(BMI) %>% 
  summarise(proportion.diabetes = mean(Diabetes_binary), n = n())
# create plot
ggplot(bmi.sum, aes(x = BMI, y = proportion.diabetes, size = n)) +
  geom_point(stat = "identity")+ 
#add title
  labs(title = "Proportion of People with Diabetes at each BMI for Education = 1")
```

![](project3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> When
looking at this graph is is helpful to look for trends. You can examine
the graph to see if there is a positive or negative correlation between
the BMI and the proportion of people with diabetes. It can also be good
to look for outliers because these may affect our results in unexpected
ways. Another aspect to pay attention to would be if there is more data
at certain levels of BMI and how this might affect our ability to
predict.

Next we will look at a Graph of proportion of Diabetes for different
ages. First we will create the proportion variable and then we will use
`ggplot` with `geom_point` to create the graph.

``` r
# group by BMI to create graph of proportion of people with diabetes at each age.
age.sum <- diabetes.ed %>% 
  group_by(Age) %>% 
  summarise(proportion.diabetes = mean(Diabetes_binary))
# create plot
ggplot(age.sum, aes(x = Age, y = proportion.diabetes)) +
  geom_point(stat = "identity")+ 
#add title
  labs(title = "Proportion of People with Diabetes at each age")
```

![](project3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> When
examining this graph, it is helpful to look for trends in the data. For
example, do higher ages have higher proportions of diabetes or lower
proportions? You can also think about what kind of trend line would best
describe the data. Is the relationship linear, parabolic, etc. ? Keep in
mind that the age is split into categories with 1 being 18 - 24, and 13
being 80 and older. The age levels are done in 5 year increments.

Next we will create a graph of diabetes proportion vs income. First we
will create the proportion variable and then we will use `ggplot` with
`geom_point` to create the graph.

``` r
# group by income to create graph of proportion of people with diabetes at each income for education 1.
income.sum <- diabetes.ed %>% 
  group_by(Income) %>% 
  summarise(proportion.diabetes = mean(Diabetes_binary), n= n())
# create plot
ggplot(income.sum, aes(x = Income, y = proportion.diabetes, size = n)) +
  geom_point(stat = "identity")+ 
#add title
  labs(title = "Proportion of People with Diabetes at each income level")
```

![](project3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> This graph
shows us the proportion of people with diabetes at each income level.
Level 1 is less than 10,000 dollars and it goes all the way up to level
8 which is greater than or equal to 75,000 dollars. It is helpful to see
if the graph shows any positive or negative correlations between the
income and the proportion of people with diabetes. Since this graph also
includes larger points when there is more data, you can see if there are
more data at lower or higher incomes.

Next we will create a table to examine some of our binary variables. We
will create a table for the number of people with diabetes by if they
have any health care or not. This will be done via the `table` function.
I will also add a row to the table for the proportion of people with
diabetes to help us better understand the data.

``` r
tab<- table(diabetes.ed$Diabetes_binary, diabetes.ed$AnyHealthcare)
rbind(tab, apply(tab, MARGIN = 2, FUN = function(x){round(x[2] / sum(x), 3)}))
```

    ##         0        1
    ## 0 990.000 6192.000
    ## 1 144.000 2152.000
    ##     0.127    0.258

In this table we have the columns giving the count of people who have
health care (0 = no, 1 = yes), and the rows providing the count of
people with diabetes( 0 = no, 1 = yes). The last row of the table gives
the proportion of people with diabetes for each column. It is helpful to
look at these proportions and see if there are greater percents of
people with diabetes for those who have healthcare vs those who don’t.

We will also examine the average and standard deviation for mental
health levels. The mental health level tells us the number of days
during the past month that the person felt their mental health was not
good from a scale of 1 to 30 days. We will group by diabetes, since this
is the thing we are wanting to predict, (0 = no, 1 = yes) as well as by
sex (0 = female, and 1 = male).

``` r
diabetes.ed %>% 
  group_by(Diabetes_binary, Sex) %>% 
  summarise(avg = mean(MentHlth), sd = sd(MentHlth))
```

    ## # A tibble: 4 × 4
    ## # Groups:   Diabetes_binary [2]
    ##   Diabetes_binary   Sex   avg    sd
    ##             <dbl> <dbl> <dbl> <dbl>
    ## 1               0     0  5.57  9.94
    ## 2               0     1  4.36  9.11
    ## 3               1     0  6.86 10.6 
    ## 4               1     1  5.06  9.72

In this table we can look at the differences between the averages of bad
mental health days for men and women who have diabetes and who do not.
Here it can be helpful to examine if there is a higher average for those
with diabetes than for those without. You can also look for any
differences between the averages of those with diabetes who are men and
those with diabetes who are women.

# Modeling

Before doing any modeling we will split the data into a training and
test set using the `createDataPartition` function from the `caret`
package. The test/train data will be split with a 70/30 ratio.

``` r
# makes Diabetes_binary variable a factor
diabetes.ed$Diabetes_binary <- as_factor(diabetes.ed$Diabetes_binary)


#make things reproducible with set.seed 
set.seed(90)
#create index
train.index <- createDataPartition(diabetes.ed$Diabetes_binary, p = .7, list = FALSE)
#create train and test sets
diabetes.train <- diabetes.ed[train.index, ]
diabetes.test <- diabetes.ed[-train.index, ]
```

## Log Loss

Log loss is a measurement we can use to assess a model in a
classification problem. A classification problem is one where we have a
categorical or binary response and we want to either predict class
membership or the probability of membership with our models. Log loss
gives us a measurement of how close the predicted probability of being
in a class is to the actual value (0 or 1 in the case of binary
classification which we have in this project). A higher log loss value
indicates that the predicted probability is farther from the actual
value, and a perfect model would have a log loss of 0. Although accuracy
is a commonly used method for measuring the performance of our models,
log loss could be superior. Accuracy is the ratio of correct predictions
to total predictions. This measurement can fall short in two cases.
First, if there are more than two classes, the model may ignore some
classes. In this case, the accuracy measurement doesn’t give the
individual accuracy for each class which is likely the information we
actually want. Second, the accuracy metric works poorly for imbalanced
data. If we had a binary classification data set with a ratio of 95:5
for the 2 classes, then the model may automatically assign everything to
the first class. This would give us a accuracy rate of 95% that tells us
little about the model. Log loss can combat these problems. Log loss
takes into account how close a prediction is to the actual value, that
is to say it considers the certainty of a prediction and penalizes
predictions that are less certain. If the prediction was close to the
correct class there will be lower loss. It provides a continuous measure
to the model’s performance as opposed to a binary measure like accuracy.

## Logistic Regression

Logistic regression is a generalized linear model that predicts the
probability that a response belongs to a certain category. Since
logistic regression models probability, the prediction will always be
between 0 and 1. Our data in this project has a binary response, so we
will focus our explanation on binary logistic regression where the
response can be either yes or no. Binary logistic regression was
commonly used in medical research to predict if patients did or did not
have a certain condition which is similar to what we are doing in this
project.

There are a few reasons why logistic regression is a better option for
this type of data than linear regression. First of all, binary responses
do not follow the assumption of linear regression that our responses are
normally distributed. Rather, with binary logistic regression this is
not the case at all. Additionally, linear regression finds the best fit
using the least squares method. If we used this to predict the
probability of class membership using linear regression, we could have
responses that are below zero or above one which are essentially
meaningless when it comes to probability. Therefore it is a good
practice to use logistic regression when predicting binary responses and
linear regression for continuous responses. Instead of using the least
squares method, logistic regression often uses the maximum likelihood
estimation. This method finds the values of β0 and β1 that maximizes the
probability that the predictions we get from the model are the same as
the data we actually have.

Logistic regression models the probability of success via the following
function: $$ P(sucessX) = \frac {e^{β_0+β_1X}}{1 + e^{β_0+β_1X}}$$ Since
logistic regression is a rather complicated model to interpret, we can
back solve to get the log-odds. The log-odds or logit function models
the log-odds of an event occurring as a linear combination of predictor
variables. It maps probability values to the full range of real numbers.
The odds are essentially a ratio of probability of success to the
probability of failure. Log-odds are represented by the following
function: $$ log(\frac {(P(successX)}{ 1-P(successX)}) = β_0+β_1X $$ It
is important to note that the interpretation of the coefficients in the
logit function differ from that of linear regression. In this case β1
represents the change in the log-odds of success.

Now that we have an understanding of logistic regression we will fit
tree candidate logistic regression models and then choose the best model
using the log loss metric.

We will use the `train` function from the `caret` package to fit out
models. We will set the method to `glm` and the family to `binomial` to
specify binomial logistic regression. We will also preprocess our data
by centering and scaling it. For each of the three models we will do
cross validation with 5 folds.

Before fitting the logistic regression models with the Log Loss metric,
we need to change the values of the factor `Diabetes_binary` so that we
do not get an error from R.

``` r
diabetes.train.log <- diabetes.train %>% 
  mutate(Diabetes_binary = as_factor(ifelse(Diabetes_binary == 1, "yes", "no")))
diabetes.test.log <- diabetes.test %>% 
  mutate(Diabetes_binary = as_factor(ifelse(Diabetes_binary == 1, "yes", "no")))
```

The first model will use the variables `AnyHealthcare`, `Sex`, `Age`,
and `HighChol`.

``` r
#Use to training set to train the first model
fit.log1 <- train(Diabetes_binary ~ AnyHealthcare + Sex + Age + HighChol, data = diabetes.train.log,
                 #select generalized linear model method
                 method = "glm",
                 #specify family
                 family = "binomial",
                 #preprocess data
                 preProcess = c("center", "scale"),
                 # calculate log loss metric
                 metric = "logLoss",
                 #do cross validation
                 trControl = trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE)
                 )
```

The second logistic model will use the variables `BMI`, `PhysActivity`,
`MentHlth`, and `Income`.

``` r
#Use to training set to train the second model
fit.log2 <- train(Diabetes_binary ~ BMI + PhysActivity + MentHlth + Income , data = diabetes.train.log,
                 #select generalized linear model method
                 method = "glm",
                 #specify family
                 family = "binomial",
                 #preprocess data
                 preProcess = c("center", "scale"),
                 # calculate log loss metric
                 metric = "logLoss",
                 #do cross validation
                 trControl = trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE)
                 )
```

The third logistic regression model will use the variables `HighBP`,
`Veggies`, `GenHlth`, and `DiffWalk`.

``` r
#Use to training set to train the third model
fit.log3 <- train(Diabetes_binary ~ HighBP + Veggies + GenHlth + DiffWalk, data = diabetes.train.log,
                 #select generalized linear model method
                 method = "glm",
                 #specify family
                 family = "binomial",
                 #preprocess data
                 preProcess = c("center", "scale"),
                 # calculate log loss metric
                 metric = "logLoss",
                 #do cross validation
                 trControl = trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE)
                 )
```

Next I will print the fit of the 3 models and choose the bets based on
the log loss metric.

``` r
print(fit.log1)
```

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##    4 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (4), scaled (4) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5310, 5308, 5309, 5308, 5309 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.5211766

``` r
print(fit.log2)
```

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##    4 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (4), scaled (4) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5310, 5308, 5309, 5308 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.5313814

``` r
print(fit.log3)
```

    ## Generalized Linear Model 
    ## 
    ## 6636 samples
    ##    4 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (4), scaled (4) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5309, 5310, 5308, 5309 
    ## Resampling results:
    ## 
    ##   logLoss  
    ##   0.4982882

Next we will make a table of the log loss for each of our 3 logistic
regression models and use this table to choose the model with the lowest
log loss.

``` r
#use vectors to create tibble of models with their log loss
name <- c('fit.log1', 'fit.log2', 'fit.log3')
logloss<- c(fit.log1$results$logLoss, fit.log2$results$logLoss, fit.log3$results$logLoss)
log.models <- tibble(name, logloss)
# filter the best model
best.log<- log.models %>% 
  mutate(min = min(logloss)) %>% 
  filter(logloss == min)
#use paste to give the best model
paste("The best logistic regression model with the lowest log loss is", best.log$name, " ")
```

    ## [1] "The best logistic regression model with the lowest log loss is fit.log3  "

## Lasso

Lasso regression can be an extension of linear regression or logistic
regression. Here, we will use it with a logistic regression model since
we are working with a categorical response variable. LASSO regression
uses lambda, a growth variable, that you should increase slowly. As we
increase lambda the variance decreases but bias increases. We want the
lowest amount of variance without introducing a lot of bias into the
model. LASSO is used to enhance the prediction capabilities of a model
and it does this via shrinkage, where the data is shrunk towards the
mean. Since LASSO improves our prediction capabilities and reduces
overfitting it may be a prefered method to basic logistic regression.
LASSO regression starts with the basic form of the model (in our case a
logistic model). It then adds an extra term. This term is a penalty term
that is based on the number of variables. This term is created by
multiplying the absolute value of the sum of our coefficients (β1, β2,
etc.) by our tuning parameter, lambda, mentioned above. When lambda
equals 0 all variables are present, and the number of variables in the
model decreases as lambda increases. The algorithm attempts to fit the
model while minimizing our penalty term. Since the penalty term is
dependent on the number of variables used, the LASSO model has built in
variable selection, making it a better choice than logistic regression.
LASSO is also known to be a good model for situations where there are
high levels of multicollinearity.

Now that we know what LASSO is we will fit the model using the `train`
function from the `caret` package with `method = "glmnet"` to ensure we
are creating a lasso model.

``` r
#create lasso logistic regression model
fit.lasso <- train(Diabetes_binary ~ ., 
                   data= diabetes.train.log,
                   #select glmnet method
                   method= "glmnet",
                   #do cross validation
                   trControl= trainControl(method = "cv",
                                           number=5,
                                           summaryFunction = mnLogLoss,
                                           classProbs = TRUE),
                   #calculate log loss metric
                   metric= "logLoss",
                   #add tuning parameters
                   tuneGrid= expand.grid(alpha=1, lambda= seq(0,1, by=0.05)))
#print results of lasso logistic regression model
print(fit.lasso)
```

    ## glmnet 
    ## 
    ## 6636 samples
    ##   21 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5309, 5308, 5309, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda  logLoss  
    ##   0.00    0.4730279
    ##   0.05    0.5048098
    ##   0.10    0.5489216
    ##   0.15    0.5537335
    ##   0.20    0.5537335
    ##   0.25    0.5537335
    ##   0.30    0.5537335
    ##   0.35    0.5537335
    ##   0.40    0.5537335
    ##   0.45    0.5537335
    ##   0.50    0.5537335
    ##   0.55    0.5537335
    ##   0.60    0.5537335
    ##   0.65    0.5537335
    ##   0.70    0.5537335
    ##   0.75    0.5537335
    ##   0.80    0.5537335
    ##   0.85    0.5537335
    ##   0.90    0.5537335
    ##   0.95    0.5537335
    ##   1.00    0.5537335
    ## 
    ## Tuning parameter 'alpha' was held constant at a value of 1
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final values used for the model were alpha = 1 and lambda = 0.

Now we will select our best Lasso model using the `Log Loss` metric.

``` r
best.lasso <- fit.lasso$results %>% 
  mutate(lambda = lambda, min = min(logLoss)) %>% 
  filter(logLoss == min)
paste("The best LASSO model is lambda=",best.lasso$lambda," ")
```

    ## [1] "The best LASSO model is lambda= 0  "

## Classification Trees

A classification tree is used to predict group membership for
classification problems (like ours) and is usually done using the most
prevalent class as the prediction. A classification tree is basically a
map of binary decisions. Each branch of the tree is called a node and
our final classifications can be seen on the leaves. At each node we see
a variable split into 2 directions. These splits continue until we get
to the leaves. In order to build a classification tree the algorithm
will have to figure out what variables should be present at the nodes
and in what order. This can be done via entropy, the gini index or
variance. The gini index is used when the response variable is
categorical like our `Diabetes_binary` variable. When fitting a single
classification tree, typically pruning is needed. The more splits our
tree has, the better it will do at classifying the data in our training
dataset, however, at some point the prediction accuracy will start to
decrease. In order to avoid overfitting our trees to our training data
we prune them back which essentially means cutting off subtrees from our
larger decision tree. Despite this extra step, we still might want to
use a decision tree because they are easy to understand and interpret
and have built in variable selection.

We will be fitting the model using the `train` function from the `caret`
package. Setting `method = "rpart"` will ensure we are creating a
classification tree. This also requires the cp (complexity Parameter)
which is used for determining the size of the classification tree. It is
imposes a penalty when the tree has too many splits, and the higher the
cp the smaller the tree is.

``` r
#create classification tree model
fit.classtree <- train(Diabetes_binary ~ ., 
                   data= diabetes.train.log,
                   #select rpart method
                   method= "rpart",
                   #do cross validation
                   trControl= trainControl(method = "cv",
                                           number=5,
                                           summaryFunction = mnLogLoss,
                                           classProbs = TRUE),
                   #calculate log loss metric
                   metric= "logLoss",
                   #add tuning parameters
                   tuneGrid= data.frame(cp= seq(0,1, by=0.001)))
#print results of classification tree model
print(fit.classtree)
```

    ## CART 
    ## 
    ## 6636 samples
    ##   21 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5308, 5309, 5309, 5309 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp     logLoss  
    ##   0.000  0.6710040
    ##   0.001  0.5751935
    ##   0.002  0.5169432
    ##   0.003  0.5160428
    ##   0.004  0.5129370
    ##   0.005  0.5112503
    ##   0.006  0.5100545
    ##   0.007  0.5107553
    ##   0.008  0.5199635
    ##   0.009  0.5197122
    ##   0.010  0.5413241
    ##   0.011  0.5413241
    ##   0.012  0.5413241
    ##   0.013  0.5413241
    ##   0.014  0.5413241
    ##   0.015  0.5413241
    ##   0.016  0.5489018
    ##   0.017  0.5537335
    ##   0.018  0.5537335
    ##   0.019  0.5537335
    ##   0.020  0.5537335
    ##   0.021  0.5537335
    ##   0.022  0.5537335
    ##   0.023  0.5537335
    ##   0.024  0.5537335
    ##   0.025  0.5537335
    ##   0.026  0.5537335
    ##   0.027  0.5537335
    ##   0.028  0.5537335
    ##   0.029  0.5537335
    ##   0.030  0.5537335
    ##   0.031  0.5537335
    ##   0.032  0.5537335
    ##   0.033  0.5537335
    ##   0.034  0.5537335
    ##   0.035  0.5537335
    ##   0.036  0.5537335
    ##   0.037  0.5537335
    ##   0.038  0.5537335
    ##   0.039  0.5537335
    ##   0.040  0.5537335
    ##   0.041  0.5537335
    ##   0.042  0.5537335
    ##   0.043  0.5537335
    ##   0.044  0.5537335
    ##   0.045  0.5537335
    ##   0.046  0.5537335
    ##   0.047  0.5537335
    ##   0.048  0.5537335
    ##   0.049  0.5537335
    ##   0.050  0.5537335
    ##   0.051  0.5537335
    ##   0.052  0.5537335
    ##   0.053  0.5537335
    ##   0.054  0.5537335
    ##   0.055  0.5537335
    ##   0.056  0.5537335
    ##   0.057  0.5537335
    ##   0.058  0.5537335
    ##   0.059  0.5537335
    ##   0.060  0.5537335
    ##   0.061  0.5537335
    ##   0.062  0.5537335
    ##   0.063  0.5537335
    ##   0.064  0.5537335
    ##   0.065  0.5537335
    ##   0.066  0.5537335
    ##   0.067  0.5537335
    ##   0.068  0.5537335
    ##   0.069  0.5537335
    ##   0.070  0.5537335
    ##   0.071  0.5537335
    ##   0.072  0.5537335
    ##   0.073  0.5537335
    ##   0.074  0.5537335
    ##   0.075  0.5537335
    ##   0.076  0.5537335
    ##   0.077  0.5537335
    ##   0.078  0.5537335
    ##   0.079  0.5537335
    ##   0.080  0.5537335
    ##   0.081  0.5537335
    ##   0.082  0.5537335
    ##   0.083  0.5537335
    ##   0.084  0.5537335
    ##   0.085  0.5537335
    ##   0.086  0.5537335
    ##   0.087  0.5537335
    ##   0.088  0.5537335
    ##   0.089  0.5537335
    ##   0.090  0.5537335
    ##   0.091  0.5537335
    ##   0.092  0.5537335
    ##   0.093  0.5537335
    ##   0.094  0.5537335
    ##   0.095  0.5537335
    ##   0.096  0.5537335
    ##   0.097  0.5537335
    ##   0.098  0.5537335
    ##   0.099  0.5537335
    ##   0.100  0.5537335
    ##   0.101  0.5537335
    ##   0.102  0.5537335
    ##   0.103  0.5537335
    ##   0.104  0.5537335
    ##   0.105  0.5537335
    ##   0.106  0.5537335
    ##   0.107  0.5537335
    ##   0.108  0.5537335
    ##   0.109  0.5537335
    ##   0.110  0.5537335
    ##   0.111  0.5537335
    ##   0.112  0.5537335
    ##   0.113  0.5537335
    ##   0.114  0.5537335
    ##   0.115  0.5537335
    ##   0.116  0.5537335
    ##   0.117  0.5537335
    ##   0.118  0.5537335
    ##   0.119  0.5537335
    ##   0.120  0.5537335
    ##   0.121  0.5537335
    ##   0.122  0.5537335
    ##   0.123  0.5537335
    ##   0.124  0.5537335
    ##   0.125  0.5537335
    ##   0.126  0.5537335
    ##   0.127  0.5537335
    ##   0.128  0.5537335
    ##   0.129  0.5537335
    ##   0.130  0.5537335
    ##   0.131  0.5537335
    ##   0.132  0.5537335
    ##   0.133  0.5537335
    ##   0.134  0.5537335
    ##   0.135  0.5537335
    ##   0.136  0.5537335
    ##   0.137  0.5537335
    ##   0.138  0.5537335
    ##   0.139  0.5537335
    ##   0.140  0.5537335
    ##   0.141  0.5537335
    ##   0.142  0.5537335
    ##   0.143  0.5537335
    ##   0.144  0.5537335
    ##   0.145  0.5537335
    ##   0.146  0.5537335
    ##   0.147  0.5537335
    ##   0.148  0.5537335
    ##   0.149  0.5537335
    ##   0.150  0.5537335
    ##   0.151  0.5537335
    ##   0.152  0.5537335
    ##   0.153  0.5537335
    ##   0.154  0.5537335
    ##   0.155  0.5537335
    ##   0.156  0.5537335
    ##   0.157  0.5537335
    ##   0.158  0.5537335
    ##   0.159  0.5537335
    ##   0.160  0.5537335
    ##   0.161  0.5537335
    ##   0.162  0.5537335
    ##   0.163  0.5537335
    ##   0.164  0.5537335
    ##   0.165  0.5537335
    ##   0.166  0.5537335
    ##   0.167  0.5537335
    ##   0.168  0.5537335
    ##   0.169  0.5537335
    ##   0.170  0.5537335
    ##   0.171  0.5537335
    ##   0.172  0.5537335
    ##   0.173  0.5537335
    ##   0.174  0.5537335
    ##   0.175  0.5537335
    ##   0.176  0.5537335
    ##   0.177  0.5537335
    ##   0.178  0.5537335
    ##   0.179  0.5537335
    ##   0.180  0.5537335
    ##   0.181  0.5537335
    ##   0.182  0.5537335
    ##   0.183  0.5537335
    ##   0.184  0.5537335
    ##   0.185  0.5537335
    ##   0.186  0.5537335
    ##   0.187  0.5537335
    ##   0.188  0.5537335
    ##   0.189  0.5537335
    ##   0.190  0.5537335
    ##   0.191  0.5537335
    ##   0.192  0.5537335
    ##   0.193  0.5537335
    ##   0.194  0.5537335
    ##   0.195  0.5537335
    ##   0.196  0.5537335
    ##   0.197  0.5537335
    ##   0.198  0.5537335
    ##   0.199  0.5537335
    ##   0.200  0.5537335
    ##   0.201  0.5537335
    ##   0.202  0.5537335
    ##   0.203  0.5537335
    ##   0.204  0.5537335
    ##   0.205  0.5537335
    ##   0.206  0.5537335
    ##   0.207  0.5537335
    ##   0.208  0.5537335
    ##   0.209  0.5537335
    ##   0.210  0.5537335
    ##   0.211  0.5537335
    ##   0.212  0.5537335
    ##   0.213  0.5537335
    ##   0.214  0.5537335
    ##   0.215  0.5537335
    ##   0.216  0.5537335
    ##   0.217  0.5537335
    ##   0.218  0.5537335
    ##   0.219  0.5537335
    ##   0.220  0.5537335
    ##   0.221  0.5537335
    ##   0.222  0.5537335
    ##   0.223  0.5537335
    ##   0.224  0.5537335
    ##   0.225  0.5537335
    ##   0.226  0.5537335
    ##   0.227  0.5537335
    ##   0.228  0.5537335
    ##   0.229  0.5537335
    ##   0.230  0.5537335
    ##   0.231  0.5537335
    ##   0.232  0.5537335
    ##   0.233  0.5537335
    ##   0.234  0.5537335
    ##   0.235  0.5537335
    ##   0.236  0.5537335
    ##   0.237  0.5537335
    ##   0.238  0.5537335
    ##   0.239  0.5537335
    ##   0.240  0.5537335
    ##   0.241  0.5537335
    ##   0.242  0.5537335
    ##   0.243  0.5537335
    ##   0.244  0.5537335
    ##   0.245  0.5537335
    ##   0.246  0.5537335
    ##   0.247  0.5537335
    ##   0.248  0.5537335
    ##   0.249  0.5537335
    ##   0.250  0.5537335
    ##   0.251  0.5537335
    ##   0.252  0.5537335
    ##   0.253  0.5537335
    ##   0.254  0.5537335
    ##   0.255  0.5537335
    ##   0.256  0.5537335
    ##   0.257  0.5537335
    ##   0.258  0.5537335
    ##   0.259  0.5537335
    ##   0.260  0.5537335
    ##   0.261  0.5537335
    ##   0.262  0.5537335
    ##   0.263  0.5537335
    ##   0.264  0.5537335
    ##   0.265  0.5537335
    ##   0.266  0.5537335
    ##   0.267  0.5537335
    ##   0.268  0.5537335
    ##   0.269  0.5537335
    ##   0.270  0.5537335
    ##   0.271  0.5537335
    ##   0.272  0.5537335
    ##   0.273  0.5537335
    ##   0.274  0.5537335
    ##   0.275  0.5537335
    ##   0.276  0.5537335
    ##   0.277  0.5537335
    ##   0.278  0.5537335
    ##   0.279  0.5537335
    ##   0.280  0.5537335
    ##   0.281  0.5537335
    ##   0.282  0.5537335
    ##   0.283  0.5537335
    ##   0.284  0.5537335
    ##   0.285  0.5537335
    ##   0.286  0.5537335
    ##   0.287  0.5537335
    ##   0.288  0.5537335
    ##   0.289  0.5537335
    ##   0.290  0.5537335
    ##   0.291  0.5537335
    ##   0.292  0.5537335
    ##   0.293  0.5537335
    ##   0.294  0.5537335
    ##   0.295  0.5537335
    ##   0.296  0.5537335
    ##   0.297  0.5537335
    ##   0.298  0.5537335
    ##   0.299  0.5537335
    ##   0.300  0.5537335
    ##   0.301  0.5537335
    ##   0.302  0.5537335
    ##   0.303  0.5537335
    ##   0.304  0.5537335
    ##   0.305  0.5537335
    ##   0.306  0.5537335
    ##   0.307  0.5537335
    ##   0.308  0.5537335
    ##   0.309  0.5537335
    ##   0.310  0.5537335
    ##   0.311  0.5537335
    ##   0.312  0.5537335
    ##   0.313  0.5537335
    ##   0.314  0.5537335
    ##   0.315  0.5537335
    ##   0.316  0.5537335
    ##   0.317  0.5537335
    ##   0.318  0.5537335
    ##   0.319  0.5537335
    ##   0.320  0.5537335
    ##   0.321  0.5537335
    ##   0.322  0.5537335
    ##   0.323  0.5537335
    ##   0.324  0.5537335
    ##   0.325  0.5537335
    ##   0.326  0.5537335
    ##   0.327  0.5537335
    ##   0.328  0.5537335
    ##   0.329  0.5537335
    ##   0.330  0.5537335
    ##   0.331  0.5537335
    ##   0.332  0.5537335
    ##   0.333  0.5537335
    ##   0.334  0.5537335
    ##   0.335  0.5537335
    ##   0.336  0.5537335
    ##   0.337  0.5537335
    ##   0.338  0.5537335
    ##   0.339  0.5537335
    ##   0.340  0.5537335
    ##   0.341  0.5537335
    ##   0.342  0.5537335
    ##   0.343  0.5537335
    ##   0.344  0.5537335
    ##   0.345  0.5537335
    ##   0.346  0.5537335
    ##   0.347  0.5537335
    ##   0.348  0.5537335
    ##   0.349  0.5537335
    ##   0.350  0.5537335
    ##   0.351  0.5537335
    ##   0.352  0.5537335
    ##   0.353  0.5537335
    ##   0.354  0.5537335
    ##   0.355  0.5537335
    ##   0.356  0.5537335
    ##   0.357  0.5537335
    ##   0.358  0.5537335
    ##   0.359  0.5537335
    ##   0.360  0.5537335
    ##   0.361  0.5537335
    ##   0.362  0.5537335
    ##   0.363  0.5537335
    ##   0.364  0.5537335
    ##   0.365  0.5537335
    ##   0.366  0.5537335
    ##   0.367  0.5537335
    ##   0.368  0.5537335
    ##   0.369  0.5537335
    ##   0.370  0.5537335
    ##   0.371  0.5537335
    ##   0.372  0.5537335
    ##   0.373  0.5537335
    ##   0.374  0.5537335
    ##   0.375  0.5537335
    ##   0.376  0.5537335
    ##   0.377  0.5537335
    ##   0.378  0.5537335
    ##   0.379  0.5537335
    ##   0.380  0.5537335
    ##   0.381  0.5537335
    ##   0.382  0.5537335
    ##   0.383  0.5537335
    ##   0.384  0.5537335
    ##   0.385  0.5537335
    ##   0.386  0.5537335
    ##   0.387  0.5537335
    ##   0.388  0.5537335
    ##   0.389  0.5537335
    ##   0.390  0.5537335
    ##   0.391  0.5537335
    ##   0.392  0.5537335
    ##   0.393  0.5537335
    ##   0.394  0.5537335
    ##   0.395  0.5537335
    ##   0.396  0.5537335
    ##   0.397  0.5537335
    ##   0.398  0.5537335
    ##   0.399  0.5537335
    ##   0.400  0.5537335
    ##   0.401  0.5537335
    ##   0.402  0.5537335
    ##   0.403  0.5537335
    ##   0.404  0.5537335
    ##   0.405  0.5537335
    ##   0.406  0.5537335
    ##   0.407  0.5537335
    ##   0.408  0.5537335
    ##   0.409  0.5537335
    ##   0.410  0.5537335
    ##   0.411  0.5537335
    ##   0.412  0.5537335
    ##   0.413  0.5537335
    ##   0.414  0.5537335
    ##   0.415  0.5537335
    ##   0.416  0.5537335
    ##   0.417  0.5537335
    ##   0.418  0.5537335
    ##   0.419  0.5537335
    ##   0.420  0.5537335
    ##   0.421  0.5537335
    ##   0.422  0.5537335
    ##   0.423  0.5537335
    ##   0.424  0.5537335
    ##   0.425  0.5537335
    ##   0.426  0.5537335
    ##   0.427  0.5537335
    ##   0.428  0.5537335
    ##   0.429  0.5537335
    ##   0.430  0.5537335
    ##   0.431  0.5537335
    ##   0.432  0.5537335
    ##   0.433  0.5537335
    ##   0.434  0.5537335
    ##   0.435  0.5537335
    ##   0.436  0.5537335
    ##   0.437  0.5537335
    ##   0.438  0.5537335
    ##   0.439  0.5537335
    ##   0.440  0.5537335
    ##   0.441  0.5537335
    ##   0.442  0.5537335
    ##   0.443  0.5537335
    ##   0.444  0.5537335
    ##   0.445  0.5537335
    ##   0.446  0.5537335
    ##   0.447  0.5537335
    ##   0.448  0.5537335
    ##   0.449  0.5537335
    ##   0.450  0.5537335
    ##   0.451  0.5537335
    ##   0.452  0.5537335
    ##   0.453  0.5537335
    ##   0.454  0.5537335
    ##   0.455  0.5537335
    ##   0.456  0.5537335
    ##   0.457  0.5537335
    ##   0.458  0.5537335
    ##   0.459  0.5537335
    ##   0.460  0.5537335
    ##   0.461  0.5537335
    ##   0.462  0.5537335
    ##   0.463  0.5537335
    ##   0.464  0.5537335
    ##   0.465  0.5537335
    ##   0.466  0.5537335
    ##   0.467  0.5537335
    ##   0.468  0.5537335
    ##   0.469  0.5537335
    ##   0.470  0.5537335
    ##   0.471  0.5537335
    ##   0.472  0.5537335
    ##   0.473  0.5537335
    ##   0.474  0.5537335
    ##   0.475  0.5537335
    ##   0.476  0.5537335
    ##   0.477  0.5537335
    ##   0.478  0.5537335
    ##   0.479  0.5537335
    ##   0.480  0.5537335
    ##   0.481  0.5537335
    ##   0.482  0.5537335
    ##   0.483  0.5537335
    ##   0.484  0.5537335
    ##   0.485  0.5537335
    ##   0.486  0.5537335
    ##   0.487  0.5537335
    ##   0.488  0.5537335
    ##   0.489  0.5537335
    ##   0.490  0.5537335
    ##   0.491  0.5537335
    ##   0.492  0.5537335
    ##   0.493  0.5537335
    ##   0.494  0.5537335
    ##   0.495  0.5537335
    ##   0.496  0.5537335
    ##   0.497  0.5537335
    ##   0.498  0.5537335
    ##   0.499  0.5537335
    ##   0.500  0.5537335
    ##   0.501  0.5537335
    ##   0.502  0.5537335
    ##   0.503  0.5537335
    ##   0.504  0.5537335
    ##   0.505  0.5537335
    ##   0.506  0.5537335
    ##   0.507  0.5537335
    ##   0.508  0.5537335
    ##   0.509  0.5537335
    ##   0.510  0.5537335
    ##   0.511  0.5537335
    ##   0.512  0.5537335
    ##   0.513  0.5537335
    ##   0.514  0.5537335
    ##   0.515  0.5537335
    ##   0.516  0.5537335
    ##   0.517  0.5537335
    ##   0.518  0.5537335
    ##   0.519  0.5537335
    ##   0.520  0.5537335
    ##   0.521  0.5537335
    ##   0.522  0.5537335
    ##   0.523  0.5537335
    ##   0.524  0.5537335
    ##   0.525  0.5537335
    ##   0.526  0.5537335
    ##   0.527  0.5537335
    ##   0.528  0.5537335
    ##   0.529  0.5537335
    ##   0.530  0.5537335
    ##   0.531  0.5537335
    ##   0.532  0.5537335
    ##   0.533  0.5537335
    ##   0.534  0.5537335
    ##   0.535  0.5537335
    ##   0.536  0.5537335
    ##   0.537  0.5537335
    ##   0.538  0.5537335
    ##   0.539  0.5537335
    ##   0.540  0.5537335
    ##   0.541  0.5537335
    ##   0.542  0.5537335
    ##   0.543  0.5537335
    ##   0.544  0.5537335
    ##   0.545  0.5537335
    ##   0.546  0.5537335
    ##   0.547  0.5537335
    ##   0.548  0.5537335
    ##   0.549  0.5537335
    ##   0.550  0.5537335
    ##   0.551  0.5537335
    ##   0.552  0.5537335
    ##   0.553  0.5537335
    ##   0.554  0.5537335
    ##   0.555  0.5537335
    ##   0.556  0.5537335
    ##   0.557  0.5537335
    ##   0.558  0.5537335
    ##   0.559  0.5537335
    ##   0.560  0.5537335
    ##   0.561  0.5537335
    ##   0.562  0.5537335
    ##   0.563  0.5537335
    ##   0.564  0.5537335
    ##   0.565  0.5537335
    ##   0.566  0.5537335
    ##   0.567  0.5537335
    ##   0.568  0.5537335
    ##   0.569  0.5537335
    ##   0.570  0.5537335
    ##   0.571  0.5537335
    ##   0.572  0.5537335
    ##   0.573  0.5537335
    ##   0.574  0.5537335
    ##   0.575  0.5537335
    ##   0.576  0.5537335
    ##   0.577  0.5537335
    ##   0.578  0.5537335
    ##   0.579  0.5537335
    ##   0.580  0.5537335
    ##   0.581  0.5537335
    ##   0.582  0.5537335
    ##   0.583  0.5537335
    ##   0.584  0.5537335
    ##   0.585  0.5537335
    ##   0.586  0.5537335
    ##   0.587  0.5537335
    ##   0.588  0.5537335
    ##   0.589  0.5537335
    ##   0.590  0.5537335
    ##   0.591  0.5537335
    ##   0.592  0.5537335
    ##   0.593  0.5537335
    ##   0.594  0.5537335
    ##   0.595  0.5537335
    ##   0.596  0.5537335
    ##   0.597  0.5537335
    ##   0.598  0.5537335
    ##   0.599  0.5537335
    ##   0.600  0.5537335
    ##   0.601  0.5537335
    ##   0.602  0.5537335
    ##   0.603  0.5537335
    ##   0.604  0.5537335
    ##   0.605  0.5537335
    ##   0.606  0.5537335
    ##   0.607  0.5537335
    ##   0.608  0.5537335
    ##   0.609  0.5537335
    ##   0.610  0.5537335
    ##   0.611  0.5537335
    ##   0.612  0.5537335
    ##   0.613  0.5537335
    ##   0.614  0.5537335
    ##   0.615  0.5537335
    ##   0.616  0.5537335
    ##   0.617  0.5537335
    ##   0.618  0.5537335
    ##   0.619  0.5537335
    ##   0.620  0.5537335
    ##   0.621  0.5537335
    ##   0.622  0.5537335
    ##   0.623  0.5537335
    ##   0.624  0.5537335
    ##   0.625  0.5537335
    ##   0.626  0.5537335
    ##   0.627  0.5537335
    ##   0.628  0.5537335
    ##   0.629  0.5537335
    ##   0.630  0.5537335
    ##   0.631  0.5537335
    ##   0.632  0.5537335
    ##   0.633  0.5537335
    ##   0.634  0.5537335
    ##   0.635  0.5537335
    ##   0.636  0.5537335
    ##   0.637  0.5537335
    ##   0.638  0.5537335
    ##   0.639  0.5537335
    ##   0.640  0.5537335
    ##   0.641  0.5537335
    ##   0.642  0.5537335
    ##   0.643  0.5537335
    ##   0.644  0.5537335
    ##   0.645  0.5537335
    ##   0.646  0.5537335
    ##   0.647  0.5537335
    ##   0.648  0.5537335
    ##   0.649  0.5537335
    ##   0.650  0.5537335
    ##   0.651  0.5537335
    ##   0.652  0.5537335
    ##   0.653  0.5537335
    ##   0.654  0.5537335
    ##   0.655  0.5537335
    ##   0.656  0.5537335
    ##   0.657  0.5537335
    ##   0.658  0.5537335
    ##   0.659  0.5537335
    ##   0.660  0.5537335
    ##   0.661  0.5537335
    ##   0.662  0.5537335
    ##   0.663  0.5537335
    ##   0.664  0.5537335
    ##   0.665  0.5537335
    ##   0.666  0.5537335
    ##   0.667  0.5537335
    ##   0.668  0.5537335
    ##   0.669  0.5537335
    ##   0.670  0.5537335
    ##   0.671  0.5537335
    ##   0.672  0.5537335
    ##   0.673  0.5537335
    ##   0.674  0.5537335
    ##   0.675  0.5537335
    ##   0.676  0.5537335
    ##   0.677  0.5537335
    ##   0.678  0.5537335
    ##   0.679  0.5537335
    ##   0.680  0.5537335
    ##   0.681  0.5537335
    ##   0.682  0.5537335
    ##   0.683  0.5537335
    ##   0.684  0.5537335
    ##   0.685  0.5537335
    ##   0.686  0.5537335
    ##   0.687  0.5537335
    ##   0.688  0.5537335
    ##   0.689  0.5537335
    ##   0.690  0.5537335
    ##   0.691  0.5537335
    ##   0.692  0.5537335
    ##   0.693  0.5537335
    ##   0.694  0.5537335
    ##   0.695  0.5537335
    ##   0.696  0.5537335
    ##   0.697  0.5537335
    ##   0.698  0.5537335
    ##   0.699  0.5537335
    ##   0.700  0.5537335
    ##   0.701  0.5537335
    ##   0.702  0.5537335
    ##   0.703  0.5537335
    ##   0.704  0.5537335
    ##   0.705  0.5537335
    ##   0.706  0.5537335
    ##   0.707  0.5537335
    ##   0.708  0.5537335
    ##   0.709  0.5537335
    ##   0.710  0.5537335
    ##   0.711  0.5537335
    ##   0.712  0.5537335
    ##   0.713  0.5537335
    ##   0.714  0.5537335
    ##   0.715  0.5537335
    ##   0.716  0.5537335
    ##   0.717  0.5537335
    ##   0.718  0.5537335
    ##   0.719  0.5537335
    ##   0.720  0.5537335
    ##   0.721  0.5537335
    ##   0.722  0.5537335
    ##   0.723  0.5537335
    ##   0.724  0.5537335
    ##   0.725  0.5537335
    ##   0.726  0.5537335
    ##   0.727  0.5537335
    ##   0.728  0.5537335
    ##   0.729  0.5537335
    ##   0.730  0.5537335
    ##   0.731  0.5537335
    ##   0.732  0.5537335
    ##   0.733  0.5537335
    ##   0.734  0.5537335
    ##   0.735  0.5537335
    ##   0.736  0.5537335
    ##   0.737  0.5537335
    ##   0.738  0.5537335
    ##   0.739  0.5537335
    ##   0.740  0.5537335
    ##   0.741  0.5537335
    ##   0.742  0.5537335
    ##   0.743  0.5537335
    ##   0.744  0.5537335
    ##   0.745  0.5537335
    ##   0.746  0.5537335
    ##   0.747  0.5537335
    ##   0.748  0.5537335
    ##   0.749  0.5537335
    ##   0.750  0.5537335
    ##   0.751  0.5537335
    ##   0.752  0.5537335
    ##   0.753  0.5537335
    ##   0.754  0.5537335
    ##   0.755  0.5537335
    ##   0.756  0.5537335
    ##   0.757  0.5537335
    ##   0.758  0.5537335
    ##   0.759  0.5537335
    ##   0.760  0.5537335
    ##   0.761  0.5537335
    ##   0.762  0.5537335
    ##   0.763  0.5537335
    ##   0.764  0.5537335
    ##   0.765  0.5537335
    ##   0.766  0.5537335
    ##   0.767  0.5537335
    ##   0.768  0.5537335
    ##   0.769  0.5537335
    ##   0.770  0.5537335
    ##   0.771  0.5537335
    ##   0.772  0.5537335
    ##   0.773  0.5537335
    ##   0.774  0.5537335
    ##   0.775  0.5537335
    ##   0.776  0.5537335
    ##   0.777  0.5537335
    ##   0.778  0.5537335
    ##   0.779  0.5537335
    ##   0.780  0.5537335
    ##   0.781  0.5537335
    ##   0.782  0.5537335
    ##   0.783  0.5537335
    ##   0.784  0.5537335
    ##   0.785  0.5537335
    ##   0.786  0.5537335
    ##   0.787  0.5537335
    ##   0.788  0.5537335
    ##   0.789  0.5537335
    ##   0.790  0.5537335
    ##   0.791  0.5537335
    ##   0.792  0.5537335
    ##   0.793  0.5537335
    ##   0.794  0.5537335
    ##   0.795  0.5537335
    ##   0.796  0.5537335
    ##   0.797  0.5537335
    ##   0.798  0.5537335
    ##   0.799  0.5537335
    ##   0.800  0.5537335
    ##   0.801  0.5537335
    ##   0.802  0.5537335
    ##   0.803  0.5537335
    ##   0.804  0.5537335
    ##   0.805  0.5537335
    ##   0.806  0.5537335
    ##   0.807  0.5537335
    ##   0.808  0.5537335
    ##   0.809  0.5537335
    ##   0.810  0.5537335
    ##   0.811  0.5537335
    ##   0.812  0.5537335
    ##   0.813  0.5537335
    ##   0.814  0.5537335
    ##   0.815  0.5537335
    ##   0.816  0.5537335
    ##   0.817  0.5537335
    ##   0.818  0.5537335
    ##   0.819  0.5537335
    ##   0.820  0.5537335
    ##   0.821  0.5537335
    ##   0.822  0.5537335
    ##   0.823  0.5537335
    ##   0.824  0.5537335
    ##   0.825  0.5537335
    ##   0.826  0.5537335
    ##   0.827  0.5537335
    ##   0.828  0.5537335
    ##   0.829  0.5537335
    ##   0.830  0.5537335
    ##   0.831  0.5537335
    ##   0.832  0.5537335
    ##   0.833  0.5537335
    ##   0.834  0.5537335
    ##   0.835  0.5537335
    ##   0.836  0.5537335
    ##   0.837  0.5537335
    ##   0.838  0.5537335
    ##   0.839  0.5537335
    ##   0.840  0.5537335
    ##   0.841  0.5537335
    ##   0.842  0.5537335
    ##   0.843  0.5537335
    ##   0.844  0.5537335
    ##   0.845  0.5537335
    ##   0.846  0.5537335
    ##   0.847  0.5537335
    ##   0.848  0.5537335
    ##   0.849  0.5537335
    ##   0.850  0.5537335
    ##   0.851  0.5537335
    ##   0.852  0.5537335
    ##   0.853  0.5537335
    ##   0.854  0.5537335
    ##   0.855  0.5537335
    ##   0.856  0.5537335
    ##   0.857  0.5537335
    ##   0.858  0.5537335
    ##   0.859  0.5537335
    ##   0.860  0.5537335
    ##   0.861  0.5537335
    ##   0.862  0.5537335
    ##   0.863  0.5537335
    ##   0.864  0.5537335
    ##   0.865  0.5537335
    ##   0.866  0.5537335
    ##   0.867  0.5537335
    ##   0.868  0.5537335
    ##   0.869  0.5537335
    ##   0.870  0.5537335
    ##   0.871  0.5537335
    ##   0.872  0.5537335
    ##   0.873  0.5537335
    ##   0.874  0.5537335
    ##   0.875  0.5537335
    ##   0.876  0.5537335
    ##   0.877  0.5537335
    ##   0.878  0.5537335
    ##   0.879  0.5537335
    ##   0.880  0.5537335
    ##   0.881  0.5537335
    ##   0.882  0.5537335
    ##   0.883  0.5537335
    ##   0.884  0.5537335
    ##   0.885  0.5537335
    ##   0.886  0.5537335
    ##   0.887  0.5537335
    ##   0.888  0.5537335
    ##   0.889  0.5537335
    ##   0.890  0.5537335
    ##   0.891  0.5537335
    ##   0.892  0.5537335
    ##   0.893  0.5537335
    ##   0.894  0.5537335
    ##   0.895  0.5537335
    ##   0.896  0.5537335
    ##   0.897  0.5537335
    ##   0.898  0.5537335
    ##   0.899  0.5537335
    ##   0.900  0.5537335
    ##   0.901  0.5537335
    ##   0.902  0.5537335
    ##   0.903  0.5537335
    ##   0.904  0.5537335
    ##   0.905  0.5537335
    ##   0.906  0.5537335
    ##   0.907  0.5537335
    ##   0.908  0.5537335
    ##   0.909  0.5537335
    ##   0.910  0.5537335
    ##   0.911  0.5537335
    ##   0.912  0.5537335
    ##   0.913  0.5537335
    ##   0.914  0.5537335
    ##   0.915  0.5537335
    ##   0.916  0.5537335
    ##   0.917  0.5537335
    ##   0.918  0.5537335
    ##   0.919  0.5537335
    ##   0.920  0.5537335
    ##   0.921  0.5537335
    ##   0.922  0.5537335
    ##   0.923  0.5537335
    ##   0.924  0.5537335
    ##   0.925  0.5537335
    ##   0.926  0.5537335
    ##   0.927  0.5537335
    ##   0.928  0.5537335
    ##   0.929  0.5537335
    ##   0.930  0.5537335
    ##   0.931  0.5537335
    ##   0.932  0.5537335
    ##   0.933  0.5537335
    ##   0.934  0.5537335
    ##   0.935  0.5537335
    ##   0.936  0.5537335
    ##   0.937  0.5537335
    ##   0.938  0.5537335
    ##   0.939  0.5537335
    ##   0.940  0.5537335
    ##   0.941  0.5537335
    ##   0.942  0.5537335
    ##   0.943  0.5537335
    ##   0.944  0.5537335
    ##   0.945  0.5537335
    ##   0.946  0.5537335
    ##   0.947  0.5537335
    ##   0.948  0.5537335
    ##   0.949  0.5537335
    ##   0.950  0.5537335
    ##   0.951  0.5537335
    ##   0.952  0.5537335
    ##   0.953  0.5537335
    ##   0.954  0.5537335
    ##   0.955  0.5537335
    ##   0.956  0.5537335
    ##   0.957  0.5537335
    ##   0.958  0.5537335
    ##   0.959  0.5537335
    ##   0.960  0.5537335
    ##   0.961  0.5537335
    ##   0.962  0.5537335
    ##   0.963  0.5537335
    ##   0.964  0.5537335
    ##   0.965  0.5537335
    ##   0.966  0.5537335
    ##   0.967  0.5537335
    ##   0.968  0.5537335
    ##   0.969  0.5537335
    ##   0.970  0.5537335
    ##   0.971  0.5537335
    ##   0.972  0.5537335
    ##   0.973  0.5537335
    ##   0.974  0.5537335
    ##   0.975  0.5537335
    ##   0.976  0.5537335
    ##   0.977  0.5537335
    ##   0.978  0.5537335
    ##   0.979  0.5537335
    ##   0.980  0.5537335
    ##   0.981  0.5537335
    ##   0.982  0.5537335
    ##   0.983  0.5537335
    ##   0.984  0.5537335
    ##   0.985  0.5537335
    ##   0.986  0.5537335
    ##   0.987  0.5537335
    ##   0.988  0.5537335
    ##   0.989  0.5537335
    ##   0.990  0.5537335
    ##   0.991  0.5537335
    ##   0.992  0.5537335
    ##   0.993  0.5537335
    ##   0.994  0.5537335
    ##   0.995  0.5537335
    ##   0.996  0.5537335
    ##   0.997  0.5537335
    ##   0.998  0.5537335
    ##   0.999  0.5537335
    ##   1.000  0.5537335
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.006.

Now we will select our best Classification Tree model using the
`Log Loss` metric.

``` r
best.classtree <- fit.classtree$results %>% 
  mutate(cp = cp, min = min(logLoss)) %>% 
  filter(logLoss == min)
paste("The best classification tree model is cp=",best.classtree$cp," ")
```

    ## [1] "The best classification tree model is cp= 0.006  "

## Random Forest Models

A random forest model is an extension of the bagged tree classification
model. In a bagged tree model, the bootstrapping method is used and the
data is resampled multiple times to create different test sets and then
each test set is trained on models separately. Then our final prediction
is the average of the individual model’s predictions. The Random Forest
model uses the same bagging method and creates multiple trees from the
bootstrap samples and averages the results. The difference is that the
random forest method does not use all of the predictors, instead it uses
a random subset of predictors for each sample and tree fit. More
specifically, in a random forest model, each time there is a split, the
predictor to split on is chosen from a random sample of m predictors
from the full set of predictors. At each node, a new sample of m
predictors is taken. For classification problems, m is about equal to
the square root of the total number of predictors.

In a random forest model, the algorithm cannot choose from the majority
of the existing predictors. This is useful in the case where extra
strong predictors exist. A regular bagged tree model will likely use
said strong predictor for the first split every time, and our resulting
trees will look very similar and likely be highly correlated. The
average prediction of many correlated tree models does not lead to much
of a reduction in the variance. Forcing the model to choose from a
subset of predictors, causes the resulting trees to be less correlated
and therefore the random forest method leads to a reduction in variance
and more reliable prediction. A basic classification tree can be
non-robust and small changes in the data can lead to large alterations
in the result. Bagging and random forest models aggregate many decision
trees to increase the predictive performance of these models. With more
models, we get more accurate predictions. Basic classification trees are
very susceptible to being overfit to our data since their accuracy
improves with each split. The multiple trees created in a random forest
model reduces the chances of overfitting our model making them superior
when it comes to prediction. However, we do lose the interpretability of
a singular classification tree when we use random forest models.

Now that we have an understanding of the random forest model we will fit
a random forest model to our data. Once again we will do cross
validation with 5 folds.

``` r
#create random forest model
fit.rf<- train(Diabetes_binary ~ ., data = diabetes.train.log,
                 #select rf method
                 method = "rf",
                 # calculate log loss metric
                 metric = "logLoss",
                 #do cross validation
                 trControl = trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE),
                 #add tuning parameter
                 tuneGrid = data.frame(mtry = 1:21)
                 )
```

``` r
#print results of random forest model
print(fit.rf)
```

    ## Random Forest 
    ## 
    ## 6636 samples
    ##   21 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5308, 5308, 5309, 5310 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  logLoss  
    ##    1    1.0003486
    ##    2    0.5247799
    ##    3    0.4816743
    ##    4    0.4811622
    ##    5    0.4827359
    ##    6    0.4863590
    ##    7    0.4885962
    ##    8    0.4891771
    ##    9    0.4917656
    ##   10    0.4928954
    ##   11    0.4955803
    ##   12    0.4954707
    ##   13    0.5002642
    ##   14    0.5110301
    ##   15    0.5028463
    ##   16    0.5080529
    ##   17    0.5009971
    ##   18    0.5099692
    ##   19    0.5068923
    ##   20    0.5151976
    ##   21    0.5152822
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 4.

``` r
best.rf <- fit.rf$results %>% 
  mutate(mtry = mtry, min = min(logLoss)) %>% 
  filter(logLoss == min)
paste("The best random forest model is mtry =",best.rf$mtry," ")
```

    ## [1] "The best random forest model is mtry = 4  "

`mtry` is a tuning parameter that gives us the number of randomly
selected predictors we will use. We tested the model on all numbers of
predictors in the dataset (1 to 21).

Next we will look at some models that were not explained in class.

## Logistic Model Tree

Since we are already familiar with logistic regression and decision
trees we decided to take it a step further and model the data using
logistic model trees. Model trees combine regression and trees to
predict an outcome. The standard model tree is best for predicting
numeric variables, which is why we are using the logistic model tree
that works well for categorical variables like `Diabetes_binary`.
Logistic model trees differ from standard decision trees in that each
node has a logistic regression function instead of just a classification
label. Since this could make the tree very complex, typically not all
variables are used at each node.

We will fit the logistic model tree using the `train` function from the
`caret` package. We will continue to use cross validation with 5 folds.
We will specify the `method = LMT`. We are choosing to do 1 to 5
iterations as our tuning parameter.

``` r
#create oblique model
fit.lmt<- train(Diabetes_binary ~ . , data = diabetes.train.log,
                 #select rf method
                 method = "LMT",
                 # calculate log loss metric
                 metric = "logLoss",
                 #preprocess data
                 preProcess = c("center", "scale"),
                 #do cross validation
                 trControl = trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE),
                 #add tuning parameter
                 tuneGrid = data.frame(iter = 1:5)
                 )
```

``` r
#print results of the fit.lmt model
print(fit.lmt)
```

    ## Logistic Model Trees 
    ## 
    ## 6636 samples
    ##   21 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (21), scaled (21) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5308, 5308, 5310, 5310, 5308 
    ## Resampling results across tuning parameters:
    ## 
    ##   iter  logLoss  
    ##   1     0.4911135
    ##   2     0.4817839
    ##   3     0.4753997
    ##   4     0.4819785
    ##   5     0.4769002
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was iter = 3.

Now we will select our best LMT model using the `logLoss` metric.

``` r
best.lmt <- fit.lmt$results %>% 
  mutate(iter = iter, min = min(logLoss)) %>% 
  filter(logLoss == min)
paste("The best Logistic Model Tree is iteration =",best.lmt$iter," ")
```

    ## [1] "The best Logistic Model Tree is iteration = 3  "

## Partial Least Squares Model

Partial Least Squares is a dimension-reduction technique. The algorithm
determines the best model as the one that covers the most variance with
the smallest number of predictors. It’s sort of a combination of
principal component analysis and linear regression. In partial least
squares regression, the predictors are reduced down to a smaller set of
uncorrelated components called latent factors and then linear regression
is performed on these new components. These latent factors summarize the
original predictors and are related to the response variable. Partial
least squares is a good option in cases where we have more predictor
variables than observations or highly correlated predictors.

We will be fitting the model using the `train` function from the `caret`
package. Setting `method = "pls"` will ensure we are creating a partial
least squares model. The `ncomp` tuning parameter represents the number
of principal components included in the model.

``` r
#create PLS model
fit.pls <- train(Diabetes_binary ~  BMI+Age+Income+MentHlth+Sex+PhysHlth+AnyHealthcare+HvyAlcoholConsump+NoDocbcCost, 
                 data= diabetes.train.log,
                 #select pls method
                 method= "pls",
                 #center and sclae the data
                 preProcess=c("center","scale"),
                 #do cross validation
                 trControl= trainControl(method = "cv",
                                           number=5,
                                         summaryFunction = mnLogLoss,
                                         classProbs = TRUE),
                 #calculate log loss metric
                 metric= "logLoss",
                 #add tuning parameters
                 tuneGrid= data.frame(ncomp=1:9))
```

    ## Warning in fitFunc(X, Y, ncomp, Y.add = Y.add, center = center, ...): No convergence in 100 iterations

    ## Warning in fitFunc(X, Y, ncomp, Y.add = Y.add, center = center, ...): No convergence in 100 iterations

``` r
#print
print(fit.pls)
```

    ## Partial Least Squares 
    ## 
    ## 6636 samples
    ##    9 predictor
    ##    2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (9), scaled (9) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 5309, 5309, 5309, 5309, 5308 
    ## Resampling results across tuning parameters:
    ## 
    ##   ncomp  logLoss  
    ##   1      0.5695192
    ##   2      0.5676947
    ##   3      0.5676315
    ##   4      0.5676211
    ##   5      0.5676199
    ##   6      0.5676195
    ##   7      0.5676195
    ##   8      0.5676195
    ##   9      0.5676195
    ## 
    ## logLoss was used to select the optimal model using the smallest value.
    ## The final value used for the model was ncomp = 9.

Now we will select our best PLS model using the `log Loss` metric.

``` r
best.pls <- fit.pls$results %>% 
  mutate(ncomp = ncomp, min = min(logLoss)) %>% 
  filter(logLoss == min)
paste("The best partial least squares model is ncomp=",best.pls$ncomp," ")
```

    ## [1] "The best partial least squares model is ncomp= 9  "

# Final Model Selection

Out of the 6 best models (one of each type) above we will now select the
final best model.

We will start by using the `predict` function with `type = "prob"` to
get the probabilities for diabetes on the test set for each model.

``` r
a<- predict(fit.log3, newdata = diabetes.test.log, type = "prob")
b<- predict(fit.rf, newdata = diabetes.test.log, type = "prob")
c<- predict(fit.lmt, newdata = diabetes.test.log, type = "prob")
d<- predict(fit.lasso, newdata = diabetes.test.log, type = "prob")
e<- predict(fit.classtree, newdata = diabetes.test.log, type = "prob")
f<- predict(fit.pls, newdata = diabetes.test.log, type = "prob")
```

Next we will create a data frame for each model that includes the test
set values for the `Diabetes_binary` variable and the probabilities
calculated above.

``` r
test.set.a <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = a$no, yes = a$yes)
test.set.b <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = b$no, yes = b$yes)
test.set.c <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = c$no, yes = c$yes)
test.set.d <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = d$no, yes = d$yes)
test.set.e <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = e$no, yes = e$yes)
test.set.f <- data.frame(obs = diabetes.test.log$Diabetes_binary, no = f$no, yes = f$yes)
```

Finally we will calculate the log loss for each of the models on the
test set using the data frame with probabilities created above.

``` r
logloss.a<- mnLogLoss(test.set.a, lev = levels(test.set.a$obs))
logloss.b<- mnLogLoss(test.set.b, lev = levels(test.set.b$obs))
logloss.c<- mnLogLoss(test.set.c, lev = levels(test.set.c$obs))
logloss.d<- mnLogLoss(test.set.d, lev = levels(test.set.d$obs))
logloss.e<- mnLogLoss(test.set.e, lev = levels(test.set.e$obs))
logloss.f<- mnLogLoss(test.set.f, lev = levels(test.set.f$obs))
```

Now we will add each of the model’s and their log loss into a tibble.

``` r
name<- c("fit.log3", "fit.rf", "fit.lmt", "fit.lasso", "fit.classtree","fit.pls")
logloss.test <- c(logloss.a, logloss.b, logloss.c, logloss.d, logloss.e, logloss.f)
all.fits<- tibble(name, logloss.test)
```

Next we will use our tibble with all the model’s and their log loss to
chose the best model. The model with the lowest log loss when predicting
in the test set will be the winner so we will use the `min` function to
find the model.

``` r
best<- all.fits %>% 
  mutate(name== name, best = min(logloss.test)) %>% 
  filter(best == logloss.test)
```

Now we will use the `paste` function to give us the result of the best
model.

``` r
paste("The best model is", best$name, " ")
```

    ## [1] "The best model is fit.lasso  "
