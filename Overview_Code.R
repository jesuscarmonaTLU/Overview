### Overview

housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows

names(housing.df)  # print a list of variables to the screen.
t(t(names(housing.df)))  # print the list in a useful column format
colnames(housing.df)[1] <- c("TOTAL_VALUE")  # change the first column's name
class(housing.df$REMODEL) # REMODEL is a factor variable
levels(housing.df[, 14])  # It can take one of three levels
class(housing.df$BEDROOMS)  # BEDROOMS is an integer variable
class(housing.df[, 1])  # Total_Value is a numeric variable

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(housing.df), train.rows) 
valid.data <- housing.df[valid.rows, ]


#### Perform linear modeling on training dataset

reg <- lm(TOTAL_VALUE ~ .-TAX -REMODEL, data = housing.df, subset = train.rows) # remove variable "TAX"
tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)
head(tr.res)



#### Perform use the training dataset to predict values on validation dataset

pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL_VALUE, pred, residuals = 
                       valid.data$TOTAL_VALUE - pred)
head(vl.res)



library(forecast)
# compute accuracy on training set
accuracy(reg$fitted.values, train.data$TOTAL_VALUE)

# compute accuracy on prediction set
pred <- predict(reg, newdata = valid.data)
accuracy(pred, valid.data$TOTAL_VALUE)

