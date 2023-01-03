## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
sales <- read.csv(file.choose(), header=T)

# Print the data frame.
head(sales)

view(sales)
# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
df2 <- sales[,c('Product','Platform','NA_Sales','EU_Sales','Global_Sales')]

# View the data frame.
View(df2)

# Check the structure of the dataframe
str(df2)

as.character(df2$Product)

# Check for duplicates
library(dplyr)
n_distinct(df2$Product)
unique(df2$Product)

# View the descriptive statistics.
summary(df2)

# sort the dataframe

order(df2$Global_Sales,decreasing = TRUE)

df2

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Global_Sales,
      Product,
      data=df2)

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, data=df2)

## 2c) Boxplots
# Create boxplots.
qplot(Product, Global_Sales, data=df2, geom='boxplot')


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(df2)

# Check output: Determine the min, max, and mean values.
print(min(df2$NA_Sales))
print(min(df2$EU_Sales))
print(min(df2$Global_Sales))

print(max(df2$NA_Sales))
print(max(df2$EU_Sales))
print(max(df2$Global_Sales))

print(mean(df2$NA_Sales))
print(mean(df2$EU_Sales))
print(mean(df2$Global_Sales))

# View the descriptive statistics.
summary(df2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df3 <- df2 %>% group_by(Product) %>%
  summarise(Total_Sales=sum(Global_Sales), Total_NA=sum(NA_Sales), Total_EU=sum(EU_Sales),
            .groups='drop')

df3$Total_EU_NA <- df3$Total_NA + df3$Total_EU

df3$Total_Other <- (df3$Total_Sales - df3$Total_EU_NA)

# View the data frame.
view(df3)

# Explore the data frame.

df <-df3[order(df3$Total_Sales),]
head(df)

view(df)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Total_Sales,
      Product,
      data=df)

# Create histograms.
qplot(Total_Sales, data=df, bins=4) +
  coord_flip() +
  theme_classic()

qplot(Total_Sales >20, data=df3) +
  coord_flip() +
  theme_classic()

# Create boxplots.
qplot(Product, Total_Sales, data=df3, geom='boxplot')


# barchart
ggplot(top_products, aes(x=Product, y=Total_Sales)) + 
  geom_bar(stat = "identity") +
  coord_flip()

df[df$Total_Sales > 20,]

top_products <-  data.frame(subset(df, Total_Sales >20))
view(top_products)

subset(df, (Total_Sales > 10) & (Total_Sales < 20) )

subset(df, Total_Sales <10)
###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(df3$Total_Sales)

# Add a reference line:
qqline(df3$Total_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test((df3$Total_Sales))

# Our p-value is <0.05, and we can conclude that the sample data is  
# not normally distributed.

## 3c) Determine Skewness and Kurtosis
# Skewness 
skewness(df3$Total_Sales)


# Kurtosis
kurtosis(df3$Total_Sales)


## 3d) Determine correlation
# Determine correlation.
cor(df3$Total_Sales, df3$Product)

# Our correlation coefficient of -0.6 suggests a strong correlation.
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(df3)

# Determine a summary of the data frame.
summary(df3)
head(df3)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
sales1 = data.frame(sales$Ranking,sales$Product,sales$Year,sales$NA_Sales,sales$EU_Sales,sales$Global_Sales)

view(sales1)

str(sales1)

# Correlation
cor(sales1)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales1$sales.Year, sales1$sales.Global_Sales)


model1 <- lm(sales.Global_Sales~sales.Year,
             data=sales1)

abline(coefficients(model1))

summary(model1)

# Second visualisation
plot(sales1$sales.NA_Sales, sales1$sales.Global_Sales)

model2 <- lm(sales.Global_Sales~sales.NA_Sales,
             data=sales1)

abline(coefficients(model2))

summary(model2)

# Third visualisation
plot(sales1$sales.EU_Sales, sales1$sales.Global_Sales)

model3 <- lm(sales.Global_Sales~sales.EU_Sales,
             data=sales1)

abline(coefficients(model3))

summary(model3)

# Fourth visualisation
plot(sales1$sales.EU_Sales, sales1$sales.NA_Sales)

model4 <- lm(sales.NA_Sales~sales.EU_Sales,
             data=sales1)

abline(coefficients(model4))

summary(model4)

# Fifth visualisation
plot(sales1$sales.Ranking sales1$sales.Global_Sales)


model7 <- lm(sales.Global_Sales~sales.Ranking,
             data=sales1)

abline(coefficients(model7))

summary(model7)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (df3) and set 
# character size (cex=2).
corPlot(sales1, cex=2)

# Multiple linear regression model.
model5 = lm(sales.Global_Sales~sales.Year+sales.Ranking+sales.Product, data=sales1)
summary(model5)

model5

model6 = lm(sales.Global_Sales~sales.NA_Sales+sales.EU_Sales, data=sales1)
summary(model6)

model6

model7 = lm(sales.Global_Sales~sales.Ranking+sales.Year, data=sales1)
summary(model7)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

predictTest = predict(model6, data=sales1$sales.NA_Sales,
                      interval='confidence')

# Print the object.
predictTest 

head(predictTest)

view(predictTest)

sales2 = data.frame(sales1[sales1$sales.NA_Sales %in% c('34.02','3.93','2.73','2.26','22.08'),])

view(sales2)

predictTest2 = data.frame(predictTest[c(1,99,176,211,10),])

view(predictTest2)

data_frame_merge <- merge(sales2, predictTest2,
                          by = 'row.names', all = TRUE)

view(data_frame_merge)
###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




