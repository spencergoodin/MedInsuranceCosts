# Installing and loading required libraries
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('lmtest')
# install.packages('car')
# install.packages('moments')
# install.packages('DHARMa')
# install.packages('knitr')
# install.packages('broom')
# install.packages('modelsummary')
# install.packages('scales')
# install.packages('kableExtra')

library('dplyr')
library('ggplot2')
library('lmtest')
library('car')
library('moments')
library('DHARMa')
library('knitr')
library('broom')
library('modelsummary')
library('scales')
library('kableExtra')

# Loading and checking data
ins.data <- read.csv('insurance.csv')

head(ins.data)

apply(ins.data[c('sex','smoker','region')], 2, unique)

# Summary of measures
summary(ins.data)

# Check for system missing values
colSums(is.na(ins.data))

# Check distribution of children
ins.data %>% count(children)
mean(ins.data$children)

# Check for outliers in charges
head(ins.data[order(-ins.data$charges),]['charges'], 20)

# Creating dummy variables
ins.data$male_dummy <- sapply(
  ins.data$sex, function (x) ifelse(x == 'male', 1, 0)
)
ins.data$smoker_dummy <- sapply(
  ins.data$smoker, function (x) ifelse(x == 'yes', 1, 0)
)

ins.data$sw_dummy <- ifelse(ins.data$region == 'southwest', 1, 0)
ins.data$ne_dummy <- ifelse(ins.data$region == 'northeast', 1, 0)
ins.data$nw_dummy <- ifelse(ins.data$region == 'northwest', 1, 0)

# North/South dummy variable for trimmed models
ins.data$north_dummy <- ifelse(
  ins.data$region == 'northeast' | ins.data$region == 'northwest', 1, 0
)

# Exploration

# Skewness of charges
agostino.test(ins.data$charges)

ggplot(data = ins.data, aes(x=charges)) + 
  geom_histogram(bins=63) + 
  geom_vline(aes(xintercept=mean(charges)), 
             color='#3366FF', 
             linetype = 'dashed',
             linewidth=1) +
  labs(title = "Frequency of Total Costs",
       subtitle = "by Thousands of Dollars",
       x = 'Total Cost', 
       y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Correlation of age and charges
cor.test(x = ins.data$age, y = ins.data$charges, method = "spearman")

ggplot(data = ins.data, aes(x=age, y=charges)) + 
  geom_point()+
  labs(title = "Insurance Cost by Age", 
       x = "Age of Respondent", y = "Total Cost") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_smooth(formula = y ~ x, method = "lm", 
              se = FALSE, fullrange = TRUE, level = 0.95)

# Correlation of bmi and charges
cor.test(x = ins.data$bmi, y = ins.data$charges, method = "spearman")

ggplot(data = ins.data, aes(x=bmi, y=charges)) + 
  geom_point()+
  labs(title = "Insurance Cost by BMI", 
       x = "BMI of Respondent", 
       y = "Total Cost") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_smooth(formula = y ~ x, method = "lm", 
              se = FALSE, fullrange = TRUE, level = 0.95)

# Correlation of children and charges
cor.test(x = ins.data$children, y = ins.data$charges, method = "spearman")

ggplot(data = ins.data, aes(group=children, x=children, y=charges)) + 
  geom_boxplot()+
  labs(title = "Insurance Cost by Number of Children", 
       x = "Number of Children", 
       y = "Total Cost") +
  theme(plot.title = element_text(hjust = 0.5))

# Analysis

# Splitting Train-Test Data
set.seed(123)
ins.data$id <- 1:nrow(ins.data)
train.data <- ins.data %>% sample_frac(0.70)
test.data <- anti_join(ins.data, train.data, by = 'id')

# Creating class for model testing

fitModel <- setRefClass(
  
  'Fitted Model',
  
  fields = list(
    logpredicted = 'numeric', 
    predicted = 'numeric', 
    observed = 'numeric', 
    residuals = 'numeric'
  ),
  
  methods = list(
    initialize = function(model, data = test.data, log = FALSE) {
      if (log == TRUE) {
        logpredicted <<- predict(model, newdata = data)
        predicted <<- exp(logpredicted)
      }
      else {
        predicted <<- predict(model, newdata = data)
      }
      observed <<- data$charges
      residuals <<- observed - predicted
    },
    # Model Root Mean Square Error
    rmse = function(r = residuals) {
      sqrt(mean(r**2))
    },
    # Model Relative Squared Error
    rse = function(r = residuals, p = predicted, o = observed) {
      sum(r**2)/sum((p - mean(o))**2)
    },
    # Model Mean Absolute Error (MAE)
    mae = function(r = residuals) {
      mean(abs(r))
    },
    # Model Relative Absolute Error (RAE)
    rae = function(r = residuals, p = predicted, o = observed) {
      sum(abs(r))/sum(abs(p - mean(o)))
    },
    allTests = function() {
      return(as.vector(c(rmse(), rse(), mae(), rae())))
    }
  )
)

# Multiple Linear Regression (MLR) model

# Fitting MLR
mlr.model <- lm(charges ~ age + male_dummy + bmi + smoker_dummy + children +
                  sw_dummy + ne_dummy + nw_dummy, 
                data = train.data)

summary(mlr.model)

# Examining residuals of MLR model
plot(density(mlr.model$residuals), 
     main = 'MLR Distribution of Residuals')
plot(mlr.model, which = 2, 
     caption = NA, sub.caption = NA, 
     main = 'MLR Q-Q Residuals')
plot(mlr.model, which = 1, 
     caption = NA, sub.caption = NA, 
     main = 'MLR Residuals vs Fitted Values')
plot(mlr.model, which = 3, 
     caption = NA, sub.caption = NA, 
     main = 'MLR Scale-Location')

hist(mlr.model$residuals,
     breaks = 20,
     main = 'MLR Residual Distribution',
     xlab = 'Residual')

# MLR Interaction Identification
mlri.model <- lm(charges ~ age + male_dummy + smoker_dummy + bmi +
                   bmi*smoker_dummy + children + sw_dummy + ne_dummy + nw_dummy, 
                 data = train.data)

summary(mlri.model)

# MLRi Residuals
hist(mlri.model$residuals,
     breaks = 20,
     main = 'MLRi Residual Distribution',
     xlab = 'Residual')

plot(mlri.model, which = 1, 
     caption = NA, sub.caption = NA, 
     main = 'MLRi Residuals vs Fitted Values') 

# Reduced MLR model
rmlr.model <- lm(charges ~ age + male_dummy + smoker_dummy + bmi +
                   smoker_dummy*bmi + children + north_dummy, 
                 data = train.data)

rmlr2.model <- lm(charges ~ age + male_dummy + smoker_dummy + bmi +
                    bmi*smoker_dummy + children,
                  data = train.data)

summary(rmlr.model)
summary(rmlr2.model)


# Table of Large Outlying Error Values in MLRi Model
results <- data.frame(mlri.model$model)
results$estimate <- mlri.model$fitted.values
results$residual <- mlri.model$residuals

results$male_dummy <- sapply(
  results$male_dummy, function (x) ifelse(x == 0, "Female", "Male")
)

outlying <- data.frame(
  "Min Non-Smoking" <- subset(results, smoker_dummy == 0) %>% 
    subset(residual == min(results$residual[results$smoker_dummy == 0])) %>%
    select(c(charges, estimate, residual, age, male_dummy, bmi, children)) %>%
    t(),
  "Max Non-Smoking" <- subset(results, smoker_dummy == 0) %>% 
    subset(residual == max(results$residual[results$smoker_dummy == 0])) %>%
    select(c(charges, estimate, residual, age, male_dummy, bmi, children)) %>%
    t(),
  "Min Smoking" <- subset(results, smoker_dummy == 1) %>% 
    subset(residual == min(results$residual[results$smoker_dummy == 1])) %>%
    select(c(charges, estimate, residual, age, male_dummy, bmi, children)) %>%
    t(),
  "Max Smoking" <- subset(results, smoker_dummy == 1) %>% 
    subset(residual == max(results$residual[results$smoker_dummy == 1])) %>%
    select(c(charges, estimate, residual, age, male_dummy, bmi, children)) %>%
    t()
)

rownames(outlying) <- c("Cost","Estimate","Residual","Age","Sex","BMI","Children")

outlying

mean(train.data$bmi)
mean(train.data$age)


# Testing Model Performance

# Creating results objects
mlriTrainFit <- fitModel(mlri.model, data = train.data)
mlriTestFit <- fitModel(mlri.model)

# Creating data frame for goodness of prediction metrics
test.metrics <- data.frame(
  'Training' = c(mlriTrainFit$rmse(), mlriTrainFit$mae()),
  'Testing' = c(mlriTestFit$rmse(), mlriTestFit$mae())
)

row.names(test.metrics) <- c('RMSE', 'MAE')

test.metrics

t.test(mlriTrainFit$residuals, mlriTestFit$residuals)

# # WLS and LogMLR models for appendix
# weights <- 1/fitted( lm(abs(residuals(mlri.model)) ~ fitted(mlri.model)) )**2
# wls.model <- lm(charges ~ age + male_dummy + smoker_dummy + bmi +
#                   smoker_dummy*bmi + children + sw_dummy + ne_dummy + nw_dummy, 
#                 data = train.data, 
#                 weights = weights)
# 
# train.data$log_charges <- log(train.data$charges)
# logmlr.model <- lm(log_charges ~ age + male_dummy + smoker_dummy + bmi +
#                      bmi*smoker_dummy + children + sw_dummy + ne_dummy + nw_dummy, 
#                    data = train.data)
# 
# summary(wls.model)
# summary(logmlr.model)
# )