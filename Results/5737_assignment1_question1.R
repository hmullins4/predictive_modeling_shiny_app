# QUESTION 1

# install.packages("stargazer")
# install.packages("dplyr")
# install.packages("glmnet")
# install.packages("e1071")
# install.packages("caret")
# install.packages("infotheo")

library(stargazer)
library(dplyr)
library(glmnet)
library(e1071)
library(caret)
library(infotheo)

# Load the dataset Happiness2015.csv. Note that
# the file path will need to be adjusted:

data <- read.csv("C:/Users/hem10/OneDrive/Desktop/Happiness2015.csv", sep = ',')

# 1. Print a subset of the rows of the data.
# We'll observe just the first ten rows.

head(data, n = 10)

# Noting the column names of some variables,
# let's rename them for simplicity.

names(data)[names(data) == "Happiness.Rank"] <- "rank"
names(data)[names(data) == "Happiness.Score"] <- "score"
names(data)[names(data) == "Standard.Error"] <- "standard_error"
names(data)[names(data) == "Economy..GDP.per.Capita."] <- "GDP"
names(data)[names(data) == "Family"] <- "family"
names(data)[names(data) == "Health..Life.Expectancy."] <- "health"
names(data)[names(data) == "Freedom"] <- "freedom"
names(data)[names(data) == "Trust..Government.Corruption."] <- "gov_trust"
names(data)[names(data) == "Generosity"] <- "generosity"
names(data)[names(data) == "Dystopia.Residual"] <- "dystopia_residual"


# 2. Produce a new dataset from it, where the
# first 2 columns are no longer present.

new_data <- data[-c(1,2)]

# 3. Produce a summary of the data, and add
# some brief comments.

summary(new_data)
stargazer(new_data, type = "text")

# What we appear to be observing in this dataset
# are the factors contributing to happiness among
# different countries. There are 158 observations,
# each clearly being ranked and scored based on
# happiness levels. There are measures related to
# the government, such as gross domestic product
# (with an average value of .8461 and a median of
# .9102) and trust in the government (with a mean
# of .14342 and a median of .10722). Additionally,
# there are measures such as family (with a mean of
# .991 and a median of 1.0295) and health (with a
# mean of .6303 and a median of .6967), which seem
# to be more related to individual values. 

# The variable measuring happiness, score, has a
# fairly large range with a mean of 5.376 and a 
# standard deviation of 1.145, suggesting there is
# quite some variation in the happiness levels of
# different countries.

# gov_trust has a fairly low mean of 0.143 and the
# maximum value is only 0.552, meaning across the
# board, even the happiness countries do not have
# much trust in their government. family, health,
# and freedom however have fairly high means in 
# relation to their maximum and minimum values.


# 4. Produce a scatter plot for the 'Happiness Rank'
# column (y) and that of the 'Happiness Score' (x).

plot(new_data$score, new_data$rank,
     xlab = "Happiness Score", ylab = "Happiness Rank",
     main = "Relationship between Rank and Score")

# We can clearly observe from this graph that there is
# an inverse relationship between the happiness rank
# and the happiness score (as the score increases, the
# rank overall decreases). From this, it appears that
# these two variables give us the same information,
# simply represented differently.


# 5. For the previous scatter plot, change the annotations
# of the plot, tick sizes, title etc, to improve the
# readability

par(mar = c(5, 5, 4, 2)) # Adjusts parameters of our plot
plot(new_data$score, new_data$rank,
     xlab = "Happiness Score", ylab = "Happiness Rank",
     main = "Relationship between Rank and Score",
     col = "red", pch = 16, cex = 1.2,
     cex.lab = 1.3, cex.main = 1.5, cex.axis = 1.2,
     xaxt = "n", yaxt = "n")

# Here, I specified the color, plot character, and size
# of axis/tick labels. Additionally, I removed the
# automatic labeling of the axes tick marks, so that I
# could create them myself, as seen below.

axis(2, at = seq(0, 150, by = 25))
axis(1, at = seq(2, 8, by = .5))



# 6. On the scatter plot, now plot a fitted line through
# the points and discuss the quality of that fit (where
# the independent is the 'happiness score' and the de-
# pendent is the 'happiness rank')

m1 <- lm(rank ~ score, data = new_data)
abline(m1, lwd = 2)

summary(m1)

# The summary of our model informs us this linear model
# represents our data quite well, yielding an R-squared
# of approximately 98% (telling us that the Happiness
# Score explains 98% of the variation in our dependent
# variable). The independent variable is extremely
# statistically significant as well. Though the line is
# not a perfect representation of our data, we can
# clearly see that it highlights the trend of these
# two variables with each other (again, an inverse
# relationship).


# Model fits 1: Produce a linear model with the inde-
# pendent as the 'happiness score' and the dependent
# the 'Health'.

lhs_vs_health <- lm(health ~ score, data = new_data)
summary(lhs_vs_health)


# Using "Health" as the dependent variable, the
# Happiness Score is a statistically significant
# explanatory variable at the 95% significance level.
# At the same time, we can see that it explains a
# ittle over half of the variance in our dependent
# variable (52.45%).



# Model Fits 2: Fit up to a degree 5 polynomial to
# the data, plot the model fit, and discuss the
# relevance of each power degree and how it compares
# to the linear fit. Which terms are significant or
# not? Plot the polynomial fit as well and provide
# comments/discussion.

phs_vs_health <- lm(health ~ poly(score, degree = 5),
                    data = new_data)
summary(phs_vs_health)

# Our intercept and "score" variable raised to the
# first power are both extremely significant at a
# significance/alpha level of .001 (99.9% confidence
# level). score raised to the third power is signifi-
# cant at a significance level of .05 (95% confidence
# level), while score raised to the fourth power is
# significant at a level of .1 (90% confidence level).

# The significance level of these variables tells us
# that the polynomial does somewhat better represent
# our data, but the linear component is the most
# relevant part.


par(mar = c(5, 5, 4, 2)) 
plot(new_data$score, new_data$health,
     xlab = "Happiness Score", 
     ylab = "Health", 
     main = "Linear Vs. Degree Five Polynomial", 
     col = "black", pch = 16, cex = 1.2,
     cex.lab = 1.3, cex.main = 1.5, cex.axis = 1.2,
     xaxt = "n", yaxt = "n")
seq <- seq(min(new_data$score), max(new_data$score), length.out = 100)
predict<- predict(phs_vs_health, newdata = data.frame(score = seq))
predict2 <- predict(lhs_vs_health, newdata = data.frame(score = seq))
lines(seq, predict, col = "blue", lwd = 2)
lines(seq, predict2, col = "red", lwd = 2)
axis(2, at = seq(0, 1, by = .1))
axis(1, at = seq(2, 8, by = .5))


# Exploring both summaries, as well as the
# visuals, we see that the polynomial model
# better represents our data, but only slightly.
# The R-squared increases from 0.5245 to 0.5509,
# telling us that the polynomial model explains
# a bit more of our data than the linear one.

# Moreover, looking at the plot, it's apparent
# that the data is not exactly linear. The
# polynomial better follows the range where
# the data dips downward and the range where
# the data rises. 

# The F-statistic is very large with an extremely
# low p-value, telling us that this model as a whole
# explains more of the variance than a model with
# no predictor variables. Collectively, our polynomial
# model has some significance, even if a few of the
# polynomial terms are not significant.


# Using Multiple Variables 1: 

# Begin by producing a linear model with each of the
# variables  specified and calculating the correspond-
# ing root mean squared error.

multi_lm <- lm(score ~ GDP + family + health + freedom 
               + gov_trust + generosity + dystopia_residual, 
               data = new_data)
summary(multi_lm)


multi_lm_rmse <- sqrt(mean(multi_lm$residuals^2))
multi_lm_rmse


# The RMSE for this model is incredibly small,
# only 0.0002748618. Thus, this linear model
# with multiple variables does a good job fitting
# the data and making accurate predictions. Every
# predictor variable is statistically significant
# at a 99.9% confidence level. Moreover, the R-squared
# is equal to 1, and the F-statistic is drastically
# high, implying that there is no way for the
# improvement of model to go but down.

# Now applying LASSO:

x <- model.matrix(score ~ GDP + family + health + freedom 
                  + gov_trust + generosity + dystopia_residual, 
                  data = new_data)[, -1]
y <- new_data$score

cv_lasso <- cv.glmnet(x, y, alpha = 1)
penalty_opt <- cv_lasso$lambda.min
print(penalty_opt)

opt_lasso <-glmnet(x, y, alpha = 1, lambda = penalty_opt ) 
coef(opt_lasso, s= cv_lasso$lambda.min)


# Relative to each other, the coefficients
# of all our variables are very close. This
# adds more confirmation that each of these
# predictors adds value to our model. Under
# this notion, it would not make sense to
# eliminate any one of them.

lasso_predict <- predict(opt_lasso, newx = x, type = "response",
                         s = opt_lasso)
lasso_rmse <- sqrt(mean((y - lasso_predict)^2))
lasso_rmse


# Using the optimal lambda value, the RMSE
# comes out to 0.03317726, which is greater
# than the RMSE of the previous model. While
# applying LASSO did adjust some of the
# coefficient values (though not drastically),
# none of them were shrunk to 0, so they were
# all still included in this LASSO model.

# Next, produce a support vector machine model
# with the same variables:


x <- new_data[, c("GDP", "family", "health", "freedom", 
                  "gov_trust", "generosity", "dystopia_residual")]
y <- new_data$score

svm <- svm(score ~ GDP + family + health + freedom 
           + gov_trust + generosity + dystopia_residual, 
           data = new_data)
summary(svm)

svm_predict <- predict(svm, new_data)
svm_rmse <- RMSE(svm_predict, y)
svm_rmse

# The RMSE for our support vector machine
# model is 0.1096093, an increase from
# both the models we previously observed.
# Based on this measure, the SVM model
# does worse at prediction than both the
# linear model and the linear w/ lasso
# model.


# Using Multiple Variables 3: 

pearson_cor <- cor(new_data[, c("score", "GDP", "family", "health", 
                                "freedom", "gov_trust", "generosity", 
                                "dystopia_residual")], method = "pearson")
pearson_cor


# The pearson coefficient is a linear measure of
# correlation, ranging between -1 and +1. 


pcor_df <- as.data.frame(as.table(pearson_cor))
neg_pcor_df <- pcor_df[pcor_df$Freq < 0, ]
neg_pcor_df

# The majority of our variables have a positive
# linear correlation, with only three negatively
# correlated relationships (those between GDP
# and generosity, dystopia_residual and gov_trust,
# and generosity and dystopia_residual). 


pearson_cor[lower.tri(pearson_cor, diag = TRUE)] <- NA

max_pcor <- max(pearson_cor, na.rm = TRUE)
max_pcor
which(pearson_cor == max_pcor, arr.ind = TRUE)


# The variables with maximum positive correlation
# are GDP and health, with a correlation value
# of 0.816478.

min_pcor <- min(pearson_cor, na.rm = TRUE)
min_pcor
which(pearson_cor == min_pcor, arr.ind = TRUE)


# The variables with maximum negative correlation
# are generosity and dystopia_residual, with a
# correlation value of -0.10130111. 

# Similarly, Spearman Rank correlation also returns
# a value between -1 and +1. It is a non-parametric
# measure of rank correlation, observing how well
# a monotonic function describes the relationship
# between two variables.

spearman_cor <- cor(new_data[, c("score", "GDP", "family", "health", 
                                 "freedom", "gov_trust", "generosity", 
                                 "dystopia_residual")], method = "spearman")
spearman_cor

# One might note that there are no negative corre-
# lations in this matrix, telling us that all the
# variables in our dataset have increasing relationships
# with one another (when one variable goes up, the other
# variable goes up as well)


spearman_cor[lower.tri(spearman_cor, diag = TRUE)] <- NA

max_scor <- max(spearman_cor, na.rm = TRUE)
max_scor
which(spearman_cor == max_scor, arr.ind = TRUE)


# The variables with maximum positive rank correlation
# are GDP and health, with a correlation value
# of 0.8470964. These are the same variables with
# maximum linear correlation.

min_scor <- min(spearman_cor, na.rm = TRUE)
min_scor
which(spearman_cor == min_scor, arr.ind = TRUE)

# The variables with the least rank correlation
# are generosity and dystopia_residual. Interestingly,
# these variables had the greatest negative linear
# correlation previously; however, now it is expressed
# that there is no negative relationship between the
# two. Essentially, there is barely any monotonic 
# relationship between these two predictor variables,
# according to the Spearman Rank results.


# Finally, mutual information is a measure of how much
# one variable tells us about another variable. Any
# values close to zero indicate a very small relationship
# between variables (and if the mutual information = 0,
# then the variables are entirely independent). The
# larger the mutual information, the more certainty we
# might acquire about one random variable given infor-
# mation about another. A benefit of Mutual Information
# is that it examines both linear and nonlinear dependencies.

discretized<- discretize(new_data[, c("score", "GDP", "family", 
                                      "health", "freedom", "gov_trust", 
                                      "generosity", "dystopia_residual")])

mi_matrix <- mutinformation(discretized)
mi_matrix


mi_matrix[lower.tri(mi_matrix, diag = TRUE)] <- NA 

max_mi <- max(mi_matrix, na.rm = TRUE)
which(mi_matrix == max_mi, arr.ind = TRUE)


# Maximum mutual information is found between GDP
# and health. Considering they had the strongest
# correlation for both Pearson and Spearman Rank,
# this further indicates a strong association
# between these two variables.


min_mi <- min(mi_matrix, na.rm = TRUE)
which(mi_matrix == min_mi, arr.ind = TRUE)

# Minimum mutual information is found between
# heatlh and dystopia_residual. We might also
# rank all of these values to determine where
# each pair of variables lies in terms of who
# has the the most/least mutual information:

mi_rank <- as.data.frame(as.table(mi_matrix))
mi_rank <- mi_rank[mi_rank$Var1 != mi_rank$Var2 & !is.na(mi_rank$Freq), ]
mi_rank <- mi_rank[!duplicated(t(apply(mi_rank, 1, sort))), ]
mi_rank <- mi_rank[order(-mi_rank$Freq), ]
mi_rank


# From this, we can determine that two of the
# variables that tend to be more independent 
# from each of the others are dystopia_residual
# and generosity, indicating that they do not
# contribute as much to any of the other variables.




