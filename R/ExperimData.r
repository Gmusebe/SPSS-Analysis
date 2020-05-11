# Analysing SPSS data in R
## import and activate the libraries needed 
library(foreign)
library(visdat)
library(dplyr)
library(psych)
library(epiDisplay)
library(ggpubr)
library(car)
library(rstatix)
library(tidyverse)


# Import and read the SPSS file
dataset = read.spss(file.choose(), to.data.frame=TRUE)
attach(dataset)

# Overview of data
dim(dataset) # dimension of data: number of rows and columns
str(dataset) # data_type/class of factors
head(dataset) # The first five rows of our data. Use View(data) to see the whole data
vis_dat(dataset) # Check missing value in data


# Reliability of the data : The Cronbch's Alpha
fost <- select(dataset, fost1, fost2, fost3)
psych::alpha(fost) # Fear of statistics reliability

depress <- select(dataset, depress1, depress2, depress3)
psych::alpha(depress) # Depression reliability

confid <- select(dataset, confid1, confid2, confid3)
psych::alpha(confid) # Confidence reliability

# Aplha equal or greater than 0.7 is acceptable 
#There's a very high internal consistency in our data, thus data is reliable


# Frequencies and Descriptives
# Frequencies are for both the strings and numeric variables 
# Descriptive are only for numeric variables.
tab1(dataset$sex, sort.group = "decreasing", cum.percentage = TRUE)
tab1(dataset$group, sort.group = "decreasing", cum.percentage = TRUE )

# How fear of statics at time 1 differ by sex: comparison by means.
with(dataset, by(fost1, sex, mean))
# Males fear statistics more than females


# Independent T-test
# Assumption checking
# Assumptions 1, 2 & 3 cannot be tested but checked by observing the data.
# • Assumption #4: Check there are "no significant outliers" in the data
# By boxplots
ggpubr::ggboxplot(dataset, x = "sex", y = "fost1", color = "sex")
# We have an 1 value that is an outlier from the semal gender.
# Check if outlier is significant: extreme:
dataset %>% group_by(sex) %>% identify_outliers(fost1) # There were no extreme outliers.


# •	Assumption #5: Normality Assumption of fost1 
# For normality: 
# 1. Skewness and Kurtosis
describeBy(dataset$fost1, group= dataset$sex)
spread <- function(x) {
    w = length(x)
    m1 = mean(x)
    m2 = sum((x-m1)^2)
    m3 = sum((x-m1)^3)
    m4 = sum((x-m1)^4)
    s1 = sd(x)
    skew = w*m3/(w-1)/(w-2)/s1^3
    sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
    zskew = skew/sdskew
    kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
    sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
    zkurtosis =kurtosis/sdkurtosis
    mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis, zskew,zkurtosis), 2,
     dimnames=list(c("skew","kurtosis"), c("estimate","se", "z-score")))
    return(mat)
}
fost1male <- dataset$fost1[dataset$sex == "male"]
fost1female <- dataset$fost1[dataset$sex == "female"]
spread(fost1male)
spread(fost1male)
# All the z-values of skewedness and kurtosis are between -1.96 to +1.96
# Data does not differ significantly from normality.

# 2. Q-Q plots
ggqqplot(dataset, "fost2", facet.by = "sex", color = "sex")
#All the points fall approximately along the reference line, for each cell. 
#So we can assume normality of the data.

#Check normality assumption by analyzing the model residuals.
lm(fost1 ~ sex, data = dataset) -> model
plot(model)

# 3. Density curve
ggdensity(dataset$fost1, )

## Shapiro test of normality

dataset %>% group_by(sex) %>% shapiro_test(fost1)

# •Assumption #6: Check that there is homogeneity of variances; Levene's test (One-Way ANOVA)
leveneTest(fost1 ~ sex, dataset, mean)
# Homogeneity of variance has not been violated.

# Having verified the six assumption, one can now perform the independent t-test.
# NB: t.test() assumes unequal variances by default, but our variances have been prooved equal.
t.test(fost1 ~ sex, dataset, var.equal = TRUE)


# Paired sample t-test between fear of statistics at time 1 and time 2.
t.test(fost1, fost2, data= dataset, paired= TRUE)
#There is a statisticlly significant decrease in the fear of statistics after the intervension.

# which intervention was  most effective?
describeBy(dataset$fost2, group = group)
leveneTest(fost2 ~ group, dataset, mean) # Homogeneity of variance has not been violated.
t.test(fost2 ~ group, dataset, var.equal = TRUE)
# There is no statistical significant differences between the two interventions.

# One can also use ANOVA to verify there being no statistical significant differences between the two interventions.
# One - Way ANOVA.
# Compute summary statistics by groups - count, mean, sd:
dataset %>% group_by(group) %>% get_summary_stats(fost2, type = "mean_sd")

ggpubr::ggboxplot(dataset, x = "group", y = "fost2", color = "group")

# Cheeck if the outlier found is significant(i.e to the extreme)
dataset %>% group_by(group) %>% identify_outliers(fost2)

# ANOVA
summary(aov(fost2 ~ group, data = dataset))
# Consistent with the t-test we did (line 88) 
# Prooving no statistical significant differences between the two interventions.

##  Check anova assumptions: test validity?
# Check the homogeneity of variance assumption : The residuals versus fits plot.
plot(aov(fost2 ~ group, data = dataset), 1)


# There is no evident relationships between residuals and fitted values (the mean of each groups), which is good.
# So, we can assume the homogeneity of variances.

# Friedman’s Analysis of Variance
# Was there a change in depression scores across the three different time periods:
# (Pre-intervention, post-intervention and follow up)?

# From the data depress
depress <- select(dataset, id, depress1, depress2, depress3)
depress <- depress %>% gather(key = "time", value = "score", depress1, depress2, depress3) %>% convert_as_factor(id, time)
head(depress)

# Summary statistics
depress %>% group_by(time) %>% get_summary_stats(score, type = "common")

# Visualisation
ggboxplot(depress, x = "time", y = "score", add = "jitter")
# The test
depress %>% friedman_test(score ~ time|id)
# The magnitude effect of the test
 depress %>% friedman_effsize(score ~ time|id)
