# Clear Workspace ---------------------------------------------------------

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(car) # for residualPlots function
library(nlme) # for multilevel modelling lme function

# Load Data ---------------------------------------------------------------

Cassidy <- read.csv("C:/R Portfolio/Cassady.csv")
glimpse(Cassidy)
unique(Cassidy$Minority)
Model1.1 <- lm(GPA ~ CTA.tot + BStotal, Cassidy)
summary(Model1.1)

# Checking Regression Model Assumptions -----------------------------------

library(car)
residualPlots(Model1.1)

###################################
# Residual Plot Output Discussion #
###################################

# The assumption of homogeneity of variance can be checked through an examination of the residual by fitted plot. If the assumption holds, this plot should display a formless cloud of data points with no discernible shapes that are equally spaced across all values of x
# In addition, the linearity of the relationships between each independent variable and the 
# dependent variable is assessed by an examination of the plots involving them. For example, it is appropriate to assume linearity for BStotal if the residual plots show no discernible pattern. This may be further explained by an examination of the fitted line. If this line is essentially flat, as is the case here, we can conclude that any relationship between BStotal and GPA is only linear

# Tukety Test tests the null hypothesis that the model is additive and that no interactions exist among the independent variables (Tukey, 1949). A nonsignificant result, such as that found for this example, indicates that no interaction is required in the model
# The other tests included here are for the squared term of each independent 
# variable. For example, given that the Test stat results for CTA.tot and BStotal are not significant, we can conclude that neither of these variables has a quadratic relationship with GPA.

##############################
# Normality Assumption Check #
##############################

qqPlot(Model1.1)

# Essentially, the graph displays the data as it actually is on the x axis and as it would be if normally distributed on the y axis. The individual data points are represented in R by black circles. The solid line represents  the  data conforming perfectly to the normal distribution. Therefore, the closer the observed data (circles) are to the solid line, the 
# more closely the data conforms to the normal distribution. In addition, R provides a 95% confidence interval for the line, so that when the data points fall within it they are deemed to conform to the normal distribution. In this example, the data appear to follow the normal distribution fairly closely.

# The NMLE Package --------------------------------------------------------

########################################################
# Simple (Intercept Only) Multilevel Models Using nlme #
########################################################

# Here, we want to determine the extent to which vocabulary scores can be used to predict general reading achievement. Students are nested within schools, which means that standard linear regression models are not appropriate. Here, school is a random effect and vocabulary scores are fixed. The first model to be fitted is the null model with no independent variable. This model is useful for obtaining estimates of the residual and intercept variance when only the clustering by school is considered The lme syntax necessary for estimating the null model appears below

Achieve <- read.csv("C:/R Portfolio/Achieve.csv")
Model3.0 <- lme(fixed = geread~1, random = ~1|school, 
                data = Achieve)
summary(Model3.0)

Linear mixed-effects model fit by REML
Data: Achieve 
AIC      BIC    logLik
46274.31 46296.03 -23134.15

Random effects:
  Formula: ~1 | school
(Intercept) Residual
StdDev:   0.6257119  2.24611

Fixed effects:  geread ~ 1 
Value  Std.Error    DF t-value p-value
(Intercept) 4.306753 0.05497501 10160 78.3402       0

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-2.3229469 -0.6377948 -0.2137753  0.2849664  3.8811630 

Number of Observations: 10320
Number of Groups: 160 

# Even though it is a model which contains no independent variable, it provides some useful information that helps us to understand the data structure. In particular, the AIC and BIC values that are of primary interest in this case will be useful in comparing this model with others that include one or more independent variables.

# Additionally, the null model also provides estimates of the variance among the individuals and among the clusters. In turn, these values can be used to estimate the  Intraclass Correlation. In cases where individuals are clustered or nested within a higher level unit (e.g., classroom, school, school district), it is possible to estimate the correlation among individuals’ scores within the cluster or nested structure using the intraclass correlation (ICC,in the population). It measures the proportion of variation in the outcome variable that occurs between groups versus the total variation present. It ranges from 0 (no variance among clusters) to 1 (variance among clusters but no within-cluster variance). It can also be conceptualized as the correlation for the dependent measure for two individuals randomly selected from the same cluster.

# From the summary, we need to take the StdDev Intercept and divide it by the same value + the StdDev Residual. So, it is 0.6257119/0.6257119 + 2.24611 = 0.2178797. This means that the correlation of reading test scores among students within the same schools is 0.22 if we round our result.

#############
# Model 3.1 #
#############

# To fit the model with vocabulary as the independent variable using lme, the following syntax is used

Model3.1 <- lme(fixed = geread~gevocab, 
                random = ~1|school, 
                data = Achieve)

# In the first part of the function call, we define the formula for the model fixed effects, very similar to model definition of linear regression using lm(). The statement fixed = geread~gevocab essentially says that the reading score is predicted with the vocabulary score fixed effect. The random part of the function call defines the random effects and the nesting structure. If only a random intercept is desired, the syntax for the intercept is 1. In this example, random = ~1|school indicates that only a random intercepts model will be used and that the random intercept varies within school. This corresponds to the data structure of students nested within schools. 

summary(Model3.1)

Linear mixed-effects model fit by REML
Data: Achieve 
AIC      BIC    logLik
46274.31 46296.03 -23134.15

Random effects:
  Formula: ~1 | school
(Intercept) Residual
StdDev:   0.6257119  2.24611

Fixed effects:  geread ~ 1 
Value  Std.Error    DF t-value p-value
(Intercept) 4.306753 0.05497501 10160 78.3402       0

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-2.3229469 -0.6377948 -0.2137753  0.2849664  3.8811630 

Number of Observations: 10320
Number of Groups: 160 

# The AIC and BIC scores are smaller for Model3.1, so geovcab needs to be retained in a model. The large t-value and low p-value for geovcab in Model3.1 also indicate this
# as vocabulary score increases by 1 point, reading ability increases by 0.513 points. It is also possible to compare this model with the null model (Model3.0), by comparing the AIC and BIC scores. It is important to remember that smaller scores indicate a better model fit. For Model 3.1, the AIC and BIC are 43145.2 and 43174.17, respectively. For Model 3.0, the AIC and BIC were 46274.31 and 46296.03. Because the values for both statistics are smaller for Model 3.1, we can conclude that it provides a better fit to the data. Substantively, this means that we should include the predictor variable geread, which the results of the hypothesis test also supported.

############
# Model3.2 #
############

# We want to determine whether school enrollment size (senroll) also produces a statistically significant impact on overall reading score. Note that in this specific function call, senroll, is included only in the  fixed part of the model and not in the random part. This variable thus has only a fixed (average) effect and is the same across all schools.

Model3.2 <- lme(fixed = geread~gevocab + senroll, 
                random = ~1|school, 
                data = Achieve)
summary(Model3.2)

#  Note that in this model, senroll, is included only in the fixed part of the model and not in the random part. This variable thus has only a fixed (average) effect and is the same across all schools. The results indicate that enrollment did not have a statistically significant relationship with reading achievement. Additionally, there are some minor changes in the estimates of the other model parameters and a fairly large change in the correlation between the fixed effect of gevocab slope and the 
fixed effect of the intercept. The slope for senroll and intercept were strongly 
negatively correlated and the slopes of the fixed effects exhibited virtually no 
correlation. As noted earlier, these correlations are typically not very helpful 
for explaining the dependent variable and are rarely discussed in any detail in 
reports of analysis results. T