
# Multilevel Models -------------------------------------------------------

###################
# Clear Workspace #
###################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lme4)

# The lme4 Package --------------------------------------------------------

#############################################
# Simple (Intercept Only) Multilevel Models #
#############################################

# Here, we want to determine the extent to which vocabulary scores can be used to predict general reading achievement. Students are nested within schools, which means that standard linear regression models are not appropriate. Here, school is a random effect and vocabulary scores are fixed. The first model to be fitted is the null model with no independent variable. This model is useful for obtaining estimates of the residual and intercept variance when only the clustering by school is considered The lme syntax necessary for estimating the null model appears below

Achieve <- read.csv("C:/R Portfolio/Achieve.csv")

Model3.0 <- lmer(geread ~ 1 + (1|school), data = Achieve)

summary(Model3.0)

Linear mixed model fit by REML. t-tests use Satterthwaites method ['lmerModLmerTest']
Formula: geread ~ 1 + (1 | school)
   Data: Achieve

REML criterion at convergence: 46268.3

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.3229 -0.6378 -0.2138  0.2850  3.8812 

Random effects:
 Groups   Name        Variance Std.Dev.
 school   (Intercept) 0.3915   0.6257  
 Residual             5.0450   2.2461  
Number of obs: 10320, groups:  school, 160

Fixed effects:
             Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)   4.30675    0.05498 158.53888   78.34   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Even though it is a model which contains no independent variable, it provides some useful information that helps us to understand the data structure. In particular, the AIC and BIC values that are of primary interest in this case will be useful in comparing this model with others that include one or more independent variables.

# Additionally, the null model also provides estimates of the variance among the individuals and among the clusters. In turn, these values can be used to estimate the  Intraclass Correlation. In cases where individuals are clustered or nested within a higher level unit (e.g., classroom, school, school district), it is possible to estimate the correlation among individuals’ scores within the cluster or nested structure using the intraclass correlation (ICC,in the population). It measures the proportion of variation in the outcome variable that occurs between groups versus the total variation present. It ranges from 0 (no variance among clusters) to 1 (variance among clusters but no within-cluster variance). It can also be conceptualized as the correlation for the dependent measure for two individuals randomly selected from the same cluster.

# From the summary, we need to take the StdDev Intercept and divide it by the same value + the StdDev Residual. So, it is 0.6257119/0.6257119 + 2.24611 = 0.2178797. This means that the correlation of reading test scores among students within the same schools is 0.22 if we round our result.

#############
# Model 3.1 #
#############

# To fit the model with vocabulary as the independent variable, the following syntax is used

Model3.1 <- lmer(geread ~ gevocab + (1|school), 
                 data = Achieve)

# In the first part of the function call, we define the formula for the model fixed effects, very similar to model definition of linear regression using lm(). The statement fixed = geread~gevocab essentially says that the reading score is predicted with the vocabulary score fixed effect. The random part of the function call defines the random effects and the nesting structure. If only a random intercept is desired, the syntax for the intercept is 1. In this example, random = ~1|school indicates that only a random intercepts model will be used and that the random intercept varies within school. This corresponds to the data structure of students nested within schools. 

summary(Model3.1)

Linear mixed-effects model fit by REML
Data: Achieve 
AIC      BIC   logLik
43145.2 43174.17 -21568.6

Random effects:
  Formula: ~1 | school
(Intercept) Residual
StdDev:   0.3158785  1.94074

Fixed effects:  geread ~ gevocab 
Value  Std.Error    DF  t-value p-value
(Intercept) 2.0233559 0.04930868 10159 41.03447       0
gevocab     0.5128977 0.00837268 10159 61.25850       0
Correlation: 
  (Intr)
gevocab -0.758

Standardized Within-Group Residuals:
  Min         Q1        Med         Q3        Max 
-3.0822506 -0.5734728 -0.2103488  0.3206692  4.4334337 

Number of Observations: 10320
Number of Groups: 160 

# The AIC and BIC scores are smaller for Model3.1, so geovcab needs to be retained in a model. The large t-value and low p-value for geovcab in Model3.1 also indicate this
# as vocabulary score increases by 1 point, reading ability increases by 0.513 points. It is also possible to compare this model with the null model (Model3.0), by comparing the AIC and BIC scores. It is important to remember that smaller scores indicate a better model fit. For Model 3.1, the AIC and BIC are 43145.2 and 43174.17, respectively. For Model 3.0, the AIC and BIC were 46274.31 and 46296.03. Because the values for both statistics are smaller for Model 3.1, we can conclude that it provides a better fit to the data. Substantively, this means that we should include the predictor variable geread, which the results of the hypothesis test also supported.

# Comparing Models ---------------------------------------------------------

# If we want to compare the fit of multiple models to identify a best fitting model given the data. Model comparison information can be obtained through use of the anova() function. This function can provide two different types of model fit information: AIC and BIC statistics, and the chi-square test of model fit. When working with nested models, where one model is a more constrained (i.e. simpler) version of another, we may wish to test whether overall fit of the two models differs significantly (as opposed to using AIC and BIC statistics, which are more general model comparison statistics and can’t provide this level of detail). Such hypothesis testing is possible using the chi-square difference test based on the deviance statistic. When the fits of nested models are being compared, the difference in chi-square values for each model deviance can be used to compare model fit. It is important to note that for the chi-square test of deviance to be accurate for multilevel models, maximum likelihood estimation must be used. When maximum likelihood is used, both fixed and random effects are compared simultaneously. When restricted maximum likelihood is used, only random effects are compared. The anova() command in lme4 automatically refits models using maximum likelihood if it was not run with this estimation previously. When we compare the first two models in this chapter. If you recall, we began by running a null model predicting geread from no predictors and a simple random intercepts model predicting geread from gevocab. We can compare the fit for these two models by using the function call anova(Model3.0,Model3.1). We obtain the following output:

anova(Model3.0, Model3.1)

# Referring to the AIC and BIC statistics, recall that smaller values reflect better model fit. For Model3.1, the AIC and BIC are 43132 and 43161, respectively, whereas for Model3.0 the AIC and BIC were 46270 and 46292. Given that the values for both statistics are smaller for Model3.1, we would conclude that it provides a better fit to the data. Since these models are nested, we may also look at the chi-square test for deviance. This test yielded a statistically significant chi-square (χ2=3139.9, p<.001) indicating that Model3.1 provided a significantly better fit to the data than does Model3.0. Substantively, this means that we should include the predictor variable geread, which results of the hypothesis test also supported.

############
# Model3.2 #
############

# We want to determine whether school enrollment size (senroll) also produces a statistically significant impact on overall reading score. Note that in this specific function call, senroll, is included only in the  fixed part of the model and not in the random part. This variable thus has only a fixed (average) effect and is the same across all schools.

Model3.2 <- lmer(geread ~ gevocab + senroll + (1|school),
                 data = Achieve)

summary(Model3.2)

Linear mixed model fit by REML. t-tests use Satterthwaites method ['lmerModLmerTest']
Formula: geread ~ gevocab + senroll + (1 | school)
   Data: Achieve

REML criterion at convergence: 43152.1

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.0834 -0.5729 -0.2103  0.3212  4.4336 

Random effects:
 Groups   Name        Variance Std.Dev.
 school   (Intercept) 0.1003   0.3168  
 Residual             3.7665   1.9408  
Number of obs: 10320, groups:  school, 160

Fixed effects:
              Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)  2.075e+00  1.140e-01  2.373e+02   18.20   <2e-16 ***
gevocab      5.129e-01  8.373e-03  9.798e+03   61.25   <2e-16 ***
senroll     -1.026e-04  2.051e-04  1.652e+02   -0.50    0.618    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
        (Intr) gevocb
gevocab -0.327       
senroll -0.901 -0.002

#  Note that in this model, senroll, is included only in the fixed part of the model and not in the random part. This variable thus has only a fixed (average) effect and is the same across all schools. The results indicate that enrollment did not have a statistically significant relationship with reading achievement.

# Interactions and Cross Level Interactions -------------------------------

# Interactions among the predictor variables, and in particular cross-level interactions, can be very important features in the application of multilevel models. Cross-level interactions occur when the impact of a level-1 variable on the outcome (e.g. vocabulary score) differs depending on the value of the level-2 predictor (e.g. school enrollment). Interactions, be they within the same level or cross-level, are simply the product of two predictors. Thus, incorporation of interactions and cross-level interactions in multilevel modeling is accomplished in much the same manner as we saw for the lm() function. Following are examples for fitting an interaction model for two level-1 variables (Model3.3) and a cross-level interaction involving level-1 and level-2 variables (Model3.4).

Model3.3 <- lmer(geread ~ gevocab + age + gevocab*age + (1|school), 
                 data = Achieve)
Model3.4 <- lmer(geread~gevocab + senroll + gevocab*senroll + (1|school), 
                 data = Achieve)

# Model3.3 defines a multilevel model where two level-1 (student level)  predictors interact with one another. Model3.4 defines a multilevel model with a cross-level interaction: where a level-1 (student level) and a level-2 (school level) predictor are interacting. As can be seen, there is no difference in the treatment of variables at different levels when computing interactions.

summary(Model3.3)

Linear mixed model fit by REML. t-tests use Satterthwaites method ['lmerModLmerTest']
Formula: geread ~ gevocab + age + gevocab * age + (1 | school)
   Data: Achieve

REML criterion at convergence: 43143.5

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.0635 -0.5706 -0.2108  0.3191  4.4467 

Random effects:
 Groups   Name        Variance Std.Dev.
 school   (Intercept) 0.09875  0.3143  
 Residual             3.76247  1.9397  
Number of obs: 10320, groups:  school, 160

Fixed effects:
              Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)  5.187e+00  8.668e-01  1.031e+04   5.984 2.24e-09 ***
gevocab     -2.808e-02  1.881e-01  1.030e+04  -0.149 0.881373    
age         -2.937e-02  8.035e-03  1.031e+04  -3.655 0.000258 ***
gevocab:age  5.027e-03  1.750e-03  1.030e+04   2.873 0.004072 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) gevocb age   
gevocab     -0.879              
age         -0.998  0.879       
gevocab:age  0.877 -0.999 -0.879

# Looking at the output from Model3.3, both age (t = −3.65) and the interaction (gevocab:age)between age and vocabulary (t = 2.87) are statistically significant predictors of reading. Focusing on the interaction, the sign on the coefficient is positive indicating an enhancing effect: as age increases, the relationship between reading and vocabulary becomes stronger. Interestingly, when both age and the interaction are included in the model, the relationship between vocabulary score and reading performance is no longer statistically significant.

Model3.4 <- lmer(geread ~ gevocab + senroll + gevocab * senroll + (1|school), 
                 data = Achieve)

summary(Model3.4)

Linear mixed model fit by REML. t-tests use Satterthwaites method ['lmerModLmerTest']
Formula: geread ~ gevocab + senroll + gevocab * senroll + (1 | school)
   Data: Achieve

REML criterion at convergence: 43163.6

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-3.1228 -0.5697 -0.2090  0.3188  4.4359 

Random effects:
 Groups   Name        Variance Std.Dev.
 school   (Intercept) 0.1002   0.3165  
 Residual             3.7646   1.9403  
Number of obs: 10320, groups:  school, 160

Fixed effects:
                  Estimate Std. Error         df t value Pr(>|t|)    
(Intercept)      1.748e+00  1.727e-01  1.058e+03  10.118   <2e-16 ***
gevocab          5.851e-01  2.986e-02  9.766e+03  19.592   <2e-16 ***
senroll          5.121e-04  3.186e-04  8.402e+02   1.607   0.1084    
gevocab:senroll -1.356e-04  5.379e-05  9.849e+03  -2.520   0.0118 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) gevocb senrll
gevocab     -0.782              
senroll     -0.958  0.735       
gevcb:snrll  0.752 -0.960 -0.766

# The output from Model3.4 has a similar interpretation to that of Model3.3. In this example, when school enrollment is used instead of age as a predictor, the main effect of vocabulary (t = 19.59) and the interaction between vocabulary and school enrollment (t = −2.51) are statistically significant predictorsof reading achievement. Given that the sign on the interaction coefficient is negative, we would conclude that there is a buffering or inhibitory effect. In other words, as the size of the school increases, the relationship between vocabulary and reading achievement becomes weaker

# lme4 and Hypothesis Testing ---------------------------------------------

# It is hard to overlook one nuance of the lme4 library: the lack of reported  p-values for the estimated effects. The standard approach for finding p-values based on using the reference t distribution, which would seem to be the intuitively correct thing to do, does in fact not yield correct values in many instances. Therefore, some alternative approach for obtaining them is necessary. Use of bootstrapped confidence intervals in order to accurately estimate the significance of fixed and random effects in multilevel models. In R, the confint() function and the following code can be used to estimate bootstrapped confidence intervals around the fixed and random effects for Model3.5. There are a number of options using the confint function. In particular, when we consider bootstrap intervals, three options include percentile bootstrap (perc), the standard error bootstrap (basic), and the normal bootstrap (norm). Each of these methods relies on the random resampling of B samples with replacement, with the default being 500.

Model3.5 <- lmer(geread~gevocab + (gevocab|school), 
                 data=Achieve)

summary(Model3.5)

Formula: geread ~ gevocab + (gevocab | school)
Data: Achieve

REML criterion at convergence: 42992.9

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.7096 -0.5674 -0.2079  0.3177  4.6765 

Random effects:
  Groups   Name        Variance Std.Dev. Corr 
school   (Intercept) 0.28050  0.5296        
gevocab     0.01922  0.1386   -0.86
Residual             3.66613  1.9147        
Number of obs: 10320, groups:  school, 160

Fixed effects:
  Estimate Std. Error        df t value Pr(>|t|)    
(Intercept)   2.00575    0.06097 154.62243   32.90   <2e-16 ***
  gevocab       0.52032    0.01440 145.40535   36.15   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
  (Intr)
gevocab -0.866
optimizer (nloptwrap) convergence code: 0 (OK)

confint(Model3.5, method=c("boot"), boot.type=c("perc"))
confint(Model3.5, method=c("boot"), boot.type=c("basic"))
confint(Model3.5, method=c("boot"), boot.type=c("norm"))
confint(Model3.5, method=c("Wald"))
confint(Model3.5, method=c("profile"))

# The last section of the output contains the confidence intervals for the fixed and random effects of Model3.5. The top three rows correspond to the random intercept (.sig01), the correlation between the random intercept and random slope (.sig02), and the random slope (.sig03). According to these confidence intervals, since 0 does not appear in the interval, there is statistically significant variation in the intercept across schools CI95[.4145422, .0.6524953], as indicated in sig01

# Three Level and Higher Models -------------------------------------------

# It is possible for a level-1 unit, such as students, to be nested in higher level units, such as classrooms. Thus, we might assume that at least a portion of a student’s performance on a reading test is due to the classroom in which they find themselves. Each classroom may have a unique learning context, which in turn would contribute to the performance of the students in the classroom. These unique contexts include such things as the quality of the teacher, the presence of disruptive students, and the time of day in which students are in the class, among others. Furthermore, the impact of fixed effects on the dependent variable can vary among the level-2 units, resulting in a random slopes model. Now we will see that it is possible to estimate models with three or more levels of a nested structure, and the command for defining and fitting these models is very similar to that which we used in the two-level case.

# Defining Simple Three Level Models #

# We can first define a null model for prediction of student reading achievement, where regressors might include student-level characteristics, classroom-level characteristics, and school-level characteristics. The syntax to fit a three-level null model appears below, with the results being stored in the object Model4.0.

Model4.0 <- lmer(geread ~ 1 + (1|school/class), 
                 data = Achieve)

# As can be seen, the syntax for fitting a random intercepts model with three levels is very similar to that for the random intercepts model with two levels. In order to define a model with more than two levels, we need to nclude the variables denoting the higher levels of the nesting structures, here school (school-level influence) and class (classroom-level influence), and designate the nesting structure of the levels (students within classrooms within schools). Nested structure in lmer is defined as A/B, where A is the higher-level data unit (e.g. school) and B is the lower-level data unit (e.g. classroom). The intercept (1) is denoted as a random effect by its inclusion in the parentheses.

summary(Model4.0)

Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ 1 + (1 | school/class)
Data: Achieve

REML criterion at convergence: 46146

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-2.3052 -0.6290 -0.2094  0.3049  3.8673 

Random effects:
  Groups       Name        Variance Std.Dev.
class:school (Intercept) 0.2727   0.5222  
school       (Intercept) 0.3118   0.5584  
Residual                 4.8470   2.2016  
Number of obs: 10320, groups:  class:school, 568; school, 160

Fixed effects:
  Estimate Std. Error t value
(Intercept)  4.30806    0.05499   78.34

# As this is a random intercept-only model, there is not much to be interpreted. There are, however, some new pieces of information to take note of. For example, we see two different sets of random effects: random effects for school, so that the intercept is modeled to vary across schools, and random effects for class:school (read, class in school), so that the intercept is modeled to vary across classrooms within schools. We can interpret these random intercepts as means of the dependent variable (reading) varying across levels of the random effects (classrooms and schools). We should also note that at the end of the output, R summarizes the sample size for each of the higher-level units. This is a good place to check and be sure that our model is being defined properly, and that the appropriate data are being used. For example with these data, there are multiple classrooms within each school, so it makes sense that we should have a smaller number of schools (school = 160) and a larger number of classrooms (class:school = 568).

# Adding predictors to the fixed portion of a multilevel model with three or more levels is done in exactly the same manner as for a two-level model. We may wish to extend the intercept-only model described above so that it includes several independent variables, such as a student’s vocabulary test  score (gevocab), the size of the student’s reading classroom (clenroll),  and the size of the student’s school (cenroll). In lmer, the R command for fitting this model and viewing the resultant output would be:

Model4.1 <- lmer(geread~gevocab+clenroll+cenroll+(1|school/class), 
                 data=Achieve)
summary(Model4.1)

Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ gevocab + clenroll + cenroll + (1 | school/class)
Data: Achieve

REML criterion at convergence: 43130.9

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.2212 -0.5673 -0.2079  0.3184  4.4736 

Random effects:
  Groups       Name        Variance Std.Dev.
class:school (Intercept) 0.09047  0.3008  
school       (Intercept) 0.07652  0.2766  
Residual                 3.69790  1.9230  
Number of obs: 10320, groups:  class:school, 568; school, 160

Fixed effects:
  Estimate Std. Error t value
(Intercept)  1.675e+00  2.081e-01   8.050
gevocab      5.076e-01  8.427e-03  60.233
clenroll     1.899e-02  9.559e-03   1.986
cenroll     -3.721e-06  3.642e-06  -1.022

Correlation of Fixed Effects:
  (Intr) gevocb clnrll
gevocab  -0.124              
clenroll -0.961 -0.062       
cenroll  -0.134  0.025 -0.007

# It is evident that from the output for Model4.1 that the student’s vocabulary score (t = 60.23) and size of the classroom (t = 1.99) are statistically significantly positive predictors of student reading achievement score, but the size of the school (t = −1.02) does not significantly predict reading achievement. As a side note, the significant positive relationship between size of the classroom and reading achievement might seem to be a bit confusing, suggesting that students in larger classrooms had higher reading achievement test scores

# Confidence intervals for the model effects can be obtained with the confint() function. 

2.5 %       97.5 %
  .sig01       2.312257e-01 3.660792e-01
.sig02       2.055494e-01 3.398754e-01
.sigma       1.896242e+00 1.950182e+00
(Intercept)  1.268352e+00 2.082537e+00
gevocab      4.910305e-01 5.244195e-01
clenroll     2.564125e-04 3.768655e-02
cenroll     -1.084902e-05 3.394918e-06

# In terms of the fixed effects, the 95% confidence intervals demonstrate that vocabulary score and class size are statistically significant predictors of reading score, but school size is not. In addition, we see that although the variation in random intercepts for schools and classrooms nested in schools declined with the inclusion of the fixed effects, we would still conclude that the random intercept terms are different from 0 in the population, indicating that mean reading scores differ across schools, and across classrooms nested within schools.

# We can also ascertain whether including the predictor variables resulted in a better fitting model, as compared to the null model. We can compare model by examining the AIC and BIC values for each, where lower values indicate better fit. Using the anova() command, we can compare Model4.0 (the null model) with Model4.1.

anova(Model4.0, Model4.1)

Data: Achieve
Models:
  Model4.0: geread ~ 1 + (1 | school/class)
Model4.1: geread ~ gevocab + clenroll + cenroll + (1 | school/class)
npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
Model4.0    4 46150 46179 -23071    46142                         
Model4.1    7 43101 43152 -21544    43087 3054.6  3  < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# For the original null model, AIC and BIC were 46,150 and 46,179, respectively, which are both larger than the AIC and BIC for Model4.1. Therefore, we can conclude that this latter model including a single predictor variable at each level provides better fit to the data, and thus is preferable to the null model with no predictors. We could also look at the chi-square deviance test since these are nested models. The chi-square test is statistically significant, indicating Model4.1 is a better fit to the data.

# Using lmer modelling, it is very easy to include both single-level and cross-level interactions in the model once the higher-level structure is understood. For example, we may have a hypothesis stating that the impact of vocabulary score on reading achievement varies depending upon the size of the school that a student attends. In order to test this hypothesis, we will need to include the interaction between vocabulary score and size of the school, as is done in Model4.2 below.

Model4.2 <- lmer(geread ~ gevocab + clenroll + cenroll + gevocab *cenroll + 
                (1|school/class), 
                data = Achieve)

summary(Model4.2)

Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ gevocab + clenroll + cenroll + gevocab * cenroll + (1 |      school/class)
Data: Achieve

REML criterion at convergence: 43151.8

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.1902 -0.5683 -0.2061  0.3183  4.4724 

Random effects:
  Groups       Name        Variance Std.Dev.
class:school (Intercept) 0.08856  0.2976  
school       (Intercept) 0.07513  0.2741  
Residual                 3.69816  1.9231  
Number of obs: 10320, groups:  class:school, 568; school, 160

Fixed effects:
  Estimate Std. Error t value
(Intercept)      1.752e+00  2.100e-01   8.341
gevocab          4.900e-01  1.168e-02  41.940
clenroll         1.880e-02  9.512e-03   1.977
cenroll         -1.316e-05  5.628e-06  -2.337
gevocab:cenroll  2.340e-06  1.069e-06   2.190

Correlation of Fixed Effects:
  (Intr) gevocb clnrll cenrll
gevocab     -0.203                     
clenroll    -0.949 -0.041              
cenroll     -0.212  0.542  0.000       
gevcb:cnrll  0.166 -0.693 -0.007 -0.766

# In this example we can see that, other than the inclusion of the higher-level nesting structure in the random effects line, defining a cross-level interaction in a model with more than two levels is no different than was the case for the two-level models that we worked with in Chapter 3. In terms of hypothesis testing results, we find that student vocabulary (t=41.94) and classroom size (t = 1.98) remain statistically significant positive predictors of reading ability. In addition, both the cross-level interaction between vocabulary and school size (t = 2.19) and the impact of school size alone (t = −2.34) are also statistically significant predictors of the reading score. The statistically significant interaction term indicates that the impact of student vocabulary score on reading achievement is dependent to some degree on the size of the school, so that the main effects for school and vocabulary cannot be interpreted in isolation, but must be considered in light of one another.

# Finally, we can determine whether or not the model including the interaction provides better fit to the data than did Model4.1, with no interaction. Once again, we will make this decision based upon the AIC/BIC values and the chi-square deviance test given by the anova() command. Given that the AIC and BIC are so similar, we can focus our interpretation on the chi-square test of deviance. The chi-square test of deviance is significant (χ2=4.81, p<.05) we would conclude that including the interaction between vocabulary score and school size does yield a better fitting model.

anova(Model4.1, Model4.2)

Data: Achieve
Models:
  Model4.1: geread ~ gevocab + clenroll + cenroll + (1 | school/class)
Model4.2: geread ~ gevocab + clenroll + cenroll + gevocab * cenroll + (1 | school/class)
npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
Model4.1    7 43101 43152 -21544    43087                       
Model4.2    8 43099 43157 -21541    43083 4.8105  1    0.02829 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Models with more than three levels --------------------------------------

# As a simple example of such higher order models, we will again fit a null model predicting reading achievement, this time incorporating four levels of data: students nested within classrooms nested within schools nested within school corporations (sometimes termed districts). In order to represent the three higher levels of influence, the random effects will now be represented as (1|corp/school/class) in Model4.3, below. In addition to fitting the model and obtaining a summary of results, we will also request bootstrapped 95% confidence intervals for the model parameters using the confint() function.

Model4.3 <- lmer(geread~1 + (1|corp/school/class), 
                 data = Achieve)

summary(Model4.3)

Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ 1 + (1 | corp/school/class)
Data: Achieve

REML criterion at convergence: 46103.2

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-2.2995 -0.6305 -0.2131  0.3029  3.9448 

Random effects:
  Groups              Name        Variance Std.Dev.
class:(school:corp) (Intercept) 0.27539  0.5248  
school:corp         (Intercept) 0.08748  0.2958  
corp                (Intercept) 0.17725  0.4210  
Residual                        4.84699  2.2016  
Number of obs: 10320, groups:  class:(school:corp), 568; school:corp, 160; corp, 59

Fixed effects:
  Estimate Std. Error t value
(Intercept)  4.32583    0.07198    60.1

# In order to ensure that the dataset is being read by R as one thinks it should be, we can first find the summary of the sample sizes for the different data levels which occurs toward the bottom of the printout. There were 10,320 students (groups) nested within 568 classrooms (within schools within corporations) nested within 160 schools (within corporations) nested within 59 school corporations. This matches what we know about the data; therefore, we can proceed with the interpretation of the results. Given that this is a null model with no fixed predictors, our primary focus is on the intercept estimates for the random effects and their associated confidence intervals. We can see from the results confidence interval results below that each level of the data yielded intercepts that were statistically significantly different from 0 (given that 0 does not appear in any of the confidence intervals), indicating that mean reading achievement scores differed among the classrooms, the schools, and the school corporations.

# Random Coefficient Models with three or more levels  ----------------------------------------------

# As an example, let’s consider a model in which we are interested in determining whether mean reading scores differ between males and females, while accounting for the relationship between vocabulary and reading. Furthermore, we believe that the relationship of gender to reading may differ across schools and across classrooms, thus leading to a model where the gender coefficient is allowed to vary across both random effects in our three-level model. Below is the lmer command sequence for fitting this model.

Model4.4 <- lmer(geread ~ gevocab + gender + (gender|school/class), 
                 data = Achieve)

summary(Model4.4)
Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ gevocab + gender + (gender | school/class)
Data: Achieve

REML criterion at convergence: 43107.8

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.2066 -0.5679 -0.2073  0.3166  4.4472 

Random effects:
  Groups       Name        Variance Std.Dev. Corr 
class:school (Intercept) 0.133330 0.36514       
gender      0.013998 0.11831  -0.56
school       (Intercept) 0.041063 0.20264       
gender      0.009605 0.09801  0.20 
Residual                 3.693148 1.92176       
Number of obs: 10320, groups:  class:school, 568; school, 160

Fixed effects:
  Estimate Std. Error t value
(Intercept) 2.015268   0.075824  26.578
gevocab     0.509105   0.008408  60.548
gender      0.017414   0.039373   0.442

Correlation of Fixed Effects:
  (Intr) gevocb
gevocab -0.526       
gender  -0.758  0.039
optimizer (nloptwrap) convergence code: 0 (OK)

# Interpreting these results, we first see that there is not a statistically significant relationship between the fixed gender effect and reading achievement (t=.439). In other words, across classrooms and schools the difference in mean reading achievement for males and females is not shown to be statistically significant, when accounting for vocabulary scores. The estimate for the gender random coefficient term at the school level is approximately 0.006, and is approximately 0.02 at the classroom nested in school level. Thus, it appears that the relationship between gender and reading achievement varies more across classrooms than it does across schools, at least descriptively.

# As noted above, in Model4.4, the coefficients for gender were allowed to vary randomly across both classes and schools. However, there may be some situations in which a researcher is interested in allowing the coefficient for a fixed effect to vary for only one of the random effects, such as classroom, for example. Using the syntax for Model4.5, we define the random coefficient with (gender|school/class), thus allowing both the intercept and slope to vary across both classrooms and schools. This model definition is not flexible enough to allow different random effects structures across nested levels of the data, meaning that we must allow the gender coefficient to vary across both school and classroom, if we want it to vary across the random effects at all. However, perhaps we would like to hypothesize that the relationship of gender and reading performance varies significantly across classrooms but not schools. In order to model this situation, the following syntax would be used.

Model4.5 <- lmer(geread ~ gevocab + gender + (1|school) + 
                (gender|class), 
                data = Achieve)

summary(Model4.5)

Linear mixed model fit by REML ['lmerMod']
Formula: geread ~ gevocab + gender + (1 | school) + (gender | class)
Data: Achieve

REML criterion at convergence: 43139.9

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-3.0875 -0.5734 -0.2105  0.3229  4.4559 

Random effects:
  Groups   Name        Variance  Std.Dev. Corr
school   (Intercept) 9.889e-02 0.314463     
class    (Intercept) 2.571e-03 0.050703     
gender      2.167e-06 0.001472 1.00
Residual             3.765e+00 1.940398     
Number of obs: 10320, groups:  school, 160; class, 8

Fixed effects:
  Estimate Std. Error t value
(Intercept) 1.999578   0.080565  24.819
gevocab     0.513166   0.008379  61.244
gender      0.019803   0.038415   0.516

Correlation of Fixed Effects:
  (Intr) gevocb
gevocab -0.495       
gender  -0.729  0.039
optimizer (nloptwrap) convergence code: 0 (OK)

# The flexibility of the lmer() definition for random effects allows for many different random effects situations to be modeled. For example, see Models4.6 and 4.7, below. Model4.6 allows gender to vary only across schools and the intercept to vary only across classrooms. Model4.7 depicts four nested levels where the intercept is allowed to vary across corporation, school, and classroom, but the gender slope is only allowed to vary across classroom.

Model4.6 <- lmer(geread ~ gevocab + gender + (-1 + gender|school) + 
                 (1|class), 
                 data = Achieve)

Model4.7 <- lmer(geread ~ gevocab + gender + (1|corp) + (1|school) 
                 + (gender|class), 
                 data = Achieve)


# Outlier Removal  --------------------------------------------------------

library(HLMdiag) # for dotplot_diag

dental <- read.csv("Dental.csv")
glimpse(dental)
model1.1.lmer <- lmer(distance ~ age + Sex + (1 | Subject), 
                     dental)
summary(model1.1.lmer)
?case_delete
subject.del.1 <- case_delete(model = model1.1.lmer, group = 
                               "Subject")
subject.diag.1 <- HLMdiag::diagnostics(subject.del.1)
?diagnostics
subject.del.1 <- case_delete(model = model1.1.lmer, 
                             level = "Subject")
?hlm_influence
subject.diag.1 <- hlm_influence(model1.1.lmer, level = 1)
subject.diag.1$covratio

subject.diag.12 <- diagnostics(subject.diag.1)

?dotplot_diag

# Cooks D #

dotplot_diag(x = cooksd, 
             data = subject.diag.1,
             name = "cooks.distance", 
             modify = FALSE,
             xlab = "Subject", 
             ylab = "Cook's Distance")

# mdffits #

dotplot_diag(x = mdffits, 
             data = subject.diag.1,
             name = "cooks.distance", 
             modify = FALSE,
             xlab = "Subject", 
             ylab = "Cook's Distance")

# Covratio #

dotplot_diag(x = covratio, 
             data = subject.diag.1,
             name = "cooks.distance", 
             modify = FALSE,
             xlab = "Subject", 
             ylab = "Covratio")

# Recall that observations with values less than 1 are associated with decreases in estimate precision,

# Multilevel Generalised Additive Models -------------------------------

prime_time <- read.csv("C:/R Portfolio/PrimeTime.csv")

str(prime_time)

library(gamm4)

# Specifically, we are interested in the random intercept model in which reading score (geread) is the dependent variable, and the verbal score (npaverb) is the predictor. In this case, however, we would use splines to investigate whether there exists a nonlinear relationship between the two variables. The R code for fitting this model appears below.

Model10.8 <- gamm4(geread ~ s(npaverb), 
                   family = gaussian, 
                   random = ~ (1|school), 
                   data = prime_time)

# In the above model,  rather than use npaverb itself, the predictor is a smoother involving npaverb, s(npaverb). By using family=gaussian, we are assuming the normality of the model errors. Finally, the random effect is expressed much as is the case with lme4, with 1|school indicating that we are fitting a random intercept model, with school as the clustering variable.The output from gamm4 is divided into the multilevel portion (mer) and the GAM portion (gam). We can see the results for each of these using the summary command, as below.

Model10.8$gam
Model10.8$mer
summary(Model10.8$mer)

Linear mixed model fit by REML ['lmerMod']

REML criterion at convergence: 114645.5

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-2.0385 -0.0529 -0.0037  0.0407 21.7666 

Random effects:
  Groups   Name        Variance  Std.Dev.
school   (Intercept)      70.1    8.373
Xr       s(npaverb)  9613898.6 3100.629
Residual                2066.1   45.454
Number of obs: 10927, groups:  school, 163; Xr, 8

Fixed effects:
  Estimate Std. Error t value
X(Intercept)      6.9296     0.8021   8.640
Xs(npaverb)Fx1 -256.0736    27.4228  -9.338

Correlation of Fixed Effects:
  X(Int)
Xs(npvrb)F1 -0.001

# Here we see that the t-value for this linear effect is -9.338, suggesting that there may be a significant linear relationship between the two variables. relationship between the two variables. The nonlinear smoothed portion of the relationship can be obtained using  the following command.

summary(Model10.8$gam)

Family: gaussian 
Link function: identity 

Formula:
  geread ~ s(npaverb)

Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.9296     0.8021    8.64   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F p-value    
s(npaverb) 8.663  8.663 33.92  <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0243   
lmer.REML = 1.1465e+05  Scale est. = 2066.1    n = 10927

# We can see that this term is statistically significant, with a p-value well below 0.05, meaning that there is a nonlinear relationship between npaverb and geread. Also note that the adjusted R2 is 0.0243, meaning that approximately 0.0243% of the variance in the reading score is associated with the model. In order to characterize the nature of the relationship between the reading and verbal scores, we can examine a plot of the GAM function.

# Just as with linear multilevel models, it is also possible to include random coefficients for the independent variables. The syntax for doing so, along with the resulting output, appears below.

Model10.9 <- gamm4(geread ~ s(npaverb), 
                   family = gaussian, 
                   random = ~(npaverb|school), 
                   data = prime_time)

summary(Model10.9$mer)

Linear mixed model fit by REML ['lmerMod']

REML criterion at convergence: 111423.8

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-7.3252 -0.0548 -0.0141  0.0267 26.5247 

Random effects:
  Groups   Name        Variance  Std.Dev. Corr 
school   (Intercept) 1328.6517 36.4507       
npaverb        0.2336  0.4833  -0.98
Xr       s(npaverb)  1933.8764 43.9759       
Residual             1467.6872 38.3104       
Number of obs: 10927, groups:  school, 163; Xr, 8

Fixed effects:
  Estimate Std. Error t value
X(Intercept)     7.1343     0.8016   8.900
Xs(npaverb)Fx1  -8.7644     5.3572  -1.636

Correlation of Fixed Effects:
  X(Int)
Xs(npvrb)F1 -0.193

# The variance for the random effect of npaverb is smaller than are the variance terms for the other model terms, indicating its relative lack of import in this case. We would interpret this result as indicating that the linear portion of the relationship between npaverb and geread is relatively similar across the schools.

summary(Model10.9$gam)

Family: gaussian 
Link function: identity 

Formula:
  geread ~ s(npaverb)

Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.1343     0.8016     8.9   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df     F  p-value    
s(npaverb) 2.574  2.574 12.49 0.000395 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0122   
lmer.REML = 1.1142e+05  Scale est. = 1467.7    n = 10927

# The smoothed component of the model is the same for the random coefficients model as it was for the random intercept-only model. 


# Predicting Level Two Outcomes with Lev

library(MicroMacroMultilevel)

setwd("C:/R Portfolio")

PrimeTime <- read.csv("PrimeTime.csv")

# It is first neccessary to modify the data so that it is a suitable format

# Remove Missing Data #

prime_time.nomiss <- na.omit(PrimeTime)

# Create Level Two ID and Predictor Variables #

z.gid <- as.vector(tapply(prime_time.nomiss$school, prime_time.nomiss$school, mean))
z.mean <- as.vector(tapply(prime_time.nomiss$sattend, prime_time.nomiss$school, mean))
z.data <- data.frame(z.gid, z.mean)

# Create Level One ID and Predictor Variables #

x.gid <- as.vector(prime_time.nomiss$school)
x.data <- data.frame(prime_time.nomiss$gemath, prime_time.nomiss$gelang)

# Create Level Two Response Variable #

y.prime_time <- as.vector(tapply(prime_time.nomiss$avgcsi1, 
                               prime_time.nomiss$school, mean))

# Obtain Adjusted Predictor Means #

micromacro.results.primetime <- adjusted.predictors(x.data, 
                                                  z.data, 
                                                  x.gid, 
                                                  z.gid)
# Specify Model Formula #

model.formula <- as.formula(y.prime_time ~ BLUP.prime_time.nomiss.gemath + 
                              BLUP.prime_time.nomiss.gelang + z.mean)

# Fit Model and Obtain Summary of Results #

model.output <- micromacro.lm(model.formula, 
                             micromacro.results.primetime$adjusted.group.means, 
                             y.prime_time, 
                             micromacro.results.primetime$unequal.groups)

micromacro.summary(model.output)

# From these results, we can see that neither level-1 independent variable was statistically significantly related to the dependent variable, whereas the level-2 predictor, average daily attendance, was positively associated with the school mean cognitive skills index score. The total variance in the cognitive index that was explained by the model was 0.3641347696, or 36.4%