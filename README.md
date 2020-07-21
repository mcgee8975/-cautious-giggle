# -cautious-giggle

This syntax was used for measurement invariance testing of the AUDIT-C across racial groups (African American, White, & Latinx) after controlling for age. The sample (N = 2038) is secondary data that uses protocal 59 in the NIDA data sharing site.

############################ imported created master_imputed excel file w/ manifest variables
AUDIT.C_data <- read.csv("AUDIT-C_INVARIANCE_08.28.19.csv")
options(scipen = 999)

-----------------------------------------------------------------------------------
###################################################################################
###### Variable names, frequencies, and descriptives
###################################################################################
-----------------------------------------------------------------------------------
  
##### Names the variables column heads
  
names(AUDIT.C_data) <- c("GENDER", "AGE", "RACE", "AUDRKFRQ", "AUDRKDAY", "AUDRINK6")

##### head of the data and summary

head(AUDIT.C_data)
# > head(AUDIT.C_data)
# GENDER AGE RACE AUDRKFRQ AUDRKDAY AUDRINK6
# 1      2  22    1        2        1        1
# 2      2  56    1        0        0        0
# 3      1  64    1        1        0        0
# 4      1  60    1        0        0        0
# 5      1  59    7        1        1        1
# 6      1  27    2        2        0        1
# >

##### Frequencies and Descriptives

summary(AUDIT.C_data)
# > summary(AUDIT.C_data)
# GENDER           AGE             RACE          AUDRKFRQ        AUDRKDAY         AUDRINK6     
# Min.   :1.000   Min.   :18.00   Min.   :1.000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.000   1st Qu.:33.00   1st Qu.:1.000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :2.000   Median :48.00   Median :1.000   Median :1.000   Median :0.0000   Median :0.0000  
# Mean   :1.567   Mean   :45.85   Mean   :3.486   Mean   :1.173   Mean   :0.4092   Mean   :0.3928  
# 3rd Qu.:2.000   3rd Qu.:56.00   3rd Qu.:7.000   3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:0.0000  
# Max.   :2.000   Max.   :94.00   Max.   :7.000   Max.   :4.000   Max.   :4.0000   Max.   :4.0000  
# NA's   :95       NA's   :4       
# >

psych::describe(AUDIT.C_data)
# > describe(AUDIT.C_data)
#          vars    n  mean    sd median trimmed   mad min max range  skew kurtosis   se
# GENDER      1 2038  1.57  0.50      2    1.58  0.00   1   2     1 -0.27    -1.93 0.01
# AGE         2 2038 45.85 14.75     48   45.88 16.31  18  94    76 -0.04    -0.68 0.33
# RACE        3 2038  3.49  2.82      1    3.36  0.00   1   7     6  0.30    -1.85 0.06
# AUDRKFRQ    4 2038  1.17  1.21      1    1.01  1.48   0   4     4  0.88    -0.17 0.03
# AUDRKDAY    5 1943  0.41  0.85      0    0.19  0.00   0   4     4  2.48     6.23 0.02
# AUDRINK6    6 2034  0.39  0.85      0    0.18  0.00   0   4     4  2.64     7.04 0.02
# > 


############################ Table of AUDIT Descriptives  ############################

#### Syntax and packages to support making tables with R

library(xtable)
library(stargazer)
library(pander)
library(tables)
library(ascii)
library(knitr)

# This project contains the materials for lesson 7 of the [R Markdown tutorial]
# (https://rmarkdown.rstudio.com/lesson-7.html). To begin the lesson, press 
# the play button at the top right of the code chunk below. Then refer to 
# the viewer pane.

# It is also easy to make tables with knitr's 'kable' function:

### For use in rmarkdown
# ```{r}
# viewer <- getOption("viewer")
# viewer("https://rmarkdown.rstudio.com/lesson-7.html", 
#        height = "maximize")
# rstudioapi::navigateToFile("6-tables.Rmd")
# ```

AUDIT_table <- kable(head(AUDIT.C_data), format = "html", caption = "AUDIT Descriptives.")

cat(AUDIT_table, sep = "\n")
options(knitr.table.format = 'html')

-----------------------------------------------------------------------------------
###################################################################################
###### Missingness and Data Management
###################################################################################
-----------------------------------------------------------------------------------
  
###### Testing for missingness
md.pattern(AUDIT.C_data)

# > md.pattern(AUDIT.C_data)
#      GENDER AGE RACE AUDRKFRQ AUDRINK6 AUDRKDAY   
# 1941      1   1    1        1        1        1  0
# 93        1   1    1        1        1        0  1
# 2         1   1    1        1        0        1  1
# 2         1   1    1        1        0        0  2
#           0   0    0        0        4       95  99
# > 

######## Test missingnes for MCAR

mcar.AUDIT.C <- LittleMCAR(AUDIT.C_data)
# > mcar.AUDIT.C <- LittleMCAR(AUDIT.C_data)
# this could take a while
# >

mcar.AUDIT.C[c("chi.square", "df", "p.value")]
# > mcar.AUDIT.C[c("chi.square", "df", "p.value")]
# $chi.square
# [1] 31.472
# 
# $df
# [1] 14
# 
# $p.value
# [1] 0.004758491
# 
# > 


TestMCARNormality(AUDIT.C_data)
# > TestMCARNormality(AUDIT.C_data)
# Call:
#   TestMCARNormality(data = AUDIT.C_data)
# 
# Number of Patterns:  2 
# 
# Total number of cases used in the analysis:  2034 
# 
# Pattern(s) used:
#   GENDER   AGE   RACE   AUDRKFRQ   AUDRKDAY   AUDRINK6   Number of cases
# group.1        1     1      1          1          1          1              1941
# group.2        1     1      1          1         NA          1                93
# 
# 
# Test of normality and Homoscedasticity:
#   -------------------------------------------
#   
#   Hawkins Test:
#   
#   P-value for the Hawkins test of normality and homoscedasticity:  0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001921527 
# 
# Either the test of multivariate normality or homoscedasticity (or both) is rejected.
# Provided that normality can be assumed, the hypothesis of MCAR is 
# rejected at 0.05 significance level. 
# 
# Non-Parametric Test:
#   
#   P-value for the non-parametric test of homoscedasticity:  0.00000004311987 
# 
# Hypothesis of MCAR is rejected at  0.05 significance level.
# The multivariate normality test is inconclusive. 
# >

############### CFA

AUDIT_C.cfa.model <- 'AUDIT.C =~ NA*AUDRKFRQ + AUDRKFRQ + AUDRKDAY + AUDRINK6
                                AUDIT.C ~ AGE
                                AUDIT.C~~1*AUDIT.C
                     '

AUDIT_C.cfa.model.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data, std.lv=TRUE, estimator = "WLSMV") 
# > AUDIT_C.cfa.model.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data, std.lv=TRUE, estimator = "WLSMV") 
# >


##################################################################################################
##########################################    Summary stats OF CFA MODEL ######################### 

summary(AUDIT_C.cfa.model.fit, fit.measures = TRUE, standardized = TRUE)
# #> summary(AUDIT_C.cfa.model.fit, fit.measures = TRUE, standardized = TRUE)
# lavaan 0.6-4 ended normally after 22 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                          8
# 
# Used       Total
# Number of observations                          1941        2038
# 
# Estimator                                       DWLS      Robust
# Model Fit Test Statistic                       3.795       9.383
# Degrees of freedom                                 2           2
# P-value (Chi-square)                           0.150       0.009
# Scaling correction factor                                  0.408
# Shift parameter                                            0.079
# for simple second-order correction (Mplus variant)
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic              694.951     481.515
# Degrees of freedom                                 6           6
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.984
# Tucker-Lewis Index (TLI)                       0.992       0.953
# 
# Robust Comparative Fit Index (CFI)                            NA
# Robust Tucker-Lewis Index (TLI)                               NA
# 
# Root Mean Square Error of Approximation:
#   
# RMSEA                                          0.022       0.044
# 90 Percent Confidence Interval          0.000  0.055       0.018  0.073
# P-value RMSEA <= 0.05                          0.915       0.586
# 
# Robust RMSEA                                                  NA
# 90 Percent Confidence Interval                                NA     NA
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.014       0.014
# 
# Parameter Estimates:
#   
# Information                                 Expected
# Information saturated (h1) model        Unstructured
# Standard Errors                           Robust.sem
# 
# Latent Variables:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFRQ          0.823    0.025   33.551    0.000    0.823    0.669
# AUDRKDAY          0.628    0.029   21.318    0.000    0.628    0.742
# AUDRINK6          0.817    0.032   25.258    0.000    0.817    0.945
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.481    0.631   -0.001   -0.012
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFRQ          0.836    0.038   21.989    0.000    0.836    0.552
# .AUDRKDAY          0.323    0.026   12.413    0.000    0.323    0.450
# .AUDRINK6          0.080    0.020    4.096    0.000    0.080    0.107
#  AGE             217.291    5.671   38.313    0.000  217.291    1.000
# > 

parameterEstimates(AUDIT_C.cfa.model.fit)
# > parameterEstimates(AUDIT_C.cfa.model.fit)
#        lhs op      rhs     est    se      z pvalue ci.lower ci.upper
# 1  AUDIT.C =~ AUDRKFRQ   0.823 0.025 33.551  0.000    0.775    0.871
# 2  AUDIT.C =~ AUDRKDAY   0.628 0.029 21.318  0.000    0.570    0.685
# 3  AUDIT.C =~ AUDRINK6   0.817 0.032 25.258  0.000    0.753    0.880
# 4  AUDIT.C  ~      AGE  -0.001 0.002 -0.481  0.631   -0.004    0.002
# 5  AUDIT.C ~~  AUDIT.C   1.000 0.000     NA     NA    1.000    1.000
# 6 AUDRKFRQ ~~ AUDRKFRQ   0.836 0.038 21.989  0.000    0.761    0.910
# 7 AUDRKDAY ~~ AUDRKDAY   0.323 0.026 12.413  0.000    0.272    0.374
# 8 AUDRINK6 ~~ AUDRINK6   0.080 0.020  4.096  0.000    0.042    0.118
# 9      AGE ~~      AGE 217.291 5.671 38.313  0.000  206.175  228.407
# >

##################################################################################################
##########################################    Below -> Ignore ######################### 

##### Testing if the errors for the indicators correlate.

##  Simple Structure CFA model is identified: If there are, at least, two indicators
##  per latent variable and the errors of those two or more indicators are
##  uncorrelated with each other and with at least one other indicator on the
##  other latent variables.

# AUDIT.se <- c(0.024, 0.029, 0.032)
# # > AUDIT.se <- c(0.024, 0.029, 0.032)
# # >
# 
# AUDIT.se <- matrix(c(
#   0.024, 0.029, 0.032
# ),3,3)
# 
# names(AUDIT.se) <- c("AUDRKFRQ", "AUDRKDAY", "AUDRINK6")
# # > names(AUDIT.se) <- c("AUDRKFRQ", "AUDRKDAY", "AUDRINK6")
# # >
# 
# cor(AUDIT.se)
# 
# 
# install.packages("stats")
# library(stats)
# 
# 
# 
# 
# stats::cor.test(AUDIT.C_data$AUDRKFRQ, AUDIT.C_data$AUDRKDAY, AUDIT.C_data$AUDRINK6)
# # Error in match.arg(alternative) : 
# #   'arg' must be NULL or a character vector
# # >
# 
# #### Testing the fit 
# 
# modindices(AUDIT.cfa.model.fit)
# # > modindices(AUDIT.cfa.model.fit)
# # [1] lhs      op       rhs      mi       epc      sepc.lv  sepc.all sepc.nox
# # <0 rows> (or 0-length row.names)
# # >
# 
# fit.indices <- c("chisq", "df", "cfi", "rmsea", "srmr", "mfi")
# 
# fitMeasures(AUDIT.cfa.model.fit, fit.indices)
# 
# ########### CFA model with Age, gender, race  
# AUDIT.cfa.model.02 <- 'AUDIT=~ NA*AUDRKFRQ+ AUDRKFRQ + AUDRKDAY + AUDRINK6
#                        AUDIT~GENDER + AGE + RACE'
# 
# 
# AUDIT.cfa.model.fit.02 <- lavaan::sem(AUDIT.cfa.model.02, AUDIT.C_data, std.lv=TRUE, estimator = "MLR")

##################################################################################################
##########################################    Above -> Ignore ######################### 


##################### INVARIANCE TESTING 

table(AUDIT.C_data$RACE)
# > table(AUDIT.C_data$RACE)
# 
#    1    2    3    5    6    7 
# 1108   38   10   37  210  635 
#54%  #2% <1%  2%   10%   31%
table(AUDIT.C_data$GENDER)
# > table(AUDIT.C_data$GENDER)
# 
# 1    2 
# 882 1156 
# >

table(AUDIT.C_data.02$RACE)

table(AUDIT.C_data.02$GENDER)
# > table(AUDIT.C_data.02$GENDER)
# 
# 1    2 
# 848 1104 


##################### INVARIANCE TESTS

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AUDIT_C.cfa.model.RACE.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data, std.lv=TRUE, estimator = "MLR", group = "RACE")

#> AUDIT.cfa.model.RACE.fit <- lavaan::cfa(AUDIT.cfa.model, AUDIT.C_data, std.lv=TRUE, estimator = "MLR", group = "RACE")
# Error in lav_samplestats_icov(COV = cov[[g]], ridge = ridge, x.idx = x.idx[[g]],  : 
#                                 lavaan ERROR: sample covariance matrix is not positive-definite

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(fit.cfa.separate, fit.measures= TRUE, standardized=TRUE)



########################  ########################  ########################  ########################
########################  ########################  ########################  ########################

######################## ADJUSTED RACE DATA

AUDIT.C_data.02 <- read.csv("AUDIT-C_INVARIANCE_08.29.19.csv")
options(scipen = 999)

###### Variable names 

names(AUDIT.C_data.02) <- c("GENDER", "AGE", "RACE", "AUDRKFRQ", "AUDRKDAY", "AUDRINK6","AUDTotal")

# head of the data and summary

head(AUDIT.C_data.02)
# >   head(AUDIT.C_data.02)
# GENDER AGE RACE AUDRKFRQ AUDRKDAY AUDRINK6 AUDTotal
# 1      2  56    1        0        0        0        0
# 2      1  64    1        1        0        0        1
# 3      1  60    1        0        0        0        0
# 4      1  59    7        1        1        1        3
# 5      2  30    6        0        0        0        0
# 6      2  28    6        0        0        0        0
# >

summary(AUDIT.C_data.02)
# >   summary(AUDIT.C_data.02)
# GENDER           AGE             RACE         AUDRKFRQ        AUDRKDAY         AUDRINK6     
# Min.   :1.000   Min.   :18.00   Min.   :1.00   Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:1.000   1st Qu.:34.00   1st Qu.:1.00   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000  
# Median :2.000   Median :48.00   Median :1.00   Median :1.000   Median :0.0000   Median :0.0000  
# Mean   :1.566   Mean   :45.95   Mean   :3.49   Mean   :1.173   Mean   :0.4096   Mean   :0.3958  
# 3rd Qu.:2.000   3rd Qu.:56.25   3rd Qu.:7.00   3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:0.0000  
# Max.   :2.000   Max.   :94.00   Max.   :7.00   Max.   :4.000   Max.   :4.0000   Max.   :4.0000  
#                                                                NA's   :94       NA's   :4       
# AUDTotal     
# Min.   : 0.000  
# 1st Qu.: 0.000  
# Median : 1.000  
# Mean   : 1.958  
# 3rd Qu.: 3.000  
# Max.   :12.000  
# >

library(psych)

psych::describe(AUDIT.C_data.02)
# >   describe(AUDIT.C_data.02)
#          vars    n  mean    sd median trimmed   mad min max range  skew kurtosis   se
# GENDER      1 1952  1.57  0.50      2    1.58  0.00   1   2     1 -0.26    -1.93 0.01
# AGE         2 1952 45.95 14.71     48   46.00 16.31  18  94    76 -0.05    -0.65 0.33
# RACE        3 1952  3.49  2.86      1    3.36  0.00   1   7     6  0.30    -1.88 0.06
# AUDRKFRQ    4 1952  1.17  1.21      1    1.01  1.48   0   4     4  0.88    -0.17 0.03
# AUDRKDAY    5 1858  0.41  0.85      0    0.19  0.00   0   4     4  2.48     6.27 0.02
# AUDRINK6    6 1948  0.40  0.85      0    0.18  0.00   0   4     4  2.64     7.00 0.02
# AUDTotal    7 1952  1.96  2.48      1    1.48  1.48   0  12    12  1.77     3.24 0.06
# >
#   




############### MEANS of of AUDTotal based on RACE

AUDIT.C_data.02 %>%
  group_by(RACE) %>%
  summarise_at(vars(AUDTotal), funs(mean(., na.rm=TRUE)))

# >   AUDIT.C_data.02 %>%
#   +     group_by(RACE) %>%
#   +     summarise_at(vars(AUDTotal), funs(mean(., na.rm=TRUE)))
# # A tibble: 3 x 2
#      RACE AUDTotal
#     <int>    <dbl>
#   1     1     1.95
#   2     6     1.69
#   3     7     2.06
# >

############### SD of the AUDTotal nased on RACE


AUDIT.C_data.02 %>%
  group_by(RACE) %>%
  summarise_at(vars(AUDTotal), funs(sd(., na.rm=TRUE)))

# >   AUDIT.C_data.02 %>%
#   +     group_by(RACE) %>%
#   +     summarise_at(vars(AUDTotal), funs(mean(., na.rm=TRUE)))
# # A tibble: 3 x 2
#    RACE AUDTotal
#   <int>    <dbl>
# 1     1     1.95
# 2     6     1.69
# 3     7     2.06
# Warning message:
#   funs() is soft deprecated as of dplyr 0.8.0
# Please use a list of either functions or lambdas: 
#   
#   # Simple named list: 
#   list(mean = mean, median = median)
# 
# # Auto named with `tibble::lst()`: 
# tibble::lst(mean, median)
# 
# # Using lambdas
# list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
# This warning is displayed once per session. 
# >


############### MEANS of of AUDTotal based on GENDER
AUDIT.C_data.02 %>%
  group_by(GENDER) %>%
  summarise_at(vars(AUDTotal), funs(mean(., na.rm=TRUE)))

# >   AUDIT.C_data.02 %>%
#   +     group_by(GENDER) %>%
#   +     summarise_at(vars(AUDTotal), funs(mean(., na.rm=TRUE)))
# # A tibble: 2 x 2
# GENDER AUDTotal
#       <int>    <dbl>
#   1      1     2.05
#   2      2     1.88
# > 


############### SD of the AUDTotal nased on GENDER

AUDIT.C_data.02 %>%
  group_by(GENDER) %>%
  summarise_at(vars(AUDTotal), funs(sd(., na.rm=TRUE)))

# >   AUDIT.C_data.02 %>%
#   +     group_by(GENDER) %>%
#   +     summarise_at(vars(AUDTotal), funs(sd(., na.rm=TRUE)))
# # A tibble: 2 x 2
#  GENDER AUDTotal
#       <int>    <dbl>
#   1      1     2.64
#   2      2     2.34
# >


############### UNIT 13 ###############

############### One Factor ANOVA ###############

# USE summary to fit an ANOVA, you use the linear model, lm command:


summary(lm(AUDTotal ~ RACE, data = AUDIT.C_data.02))

# >   summary(lm(AUDTotal ~ RACE, data = AUDIT.C_data.02))
# 
# Call:
#   lm(formula = AUDTotal ~ RACE, data = AUDIT.C_data.02)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -1.978 -1.944 -0.944  1.056 10.056 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 1.938339   0.088442  21.916   <2e-16 ***
#   RACE        0.005632   0.019591   0.287    0.774    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.479 on 1950 degrees of freedom
# Multiple R-squared:  4.237e-05,	Adjusted R-squared:  -0.0004704 
# F-statistic: 0.08263 on 1 and 1950 DF,  p-value: 0.7738
# 
# > 

# >   anova(lm(AUDTotal ~ RACE, data = AUDIT.C_data.02))
# Analysis of Variance Table
# 
# Response: AUDTotal
# Df  Sum Sq Mean Sq F value Pr(>F)
# RACE         1     0.5  0.5077  0.0826 0.7738
# Residuals 1950 11982.0  6.1446               
# > 

# That is somewhat better, but sure doesn't look like an ANOVA.
anova(lm(AUDTotal ~ RACE, data = AUDIT.C_data.02))
# That's more like it. 

################## Computing TWI ANOVA with GENDER

############### Two Factor ANOVA ###############

# Let's look at what the data look like and need to be set up like for 2 fact 
# ANOVA advanced! - just subsampling randomly to look at a few data points
AUDIT.C_data.02[which(runif(nrow(AUDIT.C_data.02)) < .01), c("RACE", "GENDER")]

# >   AUDIT.C_data.02[which(runif(nrow(AUDIT.C_data.02)) < .01), c("RACE", "GENDER")]
# RACE GENDER
# 12      7      2
# 15      1      2
# 20      1      2
# 51      1      1
# 127     7      2
# 173     1      2
# 234     7      1
# 450     1      2
# 770     1      1
# 785     1      1
# 1076    1      2
# 1093    1      2
# 1340    7      1
# 1368    1      2
# 1585    7      1
# 1683    1      2
# 1685    1      2
# 1820    1      1
# >

anova(lm(AUDTotal ~ RACE*GENDER, data = AUDIT.C_data.02))

# >   anova(lm(AUDTotal ~ RACE*GENDER, data = AUDIT.C_data.02))
# Analysis of Variance Table
# 
# Response: AUDTotal
#               Df  Sum Sq Mean Sq F value Pr(>F)
# RACE           1     0.5  0.5077  0.0827 0.7737
# GENDER         1    14.0 14.0310  2.2851 0.1308
# RACE:GENDER    1     6.8  6.8395  1.1139 0.2914
# Residuals   1948 11961.2  6.1402               
# > 

#################### computing cronbach's alpha

AUDIT.C_data.03 <- read.csv("AUC_analysis.csv")
options(scipen = 999)

psych::alpha(AUDIT.C_data.03)
# >   psych::alpha(AUDIT.C_data.03)
# 
# Reliability analysis   
# Call: psych::alpha(x = AUDIT.C_data.03)
# 
# raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd median_r
# 0.8      0.83    0.78      0.61 4.8 0.0074 0.66 0.83     0.63
# 
# lower alpha upper     95% confidence boundaries
# 0.79 0.8 0.82 
# 
# Reliability if an item is dropped:
#   raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
# X2        0.83      0.83    0.71      0.71 4.9   0.0075    NA  0.71
# X1        0.74      0.77    0.63      0.63 3.4   0.0104    NA  0.63
# X1.1      0.65      0.67    0.51      0.51 2.1   0.0144    NA  0.51
# 
# Item statistics 
# n raw.r std.r r.cor r.drop mean   sd
# X2   2037  0.87  0.83  0.67   0.61 1.17 1.21
# X1   1942  0.82  0.86  0.76   0.65 0.41 0.85
# X1.1 2033  0.88  0.90  0.85   0.76 0.39 0.85
# 
# Non missing response frequency for each item
# 0    1    2    3    4 miss
# X2   0.37 0.32 0.15 0.09 0.07 0.00
# X1   0.75 0.16 0.06 0.02 0.02 0.05
# X1.1 0.76 0.16 0.03 0.02 0.02 0.00
# >

rowSums(AUDIT.C_data.03)



round(alpha.AUDIT-C$X2 [1], 3)


# CONFIGURAL INVARIANCE MODEL

AUDIT_C.cfa.model <- 'AUDIT.C =~ NA*AUDRKFRQ+ AUDRKFRQ + AUDRKDAY + AUDRINK6
                        AUDIT.C ~ AGE
                        AUDIT.C ~~ 1*AUDIT.C'

##################### INVARIANCE TESTS

##################  ##################  CONFIGURAL INVARIANCE  ################## ##################
################## ################## ################## ################## ##################

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group=”nation” to our cfa statement.

AUDIT_C.cfa.model.RACE.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE")
# >   AUDIT_C.cfa.model.RACE.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE")
# >


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)
# >   summary(AUDIT_C.cfa.model.RACE.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 67 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         30
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                       8.450       9.426
# Degrees of freedom                                 6           6
# P-value (Chi-square)                           0.207       0.151
# Scaling correction factor                                  0.896
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
# 1                                              2.771       3.092
# 7                                              4.361       4.865
# 6                                              1.317       1.469
# 
# Model test baseline model:
#   
# Minimum Function Test Statistic             2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
# Comparative Fit Index (CFI)                    0.999       0.998
# Tucker-Lewis Index (TLI)                       0.997       0.993
# 
# Robust Comparative Fit Index (CFI)                         0.999
# Robust Tucker-Lewis Index (TLI)                            0.996
# 
# Loglikelihood and Information Criteria:
#   
# Loglikelihood user model (H0)              -6589.045   -6589.045
# Scaling correction factor                                  1.966
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction
# 
# Number of free parameters                         30          30
# Akaike (AIC)                               13238.089   13238.089
# Bayesian (BIC)                             13403.874   13403.874
# Sample-size adjusted Bayesian (BIC)        13308.565   13308.565
# 
# Root Mean Square Error of Approximation:
#   
# RMSEA                                          0.026       0.030
# 90 Percent Confidence Interval          0.000  0.062       0.000  0.067
# P-value RMSEA <= 0.05                          0.840       0.772
# 
# Robust RMSEA                                               0.029
# 90 Percent Confidence Interval                             0.000  0.062
# 


# Standardized Root Mean Square Residual:
#   
# SRMR                                           0.012       0.012
# 

#Chancge in SRME
0.031 - 0.012
# >   0.031 - 0.012
# [1] 0.019
# > 


# Parameter Estimates:
#   
# Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFRQ          0.826    0.034   24.092    0.000    0.826    0.667
# AUDRKDAY          0.623    0.040   15.685    0.000    0.623    0.730
# AUDRINK6          0.823    0.044   18.667    0.000    0.823    0.953
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.476    0.634   -0.001   -0.016
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.212    0.100   12.066    0.000    1.212    0.979
# .AUDRKDAY          0.442    0.075    5.894    0.000    0.442    0.517
# .AUDRINK6          0.444    0.097    4.596    0.000    0.444    0.514
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFRQ          0.852    0.054   15.830    0.000    0.852    0.555
# .AUDRKDAY          0.341    0.037    9.200    0.000    0.341    0.468
# .AUDRINK6          0.068    0.025    2.669    0.008    0.068    0.091
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFRQ          0.829    0.043   19.316    0.000    0.830    0.668
# AUDRKDAY          0.658    0.055   12.062    0.000    0.658    0.743
# AUDRINK6          0.844    0.057   14.898    0.000    0.844    0.947
# 
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
# AGE              -0.003    0.003   -1.035    0.301   -0.003   -0.043
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.312    0.115   11.453    0.000    1.312    1.056
# .AUDRKDAY          0.519    0.095    5.471    0.000    0.519    0.586
# .AUDRINK6          0.537    0.114    4.729    0.000    0.537    0.602
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFRQ          0.856    0.065   13.153    0.000    0.856    0.554
# .AUDRKDAY          0.352    0.049    7.150    0.000    0.352    0.448
# .AUDRINK6          0.081    0.039    2.086    0.037    0.081    0.102
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFRQ          0.857    0.071   12.080    0.000    0.858    0.754
# AUDRKDAY          0.504    0.079    6.376    0.000    0.505    0.751
# AUDRINK6          0.710    0.107    6.635    0.000    0.711    0.878
# 
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
# AGE               0.003    0.004    0.629    0.529    0.003    0.044
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          0.936    0.185    5.058    0.000    0.936    0.822
# .AUDRKDAY          0.251    0.107    2.341    0.019    0.251    0.373
# .AUDRINK6          0.271    0.144    1.873    0.061    0.271    0.334
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFRQ          0.559    0.089    6.263    0.000    0.559    0.432
# .AUDRKDAY          0.197    0.053    3.693    0.000    0.197    0.436
# .AUDRINK6          0.150    0.053    2.847    0.004    0.150    0.229
# 
# >

modindices(AUDIT_C.cfa.model.RACE.fit)
# >   modindices(AUDIT_C.cfa.model.RACE.fit)
#         lhs op      rhs block group level    mi    epc sepc.lv sepc.all sepc.nox
# 43 AUDRKFRQ ~~ AUDRKDAY     1     1     1 2.054  1.151   1.151    2.135    2.135
# 44 AUDRKFRQ ~~ AUDRINK6     1     1     1 0.235 -0.646  -0.646   -2.688   -2.688
# 45 AUDRKDAY ~~ AUDRINK6     1     1     1 2.383 -1.822  -1.822  -11.979  -11.979
# 47 AUDRKFRQ ~~ AUDRKDAY     2     2     1 0.009 -0.037  -0.037   -0.068   -0.068
# 48 AUDRKFRQ ~~ AUDRINK6     2     2     1 2.134  0.924   0.924    3.502    3.502
# 49 AUDRKDAY ~~ AUDRINK6     2     2     1 2.657 -0.990  -0.990   -5.858   -5.858
# 51 AUDRKFRQ ~~ AUDRKDAY     3     3     1 0.000  0.000   0.000   -0.001   -0.001
# 52 AUDRKFRQ ~~ AUDRINK6     3     3     1 0.845 -0.891  -0.891   -3.081   -3.081
# 53 AUDRKDAY ~~ AUDRINK6     3     3     1 0.834  0.517   0.517    3.009    3.009
# >

fitMeasures(AUDIT_C.cfa.model.RACE.fit, fit.indices)
# >   fitMeasures(AUDIT_C.cfa.model.RACE.fit, fit.indices)
# chisq    df   cfi rmsea  srmr   mfi 
# 8.450 6.000 0.999 0.026 0.012 0.999 
# >
################## Assess MFI 

MFI <- c("mfi")
fitMeasures(AUDIT.cfa.model.RACE.fit, MFI)
# >   MFI <- c("mfi")
# >   fitMeasures(AUDIT.cfa.model.RACE.fit, MFI)
# mfi 
# 0.999 
# > 


############### Metric invariance

AUDIT_C.cfa.model.METRIC.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings"))
# >   AUDIT_C.cfa.model.METRIC.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings"))
# > 


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.METRIC.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.METRIC.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 59 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         30
# Number of equality constraints                     6
# Row rank of the constraints matrix                 6
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      20.717      12.267
# Degrees of freedom                                12          12
# P-value (Chi-square)                           0.055       0.425
# Scaling correction factor                                  1.689
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
# 1                                              2.897       1.715
# 7                                              6.408       3.794
# 6                                             11.413       6.758
# 
# Model test baseline model:
#   
# Minimum Function Test Statistic             2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
# Comparative Fit Index (CFI)                    0.996       1.000
# Tucker-Lewis Index (TLI)                       0.994       1.000
# 
# Robust Comparative Fit Index (CFI)                         1.000
# Robust Tucker-Lewis Index (TLI)                            1.000
# 
# Loglikelihood and Information Criteria:
#   
# Loglikelihood user model (H0)              -6595.178   -6595.178
# Scaling correction factor                                  1.469
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction
# 
# Number of free parameters                         24          24
# Akaike (AIC)                               13238.356   13238.356
# Bayesian (BIC)                             13370.985   13370.985
# Sample-size adjusted Bayesian (BIC)        13294.737   13294.737
# 
# Root Mean Square Error of Approximation:
#   

#### Change in RMSEA
0.030 - 0.006
# >   0.030 - 0.006
# [1] 0.024
# >
# 
# RMSEA                                          0.034       0.006
# 90 Percent Confidence Interval          0.000  0.059       0.000  0.035
# P-value RMSEA <= 0.05                          0.842       0.998
# 
# Robust RMSEA                                               0.008
# 90 Percent Confidence Interval                             0.000  0.054
# 
# Standardized Root Mean Square Residual:
#   
# SRMR                                           0.030       0.030
# 


# Change in SRMR
# 0.030 - 0.031
# >  0.030 - 0.031
# [1] -0.001
# >

# Parameter Estimates:
#   
# Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.832    0.025   33.436    0.000    0.832    0.670
# AUDRKDA (.p2.)    0.619    0.030   20.449    0.000    0.619    0.728
# AUDRINK (.p3.)    0.819    0.033   24.538    0.000    0.819    0.951
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
# AGE              -0.001    0.002   -0.470    0.639   -0.001   -0.015
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.212    0.101   11.990    0.000    1.212    0.977
# .AUDRKDAY          0.441    0.075    5.920    0.000    0.441    0.519
# .AUDRINK6          0.443    0.096    4.622    0.000    0.443    0.515
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFRQ          0.849    0.053   15.972    0.000    0.849    0.551
# .AUDRKDAY          0.340    0.036    9.486    0.000    0.340    0.470
# .AUDRINK6          0.070    0.024    2.869    0.004    0.070    0.095
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.832    0.025   33.436    0.000    0.833    0.670
# AUDRKDA (.p2.)    0.619    0.030   20.449    0.000    0.620    0.721
# AUDRINK (.p3.)    0.819    0.033   24.538    0.000    0.820    0.942
# 
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
# AGE              -0.003    0.003   -1.019    0.308   -0.003   -0.044
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.314    0.118   11.097    0.000    1.314    1.058
# .AUDRKDAY          0.516    0.091    5.638    0.000    0.516    0.601
# .AUDRINK6          0.536    0.113    4.728    0.000    0.536    0.616
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFRQ          0.850    0.065   13.022    0.000    0.850    0.551
# .AUDRKDAY          0.354    0.046    7.688    0.000    0.354    0.480
# .AUDRINK6          0.085    0.038    2.257    0.024    0.085    0.112
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.832    0.025   33.436    0.000    0.832    0.729
# AUDRKDA (.p2.)    0.619    0.030   20.449    0.000    0.619    0.814
# AUDRINK (.p3.)    0.819    0.033   24.538    0.000    0.819    0.915
# 
# Regressions:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
# AGE               0.002    0.004    0.594    0.552    0.002    0.036
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          0.958    0.160    5.995    0.000    0.958    0.838
# .AUDRKDAY          0.251    0.114    2.198    0.028    0.251    0.330
# .AUDRINK6          0.276    0.146    1.895    0.058    0.276    0.308
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.612    0.089    6.892    0.000    0.612    0.469
# .AUDRKDAY          0.195    0.056    3.484    0.000    0.195    0.337
# .AUDRINK6          0.131    0.054    2.437    0.015    0.131    0.164
# >


################## Assess MFI 

fitMeasures(AUDIT_C.cfa.model.METRIC.fit, MFI)
# >   fitMeasures(AUDIT_C.cfa.model.METRIC.fit, MFI)
# mfi 
# 0.998 
# >

################# Chi-square difference test between Configural invariance and metric invariance. 

anova(AUDIT_C.cfa.model.RACE.fit, AUDIT_C.cfa.model.METRIC.fit)

# >   anova(AUDIT_C.cfa.model.RACE.fit, AUDIT_C.cfa.model.METRIC.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
#                              Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.RACE.fit    6 13238 13404  8.450                              
# AUDIT_C.cfa.model.METRIC.fit 12 13238 13371 20.717     4.9437       6     0.5511
# >

################### scalar invariance

AUDIT_C.cfa.model.Scalar.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts"))

# >   AUDIT_C.cfa.model.Scalar.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts"))
# >

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.Scalar.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.Scalar.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 56 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         32
# Number of equality constraints                    12
# Row rank of the constraints matrix                12
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      24.474      15.984
# Degrees of freedom                                16          16
# P-value (Chi-square)                           0.080       0.454
# Scaling correction factor                                  1.531
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   1                                              3.123       2.040
# 7                                              6.718       4.387
# 6                                             14.634       9.557
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.996       1.000
# Tucker-Lewis Index (TLI)                       0.996       1.000
# 
# Robust Comparative Fit Index (CFI)                         1.000
# Robust Tucker-Lewis Index (TLI)                            1.000
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6597.057   -6597.057
# Scaling correction factor                                  1.245
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction
# 
# Number of free parameters                         20          20
# Akaike (AIC)                               13234.114   13234.114
# Bayesian (BIC)                             13344.637   13344.637
# Sample-size adjusted Bayesian (BIC)        13281.098   13281.098
# 
# Root Mean Square Error of Approximation:
#   

# Change in RMSEA
# 0.006-0.000
# >   0.006-0.000
# [1] 0.006
# > 

#   RMSEA                                          0.029       0.000
# 90 Percent Confidence Interval          0.000  0.051       0.000  0.032
# P-value RMSEA <= 0.05                          0.939       1.000
# 
# Robust RMSEA                                               0.000
# 90 Percent Confidence Interval                             0.000  0.046
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.031       0.031
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.834    0.025   33.532    0.000    0.834    0.671
# AUDRKDA (.p2.)    0.621    0.030   20.573    0.000    0.621    0.729
# AUDRINK (.p3.)    0.817    0.033   24.517    0.000    0.817    0.950
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.464    0.642   -0.001   -0.015
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.206    0.100   12.107    0.000    1.206    0.971
# .AUDRKDA (.11.)    0.435    0.074    5.900    0.000    0.435    0.511
# .AUDRINK (.12.)    0.444    0.096    4.639    0.000    0.444    0.516
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFRQ          0.847    0.053   15.953    0.000    0.847    0.549
# .AUDRKDAY          0.339    0.036    9.481    0.000    0.339    0.468
# .AUDRINK6          0.072    0.024    2.967    0.003    0.072    0.098
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.834    0.025   33.532    0.000    0.835    0.672
# AUDRKDA (.p2.)    0.621    0.030   20.573    0.000    0.621    0.723
# AUDRINK (.p3.)    0.817    0.033   24.517    0.000    0.818    0.941
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -1.019    0.308   -0.003   -0.044
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.206    0.100   12.107    0.000    1.206    0.971
# .AUDRKDA (.11.)    0.435    0.074    5.900    0.000    0.435    0.506
# .AUDRINK (.12.)    0.444    0.096    4.639    0.000    0.444    0.511
# .AUDIT.C           0.116    0.181    0.639    0.523    0.116    0.116
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFRQ          0.848    0.065   13.002    0.000    0.848    0.549
# .AUDRKDAY          0.353    0.046    7.677    0.000    0.353    0.478
# .AUDRINK6          0.087    0.037    2.329    0.020    0.087    0.115
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.834    0.025   33.532    0.000    0.834    0.729
# AUDRKDA (.p2.)    0.621    0.030   20.573    0.000    0.621    0.815
# AUDRINK (.p3.)    0.817    0.033   24.517    0.000    0.818    0.911
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE               0.002    0.004    0.593    0.553    0.002    0.036
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.206    0.100   12.107    0.000    1.206    1.054
# .AUDRKDA (.11.)    0.435    0.074    5.900    0.000    0.435    0.571
# .AUDRINK (.12.)    0.444    0.096    4.639    0.000    0.444    0.495
# .AUDIT.C          -0.241    0.213   -1.131    0.258   -0.241   -0.241
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.613    0.087    7.076    0.000    0.613    0.468
# .AUDRKDAY          0.195    0.056    3.486    0.000    0.195    0.336
# .AUDRINK6          0.136    0.055    2.500    0.012    0.136    0.169
# >

######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.cfa.model.Scalar.fit, MFI)
# >   fitMeasures(AUDIT_C.cfa.model.Scalar.fit, MFI)
# mfi 
# 0.998 
# >

############################ Test difference

anova(AUDIT_C.cfa.model.METRIC.fit, AUDIT_C.cfa.model.Scalar.fit)

# >   anova(AUDIT_C.cfa.model.METRIC.fit, AUDIT_C.cfa.model.Scalar.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
#                              Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.METRIC.fit 12 13238 13371 20.717                              
# AUDIT_C.cfa.model.Scalar.fit 16 13234 13345 24.474     3.5514       4     0.4701
# >  

#equal factor means specification

AUDIT_C.factor.means<- lavaan::cfa(AUDIT_C.cfa.model, data = AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings","intercepts","means"))
# >   AUDIT_C.factor.means<- lavaan::cfa(AUDIT_C.cfa.model, data = AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings","intercepts","means"))
# >

summary(AUDIT_C.factor.means)
# >   summary(AUDIT_C.factor.means)
# lavaan 0.6-4 ended normally after 43 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         30
# Number of equality constraints                    12
# Row rank of the constraints matrix                12
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      26.475      18.106
# Degrees of freedom                                18          18
# P-value (Chi-square)                           0.089       0.449
# Scaling correction factor                                  1.462
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
# 1                                              3.133       2.142
# 7                                              7.334       5.015
# 6                                             16.009      10.948
# 
# Parameter Estimates:
#   
# Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C =~                                          
# AUDRKFR (.p1.)    0.834    0.025   33.488    0.000
# AUDRKDA (.p2.)    0.621    0.030   20.569    0.000
# AUDRINK (.p3.)    0.818    0.033   24.499    0.000
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C ~                                           
#   AGE              -0.001    0.002   -0.779    0.436
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDRKFR (.10.)    1.216    0.070   17.293    0.000
# .AUDRKDA (.11.)    0.442    0.053    8.365    0.000
# .AUDRINK (.12.)    0.454    0.067    6.811    0.000
# .AUDIT.C           0.000                           
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDIT.C           1.000                           
# .AUDRKFRQ          0.847    0.053   15.948    0.000
# .AUDRKDAY          0.340    0.036    9.492    0.000
# .AUDRINK6          0.072    0.024    2.942    0.003
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C =~                                          
# AUDRKFR (.p1.)    0.834    0.025   33.488    0.000
# AUDRKDA (.p2.)    0.621    0.030   20.569    0.000
# AUDRINK (.p3.)    0.818    0.033   24.499    0.000
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C ~                                           
#   AGE              -0.001    0.002   -0.423    0.672
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDRKFR (.10.)    1.216    0.070   17.293    0.000
# .AUDRKDA (.11.)    0.442    0.053    8.365    0.000
# .AUDRINK (.12.)    0.454    0.067    6.811    0.000
# .AUDIT.C           0.000                           
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDIT.C           1.000                           
# .AUDRKFRQ          0.848    0.065   12.956    0.000
# .AUDRKDAY          0.353    0.046    7.665    0.000
# .AUDRINK6          0.087    0.037    2.317    0.021
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C =~                                          
# AUDRKFR (.p1.)    0.834    0.025   33.488    0.000
# AUDRKDA (.p2.)    0.621    0.030   20.569    0.000
# AUDRINK (.p3.)    0.818    0.033   24.499    0.000
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)
# AUDIT.C ~                                           
#   AGE              -0.003    0.002   -1.348    0.178
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDRKFR (.10.)    1.216    0.070   17.293    0.000
# .AUDRKDA (.11.)    0.442    0.053    8.365    0.000
# .AUDRINK (.12.)    0.454    0.067    6.811    0.000
# .AUDIT.C           0.000                           
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)
# .AUDIT.C           1.000                           
# .AUDRKFRQ          0.614    0.086    7.147    0.000
# .AUDRKDAY          0.195    0.056    3.486    0.000
# .AUDRINK6          0.136    0.055    2.477    0.013
# >

anova(AUDIT_C.cfa.model.Scalar.fit, AUDIT_C.factor.means)
# >   anova(AUDIT_C.cfa.model.Scalar.fit, AUDIT_C.factor.means)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.Scalar.fit 16 13234 13345 24.474                              
# AUDIT_C.factor.means         18 13232 13332 26.475     2.1962       2     0.3335
# >


######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.factor.means, MFI)
# >   fitMeasures(AUDIT_C.factor.means, MFI)
# mfi 
# 0.998 
# >

################### strict invariance

AUDIT_C.cfa.model.Strict.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts", "residuals"))
# >   AUDIT_C.cfa.model.Strict.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts", "residuals"))
# > 


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.Strict.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.Strict.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 44 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         32
# Number of equality constraints                    18
# Row rank of the constraints matrix                18
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      46.114      24.438
# Degrees of freedom                                22          22
# P-value (Chi-square)                           0.002       0.325
# Scaling correction factor                                  1.887
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
# 1                                              3.782       2.004
# 7                                              8.011       4.246
# 6                                             34.320      18.188
# 
# Model test baseline model:
#   
# Minimum Function Test Statistic             2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
# Comparative Fit Index (CFI)                    0.989       0.998
# Tucker-Lewis Index (TLI)                       0.991       0.999
# 
# Robust Comparative Fit Index (CFI)                         0.998
# Robust Tucker-Lewis Index (TLI)                            0.998
# 
# Loglikelihood and Information Criteria:
#   
# Loglikelihood user model (H0)              -6607.876   -6607.876
# Scaling correction factor                                  0.713
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction
# 
# Number of free parameters                         14          14
# Akaike (AIC)                               13243.753   13243.753
# Bayesian (BIC)                             13321.119   13321.119
# Sample-size adjusted Bayesian (BIC)        13276.642   13276.642
# 
# Root Mean Square Error of Approximation:
#   
# RMSEA                                          0.042       0.013
# 90 Percent Confidence Interval          0.025  0.059       0.000  0.031
# P-value RMSEA <= 0.05                          0.761       1.000
# 
# Robust RMSEA                                               0.018
# 90 Percent Confidence Interval                             0.000  0.051
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.039       0.039
# 
# Parameter Estimates:
#   
# Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.830    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.440    0.660   -0.001   -0.014
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.831    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -1.026    0.305   -0.003   -0.044
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C           0.119    0.182    0.656    0.512    0.119    0.119
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.831    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE               0.002    0.004    0.628    0.530    0.002    0.038
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C          -0.227    0.214   -1.065    0.287   -0.227   -0.227
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# >


######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.cfa.model.Strict.fit, MFI)

# >   fitMeasures(AUDIT_C.cfa.model.Strict.fit, MFI)
# mfi 
# 0.994
# >

anova(AUDIT_C.cfa.model.Strict.fit, AUDIT_C.cfa.model.Scalar.fit)
# >   anova(AUDIT_C.cfa.model.Strict.fit, AUDIT_C.cfa.model.Scalar.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.Scalar.fit 16 13234 13345 24.474                              
# AUDIT_C.cfa.model.Strict.fit 22 13244 13321 46.114     7.6309       6     0.2664
# >

################### factor variances and covariances

AUDIT_C.cfa.model.FVCV.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts", "residuals",  "lv.variances", "lv.covariances"))

# >   AUDIT_C.cfa.model.FVCV.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "RACE", group.equal=c("loadings", "intercepts", "residuals",  "lv.variances", "lv.covariances"))
# >
# 

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.FVCV.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.FVCV.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 44 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         32
# Number of equality constraints                    18
# Row rank of the constraints matrix                18
# 
# Used       Total
# Number of observations per group         
# 1                                               1049        1107
# 7                                                605         635
# 6                                                202         210
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      46.114      24.438
# Degrees of freedom                                22          22
# P-value (Chi-square)                           0.002       0.325
# Scaling correction factor                                  1.887
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
# 1                                              3.782       2.004
# 7                                              8.011       4.246
# 6                                             34.320      18.188
# 
# Model test baseline model:
#   
# Minimum Function Test Statistic             2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
# Comparative Fit Index (CFI)                    0.989       0.998
# Tucker-Lewis Index (TLI)                       0.991       0.999
# 
# Robust Comparative Fit Index (CFI)                         0.998
# Robust Tucker-Lewis Index (TLI)                            0.998
# 
# Loglikelihood and Information Criteria:
#   
# Loglikelihood user model (H0)              -6607.876   -6607.876
# Scaling correction factor                                  0.713
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction
# 
# Number of free parameters                         14          14
# Akaike (AIC)                               13243.753   13243.753
# Bayesian (BIC)                             13321.119   13321.119
# Sample-size adjusted Bayesian (BIC)        13276.642   13276.642
# 
# Root Mean Square Error of Approximation:
#   
# RMSEA                                          0.042       0.013
# 90 Percent Confidence Interval          0.025  0.059       0.000  0.031
# P-value RMSEA <= 0.05                          0.761       1.000
# 
# Robust RMSEA                                               0.018
# 90 Percent Confidence Interval                             0.000  0.051
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.039       0.039
# 
# Parameter Estimates:
#   
# Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [1]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.830    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.440    0.660   -0.001   -0.014
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# 
# 
# Group 2 [7]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.831    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -1.026    0.305   -0.003   -0.044
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C           0.119    0.182    0.656    0.512    0.119    0.119
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.998    0.998
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# 
# 
# Group 3 [6]:
#   
#   Latent Variables:
#                Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
# AUDRKFR (.p1.)    0.830    0.025   33.038    0.000    0.831    0.675
# AUDRKDA (.p2.)    0.624    0.030   20.736    0.000    0.624    0.736
# AUDRINK (.p3.)    0.818    0.033   24.692    0.000    0.818    0.944
# 
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE               0.002    0.004    0.628    0.530    0.002    0.038
# 
# Intercepts:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.204    0.099   12.128    0.000    1.204    0.979
# .AUDRKDA (.11.)    0.435    0.074    5.861    0.000    0.435    0.513
# .AUDRINK (.12.)    0.442    0.096    4.607    0.000    0.442    0.510
# .AUDIT.C          -0.227    0.214   -1.065    0.287   -0.227   -0.227
# 
# Variances:
#                 Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFR (.p6.)    0.824    0.039   21.367    0.000    0.824    0.544
# .AUDRKDA (.p7.)    0.329    0.027   12.185    0.000    0.329    0.458
# .AUDRINK (.p8.)    0.082    0.020    4.132    0.000    0.082    0.109
# > 

######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT.cfa.model.FVCV.fit, MFI)

# >   fitMeasures(AUDIT.cfa.model.FVCV.fit, MFI)
# mfi 
# 0.994 
# >

##################### INVARIANCE TESTS (GENDER)

# It would be informative to see whether the item loadings are similar
# in each group. To do this, we only need to add group="GENDER" to our cfa statement.

AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit <- lavaan::cfa(AUDIT_C.cfa.model, data = AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER")
# >   AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit <- lavaan::cfa(AUDIT_C.cfa.model, data = AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER")
# >


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 44 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         20
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      10.515      11.666
# Degrees of freedom                                 4           4
# P-value (Chi-square)                           0.033       0.020
# Scaling correction factor                                  0.901
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                              2.003       2.222
# 1                                              8.512       9.443
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.997       0.995
# Tucker-Lewis Index (TLI)                       0.991       0.985
# 
# Robust Comparative Fit Index (CFI)                         0.997
# Robust Tucker-Lewis Index (TLI)                            0.991
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6582.813   -6582.813
# Scaling correction factor                                  1.916
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         20          20
# Akaike (AIC)                               13205.626   13205.626
# Bayesian (BIC)                             13316.149   13316.149
# Sample-size adjusted Bayesian (BIC)        13252.610   13252.610
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.042       0.045
# 90 Percent Confidence Interval          0.011  0.074       0.015  0.079
# P-value RMSEA <= 0.05                          0.614       0.533
# 
# Robust RMSEA                                               0.043
# 90 Percent Confidence Interval                             0.015  0.073
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.013       0.013
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFRQ          0.792    0.032   24.764    0.000    0.792    0.663
# AUDRKDAY          0.603    0.041   14.824    0.000    0.603    0.742
# AUDRINK6          0.735    0.043   17.189    0.000    0.735    0.932
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.702    0.483   -0.001   -0.022
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.202    0.082   14.731    0.000    1.202    1.006
# .AUDRKDAY          0.424    0.062    6.867    0.000    0.424    0.522
# .AUDRINK6          0.413    0.074    5.613    0.000    0.413    0.524
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFRQ          0.802    0.049   16.271    0.000    0.802    0.561
# .AUDRKDAY          0.296    0.032    9.178    0.000    0.296    0.449
# .AUDRINK6          0.082    0.024    3.480    0.001    0.082    0.131
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFRQ          0.875    0.040   22.080    0.000    0.876    0.688
# AUDRKDAY          0.649    0.045   14.480    0.000    0.649    0.728
# AUDRINK6          0.918    0.051   18.038    0.000    0.919    0.960
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.002    0.003   -0.855    0.393   -0.002   -0.033
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.293    0.133    9.711    0.000    1.293    1.015
# .AUDRKDAY          0.514    0.101    5.064    0.000    0.514    0.577
# .AUDRINK6          0.567    0.137    4.154    0.000    0.567    0.593
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.855    0.062   13.901    0.000    0.855    0.527
# .AUDRKDAY          0.373    0.046    8.115    0.000    0.373    0.470
# .AUDRINK6          0.071    0.035    2.035    0.042    0.071    0.078
# >


######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, MFI)

# >   fitMeasures(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, MFI)
# mfi 
# 0.998 
# >   


############### Metric invariance

AUDIT_C.cfa.model.METRIC.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings"))
# >   AUDIT_C.cfa.model.METRIC.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings"))
# >


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.METRIC.GENDER.fit, fit.measures= TRUE, standardized=TRUE)

# >   summary(AUDIT_C.cfa.model.METRIC.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 39 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         20
# Number of equality constraints                     3
# Row rank of the constraints matrix                 3
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      34.932      24.940
# Degrees of freedom                                 7           7
# P-value (Chi-square)                           0.000       0.001
# Scaling correction factor                                  1.401
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                             12.185       8.700
# 1                                             22.746      16.240
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.987       0.988
# Tucker-Lewis Index (TLI)                       0.978       0.980
# 
# Robust Comparative Fit Index (CFI)                         0.989
# Robust Tucker-Lewis Index (TLI)                            0.980
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6595.021   -6595.021
# Scaling correction factor                                  1.606
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         17          17
# Akaike (AIC)                               13224.042   13224.042
# Bayesian (BIC)                             13317.987   13317.987
# Sample-size adjusted Bayesian (BIC)        13263.978   13263.978
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.066       0.053
# 90 Percent Confidence Interval          0.045  0.088       0.034  0.072
# P-value RMSEA <= 0.05                          0.103       0.378
# 
# Robust RMSEA                                               0.062
# 90 Percent Confidence Interval                             0.037  0.089
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.061       0.061
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.830    0.025   32.930    0.000    0.830    0.674
# AUDRKDA (.p2.)    0.624    0.030   20.638    0.000    0.624    0.744
# AUDRINK (.p3.)    0.808    0.033   24.227    0.000    0.809    0.964
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.781    0.435   -0.001   -0.022
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.206    0.079   15.249    0.000    1.206    0.978
# .AUDRKDAY          0.426    0.059    7.226    0.000    0.426    0.508
# .AUDRINK6          0.419    0.074    5.671    0.000    0.419    0.499
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.830    0.051   16.402    0.000    0.830    0.546
# .AUDRKDAY          0.314    0.033    9.596    0.000    0.314    0.446
# .AUDRINK6          0.050    0.024    2.127    0.033    0.050    0.071
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.830    0.025   32.930    0.000    0.830    0.677
# AUDRKDA (.p2.)    0.624    0.030   20.638    0.000    0.624    0.727
# AUDRINK (.p3.)    0.808    0.033   24.227    0.000    0.809    0.911
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -0.805    0.421   -0.003   -0.034
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFRQ          1.292    0.139    9.277    0.000    1.292    1.053
# .AUDRKDAY          0.514    0.106    4.828    0.000    0.514    0.599
# .AUDRINK6          0.558    0.133    4.198    0.000    0.558    0.629
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.817    0.059   13.865    0.000    0.817    0.542
# .AUDRKDAY          0.347    0.043    8.130    0.000    0.347    0.471
# .AUDRINK6          0.134    0.032    4.134    0.000    0.134    0.170
# 
# > 


anova(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, AUDIT_C.cfa.model.METRIC.GENDER.fit)
# >   anova(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, AUDIT_C.cfa.model.METRIC.GENDER.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)   
# AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit  4 13206 13316 10.515                                 
# AUDIT_C.cfa.model.METRIC.GENDER.fit      7 13224 13318 34.932     11.817       3   0.008039 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# >

######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, MFI)

# >   fitMeasures(AUDIT_C.cfa.model.GENDER.CONFIGURAL.fit, MFI)
# mfi 
# 0.998 
# >   


################### scalar invariance

AUDIT_C.cfa.model.Scalar.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts"))
# >   AUDIT_C.cfa.model.Scalar.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts"))
# >


# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.Scalar.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# >   summary(AUDIT_C.cfa.model.Scalar.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 36 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         21
# Number of equality constraints                     6
# Row rank of the constraints matrix                 6
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      36.810      27.976
# Degrees of freedom                                 9           9
# P-value (Chi-square)                           0.000       0.001
# Scaling correction factor                                  1.316
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                             13.094       9.951
# 1                                             23.716      18.024
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.987       0.988
# Tucker-Lewis Index (TLI)                       0.983       0.983
# 
# Robust Comparative Fit Index (CFI)                         0.989
# Robust Tucker-Lewis Index (TLI)                            0.985
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6595.960   -6595.960
# Scaling correction factor                                  1.433
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         15          15
# Akaike (AIC)                               13221.920   13221.920
# Bayesian (BIC)                             13304.813   13304.813
# Sample-size adjusted Bayesian (BIC)        13257.158   13257.158
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.058       0.048
# 90 Percent Confidence Interval          0.039  0.078       0.031  0.066
# P-value RMSEA <= 0.05                          0.230       0.555
# 
# Robust RMSEA                                               0.055
# 90 Percent Confidence Interval                             0.032  0.078
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.061       0.061
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.053    0.000    0.828    0.672
# AUDRKDA (.p2.)    0.623    0.030   20.616    0.000    0.623    0.743
# AUDRINK (.p3.)    0.809    0.033   24.265    0.000    0.809    0.964
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.784    0.433   -0.001   -0.023
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.185    0.077   15.303    0.000    1.185    0.962
# .AUDRKDA (.11.)    0.421    0.058    7.279    0.000    0.421    0.502
# .AUDRINK (.12.)    0.421    0.074    5.700    0.000    0.421    0.501
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.832    0.051   16.204    0.000    0.832    0.548
# .AUDRKDAY          0.315    0.033    9.581    0.000    0.315    0.447
# .AUDRINK6          0.049    0.024    2.052    0.040    0.049    0.070
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.053    0.000    0.829    0.675
# AUDRKDA (.p2.)    0.623    0.030   20.616    0.000    0.624    0.727
# AUDRINK (.p3.)    0.809    0.033   24.265    0.000    0.810    0.912
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -0.806    0.420   -0.003   -0.034
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.185    0.077   15.303    0.000    1.185    0.966
# .AUDRKDA (.11.)    0.421    0.058    7.279    0.000    0.421    0.490
# .AUDRINK (.12.)    0.421    0.074    5.700    0.000    0.421    0.474
# .AUDIT.C           0.162    0.187    0.864    0.387    0.162    0.162
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.818    0.058   14.156    0.000    0.818    0.544
# .AUDRKDAY          0.348    0.043    8.165    0.000    0.348    0.472
# .AUDRINK6          0.133    0.032    4.136    0.000    0.133    0.169
# 
# > 

######################### ASSESS MFI for Metric invariance of RACE models

fitMeasures(AUDIT_C.cfa.model.Scalar.GENDER.fit, MFI)
# >   fitMeasures(AUDIT_C.cfa.model.Scalar.GENDER.fit, MFI)
# mfi 
# 0.993 
# >


############################## Modificaiton Index

modindices(AUDIT_C.cfa.model.Scalar.GENDER.fit)
# >   modindices(AUDIT_C.cfa.model.Scalar.GENDER.fit)
# lhs op      rhs block group level     mi    epc sepc.lv sepc.all sepc.nox
# 5   AUDIT.C ~~  AUDIT.C     1     1     1 19.360 -0.337  -0.999   -0.999   -0.999
# 19  AUDIT.C ~~  AUDIT.C     2     2     1 19.360  0.337   0.999    0.999    0.999
# 35 AUDRKFRQ ~~ AUDRKDAY     1     1     1  9.087  0.095   0.095    0.186    0.186
# 36 AUDRKFRQ ~~ AUDRINK6     1     1     1  9.823 -0.117  -0.117   -0.579   -0.579
# 37 AUDRKDAY ~~ AUDRINK6     1     1     1  8.064 -0.080  -0.080   -0.647   -0.647
# 39 AUDRKFRQ ~~ AUDRKDAY     2     2     1  7.312 -0.086  -0.086   -0.161   -0.161
# 40 AUDRKFRQ ~~ AUDRINK6     2     2     1  9.917  0.118   0.118    0.358    0.358
# 41 AUDRKDAY ~~ AUDRINK6     2     2     1  6.458  0.072   0.072    0.335    0.335
# >

############################ Test difference

anova(AUDIT_C.cfa.model.METRIC.GENDER.fit, AUDIT_C.cfa.model.Scalar.GENDER.fit)

# >   anova(AUDIT_C.cfa.model.METRIC.GENDER.fit, AUDIT_C.cfa.model.Scalar.GENDER.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.METRIC.GENDER.fit  7 13224 13318 34.932                              
# AUDIT_C.cfa.model.Scalar.GENDER.fit  9 13222 13305 36.810     1.8436       2     0.3978
# >


############ PARTIAL INVARIANCE testing

AUDIT_C.cfa.model.Scalar.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >   AUDIT_C.cfa.model.Scalar.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >

summary(AUDIT_C.cfa.model.Scalar.GENDER.fit, fit.measures=TRUE, standardized=TRUE)
# >   summary(AUDIT_C.cfa.model.Scalar.GENDER.fit, fit.measures=TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 36 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         21
# Number of equality constraints                     6
# Row rank of the constraints matrix                 6
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      36.810      27.976
# Degrees of freedom                                 9           9
# P-value (Chi-square)                           0.000       0.001
# Scaling correction factor                                  1.316
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                             13.094       9.951
# 1                                             23.716      18.024
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.987       0.988
# Tucker-Lewis Index (TLI)                       0.983       0.983
# 
# Robust Comparative Fit Index (CFI)                         0.989
# Robust Tucker-Lewis Index (TLI)                            0.985
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6595.960   -6595.960
# Scaling correction factor                                  1.433
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         15          15
# Akaike (AIC)                               13221.920   13221.920
# Bayesian (BIC)                             13304.813   13304.813
# Sample-size adjusted Bayesian (BIC)        13257.158   13257.158
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.058       0.048
# 90 Percent Confidence Interval          0.039  0.078       0.031  0.066
# P-value RMSEA <= 0.05                          0.230       0.555
# 
# Robust RMSEA                                               0.055
# 90 Percent Confidence Interval                             0.032  0.078
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.061       0.061
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.053    0.000    0.828    0.672
# AUDRKDA (.p2.)    0.623    0.030   20.616    0.000    0.623    0.743
# AUDRINK (.p3.)    0.809    0.033   24.265    0.000    0.809    0.964
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.784    0.433   -0.001   -0.023
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.185    0.077   15.303    0.000    1.185    0.962
# .AUDRKDA (.11.)    0.421    0.058    7.279    0.000    0.421    0.502
# .AUDRINK (.12.)    0.421    0.074    5.700    0.000    0.421    0.501
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.832    0.051   16.204    0.000    0.832    0.548
# .AUDRKDAY          0.315    0.033    9.581    0.000    0.315    0.447
# .AUDRINK6          0.049    0.024    2.052    0.040    0.049    0.070
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.053    0.000    0.829    0.675
# AUDRKDA (.p2.)    0.623    0.030   20.616    0.000    0.624    0.727
# AUDRINK (.p3.)    0.809    0.033   24.265    0.000    0.810    0.912
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -0.806    0.420   -0.003   -0.034
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.185    0.077   15.303    0.000    1.185    0.966
# .AUDRKDA (.11.)    0.421    0.058    7.279    0.000    0.421    0.490
# .AUDRINK (.12.)    0.421    0.074    5.700    0.000    0.421    0.474
# .AUDIT.C           0.162    0.187    0.864    0.387    0.162    0.162
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFRQ          0.818    0.058   14.156    0.000    0.818    0.544
# .AUDRKDAY          0.348    0.043    8.165    0.000    0.348    0.472
# .AUDRINK6          0.133    0.032    4.136    0.000    0.133    0.169
# 
# >

################### CHI-SQUARE DIFFERENCE TEST
anova(AUDIT_C.cfa.model.Scalar.GENDER.fit, AUDIT_C.cfa.model.METRIC.GENDER.fit)
# >   anova(AUDIT_C.cfa.model.Scalar.GENDER.fit, AUDIT_C.cfa.model.METRIC.GENDER.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# AUDIT_C.cfa.model.METRIC.GENDER.fit  7 13224 13318 34.932                              
# AUDIT_C.cfa.model.Scalar.GENDER.fit  9 13222 13305 36.810     1.8436       2     0.3978
# >

################### strict invariance

AUDIT_C.cfa.model.Strict.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts", "residuals"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >   AUDIT_C.cfa.model.Strict.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts", "residuals"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.Strict.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# >   summary(AUDIT_C.cfa.model.Strict.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 37 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         21
# Number of equality constraints                     9
# Row rank of the constraints matrix                 9
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      58.498      34.099
# Degrees of freedom                                12          12
# P-value (Chi-square)                           0.000       0.001
# Scaling correction factor                                  1.716
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                             26.117      15.224
# 1                                             32.381      18.875
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.979       0.985
# Tucker-Lewis Index (TLI)                       0.979       0.985
# 
# Robust Comparative Fit Index (CFI)                         0.983
# Robust Tucker-Lewis Index (TLI)                            0.983
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6606.804   -6606.804
# Scaling correction factor                                  1.016
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         12          12
# Akaike (AIC)                               13237.608   13237.608
# Bayesian (BIC)                             13303.922   13303.922
# Sample-size adjusted Bayesian (BIC)        13265.799   13265.799
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.065       0.045
# 90 Percent Confidence Interval          0.049  0.082       0.031  0.058
# P-value RMSEA <= 0.05                          0.066       0.729
# 
# Robust RMSEA                                               0.058
# 90 Percent Confidence Interval                             0.036  0.082
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.074       0.074
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.067    0.000    0.828    0.673
# AUDRKDA (.p2.)    0.622    0.030   20.693    0.000    0.622    0.735
# AUDRINK (.p3.)    0.819    0.033   24.788    0.000    0.819    0.946
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.722    0.470   -0.001   -0.021
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.180    0.077   15.280    0.000    1.180    0.960
# .AUDRKDA (.11.)    0.417    0.058    7.225    0.000    0.417    0.492
# .AUDRINK (.12.)    0.418    0.074    5.634    0.000    0.418    0.483
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFR (.p6.)    0.826    0.039   21.399    0.000    0.826    0.547
# .AUDRKDA (.p7.)    0.331    0.027   12.190    0.000    0.331    0.460
# .AUDRINK (.p8.)    0.079    0.020    3.941    0.000    0.079    0.106
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.067    0.000    0.828    0.674
# AUDRKDA (.p2.)    0.622    0.030   20.693    0.000    0.623    0.735
# AUDRINK (.p3.)    0.819    0.033   24.788    0.000    0.819    0.946
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -0.856    0.392   -0.003   -0.036
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.180    0.077   15.280    0.000    1.180    0.960
# .AUDRKDA (.11.)    0.417    0.058    7.225    0.000    0.417    0.492
# .AUDRINK (.12.)    0.418    0.074    5.634    0.000    0.418    0.483
# .AUDIT.C           0.175    0.187    0.935    0.350    0.175    0.175
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFR (.p6.)    0.826    0.039   21.399    0.000    0.826    0.546
# .AUDRKDA (.p7.)    0.331    0.027   12.190    0.000    0.331    0.460
# .AUDRINK (.p8.)    0.079    0.020    3.941    0.000    0.079    0.105
# 
# >

#################### TESTING MFI   
fitMeasures(AUDIT_C.cfa.model.Strict.GENDER.fit, MFI)
# >   fitMeasures(AUDIT_C.cfa.model.Strict.GENDER.fit, MFI)
# mfi 
# 0.988 
# >

anova(AUDIT_C.cfa.model.Strict.GENDER.fit, AUDIT_C.cfa.model.Scalar.GENDER.fit)
# >   anova(AUDIT_C.cfa.model.Strict.GENDER.fit, AUDIT_C.cfa.model.Scalar.GENDER.fit)
# Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
# 
# Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# AUDIT_C.cfa.model.Scalar.GENDER.fit  9 13222 13305 36.810                                
# AUDIT_C.cfa.model.Strict.GENDER.fit 12 13238 13304 58.498     7.4406       3     0.0591 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# >

# ################### factor variances and covariances

AUDIT_C.cfa.model.FVCV.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts", "residuals",  "lv.variances", "lv.covariances"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >   AUDIT_C.cfa.model.FVCV.GENDER.fit <- lavaan::cfa(AUDIT_C.cfa.model, AUDIT.C_data.02, std.lv=TRUE, estimator = "MLR", group = "GENDER", group.equal=c("loadings", "intercepts", "residuals",  "lv.variances", "lv.covariances"), group.partial = c("AUDRKFRQ ~~ AUDRINK6"))
# >

# We can then print the results by using the summary statement again (remember
# that we have to call the new object for this analysis):

summary(AUDIT_C.cfa.model.FVCV.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# >   summary(AUDIT_C.cfa.model.FVCV.GENDER.fit, fit.measures= TRUE, standardized=TRUE)
# lavaan 0.6-4 ended normally after 37 iterations
# 
# Optimization method                           NLMINB
# Number of free parameters                         21
# Number of equality constraints                     9
# Row rank of the constraints matrix                 9
# 
# Used       Total
# Number of observations per group         
# 2                                               1046        1104
# 1                                                810         848
# 
# Estimator                                         ML      Robust
# Model Fit Test Statistic                      58.498      34.099
# Degrees of freedom                                12          12
# P-value (Chi-square)                           0.000       0.001
# Scaling correction factor                                  1.716
# for the Yuan-Bentler correction (Mplus variant)
# 
# Chi-square for each group:
#   
#   2                                             26.117      15.224
# 1                                             32.381      18.875
# 
# Model test baseline model:
#   
#   Minimum Function Test Statistic             2221.524    1534.929
# Degrees of freedom                                12          12
# P-value                                        0.000       0.000
# 
# User model versus baseline model:
#   
#   Comparative Fit Index (CFI)                    0.979       0.985
# Tucker-Lewis Index (TLI)                       0.979       0.985
# 
# Robust Comparative Fit Index (CFI)                         0.983
# Robust Tucker-Lewis Index (TLI)                            0.983
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6606.804   -6606.804
# Scaling correction factor                                  1.016
# for the MLR correction
# Loglikelihood unrestricted model (H1)      -6577.555   -6577.555
# Scaling correction factor                                  1.747
# for the MLR correction
# 
# Number of free parameters                         12          12
# Akaike (AIC)                               13237.608   13237.608
# Bayesian (BIC)                             13303.922   13303.922
# Sample-size adjusted Bayesian (BIC)        13265.799   13265.799
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.065       0.045
# 90 Percent Confidence Interval          0.049  0.082       0.031  0.058
# P-value RMSEA <= 0.05                          0.066       0.729
# 
# Robust RMSEA                                               0.058
# 90 Percent Confidence Interval                             0.036  0.082
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.074       0.074
# 
# Parameter Estimates:
#   
#   Information                                 Observed
# Observed information based on                Hessian
# Standard Errors                   Robust.huber.white
# 
# 
# Group 1 [2]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.067    0.000    0.828    0.673
# AUDRKDA (.p2.)    0.622    0.030   20.693    0.000    0.622    0.735
# AUDRINK (.p3.)    0.819    0.033   24.788    0.000    0.819    0.946
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.001    0.002   -0.722    0.470   -0.001   -0.021
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.180    0.077   15.280    0.000    1.180    0.960
# .AUDRKDA (.11.)    0.417    0.058    7.225    0.000    0.417    0.492
# .AUDRINK (.12.)    0.418    0.074    5.634    0.000    0.418    0.483
# .AUDIT.C           0.000                               0.000    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               1.000    1.000
# .AUDRKFR (.p6.)    0.826    0.039   21.399    0.000    0.826    0.547
# .AUDRKDA (.p7.)    0.331    0.027   12.190    0.000    0.331    0.460
# .AUDRINK (.p8.)    0.079    0.020    3.941    0.000    0.079    0.106
# 
# 
# Group 2 [1]:
#   
#   Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C =~                                                            
#   AUDRKFR (.p1.)    0.828    0.025   33.067    0.000    0.828    0.674
# AUDRKDA (.p2.)    0.622    0.030   20.693    0.000    0.623    0.735
# AUDRINK (.p3.)    0.819    0.033   24.788    0.000    0.819    0.946
# 
# Regressions:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# AUDIT.C ~                                                             
#   AGE              -0.003    0.003   -0.856    0.392   -0.003   -0.036
# 
# Intercepts:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDRKFR (.10.)    1.180    0.077   15.280    0.000    1.180    0.960
# .AUDRKDA (.11.)    0.417    0.058    7.225    0.000    0.417    0.492
# .AUDRINK (.12.)    0.418    0.074    5.634    0.000    0.418    0.483
# .AUDIT.C           0.175    0.187    0.935    0.350    0.175    0.175
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
# .AUDIT.C           1.000                               0.999    0.999
# .AUDRKFR (.p6.)    0.826    0.039   21.399    0.000    0.826    0.546
# .AUDRKDA (.p7.)    0.331    0.027   12.190    0.000    0.331    0.460
# .AUDRINK (.p8.)    0.079    0.020    3.941    0.000    0.079    0.105
# >

#################### TESTING MFI   
fitMeasures(AUDIT_C.cfa.model.FVCV.GENDER.fit, MFI)
# >   fitMeasures(AUDIT_C.cfa.model.FVCV.GENDER.fit, MFI)
# mfi 
# 0.988 
# >
# 
#CFA of AUDIT-C

library(psych)
library(lavaan)
install.packages("regsem")
library(regsem)

rrsdlabels<-c("D1", "D2", "D3", "AGE", "AUDIT-C")

semPaths(AUDIT_C.cfa.model.fit, what = "path", layout = "tree",
         style="lisrel", intercepts = TRUE, residuals = F,
         thresholds = TRUE, intStyle = "multi", rotation=1,
         nodeLabels = rrsdlabels, nCharNodes = 3, nCharEdges = 3,
         sizeMan=8, sizeLat=12, sizeInt = 8, edge.label.cex = 1,
         whatLabels = "stand", fixedStyle = 2, freeStyle = 1,
         measurementLayout = "tree", fade = FALSE, curvePivot=FALSE,
         ThreshAtSide = TRUE, intAtSide = TRUE, springLevels = tree3,
         exoCov = TRUE, centerLevels = TRUE, cardinal = c("exo cov","load end"), 
         covAtResiduals = FALSE, weighted = FALSE)

install.packages("semPlotModel")
library(semPlotModel)

# fit the modelfit <- cfa(mod, bfi2)
out.reg <- regsem(AUDIT_C.cfa.model.fit, type="lasso", pars_pen=c(1:4))
# plot the model
semPaths(semPlotModel.regsem(object = out.reg))

semPaths(AUDIT_C.cfa.model.fit, title=FALSE, exoVar = FALSE, exoCov = FALSE)

semPaths(AUDIT_C.cfa.model.fit,"std", edge.label.cex = 0.5, exoVar = FALSE, 
         exoCov = FALSE)

semPaths(AUDIT_C.cfa.model.fit, 'std', 'est',
         curveAdjacent = TRUE, nodeLabels = rrsdlabels,
         style = "lisrel",
         weighted = FALSE)



install.packages("igraph")
library(igraph)

title("AUDIT-C Confirmatory Factor Analysis", line=3) 


semPaths(AUDIT_C.cfa.model.fit,title=FALSE, curvePivot = TRUE)

semPaths(AUDIT_C.cfa.model.fit,"std",edge.label.cex=1.0, curvePivot = TRUE)

semPaths(AUDIT_C.cfa.model.fit, sizeMan=10)

semPaths(AUDIT_C.cfa.model.fit,what="std",edge.label.cex=0.75,edge.color="black",
         ,sizeMan=10,sizeLat=10,fade=FALSE, esize=2, asize=2)

title("AUDIT-C Confirmatory Factor Analysis", line=3) 

rrsdlabels<-c("D1", "D2", "D3", "AGE", "AUDIT-C")
semPaths(AUDIT_C.cfa.model.fit, what="std", edge.label.cex=0.75, 
         edge.color="black", rotation=2, nodeLabels = rrsdlabels, 
         nCharNodes = 2, nCharEdges = 2,sizeMan=10, sizeLat=10,
         sizeInt = 8, whatLabels = "stand", fixedStyle = 3,
         freeStyle = 1, measurementLayout = "tree",
         fade = FALSE, style="lisrel", curvePivot=TRUE)

semPaths(AUDIT_C.cfa.model.fit, what = "paths", layout = "tree2",
         intercepts = TRUE, residuals = TRUE, thresholds = TRUE, 
         intStyle = "multi", rotation=1, nCharNodes = 2, 
         nCharEdges = 2, sizeMan=10, sizeLat=10, sizeInt = 8,
         edge.label.cex = 1, whatLabels = "stand", fixedStyle = 3,
         freeStyle = 1, measurementLayout = "circle")

title("AUDIT-C Confirmatory Factor Analysis", line=1) 

library(ggsem)


#SCALAR INVARIANCE AUDIT-C W/RACE

semPaths(AUDIT_C.cfa.model.Scalar.fit, what = "paths", layout = "tree",
         intercepts = TRUE, residuals = TRUE, thresholds = TRUE, 
         intStyle = "multi", rotation=1, nCharNodes = 3, 
         nCharEdges = 2, sizeMan=8, sizeLat=8, sizeInt = 8,
         edge.label.cex = 1, whatLabels = "stand", fixedStyle = 3,
         freeStyle = 1, measurementLayout = "tree")


#SCALAR INVARIANCE AUDIT-C W/ GENDER

semPaths(AUDIT_C.cfa.model.Scalar.GENDER.fit, what = "paths", layout = "tree",
         intercepts = TRUE, residuals = TRUE, thresholds = TRUE, 
         intStyle = "multi", rotation=2, nCharNodes = 3, 
         nCharEdges = 2, sizeMan=8, sizeLat=8, sizeInt = 8,
         edge.label.cex = 1, whatLabels = "stand", fixedStyle = 3,
         freeStyle = 1, measurementLayout = "tree")



##### Power for CFA model
#Power analysis for CSM

alpha <- 0.05 #alpha level
d <- 3 #degrees of freedom
n <- 2038 #sample size
rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0.05 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)

# >   alpha <- 0.05 #alpha level
# >   d <- 3 #degrees of freedom
# >   n <- 2038 #sample size
# >   rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0.05 #alternative hypothesized RMSEA
# >   
#   >   #Code below this point need not be changed by user
#   >   ncp0 <- (n-1)*d*rmsea0^2
# >   ncpa <- (n-1)*d*rmseaa^2
# >   
#   >   #Compute power
#   >   if(rmsea0<rmseaa) {
#     +     cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#     +     pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +   }
# >   if(rmsea0>rmseaa) {
#   +     cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#   +     pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#   +   }
# >   print(pow)
# [1] 0.9232578
# >


###################### Power analysis of CAF model 2 with regression IVs. 
#Power analysis for CSM

alpha <- 0.05 #alpha level
d <- 6 #degrees of freedom
n <- 2038 #sample size
rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0.05 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)

# > alpha <- 0.05 #alpha level
# >   d <- 6 #degrees of freedom
# >   n <- 2038 #sample size
# >   rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0.05 #alternative hypothesized RMSEA
# >   
#   >   #Code below this point need not be changed by user
#   >   ncp0 <- (n-1)*d*rmsea0^2
# >   ncpa <- (n-1)*d*rmseaa^2
# >   
#   >   #Compute power
#   >   if(rmsea0<rmseaa) {
#     +     cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#     +     pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +   }
# >   if(rmsea0>rmseaa) {
#   +     cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#   +     pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#   +   }
# >   print(pow)
# [1] 0.9963968
# >

############## INvariance testing for power of CFA RACE (AA)

#Power analysis for CSM

alpha <- 0.05 #alpha level
d <- 9 #degrees of freedom
n <- 1107 #sample size
rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0.05 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)

# >   alpha <- 0.05 #alpha level
# >   d <- 9 #degrees of freedom
# >   n <- 1107 #sample size
# >   rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0.05 #alternative hypothesized RMSEA
# >   
#   >   #Code below this point need not be changed by user
#   >   ncp0 <- (n-1)*d*rmsea0^2
# >   ncpa <- (n-1)*d*rmseaa^2
# >   
#   >   #Compute power
#   >   if(rmsea0<rmseaa) {
#     +     cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#     +     pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +   }
# >   if(rmsea0>rmseaa) {
#   +     cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#   +     pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#   +   }
# >   print(pow)
# [1] 0.9856558
# >


#Power analysis for CSM

alpha <- 0.05 #alpha level
d <- 9 #degrees of freedom
n <- 635 #sample size
rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0.05 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)

# >   alpha <- 0.05 #alpha level
# >   d <- 9 #degrees of freedom
# >   n <- 635 #sample size
# >   rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0.05 #alternative hypothesized RMSEA
# >   
#   >   #Code below this point need not be changed by user
#   >   ncp0 <- (n-1)*d*rmsea0^2
# >   ncpa <- (n-1)*d*rmseaa^2
# >   
#   >   #Compute power
#   >   if(rmsea0<rmseaa) {
#     +     cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#     +     pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +   }
# >   if(rmsea0>rmseaa) {
#   +     cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#   +     pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#   +   }
# >   print(pow)
# [1] 0.8780032
# >

#Power analysis for CSM

alpha <- 0.05 #alpha level
d <- 9 #degrees of freedom
n <- 210 #sample size
rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0.0 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
  cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
  pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
  cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
  pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)

# >   alpha <- 0.05 #alpha level
# >   d <- 9 #degrees of freedom
# >   n <- 210 #sample size
# >   rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0.0 #alternative hypothesized RMSEA
# >   
#   >   #Code below this point need not be changed by user
#   >   ncp0 <- (n-1)*d*rmsea0^2
# >   ncpa <- (n-1)*d*rmseaa^2
# >   
#   >   #Compute power
#   >   if(rmsea0<rmseaa) {
#     +     cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#     +     pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +   }
# >   if(rmsea0>rmseaa) {
#   +     cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#   +     pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#   +   }
# >   print(pow)
# [1] 0.7488189
# >


#Computation of minimum sample size for test of fit

rmsea0 <- 0.09 #null hypothesized RMSEA
rmseaa <- 0 #alternative hypothesized RMSEA
d <- 9 #degrees of freedom
alpha <- 0.05 #alpha level
desired <- 0.8 #desired power

#Code below need not be changed by user
#initialize values
pow <- 0.0
n <- 0
#begin loop for finding initial level of n
while (pow<desired) {
  n <- n+100
  ncp0 <- (n-1)*d*rmsea0^2
  ncpa <- (n-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
}

#begin loop for interval halving
foo <- -1
newn <- n
interval <- 200
powdiff <- pow - desired
while (powdiff>.001) {
  interval <- interval*.5
  newn <- newn + foo*interval*.5
  ncp0 <- (newn-1)*d*rmsea0^2
  ncpa <- (newn-1)*d*rmseaa^2
  #compute power
  if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  else {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
  }
  powdiff <- abs(pow-desired)
  if (pow<desired) {
    foo <- 1
  }
  if (pow>desired) {
    foo <- -1
  }
}

minn <- newn
print(minn)

# > rmsea0 <- 0.09 #null hypothesized RMSEA
# >   rmseaa <- 0 #alternative hypothesized RMSEA
# >   d <- 9 #degrees of freedom
# >   alpha <- 0.05 #alpha level
# >   desired <- 0.8 #desired power
# >   
#   >   #Code below need not be changed by user
#   >   #initialize values
#   >   pow <- 0.0
# >   n <- 0
# >   #begin loop for finding initial level of n
#   >   while (pow<desired) {
#     +     n <- n+100
#     +     ncp0 <- (n-1)*d*rmsea0^2
#     +     ncpa <- (n-1)*d*rmseaa^2
#     +     #compute power
#       +     if(rmsea0<rmseaa) {
#         +       cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#         +       pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#         +     }
#     +     else {
#       +       cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#       +       pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#       +     }
#     +   }
# >   
#   >   #begin loop for interval halving
#   >   foo <- -1
# >   newn <- n
# >   interval <- 200
# >   powdiff <- pow - desired
# >   while (powdiff>.001) {
#   +     interval <- interval*.5
#   +     newn <- newn + foo*interval*.5
#   +     ncp0 <- (newn-1)*d*rmsea0^2
#   +     ncpa <- (newn-1)*d*rmseaa^2
#   +     #compute power
#     +     if(rmsea0<rmseaa) {
#       +       cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
#       +       pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
#       +     }
#   +     else {
#     +       cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
#     +       pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
#     +     }
#   +     powdiff <- abs(pow-desired)
#   +     if (pow<desired) {
#     +       foo <- 1
#     +     }
#   +     if (pow>desired) {
#     +       foo <- -1
#     +     }
#   +   }
# >   
#   >   minn <- newn
# >   print(minn)
# [1] 228.9062
# >

library(semTools)

auditvfit <- measurementInvariance (model = AUDIT_C.cfa.model, data = AUDIT.C_data.02, group = "RACE", estimator = "MLR")

#   >   auditvfit <- measurementInvariance (model = AUDIT.cfa.model, data = AUDIT.C_data.02, group = "RACE", estimator = "MLR")
#   
#   Measurement invariance models:
#     
#   Model 1 : fit.configural
#   Model 2 : fit.loadings
#   Model 3 : fit.intercepts
#   Model 4 : fit.means
#   
#   Scaled Chi Square Difference Test (method = "satorra.bentler.2001")
#   
#                 Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
#   fit.configural  0 13234 13383  0.000                              
#   fit.loadings    6 13234 13350 12.193     4.9550       6     0.5496
#   fit.intercepts 10 13230 13324 15.944     3.5465       4     0.4708
#   fit.means      12 13228 13311 18.350     2.5029       2     0.2861
#   
#   
#   Fit measures:
#     
#     cfi.scaled rmsea.scaled cfi.scaled.delta rmsea.scaled.delta
#   fit.configural          1            0               NA                 NA
#   fit.loadings            1            0                0                  0
#   fit.intercepts          1            0                0                  0
#   fit.means               1            0                0                  0
#   
#   Warning message:
#     The measurementInvariance function is deprecated, and it will cease to be included in future versions of semTools. See help('semTools-deprecated) for details. 
# > 

summary(auditvfit$fit.configural, standardized = TRUE, fit.measures = TRUE)
# > summary(auditvfit$fit.configural, standardized = TRUE, fit.measures = TRUE)
# lavaan 0.6-6 ended normally after 67 iterations
# 
# Estimator                                         ML
# Optimization method                           NLMINB
# Number of free parameters                         30
# 
# Number of observations per group:               Used       Total
# 1                                             1049        1107
# 7                                              605         635
# 6                                              202         210
# 
# Model Test User Model:
#   Standard      Robust
# Test Statistic                                 8.450       9.426
# Degrees of freedom                                 6           6
# P-value (Chi-square)                           0.207       0.151
# Scaling correction factor                                  0.896
# 
# Test statistic for each group:
#   1                                            2.771       3.092
# 7                                            4.361       4.865
# 6                                            1.317       1.469
# 
# Model Test Baseline Model:
#   
#   Test statistic                              2225.422    1578.950
# Degrees of freedom                                18          18
# P-value                                        0.000       0.000
# Scaling correction factor                                  1.409
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.999       0.998
# Tucker-Lewis Index (TLI)                       0.997       0.993
# 
# Robust Comparative Fit Index (CFI)                         0.999
# Robust Tucker-Lewis Index (TLI)                            0.996
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -6589.045   -6589.045
# Scaling correction factor                                  1.966
# for the MLR correction                                      
# Loglikelihood unrestricted model (H1)      -6584.820   -6584.820
# Scaling correction factor                                  1.787
# for the MLR correction                                      
# 
# Akaike (AIC)                               13238.089   13238.089
# Bayesian (BIC)                             13403.874   13403.874
# Sample-size adjusted Bayesian (BIC)        13308.565   13308.565
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.026       0.030
# 90 Percent confidence interval - lower         0.000       0.000
# 90 Percent confidence interval - upper         0.062       0.067
# P-value RMSEA <= 0.05                          0.840       0.772
# 
# Robust RMSEA                                               0.029
# 90 Percent confidence interval - lower                     0.000
# 90 Percent confidence interval - upper                     0.062
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.012       0.012
# 
# Parameter Estimates:
#   
#   Error in if (attr(x, "se") == "robust.huber.white" && attr(x, "information.meat") !=  : 
#                missing value where TRUE/FALSE needed
#                > 


 
