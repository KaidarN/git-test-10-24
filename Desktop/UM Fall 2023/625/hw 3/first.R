####################################################################################################
#                                We Mix Code for Domestic PISA Analysis                            #
####################################################################################################
rm(list=ls())
setwd("/Users/user/Desktop/Current Projects/PISA analysis")

# Install packages
if (!require("pacman")){install.packages("pacman", dependencies = TRUE)
  library(tidyverse)
}
pacman::p_load(haven, car, EdSurvey, misty, WeMix)   

# Download and merge 2018 student and school 2018 data
dir()
data.std <- haven::read_sav("CY07_MSU_STU_QQQ.sav")
data.sch <- haven::read_sav("CY07_MSU_SCH_QQQ.sav")  
colnames(data.std)
colnames(data.sch)
data <- merge(data.std, data.sch, all = T, by = "CNTSCHID")
colnames(data)

# Subset USA!
data <- data[data$CNT.x == "USA", ]
dim(data)                                 # 4838 students
length(unique(data$CNTSCHID))             # 164 schools

# Deisgn-based variables for MLM analysis
colnames(data)
data$CNTSCHID                             # Random effect
data$PV1MATH                              # Dependent variable (just 1 of 10 PVs)

data$W_FSTUWT                             # Base student weight

data$W_FSTURWT1                           # First replicate weight

data$W_FSTURWT80                          # 80th student replicate weight

data$W_FSTUWT_SCH_SUM                     # Base school weight

# Student level independent variable
attributes(data$ST004D01T)                # Gender, 1 = female, 2 = male
attributes(data$ESCS)                     # Student ses (-4.1 to 3.3)
attributes(data$ST212Q02HA)               # Perceived teacher help in subject: 1 = never, 4 = every lesson

# School level indepedent variable
attributes(data$SC017Q02NA)               # Instruction hindered by poorly qualified teaching staff: 1 = not at all, 4 = a lot
 
# Conditional weights                                                                               # Based on WeMix package vignettes, p. 16: https://cran.r-project.org/web/packages/WeMix/index.html
pwt2 <- data$W_FSTUWT_SCH_SUM
pwt1 <- data$W_FSTUWT / data$W_FSTUWT_SCH_SUM
data <- cbind.data.frame(data, pwt1, pwt2)

# Data prep for variables
summary(data$ST004D01T)                        # 0 NAs for gender
summary(data$ESCS)                             # 71  NAs for ses
summary(data$ST212Q02HA)                       # 133 NAs for perceived teacher help

summary(data$SC017Q02NA)                       # 471 NAs effect of poor qualified teachers (school level)

# Remove data with missing values
IVs <- which(colnames(data) %in% c("ST004D01T", "ESCS", "ST212Q02HA", "SC017Q02NA"))
print(IVs)
retain.logical <- complete.cases(data[,IVs])
dim(data)                                      # 4838
data <- data[retain.logical,]
dim(data)                                      # 4224

# Remove schools with fewer than 10 students
min.school.size <- 10                                                                               # paramter
sort(table(data$CNTSCHID), decreasing = T)
sort(table(data$CNTSCHID), decreasing = T) < min.school.size
names(sort(table(data$CNTSCHID), decreasing = T) < min.school.size)
ommit.schools <- names(sort(table(data$CNTSCHID), decreasing = T) < min.school.size)[sort(table(data$CNTSCHID), decreasing = T) < min.school.size]

data <- data[!data$CNTSCHID %in% ommit.schools,]
dim(data)                                      # 4204

# Check for within school variance for within-school variables (PVs assumed to vary within schools)

# Gender ST004D01T
sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T)
sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T) == 0
names(sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T))
names(sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T))[sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T) == 0]
ommit.schools <- names(sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T))[sort(tapply(data$ST004D01T, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T) == 0]

data <- data[!data$CNTSCHID %in% ommit.schools,]
dim(data)                                      # 4113

# SES ESCS
sort(tapply(data$ESCS, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T)                      # No issue
sort(tapply(data$ST212Q02HA, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T)                # No issue

# Note. For automation to work, amalyst should:
# 1. Upload student and school files into app for merge to occur in app tab 1
# 2. Separate with-school variable dataset from merged file
# 3. Upload (a) within-school and (b) between-school variable-PLUS datasets into app tab 2 to perform (1) missing value analysis, and (2) within-school variance analysis to putput refine data
# 4. In tab 3, analyst upplaods output data from tab 2 and sets paramters for MLM.

#################################################################################################### 
# Ensure school-level variable (teacher shortage) varies by school
sort(tapply(data$SC017Q02NA, data$CNTSCHID, FUN = function(x)sd(x)), decreasing = T)                # yes

# Run null model
misty::multilevel.icc(data$PV1MATH, data$CNTSCHID)                                                  # 0.203246

# Run null model to extract variance components... (do later)
null <-  WeMix::mix(PV1MATH ~ 1 + (1|CNTSCHID), 
                    data = data, 
                    weights = c("W_FSTUWT", "W_SCHGRNRABWT"))
summary(null)                                                                                       # 1937/ (1937 + 6492)  ICC = 0.2298019

# Check null model without weights
null.b <- lme4::lmer(PV1MATH ~ 1 + (1|CNTSCHID), 
                     data = data)
summary(null.b)                                                                                     # CNTSCHID variance = 1696; Residual = 6649     
1696/(1696 + 6649)                                                                                  # 0.2032355

# Run mixed model with weights
m1 <- WeMix::mix(PV1MATH ~ ST004D01T + ESCS + ST212Q02HA + SC017Q02NA + (1|CNTSCHID), 
                 data = data, 
                 weights = c("W_FSTUWT", "W_SCHGRNRABWT"))

options(scipen = 9999)
summary(m1)                                                                                         # CNTSCHID variance = 986.7; Residual = 5895.5

# Fixed Effects:
#             Estimate Std. Error t value
# (Intercept)  444.031     12.476  35.592
# ST004D01T      9.529      2.586   3.685
# ESCS          24.541      1.676  14.646
# ST212Q02HA    11.954      1.609   7.431
# SC017Q02NA   -11.211      6.968  -1.609
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Fixed Effects:
#                  Estimate Std. Error t value
#               Within-Schools               
# (Intercept)       444.031     12.476  35.592
# Gender(M)           9.529      2.586   3.685
# SES                24.541      1.676  14.646
# teacher support    11.954      1.609   7.431
#               Between-Schools
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# teacher short     -11.211      6.968  -1.609

# Report reduction in variance explained at each level... (do later)

#################################################################################################### 
# Questions... Kaidar...
# mu_coef? standard errors? DoF?



