# clearing memory
rm(list=ls())
gc()

# set seed
set.seed(666)
options(scipen = 999)

# load packages
library(ggplot2)
library(reshape2)
library(COINr)

# We begin by loading the synthetic dataset and inspecting its structure. 
# We want to create a composite indicator which is composed by 3 pillars: 
# **economic**, **social** and **environmental**. 
# Each pillar is measured by two indicators as in the following table:

# | Pillar      | Indicator 1 | Indicator 2 |
# |-------------|-------------|-------------|
# | Economic    | PubDebt     | GDP         |
# | Social      | TertGrad    | FreePress   |
# | Environmental| CO2        | Renew       |

# Metadata have stricht rules. Parent and iCode must match. 
# Also, the entries in iMeta must include all columns in iData, 
# except the three “special” column names: uCode, uName, and Time.

names(ASEM_iData_p)
# select the variables we want
idata <- ASEM_iData_p[,c("uCode","Time","PubDebt", "GDP", "TertGrad", "FreePress", "CO2", "Renew")] 

# we create some missing data
idata$GDP[sample(1:255,10)] <- NA
idata$FreePress[sample(1:255,10)] <- NA
idata$Renew[sample(1:255,10)] <- NA
names(idata)
summary(idata)
check_iData(idata)

# build metadata
imeta <- data.frame(
  Level=c(1,1,1,1,1,1,2,2,2,3),
  iCode=c("PubDebt", "GDP", "TertGrad", "FreePress", "CO2", "Renew","Economic","Social","Environmental","Index"),
  iName=c("Public Debt", "Gross Domestic Product", "Tertiary Graduates", "Free Press", "CO2 Emissions", "Renewable Energy",
  "Economic Pillar","Social Pillar", "Environmental Pillar","Wellbeing Index"),
  Parent=c("Economic", "Economic", "Social", "Social", "Environmental", "Environmental","Index","Index","Index",NA),
  Direction=c(-1, 1, 1, 1, -1, 1,1,1,1,1),
  Weight=c(1, 1, 1, 1, 1, 1,1,1,1,1),
  Type=c("Indicator", "Indicator", "Indicator", "Indicator", "Indicator", "Indicator","Aggregate","Aggregate","Aggregate","Aggregate")
)
check_iMeta(imeta)

# check for missing data
idata_scr <- Screen(idata, unit_screen = "byNA", dat_thresh = 0.60)
idata_scr$DataSummary[idata_scr$DataSummary$N_miss_or_zero>0,]

# we impute missing data using the previous year available
# One of "latest" "constant", "linear" or "linear-constant". 
idata_imp <- impute_panel(idata, time_col="Time", unit_col="uCode",imp_type = "latest")

# Check for outliers. 
# The skewness thresholds are between 0.5 and 1 moderate, above 1 high skewness. 
# An absolute excess kurtosis value less than 2 or 3, indicating acceptable normality.

apply(idata[,-c(1,2)],2,function(x) skew(x,na.rm = T))
apply(idata[,-c(1,2)],2,function(x) kurt(x,na.rm = T))
# as expected public debt and gdp must be treated
apply(idata[,-c(1,2)],2,function(x) check_SkewKurt(x,na.rm = T))
# the check function would treat only gdp, let's see the distributions
idata_melt <- melt(idata,id=c("uCode","Time"))
ggplot(idata_melt, aes(x = value, fill =as.factor(Time), group=as.factor(Time))) +
  geom_histogram(alpha = 0.8, position = "identity", bins = 10) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Value", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")
# I see more issues

# let's windsorise
l_treat <- Treat(idata[,-c(1,2)], f1 = "winsorise", f1_para = list(winmax = 2),
                 f_pass = "check_SkewKurt")
# check visually
l_treat_melt <- melt( l_treat$x_treat )  
ggplot(l_treat_melt, aes(x = value)) +
  geom_histogram(alpha = 0.8, position = "identity", bins = 10) +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Value", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")
# I still see issues


# build a new coin using example data
coin <- new_coin(iData = idata,
                 iMeta = imeta,
                 split_to = "all", # only for panel data
                 level_names = c("Indicator", "Pillar", "Index"))

coin