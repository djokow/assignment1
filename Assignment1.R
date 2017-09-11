# Q1 ----

library(dplyr)

setwd("D:\\temp")
d <- read.csv("35478-0001-Data.csv")

d <- d %>% select(AGE, DEGREE, EDUC, HAPPY, HOMPOP, INCOME, MARITAL, RACE, RINCOME, SEX)

nrow(d)
# [1] 4820

# Ans: There are 4820 rows



# Q2 ----

# AGE Missing Data
# 98 Don't Know
# 99 No Answer

length(d$AGE[d$AGE == 98 | d$AGE == 99])
# [1] 51

d$AGE[d$AGE == 98 | d$AGE == 99]
#  [1] 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99
# [25] 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99
# [49] 99 99 99


# DEGREE Missing Data
# 7 Inapplicable
# 8 Don't Know
# 9 No Answer

length(d$DEGREE[d$DEGREE == 7 | d$DEGREE == 8 | d$DEGREE == 9])
# [1] 0
# No missing data for DEGREE


# EDUC Missing Data
# 97 Inapplicable
# 98 Don't know
# 99 No answer

length(d$EDUC[d$EDUC == 97 | d$EDUC == 98 | d$EDUC == 99])
# [1] 6

d$EDUC[d$EDUC == 97 | d$EDUC == 98 | d$EDUC == 99]
# [1] 98 99 98 98 99 98


# HAPPY Missing Data
# 0 Inapplicable
# 8 Don't know
# 9 No answer

length(d$HAPPY[d$HAPPY == 0 | d$HAPPY == 8 | d$HAPPY == 9])
# [1] 14

d$HAPPY[d$HAPPY == 0 | d$HAPPY == 8 | d$HAPPY == 9]
# [1] 8 8 8 9 9 8 8 9 9 8 8 9 9 8


# HOMPOP Missing Data
# 98 Don't know
# 99 No answer

length(d$HOMPOP[d$HOMPOP == 98 | d$HOMPOP == 99])
# [1] 1

d$HOMPOP[d$HOMPOP == 98 | d$HOMPOP == 99]
# [1] 99


# INCOME Missing Data
# 0 Inapplicable
# 13 Refused
# 98 Don't know
# 99 No answer

length(d$INCOME[d$INCOME == 0 | d$INCOME == 13 | d$INCOME == 98 | d$INCOME == 99])
# [1] 446


# MARITAL Missing Data
# 9 No answer

length(d$MARITAL[d$MARITAL == 9])
# [1] 2


# RACE Missing Data
# 0 Inapplicable

length(d$RACE[d$RACE == 0])
# [1] 0
# No missing data for RACE


# RINCOME Missing Data
# 0 Inapplicable
# 13 Refused
# 98 Don't know
# 99 No answer

length(d$RINCOME[d$RINCOME == 0 | d$RINCOME == 13 | d$RINCOME == 98 | d$RINCOME == 99])
# [1] 1988


# SEX Missing Data
# No missing data for SEX



# Q3 ----

d$AGE[d$AGE == 98 | d$AGE == 99] <- NA
d$EDUC[d$EDUC == 97 | d$EDUC == 98 | d$EDUC == 99] <- NA
d$HAPPY[d$HAPPY == 0 | d$HAPPY == 8 | d$HAPPY == 9] <- NA
d$HOMPOP[d$HOMPOP == 98 | d$HOMPOP == 99] <- NA
d$INCOME[d$INCOME == 0 | d$INCOME == 13 | d$INCOME == 98 | d$INCOME == 99] <- NA
d$MARITAL[d$MARITAL == 9] <- NA
d$RINCOME[d$RINCOME == 0 | d$RINCOME == 13 | d$RINCOME == 98 | d$RINCOME == 99] <- NA

d <- na.omit(d)

nrow(d)
# [1] 2749

# Ans: 2749 rows of data left



# Q4 ----

incomeInDollar <- c(0, 1000, 3000, 4000, 5000, 6000, 7000, 8000,
                      10000, 15000, 20000, 25000)
PERCAPITA <- incomeInDollar[d$INCOME] / d$HOMPOP

# if required, the variable could be added to the data frame:
d$PERCAPITA <- incomeInDollar[d$INCOME] / d$HOMPOP



# Q5 ----

summary(PERCAPITA)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0    6250    8333   10960   12500   25000 

# PERCAPITA has mean of $10960 and median of $8333,
# with minimum value of $0 and max value of $25000.
# 25% of respondents have PERCAPITA $6250 or less.
# 25% of respondents have PERCAPITA $12500 or more.

sd(PERCAPITA)
# [1] 6611.341
# Standard deviation of PERCAPITA is 6611.341



# Q6 ----

sum(d$DEGREE >= 3)
# [1] 970
# There are 970 respondents have attained at least a Bachelor degree or higher

sum(d$DEGREE >= 3) / length(d$DEGREE)
# [1] 0.3528556
# Ans: About 35% of respondents have attained at least a Bachelor degree or higher



# Q7 ----

d$AGE[d$AGE >= 56] <- "Old"
d$AGE[d$AGE >= 36 & d$AGE <= 55] <- "Middle"
d$AGE[d$AGE >= 18 & d$AGE <= 35] <- "Young"



# Q8 ----

table(d$AGE, d$HAPPY)
#          1   2   3
# Middle 360 775 150
# Old    201 376  65
# Young  257 488  77

prop.table(table(d$AGE, d$HAPPY))
#                 1          2          3
# Middle 0.13015184 0.28018800 0.05422993
# Old    0.07339118 0.14027477 0.02458424
# Young  0.09291396 0.17642805 0.02783803


prop.table(table(d$AGE, d$HAPPY), 1)
#                 1          2          3
# Middle 0.28015564 0.60311284 0.11673152
# Old    0.30804249 0.58877086 0.10318665
# Young  0.31265207 0.59367397 0.09367397

# Within each age group,
# the proportion of very happy respondents within young respondents (about 31%)
# is greater than the proportion of very happy respondents in other age groups.

# And if we combine very happy and pretty happy as "Happy", and not too happy as "Not Happy":

prop.table(table(d$AGE, c("Happy","Happy","Not Happy")[d$HAPPY]), 1)
#             Happy  Not Happy
# Middle 0.88326848 0.11673152
# Old    0.89875389 0.10124611
# Young  0.90632603 0.09367397

# The proportion of young respondent who are happy (90.6%)
# is greater that the proportion of happy respondents from other age group:
# old respondent who are happy (89.9%)
# and middle age who are happy (88.3%).



# Q9 ----
# (Answer and plots are in Word document)

d$RINCOME_DOLLAR = incomeInDollar[d$RINCOME]

meanIncomeByEduc <- aggregate(RINCOME_DOLLAR ~ EDUC, d, mean)
meanIncomeByEduc
#    EDUC RINCOME_DOLLAR
# 1     0       25000.00
# 2     1       14500.00
# 3     2       12500.00
# 4     3       12500.00
# 5     4       14600.00
# 6     5       23333.33
# 7     6       14100.00
# 8     7       15500.00
# 9     8       12552.63
# 10    9       12408.16
# 11   10       15285.71
# 12   11       14611.65
# 13   12       17975.57
# 14   13       18449.58
# 15   14       19401.52
# 16   15       17781.25
# 17   16       21538.32
# 18   17       20900.83
# 19   18       22582.35
# 20   19       22337.84
# 21   20       22856.00

plot(meanIncomeByEduc, type="l")

table(d$EDUC)
# 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
# 1   2   2   2   5   3  30   6  38  49  42 103 696 238 396 128 535 121 170  74 125 

library(zoo)
plot(rollmean(meanIncomeByEduc %>% filter(EDUC > 5), 5), type="l")


# Other plots:

plot(d$EDUC, d$RINCOME_DOLLAR)
abline(lm(RINCOME_DOLLAR ~ EDUC, data=d))


ggplot(d, aes(x = RINCOME_DOLLAR)) +
  geom_histogram(binwidth=5000) +
  facet_wrap(~ EDUC, scales="free_y")


boxplot(d$RINCOME_DOLLAR ~ d$EDUC,
        xlab="Year of Education",
        ylab="Income")



# Q10 ----

meanIncomeByRace <- aggregate(RINCOME_DOLLAR ~ RACE, d, mean)
meanIncomeByRace
#   RACE RINCOME_DOLLAR
# 1    1       19571.23
# 2    2       18304.02
# 3    3       18068.55


byRace <- d %>% select(RACE, RINCOME_DOLLAR) %>% group_by(RACE)
summarise(byRace,
          count = n(),
          meanIncome = mean(RINCOME_DOLLAR))


t.test(d$RINCOME_DOLLAR[d$RACE == 1], d$RINCOME_DOLLAR[d$RACE == 2])

#         Welch Two Sample t-test

# data:  d$RINCOME_DOLLAR[d$RACE == 1] and d$RINCOME_DOLLAR[d$RACE == 2]
# t = 2.7506, df = 537.13, p-value = 0.00615
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   362.1953 2172.2173
# sample estimates:
# mean of x mean of y 
#  19571.23  18304.02 

t.test(d$RINCOME_DOLLAR[d$RACE == 1], d$RINCOME_DOLLAR[d$RACE == 3])

#         Welch Two Sample t-test
# 
# data:  d$RINCOME_DOLLAR[d$RACE == 1] and d$RINCOME_DOLLAR[d$RACE == 3]
# t = 2.6331, df = 299.79, p-value = 0.0089
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   379.6056 2625.7504
# sample estimates:
# mean of x mean of y 
#  19571.23  18068.55 

# Ans: White race (1) tends to earn more than the other races.
# 95% confidence interval of the above T tests does not contain 0,
# therefore we reject the null hypothesis and accept the alternate hypothesis:
# True difference in means is not equal to 0
# So, the difference is statistically significant
