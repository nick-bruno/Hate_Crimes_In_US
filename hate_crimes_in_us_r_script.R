## Final STAT 3080 Project ##
## By: Nick Bruno ##
library(car)
library(leaps)
library(lmtest)
library(boot)
library(ggplot2)

# Hate crimes
crime <- read.csv("hate_crimes.csv")
  # Reads in the dataset I will analyze

# Before I start I will take out Washington DC, for reasons mentioned in the paper
crime = crime[-9,]
  # ONLY HIT ONCE OR ELSE IT MESSES UP THE OTHER RESULTS

## Comparing hate crimes in 2016 compared to 2010 - 2015 ##

## Post-election crime rate data
summary(crime$hate_crimes_per_100k_splc)
  # Takes a look at the statistics of the hate crime rate in 2016 (numbers per 100,000)
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
  # 0.06745 0.14220 0.22580 0.27760 0.34680 0.83280
0.2776 * 10
# Number of hate crime victims per 1 million US citizens
# [1] 2.776
2.776 * 323.1
# The US population in 2016 was 323.1 million people, so multiplying that by the number of hate crime
# gives around the number of hate crime victims in the US during the post-election time frame
# [1] 896.9256

# Confidence Interval
# 2016 Hate crime rate confidence interval
crimemean <- mean(crime$hate_crimes_per_100k_splc, na.rm=T)
# [1] 0.2776102
crimesd <- sd(crime$hate_crimes_per_100k_splc, na.rm=T)
# [1] 0.1777228
length(crime$hate_crimes_per_100k_splc, na.rm=T)
# [1] 50
summary(crime$hate_crimes_per_100k_splc)
# 4 null values, so n = 50-4
n <- length(which(crime$hate_crimes_per_100k_splc > 0))
# Because all of the values are greater than zero, this gives the total number of data points
# within this variable, taking into account the null values. In this case, there are 
# 4 null values out of 50 states, so n = 46
# [1] 46
tstat <- qt(0.975, df=n-1)
# [1] 2.014103
error <- tstat*crimesd/sqrt(n)
  # [1] 0.05104793
lower <- crimemean-error
  # [1] 0.2265622
upper <- crimemean+error
  # [1] 0.3286581
# CI: (0.224833, 0.3303873)

# 2010 - 2015 hate crime rate data
summary(crime$avg_hatecrimes_per_100k_fbi)
  # Same as above. Divided by 6 because it was the sum of the averages over 6 years (2010-2015)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
  # 0.2669  1.2830  1.9370  2.1920  3.1360  4.8020          
hatepermil <- 2.192 * 10
  # Number of hate crime victims per 1 million US citizens
  # [1] 21.92
# I will now find the average number of hate crimes per year from 2010-2015. I will do this by first
# finding the US population per year during that time frame.
US15 <- 320.9
US14 <- 318.6
US13 <- 316.2
US12 <- 314
US11 <- 311.7
US10 <- 309.3
avgpop10to15 <- (US15 + US14 + US13 + US12 + US11 + US10)/6
  # Average US population from 2010-2015
  # [1] 315.1167
tot1015 <- hatepermil*avgpop10to15
  # [1] 6907.357
  # This number shows the average numbe of victims who fell to a hate crime per million per year from 2010-2015
per1000 <- 0.2776 * 36.5
  # [1] 10.1324
permilnew <- per1000*10
  # [1] 101.324
US16 <- 323.1
tot16 <- US16*permilnew
  # [1] 32737.78
tot16/tot1015
  # [1] 4.739553

## Taking a deeper look into the variables of the data set ##

# Find median income per household in US across states
median(crime$median_household_income)
  # [1] 54613
# Highest and lowest crime rates
sort(crime$avg_hatecrimes_per_100k_fbi)
  # Highest:
    # Oregon = 0.83284961
    # Washington = 0.67748765
  # Lowest:
    # Mississippi = 0.06744680
    # Arkansas = 0.06906077
# Median Household Incomes of these four states
# Oregon
crime[37,2]
  # [1] 58875
# Washington
crime[47,2]
  # [1] 59068
# Mississippi
crime[24,2]
  # [1] 35521
# Arkansas
crime[4,2]
  # [1] 44922

## Histograms of hate crime rates ##

# Post-election hate crime histogram
crimeratehist <- ggplot(crime, aes(x=crime$hate_crimes_per_100k_splc))
crimeratehist + geom_histogram(binwidth=0.075, fill='white', colour='black') + labs(title='Hate Crime Rate Histogram', x = 'Hate Crime Rate')
  # post-election hate crime rate histogram

# 2010-2015 hate crime histogram
crimeratehist2010 <- ggplot(crime, aes(x=crime$avg_hatecrimes_per_100k_fbi))
crimeratehist2010 + geom_histogram(binwidth=0.5, fill='white', colour='black') + labs(title='2010 - 2015 Hate Crime Rate Histogram', x = 'Hate Crime Rate')
  # 2010 - 2015 hate crime rate histogram

## Scatterplots of each column in the data set ##
# Household Income Scatterplot
ggplot(crime, aes(x=crime$median_household_income, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title="Relationship Between Household Income and Hate Crime Rate",
       x="Median Household Income", y="Hate Crime Rate") 
 # somewhat strong positive correlation

# Gini Index plot
ggplot(crime, aes(x=crime$gini_index, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title="Relationship Between Economic Inequality and Hate Crime Rate", x= "Gini Index", y="Hate Crime Rate")
  # Very weak negative correlation

# Trump plot
ggplot(crime, aes(x=crime$share_voters_voted_trump, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between Trump Voter Support and Hate Crime Rate',                                                                                                                            x='Share of Trump Voters', y='Hate Crime Rate')
  # substantial negative correlation

# Unemployment plot  
ggplot(crime, aes(x=crime$share_unemployed_seasonal, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between Unemployment and Hate Crime Rate', x='Unemployemnt', y='Hate Crime Rate')
  # slight negative correlation

# Metro Area plot
ggplot(crime, aes(x=crime$share_population_in_metro_areas, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between Metro Area Habitation and Hate Crime Rate', x='Metro Habitation', y='Hate Crime Rate')
  # almost absolutely no correlation

# Education plot
ggplot(crime, aes(x=crime$share_population_with_high_school_degree, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between Education and Hate Crime Rate', x = 'High School Degree Owners', y='Hate Crime Rate')
  # strange positive correlation (possible outlier though)

# Noncitizens plot
ggplot(crime, aes(x=crime$share_non_citizen, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between Share of Population that are not US Citizens and Hate Crime Rate', x='Non US Citizens', y='Hate Crime Rate')
  # very small positive correlation

# White Poverty Plot
ggplot(crime, aes(x=crime$share_white_poverty, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between White Poverty and Hate Crime Rate', x='White Residents Living in Poverty', y='Hate Crime Rate')
  # slightly negative correlation (outlier pulling the line in a negative direction)

# Nonwhite population plot
ggplot(crime, aes(x=crime$share_non_white, y=crime$hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method=lm) + labs(title='Relationship Between the Non-White Population and Hate Crime Rate', x='Non-White Population', y='Hate Crime Rate')
  # pretty significant negative correlation

## White poverty sampling
histpov <- ggplot(crime, aes(x=crime$share_white_poverty))
histpov + geom_histogram(binwidth = .01, fill='white', colour='black') + labs(title='Share of White Poverty Histogram', x = 'White Poverty Rate')
# will do normal sampling (most normal)

## Trump histogram
histpov <- ggplot(crime, aes(x=crime$share_voters_voted_trump))
histpov + geom_histogram(bins = 10, fill='white', colour='black') + labs(title='Share of Trump Voters Histogram', x = 'Share of Voters for Trump')
# almost normal (kind of trimodal)

## White poverty Monte Carlo simulation sampling
meanpov <- mean(crime$share_white_poverty)
# [1] 0.0928
sdpov <- sd(crime$share_white_poverty)
# [1] 0.02382247
K <- 10000
# number of simulations that will be run
a <- 4
## Draw 10,000 samples of size 5 from the population distribution
set.seed(21)
samps <- replicate(K, rnorm(5, mean=meanpov, sd=sdpov))
mean(samps)
# [1] 0.09297471
sd(samps)
# [1] 0.02392001
## Determine the sample mean from each random sample
means5 <- apply(samps,2,mean)
## QQ Plot of Monte Carolo
mean_data <- data.frame(X=means5)
y <- quantile(mean_data$X, c(0.25, 0.75)) 
#    25%        75% 
# 0.08565365 0.10020053 
x <- qnorm(c(0.25, 0.75))
# [1] -0.6744898  0.6744898
slope <- diff(y)/diff(x) 
#   75% 
# 0.01078362 
int <- y[1] - slope*x[1] 
# 25% 
# 0.09292709 
# Actual QQ Plot
ggplot(mean_data, aes(sample = X)) + stat_qq() + 
  geom_abline(intercept=int, slope=slope) + labs(title="n=5") + labs(title='White Poverty QQ Plot', y='Share of White Poverty')


## Correlation tests testing the correlation between each variable and the post-election hate crime
# rate
cor.test(hatecrimes, trump, method="pearson", alternative="two.sided")
# p-value = 0.003873
cor.test(hatecrimes, education, method="pearson", alternative="two.sided")
# p-value = 0.003532
cor.test(hatecrimes, metro, method="pearson", alternative="two.sided")
# p-value = 0.873
cor.test(hatecrimes, householdincome, method="pearson", alternative="two.sided")
# p-value = 0.0532
cor.test(hatecrimes, gini_index, method="pearson", alternative="two.sided")
# p-value = 0.4531
cor.test(hatecrimes, unemployment, method="pearson", alternative="two.sided")
# p-value = 0.3837
cor.test(hatecrimes, nonuscitizen, method="pearson", alternative="two.sided")
# p-value = 0.6459
cor.test(hatecrimes, poverty, method="pearson", alternative="two.sided")
# p-value = 0.5134
cor.test(hatecrimes, nonwhite, method="pearson", alternative="two.sided")
# p-value = 0.07261

## Linear Regressions ##
# First will set variables to shorten the plot equations
householdincome <- crime$median_household_income
hatecrimes <- crime$hate_crimes_per_100k_splc
gini_index <- crime$gini_index

# Model 1 (scrapped from report)
model1 <- lm(hatecrimes ~ median_household_income + gini_index, data=crime)
summary(model1)
  # Low r-squared (0.08284)
  # Neither variables are significant
  # Household income has a positive coefficient, gini index is a negative coefficient
  # Gini index small, but substantially larger than the household income
  # Residual info:
    # Residuals:
    # Min       1Q   Median       3Q      Max 
    # -0.25516 -0.10431 -0.02547  0.07459  0.53033
  # Coefficients:
  #                           Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)              9.195e-02  7.797e-01   0.118   0.9067  
  # median_household_income  5.433e-06  3.000e-06   1.811   0.0771 .
  # gini_index              -2.434e-01  1.567e+00  -0.155   0.8773  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1741 on 43 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.08284,	Adjusted R-squared:  0.04018 
  # F-statistic: 1.942 on 2 and 43 DF,  p-value: 0.1558
model1res <- residuals(model1)
  # Creates a list of residual points
plot(model1res) 
  # residuals look good

# Model 1 (actual)
trump <- crime$share_voters_voted_trump
model2 <- lm(hatecrimes ~ median_household_income + gini_index + trump, data=crime)
summary(model2)
  # R-squared still low, but increased significantly from Model 1, is now 0.2067
  # All coefficients are negative now
  # Only the Trump voter share variable is significant
  # F-statistic is significant, meaning that at least one of the variables is important
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.23775 -0.11698 -0.02146  0.07808  0.47173 
  # Coefficients:
  #                         Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)              1.848e+00  1.005e+00   1.840   0.0728 .
  # median_household_income -2.118e-06  4.083e-06  -0.519   0.6066  
  # gini_index              -2.132e+00  1.649e+00  -1.293   0.2032  
  # trump                   -9.888e-01  3.862e-01  -2.560   0.0141 *
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1639 on 42 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.2067,	Adjusted R-squared:   0.15 
  # F-statistic: 3.647 on 3 and 42 DF,  p-value: 0.02002
resid2 <- residuals(model2)
  # creates a list of residuals
plot(resid2)
  # Residuals look good
vif(model2)
  # tests for multicollinearity
  # median_household_income              gini_index                   trump 
  # 2.330944                                1.392941                2.122251 
anova(model2)
  # Response: hatecrimes
  # Df  Sum Sq  Mean Sq F value  Pr(>F)  
  # median_household_income  1 0.11701 0.117010  4.3583 0.04293 *
    # gini_index               1 0.00073 0.000731  0.0272 0.86969  
    # trump                    1 0.17599 0.175989  6.5551 0.01414 *
    # Residuals               42 1.12761 0.026848  

# Model 3 (but second one mentioned in the paper)
unemployment <- crime$share_unemployed_seasonal
metro <- crime$share_population_in_metro_areas
education <- crime$share_population_with_high_school_degree
nonuscitizen <- crime$share_non_citizen
poverty <- crime$share_white_poverty
nonwhite <- crime$share_non_white
model3 <- lm(hatecrimes ~ unemployment + metro + education + nonuscitizen + poverty + nonwhite, data=crime)
summary(model3)
  # R-squared higher, but still low (0.2747) (adjusted R-squared = 0.1571)
  # F-statistic is statistically insignificant
  # Education is the only variable that is statistically significant
  # Nonuscitizen is almost significant, so I do not want to throw that one out
  # metro is incredibly insignificant
  # unemployment, education, and nonuscitizen are all positive coefficients
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.31247 -0.10031  0.00521  0.09004  0.35086 
  # Coefficients:
    #             Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)  -2.94891    1.38283  -2.133   0.0397 *
  # unemployment  4.29035    3.25890   1.317   0.1961  
  # metro         0.06149    0.23743   0.259   0.7971  
  # education     3.19754    1.33301   2.399   0.0216 *
  # nonuscitizen  2.54654    1.45182   1.754   0.0877 .
  # poverty       1.80619    1.68995   1.069   0.2921  
  # nonwhite     -0.37105    0.33779  -1.098   0.2791  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  # Residual standard error: 0.1573 on 37 degrees of freedom
  # (6 observations deleted due to missingness)
  # Multiple R-squared:  0.2747,	Adjusted R-squared:  0.1571 
  # F-statistic: 2.336 on 6 and 37 DF,  p-value: 0.0517
resid3 <- residuals(model3)
plot(resid3)
  # Residuals more spread out, but still look random and good
vif(model3)
  # unemployment        metro    education nonuscitizen      poverty     nonwhite 
  # 1.807864     2.519537     3.367868     3.395102     2.782949     4.024919 

# Model 4 (third mentioned in  the paper)
model4 <- lm(hatecrimes ~ unemployment + education + nonuscitizen, data=crime)
summary(model4)
  # Adjusted R-squared even lower than model 3 (0.1293)
  # All coefficients are positive
  # Education is the only significant variable
  # Maybe some multicollinearity between unemployment and nonuscitizen?
  # F-statistic is significant
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.28957 -0.09306  0.00074  0.08488  0.43912 
  # Coefficients:
  #               Estimate Std. Error t value Pr(>|t|)   
  # (Intercept)   -2.2927     0.9036  -2.537  0.01517 * 
  # unemployment   3.1816     3.0421   1.046  0.30191   
  # education      2.7163     0.9244   2.938  0.00545 **
  # nonuscitizen   0.9852     0.8365   1.178  0.24586   
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1599 on 40 degrees of freedom
# (6 observations deleted due to missingness)
# Multiple R-squared:   0.19,	Adjusted R-squared:  0.1293 
# F-statistic: 3.128 on 3 and 40 DF,  p-value: 0.0362
resid4 <- residuals(model4)
plot(resid4)
  # residuals may be a little heterodastic
vif(model4)
  # unemployment    education nonuscitizen 
  # 1.524902     1.567763     1.091009

# Model 5 (fourth mentioned in the paper)
model5 <- lm(hatecrimes ~ trump + education, data=crime)
summary(model5)
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.25824 -0.10538 -0.01873  0.05752  0.46125 
  # Coefficients:
  #             Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)  -0.8895     0.6690  -1.330   0.1907  
  # trump        -0.6168     0.2634  -2.342   0.0239 *
  # education     1.6992     0.7136   2.381   0.0218 *
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1553 on 43 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.2706,	Adjusted R-squared:  0.2367 
  # F-statistic: 7.978 on 2 and 43 DF,  p-value: 0.00113
vif(model5)
  # trump education 
  # 1.099679  1.099679 
resid5 <- residuals(model5)
plot(resid5)

# Model 6 (not mentioned in paper)
trump2 <- trump^2 
education2 <- education^2
model6 <- lm(hatecrimes ~ trump + education + trump2, data=crime)
summary(model6)
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.24815 -0.08992 -0.02347  0.07548  0.46124 
  # Coefficients:
  #               Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)  -0.2451     0.8012  -0.306   0.7611  
  # trump        -4.2851     2.5887  -1.655   0.1053  
  # education     1.9593     0.7285   2.689   0.0102 *
  # trump2        3.7352     2.6226   1.424   0.1618  
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1534 on 42 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.3042,	Adjusted R-squared:  0.2545 
  # F-statistic: 6.122 on 3 and 42 DF,  p-value: 0.001498
vif(model6)
  # going to obviously be very high
  #     trump  education     trump2 
  # 108.727287   1.173404 110.397494 
resid6 <- residuals(model6)
plot(resid6)
ggplot(model6, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed")
new <- predict.lm(model5, interval="confidence")
plot(new)

# Model 7 (used in paper)
model7 <- lm(hatecrimes ~ trump + education + trump2 + trump*education, data=crime)
summary(model7)
  # best adjusted R-squared model so far (0.3431) and adjusted r-squared (0.279)
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.27455 -0.08798 -0.01757  0.08061  0.43708 
  # Coefficients:
  #                   Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)       -5.637      3.551  -1.587   0.1201  
  # trump              7.323      7.878   0.930   0.3580  
  # education          7.881      3.870   2.037   0.0482 *
  # trump2             2.435      2.711   0.898   0.3743  
  # trump:education  -12.048      7.737  -1.557   0.1271  
  #---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1509 on 41 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.3431,	Adjusted R-squared:  0.279 
  # F-statistic: 5.353 on 4 and 41 DF,  p-value: 0.001458
resid7 <- residuals(model7)
plot(resid7)
ggplot(model7, aes(x=.fitted, y=.resid)) + geom_point() + 
  geom_hline(yintercept=0, linetype="dashed") + labs(title='Residuals')
  # Maybe a littler heteroskedastic

# Model 9 (not used in paper)
model9 <- lm(hatecrimes ~ trump + education + trump*education, data=crime)
summary(model9)
  # education is the only statistically significant value (but the other ones are very close)
  # R-squared = 0.3302 and adjusted R-squared = 0.2823
  # Residuals:
  # Min       1Q   Median       3Q      Max 
  # -0.26289 -0.09102 -0.01923  0.07622  0.43280 
  # Coefficients:
  # Estimate Std. Error t value Pr(>|t|)  
  # (Intercept)       -6.975      3.216  -2.169   0.0358 *
  # trump             11.550      6.303   1.833   0.0740 .
  # education          8.779      3.729   2.354   0.0233 *
  # trump:education  -14.188      7.344  -1.932   0.0601 .
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # Residual standard error: 0.1506 on 42 degrees of freedom
  # (4 observations deleted due to missingness)
  # Multiple R-squared:  0.3302,	Adjusted R-squared:  0.2823   
  # F-statistic: 6.901 on 3 and 42 DF,  p-value: 0.0006994
residfit3 <- residuals(model9)
plot(residfit3)
  # residuals look fine. Maybe heteroskedasticity?
  # F-statistic for the model is significant
  # education is significant, but Trump is not
vif(dtime.fit3.1)
  # there is obviously multicollinearity because of the interaction term
