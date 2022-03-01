install.packages("devtools")
install_github("easyGgplot2", "kassambara")
install.packages("ggplot2")
install.packages("SciencesPo")
install.packages("car")
library(stats)
library(devtools)
library(ggplot2)
library(car)
library(dplyr)

##Load the files##
save_file_1a = read.csv("/Users/rishi/Documents/SMM 2021 - 2022/Marketing Research/Assignment 1/DATA/GRA 64371 ex_1_a_ns.csv", header = TRUE)
save_file_1b = read.csv("/Users/rishi/Documents/SMM 2021 - 2022/Marketing Research/Assignment 1/DATA/GRA 64371 ex_1_b.csv", header = TRUE)
save_file_2 =  read.csv("/Users/rishi/Documents/SMM 2021 - 2022/Marketing Research/Assignment 1/DATA/GRA 64371 ex_2.csv", header = TRUE)
save_file_3 = read.csv("/Users/rishi/Documents/SMM 2021 - 2022/Marketing Research/Assignment 1/DATA/Gra 64371 ex_3.csv", header = TRUE)

##Testing the data: Does it follow a normal distribution?

shapiro.test(save_file_1a$sat_paperA) ## W = 0.8403, p-value = 6.612e-09.
#Reject H0: Data is not normally distributed. 

shapiro.test(save_file_1a$sat_paperB) ## W = 0.82247, p-value = 1.29e-09
#Reject H0: Data is not normally distributed. 

shapiro.test(paperA_readers$Digital) ## W =  0.99782, p-value = 0.7763
#Do not Reject H0: Data IS normally distributed

shapiro.test(paperB_readers$Digital) ## W =  0.9764, p-value = 8.246e-07
#Reject H0: Data is not normally distributed.
#Could be the skewness in top right?

#Plot QQPlots for data
qqPlot(save_file_1a$sat_paperA, xlab = "QQPlot, paper A (store) [Non-normal data]", ylab = 'Paper A')
qqPlot(save_file_1a$sat_paperB, xlab = "QQPlot, paper B (store) [Non-normal data]", ylab = 'Paper B')
qqPlot(paperA_readers$save_file_1b.dig_paperA_consat..save_file_1b.dig_paperA_consat...,
       xlab = "QQPlot, paper A (digital)", ylab = 'Paper A')
qqPlot(paperB_readers$save_file_1b.dig_paperB_consat..save_file_1b.dig_paperB_consat...,, 
       xlab = "QQPlot, paper B (digital)", ylab = 'Paper B')

##Question 1a. Comparing Means with a Two-Tailed test.
paired_test_1a = t.test(save_file_1a$sat_paperA, save_file_1a$sat_paperB, paired = TRUE)
paired_test_1a ## p-value =                     0.06532 
               ## 95% conf. int.               -0.41292369  0.01292369
               ## Mean of the differences =    -0.2

##One-Tailed test. 
paired_test_1a_onetail = t.test(save_file_1a$sat_paperA, save_file_1a$sat_paperB, paired = TRUE, 
                                alternative = 'less')
paired_test_1a_onetail ## p-value =             0.03266
                       ## 95% conf. int.       -Inf  -0.02182563
                       ## Mean of the differences =    -0.2


##Question 1b. Comparing means with a Two-Tailed Test. 
##Check the data. 
summary(save_file_1b) ##Shows values larger than 5 and smaller than 1. 

##Check how many values.
too_large_b = length(which(save_file_1b$dig_paperB_consat>5)) ##33 Values are larger thanr 5/Wrongly inputted. 
too_small_b = length(which(save_file_1b$dig_paperB_consat<1)) ## 5 Values are smaller than 1 --> Wrong inputted. 
too_large_a = length(which(save_file_1b$dig_paperA_consat>5)) ##3 Values are larger thanr 5/Wrongly inputted. 
too_small_a = length(which(save_file_1b$dig_paperA_consat<1)) ## No values smaller than 1. 

##Remove wrong values, and create new df. 
paperA_readers = data.frame(save_file_1b$dig_paperA_consat[(save_file_1b$dig_paperA_consat < 5) & (save_file_1b$dig_paperA_consat > 1)])
paperB_readers = data.frame(save_file_1b$dig_paperB_consat[(save_file_1b$dig_paperB_consat < 5) & (save_file_1b$dig_paperB_consat > 1)])

##Check again
summary(paperA_readers)
summary(paperB_readers)

##Data not normally distributed, but still use T-test anyway. Use welch test for unequal sample sizes.  
paired_test_1b = t.test(paperA_readers, paperB_readers, paired = FALSE)
paired_test_1b           ## p-value                        < 2.2e-16. 
                         ## 95% conf. int.                -0.5919434 -0.3928612
                         ## Mean of X                      3.000841
                         ## Mean of Y                      3.493243  
                        
##F-test to test equality of variances for digital Papers.  
var_test_1b = var.test(paperA_readers$save_file_1b.dig_paperA_consat..save_file_1b.dig_paperA_consat..., paperB_readers$save_file_1b.dig_paperB_consat..save_file_1b.dig_paperB_consat..., "two.sided" = TRUE)
var_test_1b              ## p-value                        0.0007583
                         ## Ratio of variances             0.7347744##
                         ## 95% conf. int.                 0.6138379 0.8790122

##Question 1c. No Code. 

##Question 1d. 
##Shapiro-Wilkes has been done in question 1a.
##Here, one tailed test is used (assumption is that avg. is going to be > 3)

##Doing simple test with test value 3 to see where we can truly say avg. is > 3. 
physical.a.test = t.test(save_file_1a$sat_paperA, mu = 3, alternative = 'greater')
physical.a.test           ##p-value                       0.5542

physical.b.test = t.test(save_file_1a$sat_paperB, mu = 3, alternative = 'greater')
physical.b.test           ##p-value                       0.003581 Great example of how to influence results 
                                                                  ##                 with 1 vs 2 tailed test

digital.a.test = t.test(paperA_readers, mu = 3, alternative = 'greater')
digital.a.test            ##p-value                       0.4896

digital.b.test = t.test(paperB_readers, mu = 3, alternative = 'greater')
digital.b.test            ##p-value                       < 2.2e-16



##Calculating proportion of the sample that is not satisfied currently (paper B digital)
digB_var = var(paperB_readers)    ##0.7061616
digB_sd = sqrt(digB_var)          ##0.8403342
digB_mean = mean(paperB_readers$save_file_1b.dig_paperB_consat..save_file_1b.dig_paperB_consat...) ##3.493243

proportion_notsatisfied_b_digital = pnorm(3.00, mean = mean(digB_mean), sd = digB_sd) ##0.2786151
## Within the sample (27.86% is NOT satisfied)
