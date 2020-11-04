#### AB Testing Exercise

################## Background - 

# Decco is an online retailer that sells home decor items. They recently added ‘Lamps’ 
# as a new category of products which they were not selling before. In order to generate 
# awareness and boost sales, they want to do a promotion through their App. Their 
# notifications have had good success in the past and they are considering to send a message 
# making users aware of this new category, through in-app notification. But at the same time, 
# they want to be judicious about any features or releases when it comes to their app because 
# they know that LTV of a customer who has installed their app is much higher. They want to 
# be careful so as not to drive users to uninstall the app. 
# 



############################## Hypothesis - 

# Because we have seen good success through  in-app notifications in the past, if we send an
# in-app notification with a promotional offer for Lamps, then the % of users that purchase 
# from the Lamp Category will increase. 


# Primary Metrics - Transaction Rate i.e. % of users that will make a purchase
# Secondary metrics are - Purchase Value
# Other metrics - Uninstall rate

############################################################################################################

############################### Sample Size Calculation 


install.packages("ggplot2")
install.packages("tidyverse")
install.packages("pwr")

library(ggplot2)
library(dplyr)
library(pwr)


control = 0.101
uplift = 0.2
variant = (1+uplift)*control

effect_size = ES.h(control, variant)
sample_size_output = pwr.p.test(h=effect_size,
                                n= ,
                                sig.level = 0.05,
                                power= 0.8)

sample_size_output = ceiling(sample_size_output$n)
sample_size_output


#################################  Understanding the data

getwd()
setwd("~/Documents/AB_Testing/")
df = read.csv("_AB_Test_Data.csv")

summary(df)
head(df)

# Overview of Dataset

## user_id: ID of User\
## allocation: Treatment indicator *control vs treatment"
## active_6m: active on the last 6 months
## days since: days since last activity
## addtocart_flag: Intent of buying.
## transaction_flag: The buy was achieved
## uninstall_flag: The user uninstalled the app.

summary(df[,c("active_6m", "addtocart_flag","transaction_flag", "uninstall_flag", 
              "purchase_value", "days_since")])

df %>% ggplot(aes(x = purchase_value)) +  
  geom_histogram( color="#e9ecef", fill = "#E69F00", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df %>% ggplot(aes(x = purchase_value)) +  
  geom_density( color="#E69F00")

df %>% ggplot(aes(x = days_since)) +  
  geom_histogram( color="#e9ecef", fill = "#56B4E9", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df %>% ggplot(aes(x = days_since)) +  
  geom_density( color="#56B4E9")

# Categorical Variable Stats

table(df$allocation)
table(df$allocation)/nrow(df)



# ############################### Check for randomization - baseline variables

df %>% group_by(allocation) %>% summarise(mean(active_6m),
                                          mean(days_since),
                                          mean(uninstall_flag),
                                          mean(transaction_flag))

df %>% ggplot(aes(x = days_since, fill = allocation)) +  
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")


################################ Treatment Effects

# Compare performance of response variables across the two groups

df %>% group_by(allocation) %>% summarise(mean(addtocart_flag), mean(transaction_flag), 
                                          mean(purchase_value, na.rm = TRUE))


xtabs(~ allocation + transaction_flag, data = df)

prop.test(xtabs(~ allocation + transaction_flag, data = df)[,2:1])

# This is telling us that the treatment group performed between 7.8% and 6.9% better than the control group.
# i.e. if we were to repeat this test 20 times with 20 different samples, the difference between the
# performance of the two groups will lie somewhere between these two numbers, at least 19 times.
# i.e those differences are statistically significant.

prop.test(xtabs(~ allocation + addtocart_flag, data = df)[,2:1])

t.test(purchase_value ~ allocation, data = df)


#Checking for Uninstalls

df %>% group_by(allocation) %>% summarise(mean(uninstall_flag))


prop.test(xtabs(~ allocation + uninstall_flag, data = df)[,2:1]) # We observe that variation induces uninstall, and hence we do not implement the variation.
