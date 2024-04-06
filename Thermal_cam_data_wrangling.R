#######################################################
############ PRE TASKS: RELEVANT PACKAGES #############
#######################################################

install.packages("stringr")
library("stringr")

install.packages("tidyverse")
library(tidyverse)

install.packages("mixtools")
library(mixtools)

#######################################################
####################### PROBLEM 1 #####################
#######################################################


######################### TASK 1.1 ####################


THETA<- seq(0, 1, 0.005)
THETA

alternative_dbl<-0:9
alternative_sgl<-4

l<-dbinom(x=alternative_sgl, prob=THETA, size=9)

plot(THETA, l, type="l", lwd =2)

######################### TASK 1.2 ####################

THETA[which.max(l)]





#######################################################
####################### PROBLEM 2 #####################
#######################################################

######################### Task 2.1 ####################

Task2_1df
Task2_1df <- data.frame(
  Individual = 1:10,
  Genotype_Site1 = c('00', '00', '11', '01', '11', '00', '01', '00', '11', '11'),
  Genotype_Site2 = c('01', '01', '00', '11', '01', '00', '01', '11', '00', '11'))

Task2_1df
Task2_1df$conc_genotypes <- paste0(Task2_1df$Genotype_Site1, Task2_1df$Genotype_Site2)

Task2_1df
Task2_1df$Num_G <- str_count(Task2_1df$conc_genotypes, "1")
Task2_1df
Task2_1df <- as.factor(Task2_1df$Individual)
Task2_1df$Individual <- as.factor(Task2_1df$Individual)


Task2_1df
Task2_1df$conc_genotypes <- paste0(Task2_1df$Genotype_Site1, Task2_1df$Genotype_Site2)
Task2_1df$Num_G <- str_count(Task2_1df$conc_genotypes, "1")

Task2_1df$Individual <- as.factor(Task2_1df$Individual)
Task2_1df
ggplot(data = Task2_1df, aes(x=Individual, y=Num_G)) +
  geom_bar(stat="identity", position="dodge")

ggplot(data = Task2_1df, aes(x=Individual, y=Num_G)) +
  geom_bar(stat="identity", position="dodge") +
  ylab("G count")


######################## TASK 2.2 #######################

t.test(G1$G1, G2$G2)

G_t_test<-t.test(G1$G1, G2$G2)

summary(G_t_test)




Task2_1df <- data.frame(
  Individual = 1:10,
  Genotype_Site1 = c('00', '00', '11', '01', '11', '00', '01', '00', '11', '11'),
  Genotype_Site2 = c('01', '01', '00', '11', '01', '00', '01', '11', '00', '11'))

Task2_1df$conc_genotypes <- paste0(Task2_1df$Genotype_Site1, Task2_1df$Genotype_Site2)

Task2_1df$Num_G <- str_count(Task2_1df$conc_genotypes, "1")

Task2_1df$Individual <- as.factor(Task2_1df$Individual)

Task2_1df

ggplot(data = Task2_1df, aes(x=Individual, y=Num_G)) +
  geom_bar(stat="identity", position="dodge") +
  ylab("G count")


###############################################################
######################### TASK 3 ##############################
###############################################################


assignment <- read.csv("C:/Users/HP EliteBook/Downloads/assignment.csv")


assignment$risk<-as.numeric(assignment$risk)
assignment$genotypes<-as.numeric(assignment$genotypes)
assignment$ancestry<-as.factor(assignment$ancestry)
assignment$income<-as.numeric(assignment$income)

#linear model of data including 3 explanatory and 1 response
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)

model_1_anova<-anova(model_1)
modeL_1_anova

#linear model of data including 2 explanatory and 1 response
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
model_2
summary(model_2)

model_2_anova<-anova(model_2)
model_2_anova

ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) + 
  scale_color_manual(values=c("#e4181b", "#006ab5", "#01796f")) + 
  ggtitle("Problem 3: Linear regression plot") + 
  ylab("Risk") + 
  xlab("Genotypes")



######################################################################
############################ TASK 4 ##################################
######################################################################

#Plot graph see if potential for two normal distributions. 
hist(assignment$risk, probability = TRUE, col = "skyblue")
lines(density(assignment$risk), col = "red", lwd = 2)

fit<-normalmixEM(assignment$income, k=2)

alpha <- fit$lambda

mu <- fit$mu
sigma <- sqrt(fit$sigma^2)
# Display the estimated parameters
cat("Alpha:", alpha, "\n")
cat("Mu1:", mu[1], "\n")
cat("Mu2:", mu[2], "\n")








assignment
ggplot(data = Task2_1df, aes(x=Individual, y=Num_G)) +
  geom_bar(stat="identity", position="dodge") +
  ylab("G count")

ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry)) +
  geom_point() +
  geom_smooth(method=lm)

