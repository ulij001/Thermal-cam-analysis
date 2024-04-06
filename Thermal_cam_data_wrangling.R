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

str(assignment)
assignment$ancestry <- factor(assignment$ancestry, levels = c("3", "2", "1"))
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
summary(model)
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model)
assignment
assignment$ancestry<-as.factor(assignment$ancestry)
assignment
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model)
str(assignment)
str(assignment)
assignment$ancestry<-as.factor(assignment$ancestry)
assignment$risk<-as.numeric(assignment$risk)
assignment$income<-as.numeric(assignment$income)
assignment$genotypes<-as.numeric(assignment$genotypes)
assignment
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model)
model_1
summary(model_1)
model_2 <- lm(risk ~ genotypes + ancestry1, data = assignment)
model_2 <- lm(risk ~ genotypes + ancestry2, data = assignment)
assignment$ancestry1 <- as.numeric(assignment$ancestry == "ancestry1")
assignment$ancestry2 <- as.numeric(assignment$ancestry == "ancestry2")
assignment$ancestry3 <- as.numeric(assignment$ancestry == "ancestry3")
model_1 <- lm(risk ~ genotypes + ancestry1, ancestry2, ancestr3 + income, data = assignment)
model_1 <- lm(risk ~ genotypes + ancestry1, ancestry2, ancestry3 + income, data = assignment)
model_1 <- lm(risk ~ genotypes + ancestry1 + ancestry2 + ancestry3 + income, data = assignment)
model_1
summary(model_1)
assignment
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)
model_2 <- lm(risk ~ genotypes + ancestry2, data = assignment)
summary(model_2)
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
summary(model_2)
assignment
assignment$ancestry<-as.factor(assignment$ancestry)
assignment$risk<-as.numeric(assignment$risk)
assignment$income<-as.numeric(assignment$income)
assignment$genotypes<-as.numeric(assignment$genotypes)
assignment
rm(assignment)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes")
assignment
assignment <- read.csv("C:/Users/HP EliteBook/Downloads/assignment.csv")
View(assignment)
assignment
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape =ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes")
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes")
Task2_1df
G1<-c(1, 1, 2, 3, 3, 0, 2, 2, 2, 4)
G2<-C(2, 4, 2, 3, 3, 3, 2, 3, 3, 4)
G2<-c(2, 4, 2, 3, 3, 3, 2, 3, 3, 4)
G1<-as.data.frame(G1)
G2<-as.data.grame(G2)
G2<-as.data.frame(G2)
mean(G2)
G1<-as.data.frame(G1)
G2<-as.data.frame(G2)
G2<-as.data.frame(G2)
G2
mean(G2$G2)
mean(G1$G1)
G1_std<-sd(G1$G1)
G1_mean<-mean(G1$G1)
G2_mean<-mean(G2$G2)
G2_std<-sd(G2$G2)
G1_std<-sd(G1$G1)
t.test(G1$G1, G2$G2)
G_t_test<-t.test(G1$G1, G2$G2)
summary(G_t_test)
G_t_test
G2_std<-sd(G2$G2)
G1_mean
G2_mean
G2_std
G_t_test<-t.test(G1$G1, G2$G2)
G1_std
G2_std
G1_std
G1_mean
G2_mean
G1_std
#Measure of scale using Interquartile range.
G1_LQ<-quantile(G1, 0.25)
#Measure of scale using Interquartile range.
G1_LQ<-quantile(G1, 0.25)
#Measure of scale using Interquartile range.
G1_LQ<-quantile(G1$G1, 0.25)
G2_L1<-quantile(G2$G2, 0.75)
rm(G2_L1)
#Measure of scale using Interquartile range.
G1_LQ<-quantile(G1$G1, 0.25)
G2_UQ1<-quantile(G2$G2, 0.75)
#Measure of scale using Interquartile range.
G1_LQ<-quantile(G1$G1, 0.25)
G1_UQ<-quantile(G2$G2, 0.75)
G1_IQR<-G1_UQ_G1_LQ
G1_IQR<-G1_UQ-G1_LQ
G1_IQR
G1_UQ
G1_LQ
G1_IQR<-(G1_UQ-G1_LQ)
G1_IQR<-(G1_UQ-G1_LQ)
G1_IQR
G1_UQ
G1_LQ
G2_UP<-quantile(G2$G2, 0.75)
G2_IQR<-(G2_UQ-G2_LQ)
G2_IQR
G2_LQ<-quantile(G2$G2, 0.25)
G2_UP<-quantile(G2$G2, 0.75)
G2_IQR<-(G2_UQ-G2_LQ)
G2_IQR
G2_IQR<-(G2_UQ-G2_LQ)
G2_LQ<-quantile(G2$G2, 0.25)
G2_UQ<-quantile(G2$G2, 0.75)
G2_IQR<-(G2_UQ-G2_LQ)
G2_IQR
G_t_test
#
plot(THETA, l, type="l", lwd =2)
#
plot(THETA, l, type="l", lwd =2, ylim=c(0, max(1)))
#
plot(THETA, l, type="l", lwd =2)
THETA[which.max(l)]
assignment$ancestry<-as.factor(assignment$ancestry)
assignment$risk<-as.numeric(assignment$risk)
assignment$income<-as.numeric(assignment$income)
assignment$genotypes<-as.numeric(assignment$genotypes)
rm(assignment)
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
summary(model_2)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes")
assignment
library(readxl)
assignment <- read_excel("C:/Users/HP EliteBook/Downloads/assignment.csv")
assignment
assignment <- read.csv("C:/Users/HP EliteBook/Downloads/assignment.csv")
View(assignment)
assignment
assignment$ancestry<-as.factor(assignment$ancestry)
assignment$risk<-as.numeric(assignment$risk)
assignment$income<-as.numeric(assignment$income)
assignment$genotypes<-as.numeric(assignment$genotypes)
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
summary(model_2)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes")
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill="transparent") +
  xlab("Genotypes") +
  ggtitle
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill=transparent) +
  xlab("Genotypes") +
  ggtitle
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill=transparent) +
  xlab("Genotypes") +
  ylim (0, 50)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill=transparent) +
  xlab("Genotypes") +
  ylim (0, 50)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill=transparent) +
  xlab("Genotypes") +
  ylim(0, 50)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm, fill=transparent) +
  xlab("Genotypes")
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Genotypes") +
  ylim(0, 50)
summary(model_1)
drop(model_1, test="F")
drop1(model_1, test="F)
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
summary(model_2)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point() +
geom_smooth(method=lm) +
xlab("Genotypes") +
drop1(model_1, test="F")
drop1(model_1, test="F")
summary(model_1)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point() +
geom_smooth(method=lm, fill=transparent) +
xlab("Genotypes") +
ylim(0, 50)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point() +
geom_smooth(method=lm) +
xlab("Genotypes") +
ylim(0, 50)
model_1 <- lm(risk ~ genotypes + ancestry + income, data = assignment)
model_1
summary(model_1)
anova(model_1)
model_2 <- lm(risk ~ genotypes + ancestry, data = assignment)
summary(model_2)
anova(model_2)
anova(model_1)
model_1
summary(model_1)
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point() +
geom_smooth(method=lm) +
xlab("Genotypes") +
ylim(0, 50) +
ggtitle("Problem 3: linear regression plot")
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point(size=2) +
geom_smooth(method=lm) +
xlab("Genotypes") +
ylim(0, 50) +
ggtitle("Problem 3: linear regression plot")
ggplot(data = assignment, aes(x=genotypes, y=risk, col=ancestry, shape=ancestry)) +
geom_point(size=2) +
geom_smooth(method=lm, se=FALSE) +
xlab("Genotypes") +
ylim(0, 50) +
ggtitle("Problem 3: linear regression plot")
############## WRITTEN STATEMENT ###########
# There was a significant main effect of genotypes on
# influencing disease risk suceptability (ANCOVA, p<0.01)
assignment
ggplot(data = assignment, aes(x=risk, y=income))
ggplot(data = assignment, aes(x=risk, y=income)) +
geom_point()
ggplot(data = assignment, aes(x=income, y=rixk)) +
geom_point()
ggplot(data = assignment, aes(x=income, y=risk)) +
geom_point()
ggplot(data = assignment, aes(x=income, y=risk)) +
geom_point() +
geom_line()
ggplot(data = assignment, aes(x=income, y=risk)) +
geom_bar(stat=identisty, position=dodge)
ggplot(data = assignment, aes(x=income, y=risk)) +
geom_bar(stat="identisty", position="dodge")
ggplot(data = assignment, aes(x=income, y=risk)) +
geom_bar(stat="identity", position="dodge")
ggplot(data = assignment, aes(x=genotypes, y=risk)) +
geom_bar(stat="identity", position="dodge")
ggplot(data = assignment, aes(x=genotypes, y=risk)) +
geom_point(stat="identity", position="dodge")
ggplot(data = assignment, aes(x=ancestry, y=risk)) +
geom_point(stat="identity", position="dodge")
assignment
hist(assignment, probability = TRUE, col = "skyblue")
hist(assignment$risk, probability = TRUE, col = "skyblue")
lines(density(assignment$risk), col = "red", lwd = 2)
library(mixtools)
install.packages("mixtools")
library(mixtools)
fit<-normalmixEM(assignment$income, k=2)
alpha <- fit$lambda
mu <- fit$mu
sigma <- sqrt(fit$sigma^2)
# Display the estimated parameters
cat("Alpha:", alpha, "\n")
cat("Mu1:", mu[1], "\n")
cat("Mu2:", mu[2], "\n")
all_ndvi_data
#repeat 1 absorbance levels
r1_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_5s_post_edit.xlsx")
r1_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_10s_post_edit.xlsx")
#repeat 2 absorbance levels
r2_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Repeat_2_5s_post_edit.xlsx")
#repeat 1 absorbance levels
r1_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_5s_post_edit.xlsx")
#Mass of the harvests executed.
Final_Harvests<-read_excel("C:\\Users\\HP EliteBook\\Documents\\Thermal Imagin\\VFT51\\Harvests_2nd.xlsx")
r2_10s<-read_excel("C:\Users\HP EliteBook\Documents\pak_choi_pp\Updated\Repeat_2\Absorbance\Repeat_2_10s_NDVI.xlsx")
r2_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Absorbance\\Repeat_2_10s_NDVI.xlsx")
r2_10s
r1_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Absorbance\\Repeat_1_10s_NDVI.xlsx")
r1_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Absorbance\\Repeat_1_10s_NDVI.xlsx")
r1_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Absorbance\\Repeat_1_10s_NDVI.xlsx")
Final_Harvests<-read_excel("C:\\Users\\HP EliteBook\\Documents\\Thermal Imagin\\VFT51\\Harvests_2nd.xlsx")
leaf_area<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\leaf_area.xlsx")
leaf_area
#reading light density
#Leave out for now
str(leaf_area)
str(Final_harvests)
str(Final_harvest)
str(Final_Harvests)
#reading light density
#Leave out for now
str(leaf_area)
str(r2_5s)
str(r2_5s)
r2_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Absorbance\\Repeat_2_5s_NDVI.xlsx")
str(r2_5s)
rd_5s$Group<-5s
rd_5s$Group<-"5s"
r2_5s$Group<-5s
r2_5s$Group<-"5s"
r2_5s
r2_5s$Repeat<-2
r2_10s$Group<-"10s"
r2_10s$Repeat<-2
r1_5s$Group<-"5s"
r1_5s$Repeat<-1
r1_10s$Group<-"10s"
r1_10s$Repeat<-1
#reading light density
#Leave out for now
str(leaf_area)
str(Final_Harvests)
total_ndvi<-cbind(r1_10s, r1_5s, r2_5s, r2_10s)
total_ndvi<-rbind(r1_10s, r1_5s, r2_5s, r2_10s)
total_ndvi
str(Final_Harvests)
#reading light density
#Leave out for now
str(leaf_area)
leaf_area
str(Final_Harvests)
total_ndvi
names(leaf_area)[names(leaf_area) == "GROUP"] <- "Repeat"
leaf_area
names(leaf_area)[names(leaf_area) == "Group"] <- "Repeat"
leaf_area<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\leaf_area.xlsx")
leaf_area
names(leaf_area)[names(leaf_area) == "Group"] <- "Repeat"
leaf_area
names(leaf_area)[names(leaf_area) == "Condition"] <- "Group"
leaf_area
Spectrum_10s <- read.csv("~/Spectrum_10s.csv", header=FALSE)
View(Spectrum_10s)
library(readr)
Spectrum_5s <- read_csv("Spectrum_5s.csv")
View(Spectrum_5s)
################# Import relevant data spectrum ##########
Spectrum_5s
################# Import relevant data spectrum ##########
Spectrum_5s
ggplot(data = Spectrum_5s, aes(x=nm, y=Mean)) + geom_point()
Spc_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\Spectrum_10s.csv")
Spc_5s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_10s.csv")
Spc_5s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_5s.csv")
plot(Spc_5s)
Spc_10s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_10s.csv")
Spc_5s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_5s.csv")
Spc_5s<-t(Spc_5s)
Spc_5s
Spc_10s<-t(Spc_10s)
Spc_10s
Spc_5s$sum <- rowSums(Spc_5s[, !colnames(Spx_5s) %in% "nm"])
Spc_5s$sum <- rowSums(Spc_5s[, !colnames(Spc_5s) %in% "nm"])
Spc_5s$sum <- as.data.frame(rowSums(Spc_5s[, !colnames(Spc_5s) %in% "nm"]))
Spc_5s$sum <- rowSums(Spc_5s[, !colnames(Spc_5s) %in% "nm"])
Spc_5s
rm(Spc_5s)
Spc_5s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_5s.csv")
Spc_5s<-t(Spc_5s)
Spc_5s
Spc_5s$sum <- rowSums(Spc_5s[, !colnames(Spc_5s) %in% "nm"])
Spc_10s
c_5s <- cbind(c_5s, sum = c_5s$sum)  # Add the vector as a new column
Spc_5s <- cbind(Spc_5s, sum = Spc_5s$sum)  # Add the vector as a new column
