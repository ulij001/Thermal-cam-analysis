###############################################
############# Loading spectrum data ############
################################################
library(readxl)
library(tidyverse)
library(reshape)
library(dplyr)
library(tidyverse)

Spectrum_5s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_5s.csv")
Spectrum_10s<-read_csv("C:\\Users\\HP EliteBook\\Documents\\Spectrum_10s.csv")
Spectrum_10s
Spectrum_5s<-transpose(Spectrum_5s)
Spectrum_10s<-transpose(Spectrum_10s)
Spectrum_5s<-as.data.frame(Spectrum_5s)
Spectrum_10s<-as.data.frame(Spectrum_10s)

write.csv(Spectrum_5s, "C:\\Users\\HP EliteBook\\Documents\\T_Spectrum_5s.csv")
write.csv(Spectrum_10s, "C:\\Users\\HP EliteBook\\Documents\\T_Spectrum_10s.csv")

melt_spectrum_5s<-melt(Spectrum_5s, id.vars="nm")
melt_spectrum_5s$nm<-as.numeric(melt_spectrum_5s$nm)
melt_spectrum_5s

melt_spectrum_10s<-melt(Spectrum_10s, id.vars="nm")
melt_spectrum_10s$nm<-as.numeric(melt_spectrum_10s$nm)
melt_spectrum_10s

spectrum_5s_means<- melt_spectrum_5s %>%
  group_by(variable)%>%
  summarize(sum = sum(value, na.rm=TRUE), .groups="drop")

spectrum_5s_means
spectrum_5s_means$Group<- "5s"

spectrum_10s_means<-melt_spectrum_10s %>%
  group_by(variable)%>%
  summarize(sum = sum(value, na.rm=TRUE), .groups="drop")

spectrum_10s_means
spectrum_10s_means$Group<- "10s"

all_spectrum<-rbind(spectrum_10s_means, spectrum_5s_means)
all_spectrum<-as.data.frame(all_spectrum)
all_spectrum
all_spectrum_1<-all_spectrum
all_spectrum_1$Group<-"5s"

all_spectrum_1$Repeat<-"1"
all_spectrum_1<-as.factor(all_spectrum_1$Group)
all_spectrum_1<-as.factor(all_spectrum_1$Repeat)
rm(all_spectrum_1)
all_spectrum_1

all_spectrum$Repeat<-"2"
all_spectrum_2<-all_spectrum
all_spectrum_2<-as.data.frame(all_spectrum_2)
all_spectrum_2
#all_spectrum_2$Group<-"5s"
all_spectrum_2$Repeat<-"2"
#all_spectrum_2<-as.factor(all_spectrum_2$Group)
#all_spectrum_2<-as.factor(all_spectrum_2$Repeat)
#rm(all_spectrum_2)

#all_spectrum_2

all_spectrum_data<-rbind(all_spectrum_1, all_spectrum)
all_spectrum_2
all_spectrum_data
all_spectrum_data$Group<-as.factor(all_spectrum_data$Group)
all_spectrum_data$Repeat<-as.factor(all_spectrum_data$Repeat)
all_specrum

#####################################################
############# IMPORT HARVEST ########################
#####################################################
Final_Harvests<-read_excel("C:\\Users\\HP EliteBook\\Documents\\Thermal Imagin\\VFT51\\Harvests_2nd.xlsx")
Final_Harvests<-as.data.frame(Final_Harvests)
str(Final_Harvests)
Final_Harvests$Group<-as.factor(Final_Harvests$Group)
Final_Harvests$Repeat<-as.factor(Final_Harvests$Repeat)


#####################################################
############# NDVI         ##########################
#####################################################
all_NDVI_values<-read_excel("C:\\Users\\HP EliteBook\\Documents\\NDVI.xlsx")
all_NDVI_values<-as.data.frame(all_NDVI_values)
str(all_NDVI_values)
all_NDVI_values$Group<-as.factor(all_NDVI_values$Group)
all_NDVI_values$Repeat<-as.factor(all_NDVI_values$Repeat)
#####################################################
############# IMPORT AREAS ##########################
#####################################################
leaf_area <- read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\leaf_area.xlsx")
leaf_area<-as.data.frame(leaf_area)
str(leaf_area)
leaf_area$Group<-as.factor(leaf_area$Group)
leaf_area$Repeat<-as.factor(leaf_area$Repeat)


#merged_data <- merge(leaf_area, all_NDVI_values, by = c("Group", "Repeat"))
#merged_data
#rm(merged_data)
#merged_data <- merge(merged_data, Final_Harvests, by = c("Group", "Repeat"))
#merged_data <- merge(merged_data, all_spectrum_data, by = c("Group", "Repeat"))
#rm(merged_data)


merged_data <- full_join(leaf_area, all_NDVI_values, by = c("Group", "Repeat"), all.x=TRUE) %>%
  full_join(Final_Harvests, by = c("Group", "Repeat")) %>%
  full_join(all_spectrum_data, by = c("Group", "Repeat"))
merged_data

model <- manova(cbind(area, ndvi_value, Mass_g) ~ Repeat + Group + `sum`, data = merged_data)
summary(model)
model

write_csv(leaf_area, "C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\pre_leaf_area.csv") 
write_csv(Final_Harvests, "C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\pre_Final_harvest.csv")                     
write_csv(all_NDVI_values, "C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\pre_NDVI.csv")
write_csv(all_spectrum_data, "C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Images\\pre_spectrum.csv")



##########################################
################## MANOVA ################
##########################################

all_data<-read.csv("C:\\Users\\HP EliteBook\\Documents\\All_data.csv")
all_data<-as.data.frame(all_data)
rm(all_data)
str(all_data)
all_data$Individuality<-as.factor(all_data$Group)
all_data$Repeat<-as.factor(all_data$Group)
all_data$spectrum<-as.factor(all_data$spectrum)
rm(result)
result<-manova(cbind(area, NDVI, Mass_g) ~ Repeat * spectrum * Group * Individuality, data = all_data)
summary(result)
model
summary(model_Group)

rm(result_NDVI)
result_Repeat<-manova(cbind(area, NDVI, Mass_g) ~ Repeat* Group * Individuality * spectrum, data = all_data)
result_Repeat
rm(result_Repeat)

new <- manova(cbind(area, NDVI, Mass_g) ~ Group * Individuality * spectrum, data = all_data)
new
summary(new)


###############
# Assuming your data frame is named 'all_data'
# Variables: area, NDVI, Mass_g are response variables, and Group, Repeat, spectrum are predictor variables
manova_area <- manova(area ~ Group * Individuality * spectrum, data = all_data)
manova_NDVI <- manova(NDVI ~ Group * In * spectrum, data = all_data)
manova_Mass_g <- manova(Mass_g ~ Group * Repeat * spectrum, data = all_data)

#ANOVA for each response vairable start with NDVI
model_NDVI <- lm(NDVI ~ Group * Individuality * spectrum, data = all_data)
model_NDVI
NDVI_result<-anova(model_NDVI)
print(NDVI_result)


#ANOVA FOR LEAF AREA WITHIN THE PLANT. 
model_area <- lm(area ~ Group * Individuality * spectrum, data = all_data)
model_area
model_area<-anova(model_area)
print(model_area)

NDVI_result<-anova(area ~ Repeat * Group * spectrum, data = all_data)


#ANOVA WITHIN THE MASS OF THE PLANT. 
model_mass <- lm(Mass_g ~ Group * Individuality * spectrum, data = all_data)
model_mass
model_mass<-anova(model_mass)
print(model_mass)

NDVI_result<-anova(area ~ Repeat * Group * spectrum, data = all_data)


#ANOVA FOR MASS WITHIN THE PLANT 
all_data


# Print the summaries
summary(manova_area)
summary(manova_NDVI)
summary(manova_Mass_g)

resulting_individuality<-anova(Individuality ~ NDVI, data = all_data)
all_data$NDVI<-as.numeric(all_data$NDVI)
all_data
group1_data <- all_data$NDVI[all_data$Individuality == "1"]
group2_data <- all_data$NDVI[all_data$Individuality == "2"]
group3_data <- all_data$NDVI[all_data$Individuality == "3"]
group4_data <- all_data$NDVI[all_data$Individuality == "4"]
# Perform t-tests
t_test_group1_vs_group2 <- t.test(group1_data, group2_data)
t_test_group1_vs_group3 <- t.test(group1_data, group3_data)
t_test_group2_vs_group3 <- t.test(group2_data, group3_data)
t_test_group1_vs_group4 <- t.test(group1_data, group4_data)
t_test_group2_vs_group4 <- t.test(group2_data, group4_data)
t_test_group3_vs_group4 <- t.test(group3_data, group4_data)

group1_data
# Print the results
print(t_test_group1_vs_group2)
print(t_test_group1_vs_group3)
print(t_test_group2_vs_group3)
print(t_test_group1_vs_group4)
print(t_test_group2_vs_group4)
print(t_test_group3_vs_group4)


str(all_data)
library("tidyverse")

all_data$Individuality<-as.factor(all_data$Individuality)

ggplot(data = all_data, aes(x=Individuality, y =spectrum)) + 
  geom_boxplot()



#### Creating time series plots for the data ###
g2_10s<-read_xlsx("C:\\Users\\HP EliteBook\\Documents\\group2_10s_meta.xlsx")
g2_10s<-data.frame(g2_10s)
g1_10s<-read_xlsx("C:\\Users\\HP EliteBook\\Documents\\group1_10s_meta.xlsx")
g1_10s<-data.frame(g1_10s)
g1_5s<-read_xlsx("C:\\Users\\HP EliteBook\\Documents\\Group1_5s_meta.xlsx")
g1_5s<-data.frame(g1_5s)

g2_10s$variable<-(g2_10s$variable = "repeat 2 : 10 seconds")
g1_10s$variable<-(g1_10s$variable = "repeat 1 : 10 seconds")
g1_5s$variable<-(g1_5s$variable = "repeat 1 : 5 seconds")

time_series<-rbind(g1_5s, g1_10s, g2_10s)
time_series

str(time_series)

ggplot(data = time_series, aes(x=hour, y = diff_in_means, shape = variable, color = variable)) + 
  geom_point() + 
  geom_smooth(size=1.3) + 
  ylim(-1, 1) + 
  scale_colour_manual(values=c("#6c1f31","#0e496c","#008080")) + 
  ylab("Avg temperature difference") + 
  xlab("Hour") + 
  ggtitle("Relative plant temperature difference")
  
  

ggplot(data = time_series, aes(x=cum_hours, y = diff_in_means, shape = variable, color = variable)) + 
  geom_point() + 
  geom_smooth(size=1.3) + 
  ylim(-1, 1) + 
  scale_colour_manual(values=c("#6c1f31","#0e496c","#008080"))
