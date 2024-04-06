install.packages("readxl")
library(readxl)

library(tidyverse)

install.packages("MANOVA.RM")  # If not already installed
library(MANOVA.RM)

install.packages("gridExtra")
library(gridExtra)

library(dplyr)

rm(Repeat_1_5s_post_edit)

##############################################################
######## Reading in relevant files (Absorbance Files) ########
##############################################################


#repeat 1 absorbance levels
r1_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_5s_post_edit.xlsx")
r1_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_10s_post_edit.xlsx")

#repeat 2 absorbance levels
r2_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Repeat_2_5s_post_edit.xlsx")
r2_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Repeat_2_10s_post_edit.xlsx")

#repeat 3 absorbance levels
r3_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Repeat_3_5s_post_edit.xlsx")
r3_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Repeat_3_10s_post_edit.xlsx")

r3_5s

#Self calculated NDVI levels.
NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\NDVI_values.xlsx")
NDVI



###################################
##### READING IN HARVEST DATA #####
###################################

#Mass of the harvests executed. 
Final_Harvests<-read_excel("C:\\Users\\HP EliteBook\\Documents\\Thermal Imagin\\VFT51\\Harvests_2nd.xlsx")




######################################################
##### CALCULATE ABORBANCE MEANS (MINUS HEADERS) ######
######################################################

row_means_r1_5s <- rowMeans(r1_5s[, -1])
row_means_r1_10s <- rowMeans(r1_10s[, -1])

row_means_r2_5s <- rowMeans(r2_5s[, -1])
row_means_r2_10s <- rowMeans(r2_10s[, -1])

row_means_r3_5s <- rowMeans(r3_5s[, -1])
row_means_r3_10s <- rowMeans(r3_10s[, -1])
#rm(row_means_r3_10s)
#row_means_r3_10s
# Create a data frame for plotting
#plot_data <- data.frame(nm = data_set1$nm, mean_set1 = row_means_set1, mean_set2 = row_means_set2)


#SIMPLIFYING THE DATAFRAMES
repeat_1 <- data.frame(nm = r1_5s$nm, r1_5s_mean=row_means_r1_5s, r1_10s_mean=row_means_r1_10s, r2_5s_mean=row_means_r2_5s, r2_10s_mean=row_means_r2_10s, r3_5s_mean=row_means_r3_5s, r3_10s_mean=row_means_r3_10s)
repeat_1
######################################################
##### PLOTTING ABSORBANCE MEANS - GEOM_POINT() #######
######################################################

par(mfrow = c(1, 2))

ggplot(data = repeat_1, aes(x=nm)) + 
  geom_point(aes(y=r1_5s_mean), col="#00ECD1") +
  geom_point(aes(y=r1_10s_mean), col = "#008080") +
  xlab("Wavelenghts of light (nm)") + 
  ylab("Percent of wavelength absobtion (%)") +
  ggtitle("Repeat 1: % Absorption of light")


Absorbance_r_1<-ggplot(data = repeat_1, aes(x = nm)) +
  geom_point(aes(y = r1_5s_mean, color = "Duration: 5 seconds"), size = 1) +
  geom_point(aes(y = r1_10s_mean, color = "Duration: 10 seconds"), size = 1) +
  xlab("Wavelengths of Light (nm)") +
  ylab("Percent of Wavelength Absorption (%)") +
  ggtitle("Repeat 1: % Absorption of Light") +
  scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c")) +
  labs(color = "Legend Title")

Absorbance_r_2<-ggplot(data = repeat_1, aes(x = nm)) +
  geom_point(aes(y = r2_5s_mean, color = "Duration: 5 seconds"), size = 1) +
  geom_point(aes(y = r2_10s_mean, color = "Duration: 10 seconds"), size = 1) +
  xlab("Wavelengths of Light (nm)") +
  ylab("Percent of Wavelength Absorption (%)") +
  ggtitle("Repeat 2: % Absorption of Light") +
  scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c")) +
  labs(color = "Legend Title")

Absorbance_r_3<-ggplot(data = repeat_1, aes( x = nm)) +
  geom_point(aes (y = r3_5s_mean, color = "Duration: 5 seconds"), size = 1) + 
  geom_point(aes (y = r3_10s_mean, color = "Duration: 10 seconds"), size = 1) + 
  xlab("Wavelenghts of light(nm)") + 
  ylab ("Percent of Wavelength Absorption (%)") + 
  ggtitle("Repeat 3: Absorption of light") + 
  scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#03496c")) +
  labs(color = "Legend Title")

Absorbance_r_3
Absorbance_r_2
Absorbance_r_1
#combine the two plots
grid.arrange(Absorbance_r_1, Absorbance_r_2, Absorbance_r_3, NDVI_r1, NDVI_r2, NDVI_r3, ncol = 3, nrow=2)

rm(Absorbance_repeat_2)

  
#####################################################
##### ASSIGNING COLORS TO REQUIRED WAVELENGTHS ######
#####################################################


repeat_1 <- repeat_1 %>%
  mutate(color = ifelse(nm >= 600 & nm < 700, "red",
                           ifelse(nm >= 700 & nm <= 800, "ir", NA)))



#####################################################
##### PLOTTING THE HARVEST DATA AND T TEST ##########
#####################################################

ggplot(data = Final_Harvests, aes(x=Group, y=Mass_g, fill = Group)) + geom_boxplot() + 
  facet_grid(~Repeat) + 
  geom_jitter(width = 0.25) +
  scale_fill_manual(values=c("#008080", "#00ECD1")) + 
  xlab("repeat") + 
  ylab("Mass_g_plant") + 
  ggtitle("Watering durations (High vs low)")


t_test <- t.test(Mass_g ~ Group, data = Final_Harvests)
t_test

ggplot(data = Final_Harvests, aes(x=Group, y=Mass_g, fill = Group)) + geom_boxplot() + 
  scale_fill_manual(values = c("#008080", "#00ECD1"))

#Tt




  # Create a scatter plot using ggplot2
#ggplot(plot_data, aes(x = mean_set1, y = mean_set2)) +
 # geom_point() +
  #labs(title = "Scatter Plot of Row Means", x = "Row Mean Set 1", y = "Row Mean Set 2") +
  #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")  # Add a diagonal reference line
#In this code:


###############################################################
######### PLOTTING NDVI VALUES AND DATA WRANGLING #############
###############################################################


##### PRIOR TO IMPORTING ALL CSVS HAD BEEN EDITED WITHIN PYTHON JUPITER #####
.
#repeat 1 absorbance levels
r1_5s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_5s_NDVI.xlsx")
r1_10s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Repeat_1_10s_NDVI.xlsx")

#repeat 2 absorbance levels
r2_5s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Repeat_2_5s_NDVI.xlsx")
r2_10s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Repeat_2_10s_NDVI.xlsx")

r3_5s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Repeat_3_5s_NDVI.xlsx")
r3_10s_NDVI<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Repeat_3_10s_NDVI.xlsx")
#Adding a new column

r3_5s_NDVI



r1_5s_NDVI_values<-data.frame(r1_5s_NDVI$NDVI, r1_5s_NDVI$Time )
r1_10s_NDVI_values<-data.frame(r1_10s_NDVI$NDVI, r1_10s_NDVI$Time )

r2_5s_NDVI_values<-data.frame(r2_5s_NDVI$NDVI, r2_5s_NDVI$Time )
r2_10s_NDVI_values<-data.frame(r2_10s_NDVI$NDVI, r2_10s_NDVI$Time )

r3_5s_NDVI_values<-data.frame(r3_5s_NDVI$NDVI, r3_5s_NDVI$Time )
r3_10s_NDVI_values<-data.frame(r3_10s_NDVI$NDVI, r3_10s_NDVI$Time )




##### adding a column with the grouped and repeat within aids plotting later #####
################################################
r1_5s_NDVI_values <- data.frame(r1_5s_NDVI_values)

r1_5s_NDVI_values <- r1_5s_NDVI_values %>%
  mutate(Duration = "5s", Repeat="1")

colnames(r1_5s_NDVI_values)[colnames(r1_5s_NDVI_values) == "r1_5s_NDVI.NDVI"] <- "NDVI_values"
colnames(r1_5s_NDVI_values)[colnames(r1_5s_NDVI_values) == "r1_5s_NDVI.Time"] <- "Time"



r1_5s_NDVI_values


##### now again for repeat 1 and 10seconds ##### 
################################################
r1_10s_NDVI_values <- data.frame(r1_10s_NDVI_values)


r1_10s_NDVI_values <- r1_10s_NDVI_values %>%
  mutate(Duration = "10s", Repeat = "1")

colnames(r1_10s_NDVI_values)[colnames(r1_10s_NDVI_values) == "r1_10s_NDVI.NDVI"] <- "NDVI_values"
colnames(r1_10s_NDVI_values)[colnames(r1_10s_NDVI_values) == "r1_10s_NDVI.Time"] <- "Time"

r1_10s_NDVI_values




##### now again for repeat 2 and 5seconds ######
################################################
r2_5s_NDVI_values <- data.frame(r2_5s_NDVI_values)

r2_5s_NDVI_values <- r2_5s_NDVI_values %>%
  mutate(Duration = "5s", Repeat = "2")

colnames(r2_5s_NDVI_values)[colnames(r2_5s_NDVI_values) == "r2_5s_NDVI.NDVI"] <- "NDVI_values"
colnames(r2_5s_NDVI_values)[colnames(r2_5s_NDVI_values) == "r2_5s_NDVI.Time"] <- "Time"

r2_5s_NDVI_values



##### now again for repeat 2 and 10s #######
############################################
r2_10s_NDVI_values <- data.frame(r2_10s_NDVI_values)

r2_10s_NDVI_values <- r2_10s_NDVI_values %>%
  mutate(Duration = "10s", Repeat= "2")

colnames(r2_10s_NDVI_values)[colnames(r2_10s_NDVI_values) == "r2_10s_NDVI.NDVI"] <- "NDVI_values"
colnames(r2_10s_NDVI_values)[colnames(r2_10s_NDVI_values) == "r2_10s_NDVI.Time"] <- "Time"

r2_10s_NDVI_values



##### again for repeat 3 5s ##############
##########################################
r3_5s_NDVI_values

r3_5s_NDVI_values <- data.frame(r3_5s_NDVI_values)

r3_5s_NDVI_values <- r3_5s_NDVI_values %>%
  mutate(Duration = "5s", Repeat= "2")

colnames(r3_5s_NDVI_values)[colnames(r3_5s_NDVI_values) == "r3_5s_NDVI.NDVI"] <- "NDVI_values"
colnames(r3_5s_NDVI_values)[colnames(r3_5s_NDVI_values) == "r3_5s_NDVI.Time"] <- "Time"

r3_5s_NDVI_values
#rm(combined_df)




##### Again for repeat and 10s ######
#####################################

r3_10s_NDVI_values <- data.frame(r3_10s_NDVI_values)

r3_10s_NDVI_values <- r3_10s_NDVI_values %>%
  mutate(Duration = "10s", Repeat= "2")

colnames(r3_10s_NDVI_values)[colnames(r3_10s_NDVI_values) == "r3_10s_NDVI.NDVI"] <- "NDVI_values"
colnames(r3_10s_NDVI_values)[colnames(r3_10s_NDVI_values) == "r3_10s_NDVI.Time"] <- "Time"

r3_10s_NDVI_values
#rm(combined_df)




ultimate_df <- rbind(df1, df2, df3, df4, df5, df6)
ultimate_df

ultimate_NDVI_plot <- ggplot(data = ultimate_df, aes(x=Duration, y=NDVI_values, fill=Duration)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#6c1f31", "#0e496c")) + 
  geom_jitter(width = 0.25)


group1_values <- ultimate_df$NDVI_values[ultimate_df$Duration == "5s"]
group2_values <- ultimate_df$NDVI_values[ultimate_df$Duration == "10s"]

t_test_results <- t.test(group1_values, group2_values)
print(t_test_result)

# Remove column names from other data frames
df1 <- r1_5s_NDVI_values
df2 <- r1_10s_NDVI_values
df3 <- r2_5s_NDVI_values
df4 <- r2_10s_NDVI_values
df5 <- r3_5s_NDVI_values
df6 <- r3_10s_NDVI_values

df1
df2
df3
df4
df5
df6



rm(r1_5s_NDVI_values)
rm(r1_10s_NDVI_values)
rm(r2_5s_NDVI_values)
rm(r2_10s_NDVI_values)

colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df5)
colnames(df6)



ncol(df1)
ncol(df2)
ncol(df3)
ncol(df4)
ncol(df5)
ncol(df6)

data_frames <- list(df1, df2, df3, df4)

Repeat_1_combined_df<-rbind(df1, df2)
Repeat_1_combined_df

Repeat_2_combined_df<-rbind(df3, df4)
Repeat_2_combined_df


Repeat_3_combined_df<-rbind(df5, df6)
Repeat_3_combined_df

rm(combined_df)
combined_df<-rbind(df1, df2, df3, df4, df5, df6)
combined_df


NDVI_r1 <- ggplot(data = Repeat_1_combined_df, aes(x=Duration, y=NDVI_values, fill=Duration)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) +
  geom_jitter(width = 0.25)
  
NDVI_r2 <- ggplot(data = Repeat_2_combined_df, aes(x=Duration, y=NDVI_values, fill=Duration)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) +
  geom_jitter(width = 0.25)

NDVI_r3 <- ggplot(data = Repeat_3_combined_df, aes(x=Duration, y=NDVI_values, fill=Duration)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#6c1f31", "#0e496c")) + 
  geom_jitter(width=0.25)


NDVI_r1

ggplot(combined_df, aes(x=Group, y=NDVI_values, fill=Group)) + 
    geom_boxplot() +
    scale_fill_manual(values = c("#00D9CC", "#007D73"))+
    geom_jitter(width=0.25) + facet_grid(~Repeat)
  
NDVI_boxplot_1
  
  
Low_watering <- combined_df$NDVI_values[combined_df$Group == "5s"]
High_watering <- combined_df$NDVI_values[combined_df$Group == "10s"]

t_test_result<-t.test(Low_watering, High_watering)
t_test_result


#Get_Data for response1

# Perform the two-way ANOVA
anova_results <- aov(cbind(Response1, Response2) ~ Group, data = combined_df)

# Summarize the ANOVA results
summary(anova_results)

# If ANOVA results are significant, perform post hoc tests (e.g., Tukey's HSD)
# posthoc <- TukeyHSD(anova_results)
# print(posthoc)

anova_results_NDVI <- aov(NDVI_values ~ Group + Error(Time/Group), data = combined_df)
summary(anova_results_NDVI)

Final_Harvests
anova_results_harvests<-aov(Mass_g ~ Group + Error(Date/Group), data=Final_Harvests)
anova_results_harvests
colnames(combined_df)

# Load necessary packages


# Assuming your data frame is named 'data'
# IndependentVariable: The factor you're manipulating
# DependentVariable1 and DependentVariable2: The two dependent variables

# Create a matrix of dependent variables
merged_data <- merge(combined_df, Final_Harvests, by = "Group")
manova_results <- manova(cbind(NDVI_values, Mass_g) ~ Group, data = merged_data)
manova_results

dependent_vars <- cbind(combined_df$NDVI_values, Final_Harvests$Mass_g)


#######################################################
##### MANOVA ON EFFECT ON COMBINED NDVI-VARIABLES #####
#######################################################

##### PREVIOUS STATS STATE HARVEST AND ABSORBANCE ALONE ISN'T STATSISTICALLY SIGNIFICANT ######

manova_results <- manova(dependent_vars ~ IndependentVariable, data = data)
summary(manova_results)



combined_df <- cbind(r1_5s_NDVI_values, r1_10s_NDVI_values, r2_5s_NDVI_values, r2_10s_NDVI_values)
combined_df

combined_df <- rbind(df1, df2, df3, df4)
combined_df 




#################################################################
####### IMPORTING REFLECTANCE DATA - ANALYSIS AND PLOTTING ######
#################################################################


reflectance_r1_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Reflectance\\Final_reflectance_data_r1_5s.xlsx")
reflectance_r1_5s <- data.frame(reflectance_r1_5s)
reflectance_r1_5s
nrow(reflectance_r1_5s)

reflectance_r1_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Reflectance\\Final_reflectance_data_r1_10s.xlsx")  
reflectance_r1_10s <- data.frame(reflectance_r1_10s)
colnames(reflectance_r1_10s)

r1_reflectance<-cbind(reflectance_r1_5s, reflectance_r1_10s)

reflectance_r2_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Reflectance\\Finalised_reflectance_data_r2_5s.xlsx")
reflectance_r2_5s <- data.frame(reflectance_r2_5s)
.
reflectance_r2_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_2\\Reflectance\\Finalised_reflectance_data_r2_10s.xlsx")  
reflectance_r2_10s <- data.frame(reflectance_r2_10s)



reflectance_r3_5s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Reflectance\\Finalised_reflectance_data_r3_5s.xlsx")
reflectance_r3_5s <- data.frame(reflectance_r3_5s)

reflectance_r3_10s<-read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_3\\Reflectance\\Finalised_reflectance_data_r3_10s.xlsx")
reflectance_r3_10s <- data.frame(reflectance_r3_10s)

#checking for double names using colnames 
print(reflectance_r1_5s)

reflectance_r1_5s$...1 <- NULL
reflectance_r1_5s$Color...5 <- NULL 

ggplot(data = reflectance_r1_5, aes( x= Wavelength)

       
reflectance_r2_10s       
       
###############################################################
######## CREATING A NEW DATAFRAME WITH ALL REFLECTANCES #######
###############################################################

mean_reflectance_r1_5s <- reflectance_r1_5s %>%
  group_by(Wavelength) %>%
  summarize(Reflectance_mean_r1_5s = mean(Reflectance))


mean_reflectance_r1_10s <- reflectance_r1_10s %>%
  group_by(Wavelength) %>%
  summarize(Reflectance_mean_r1_10s = mean(Reflectance))

combined_r1_ref<-cbind(mean_reflectance_r1_10s, mean_reflectance_r1_5s)

colnames(combined_r1_ref)
combined_r1_ref

combined_r1_ref<-subset(combined_r1_ref, select = -Wavelength)

ref_r1_plot<-ggplot(data = combined_r1_ref, aes(x = Wavelength)) +
  geom_point(aes(y = Reflectance_mean_r1_5s, color = "Duration: 5 seconds"), size = 1.5) +
  geom_point(aes(y = Reflectance_mean_r1_10s, color = "Duration: 10 seconds"), size = 1.5) +
  scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c")) + 
  ylim(97.5, 99.5)+
  labs(color = "Watering duration / 0.5 hour",
       y = "Percentage of light reflected (%)",
       x = "Wavelength of light (nm)",
       title = "Fig 1.a: Repeat 1 light reflectance")

  
ref_r1_plot
  
  mean_reflectance_r2_5s <- reflectance_r2_5s %>%
    group_by(Wavelength) %>%
    summarize(Reflectance_mean_r2_5s = mean(Reflectance))
  
  
  mean_reflectance_r2_10s <- reflectance_r2_10s %>%
    group_by(Wavelength) %>%
    summarize(Reflectance_mean_r2_10s = mean(Reflectance))
  
  combined_r2_ref<-cbind(mean_reflectance_r2_10s, mean_reflectance_r2_5s)
  
  colnames(combined_r2_ref)
  combined_r2_ref = subset(combined_r2_ref, select = -Wavelength)
  combined_r2_ref
  
  total_data_points<-length(combined_r2_ref$Reflectance_mean_r2_5s)
  total_data_points
  
  
ref_r2_plot<-ggplot(data=combined_r2_ref, aes(x = Wavelength)) +
    geom_point(aes(y=Reflectance_mean_r2_5s, color="Duration: 5 seconds"), size=1.5) +
    geom_point(aes(y=Reflectance_mean_r2_10s, color="Duration: 10 seconds"), size =1.5) + 
    scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c")) + 
    ylim(97.5, 99.5) +
    labs(color="Watering duration / 0.5 hour")+
    ylab("Percentage of light reflected (%)") +
    xlab("Wavelength of light (nm)") + 
    ggtitle("Fig 1.b: Repeat 2 light reflectance")
  
ref_r2_plot  

labs(color = "Legend Title")
  #labs(x = "Wavelength", y = "Mean Reflectance") +
  #ggtitle("Mean Reflectance per Wavelength")
  
ref_r2_plot  
  
  
  
  mean_reflectance_r3_5s <- reflectance_r3_5s %>%
    group_by(Wavelength) %>%
    summarize(Reflectance_mean_r3_5s = mean(Reflectance))
  
  
  mean_reflectance_r3_10s <- reflectance_r3_10s %>%
    group_by(Wavelength) %>%
    summarize(Reflectance_mean_r3_10s = mean(Reflectance))
  
  combined_r3_ref<-cbind(mean_reflectance_r3_10s, mean_reflectance_r3_5s)
  
  colnames(combined_r3_ref)
  combined_r3_ref = subset(combined_r3_ref, select = -Wavelength)
  combined_r3_ref
  
  total_data_points<-length(combined_r3_ref$Reflectance_mean_r3_5s)
  total_data_points
  
  
  ref_r3_plot<-ggplot(data=combined_r3_ref, aes(x = Wavelength)) +
    geom_point(aes(y=Reflectance_mean_r3_5s, color="Duration: 5 seconds"), size=1.5) +
    geom_point(aes(y=Reflectance_mean_r3_10s, color="Duration: 10 seconds"), size =1.5) + 
    scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c")) + 
    ylim(97.5, 99.5)+
    labs(color="Watering duration / 0.5 hour")+
    ylab("Percentage of light reflected (%)") +
    xlab("Wavelength of light (nm)") + 
    ggtitle("Fig 1.c: Repeat 3 light reflectance")
  
ref_r3_plot
  
  labs(color = "Legend Title")
  #labs(x = "Wavelength", y = "Mean Reflectance") +
  #ggtitle("Mean Reflectance per Wavelength")
  

  colnames(combined_r2_ref) <- colnames(combined_r1_ref)
  colnames(combined_r3_ref) <- colnames(combined_r1_ref)

combined_ref<-rbind(combined_r1_ref, combined_r2_ref, combined_r3_ref)

combined_mean_ref<-mean(combined_ref)
colnames(combined_ref)


ref_10s_mean <- combined_ref %>%
  group_by(Wavelength) %>%
  summarize(Mean_10s = mean(Reflectance_mean_r1_10s))


ref_5s_mean <- combined_ref %>%
  group_by(Wavelength) %>%
  summarize(Mean_5s = mean(Reflectance_mean_r1_5s))

combined_ref<-cbind(ref_5s_mean, ref_10s_mean)

combined_ref = subset(combined_ref, select=-Wavelength)
colnames(combined_ref)

all_ref_plot<-ggplot(data = combined_ref, aes(x=Wavelength)) + 
  geom_point(aes(y=Mean_5s, color = "Duration: 5 seconds"), size=1.25) + 
  geom_point(aes(y=Mean_10s, color = "Duration: 10 seconds"), size=1.25) + 
  scale_color_manual(values=c( scale_color_manual(values = c("Duration: 5 seconds" = "#6c1f31", "Duration: 10 seconds" = "#0e496c"))
  
#  
colnames(combined_r1_ref)
colnames(combined_r1_ref)[colnames(combined_r1_ref) == "Reflectance_mean_r1_10s"] <- "10_seconds"
colnames(combined_r1_ref)[colnames(combined_r1_ref) == "Reflectance_mean_r1_5s"] <- "5_seconds"
combined_r1_ref


colnames(combined_r2_ref)
colnames(combined_r2_ref)[colnames(combined_r2_ref) == "Reflectance_mean_r2_10s"] <- "10_seconds"
colnames(combined_r2_ref)[colnames(combined_r2_ref) == "Reflectance_mean_r2_5s"] <- "5_seconds"


colnames(combined_r3_ref)
colnames(combined_r3_ref)[colnames(combined_r3_ref) == "Reflectance_mean_r3_10s"] <- "10_seconds"
colnames(combined_r3_ref)[colnames(combined_r3_ref) == "Reflectance_mean_r3_5s"] <- "5_seconds"



all_reflectances<-rbind(combined_r1_ref, combined_r2_ref, combined_r3_ref)

all_reflectances<-data.frame(all_reflectances)

#ggplot(data = all_reflectances, aes(x= Wavelength)) + 
#  geom_point(aes(y='5_seconds'), color = "Duration: 5 seconsd", size=1.5) + 
#  geom_point(aes(y='10_seconds'), color = "Duration: 10 seconds", size =1.5) +
#  scale_color_manual(values=c("Duration: 10 seconds" = "#6c1f31", "Duration: 5 seconds" = "#0e496c"))
  

#ggplot(data = all_reflectances, aes(x= Wavelength)) + 
#  geom_point(aes(y='5_seconds'), color = "Duration: 5 seconsd"), size=1.5)) + 
#  geom_point(aes(y='10_seconds'), color = "Duration: 10 seconds"), size =1.5)) +
#  scale_color_manual(values=c("Duration: 10 seconds" = "#6c1f31", "Duration: 5 seconds" = "#0e496c"))




############################################################
#######PLOTTING THE NDVI VALUES AND CALCULATING THEM #######
############################################################


#working out means for each reflectance valuee at each time point
r1_5s_red_band <- subset(reflectance_r1_5s, Color == "Red")
r1_5s_red_band_mean <- aggregate(Reflectance ~ Time, data = r1_5s_red_band, FUN = mean)
r1_5s_red_band_mean <- data.frame(r1_5s_red_band_mean)
r1_5s_red_band_mean

ggbostplo

#working out means for each reflectance value at each time point
r1_5s_nir_band <- subset(reflectance_r1_5s, Color == "IR")
r1_5s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r1_5s_nir_band, FUN = mean)
r1_5s_nir_band_mean <- data.frame(r1_5s_nir_band_mean)
r1_5s_nir_band_mean


#try thinking the means for each time- this might remove negative values seen in the dataframe
  r1_5s_ndvi <- (r1_5s_nir_band_mean$Reflectance - r1_5s_red_band_mean$Reflectance) / (r1_5s_nir_band_mean$Reflectance + r1_5s_red_band_mean$Reflectance)
r1_5s_ndvi <- data.frame(r1_5s_ndvi)
r1_5s_ndvi


#working out means for each reflectance valuee at each time point
r1_10s_red_band <- subset(reflectance_r1_10s, Color == "Red")
r1_10s_red_band_mean <- aggregate(Reflectance ~ Time, data = r1_10s_red_band, FUN = mean)
r1_10s_red_band_mean <- data.frame(r1_10s_red_band_mean)
r1_10s_red_band_mean
r1_10s_red_band_mean

#working out means for each reflectance value at each time point
r1_10s_nir_band <- subset(reflectance_r1_10s, Color == "IR")
r1_10s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r1_10s_nir_band, FUN = mean)
r1_10s_nir_band_mean <- data.frame(r1_10s_nir_band_mean)
r1_10s_nir_band_mean
r1_10s_nir_band_mean

#try thinking the means for each time- this might remove negative values seen in the dataframe
r1_10s_ndvi <- (r1_10s_nir_band_mean$Reflectance - r1_10s_red_band_mean$Reflectance) / (r1_10s_nir_band_mean$Reflectance + r1_10s_red_band_mean$Reflectance)
r1_10s_ndvi <- data.frame(r1_10s_ndvi)
r1_10s_ndvi


r1_5s_ndvi <- r1_5s_ndvi %>%
  mutate(Label = "5s")

r1_5s_ndvi

colnames(r1_5s_ndvi)[colnames(r1_5s_ndvi) == "r1_5s_ndvi"] <- "ndvi_value"

r1_10s_ndvi <- r1_10s_ndvi %>%
  mutate(Label = "10s")

r1_10s_ndvi

colnames(r1_10s_ndvi)[colnames(r1_10s_ndvi) == "r1_10s_ndvi"] <- "ndvi_value"


# Create a geom boxplot
ggplot(data = df, aes(x = Variable, y = Value)) +
  geom_boxplot()


r1_combined_ndvi<-rbind(r1_10s_ndvi, r1_5s_ndvi)
r1_combined_ndvi$ndvi_value <- r1_combined_ndvi$ndvi_value*100
r1_combined_ndvi


r1_combined_ndvi$Label <- factor(r1_combined_ndvi$Label, levels = c("5s", "10s"))



r1_ndvi_plot<-ggplot(data=r1_combined_ndvi, aes(x=Label, y=ndvi_value, fill=Label)) + 
  geom_boxplot() +
  ylim(0,1)+
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) +
  labs(fill="Watering duration / 0.5 hour") +
  geom_jitter(width=0.2) +
  xlab("Wavelength of light (nm)") + 
  ggtitle("Fig 2.a: Repeat 1 NDVI values")

write.xlsx(, "C:\\Users\\HP EliteBook\\Documents\\ndvi_values.xlsx")


r1_ndvi_plot



colnames(r1_5s_ndvi) <- c("Watering_duration", "ndvi")

colnames(r1_5s_ndvi)

colnames(r1_10s_ndvi) <- c("Watering_duration", "ndvi")


r1_combined_ndvi<-rbind(r1_5s_ndvi, r1_10s_ndvi)
r1_combined_ndvi<-data.frame(r1_combined_ndvi)
r1_combined_ndvi

ref_1_ndvi <- pivot_longer(ref_r1_combined_ndvi, cols = starts_with("r1_"), names_to = "Watering_duration", values_to = "ndvi")
ref_1_ndvi <- data.frame(ref_1_ndvi)
ref_1_ndvi

ggplot(data = ref_1_ndvi, aes(x = Watering_duration, y=ndvi)) + geom_boxplot()



r1_5s_ndvi <- subset(r1_5s_ndvi, r1_5s_ndvi >= 0)
boxplot(r1_5s_ndvi , x = r1_5s_ndvi)


##### Adding a column with the canal number incase incedents difference in glm 








                                                                     ######################################################
                                                                     ######## NDVI PLOTS FOR EACH second REPEAT NOW ##############
                                                                    #######################################################

#working out means for each reflectance valuee at each time point
r2_5s_red_band <- subset(reflectance_r2_5s, Color == "Red")
r2_5s_red_band_mean <- aggregate(Reflectance ~ Time, data = r2_5s_red_band, FUN = mean)
r2_5s_red_band_mean <- data.frame(r2_5s_red_band_mean)
r2_5s_red_band_mean

ggbostplo

#working out means for each reflectance value at each time point
r2_5s_nir_band <- subset(reflectance_r2_5s, Color == "IR")
r2_5s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r2_5s_nir_band, FUN = mean)
r2_5s_nir_band_mean <- data.frame(r2_5s_nir_band_mean)
r2_5s_nir_band_mean


#try thinking the means for each time- this might remove negative values seen in the dataframe
r2_5s_ndvi <- (r2_5s_nir_band_mean$Reflectance - r2_5s_red_band_mean$Reflectance) / (r2_5s_nir_band_mean$Reflectance + r2_5s_red_band_mean$Reflectance)
r2_5s_ndvi <- data.frame(r2_5s_ndvi)
r2_5s_ndvi


#working out means for each reflectance valuee at each time point
r2_10s_red_band <- subset(reflectance_r2_10s, Color == "Red")
r2_10s_red_band_mean <- aggregate(Reflectance ~ Time, data = r2_10s_red_band, FUN = mean)
r2_10s_red_band_mean <- data.frame(r2_10s_red_band_mean)
r2_10s_red_band_mean
nrow(r2_10s_red_band_mean)

#working out means for each reflectance value at each time point
r2_10s_nir_band <- subset(reflectance_r2_10s, Color == "IR")
r2_10s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r2_10s_nir_band, FUN = mean)
r2_10s_nir_band_mean <- data.frame(r2_10s_nir_band_mean)
r2_10s_nir_band_mean
nrow(r2_10s_nir_band_mean)


#try thinking the means for each time- this might remove negative values seen in the dataframe
r2_10s_ndvi <- (r2_10s_nir_band_mean$Reflectance - r2_10s_red_band_mean$Reflectance) / (r2_10s_nir_band_mean$Reflectance + r2_10s_red_band_mean$Reflectance)
r2_10s_ndvi <- data.frame(r2_10s_ndvi)
r2_10s_ndvi

r2_5s_ndvi <- r2_5s_ndvi %>%
  mutate(Label = "5s")

r2_5s_ndvi

colnames(r2_5s_ndvi)[colnames(r2_5s_ndvi) == "r2_5s_ndvi"] <- "ndvi_value"

r2_10s_ndvi <- r2_10s_ndvi %>%
  mutate(Label = "10s")

r2_10s_ndvi

colnames(r2_10s_ndvi)[colnames(r2_10s_ndvi) == "r2_10s_ndvi"] <- "ndvi_value"

r2_combined_ndvi <- rbind(r2_10s_ndvi, r2_5s_ndvi)

r2_combined_ndvi$ndvi_value <- r2_combined_ndvi$ndvi_value*100
#r2_combined_ndvi

r2_combined_ndvi$Label <- factor(r2_combined_ndvi$Label, levels = c("5s", "10s"))


r2_ndvi_plot<-ggplot(data=r2_combined_ndvi, aes(x=Label, y=ndvi_value, fill=Label)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) +
  labs(fill="Watering duration / 0.5 hour") +
  geom_jitter(width=0.2) +
  xlab("Wavelength of light (nm)") + 
  ggtitle("Fig 2.b: Repeat 2 NDVI values") +
  ylab("NDVI value")





r2_ndvi_plot

r2_combined_ndvi
                                                          ######################################################
                                                          ######## NDVI PLOTS FOR EACH second REPEAT NOW ##############
                                                          #######################################################


#working out means for each reflectance valuee at each time point
r3_5s_red_band <- subset(reflectance_r3_5s, Color == "Red")
r3_5s_red_band_mean <- aggregate(Reflectance ~ Time, data = r3_5s_red_band, FUN = mean)
r3_5s_red_band_mean <- data.frame(r3_5s_red_band_mean)
r3_5s_red_band_mean

ggbostplo

#working out means for each reflectance value at each time point
r3_5s_nir_band <- subset(reflectance_r3_5s, Color == "IR")
r3_5s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r3_5s_nir_band, FUN = mean)
r3_5s_nir_band_mean <- data.frame(r3_5s_nir_band_mean)
r3_5s_nir_band_mean


#try thinking the means for each time- this might remove negative values seen in the dataframe
r3_5s_ndvi <- (r3_5s_nir_band_mean$Reflectance - r3_5s_red_band_mean$Reflectance) / (r3_5s_nir_band_mean$Reflectance + r3_5s_red_band_mean$Reflectance)
r3_5s_ndvi <- data.frame(r3_5s_ndvi)
r3_5s_ndvi


#working out means for each reflectance valuee at each time point
r3_10s_red_band <- subset(reflectance_r3_10s, Color == "Red")
r3_10s_red_band_mean <- aggregate(Reflectance ~ Time, data = r3_10s_red_band, FUN = mean)
r3_10s_red_band_mean <- data.frame(r3_10s_red_band_mean)
r3_10s_red_band_mean
r3_10s_red_band_mean

#working out means for each reflectance value at each time point
r3_10s_nir_band <- subset(reflectance_r3_10s, Color == "IR")
r3_10s_nir_band_mean <- aggregate(Reflectance ~ Time, data = r3_10s_nir_band, FUN = mean)
r3_10s_nir_band_mean <- data.frame(r3_10s_nir_band_mean)
r3_10s_nir_band_mean
r3_10s_nir_band_mean

#try thinking the means for each time- this might remove negative values seen in the dataframe
r3_10s_ndvi <- (r3_10s_nir_band_mean$Reflectance - r3_10s_red_band_mean$Reflectance) / (r3_10s_nir_band_mean$Reflectance + r3_10s_red_band_mean$Reflectance)
r3_10s_ndvi <- data.frame(r3_10s_ndvi)
r3_10s_ndvi


r3_5s_ndvi <- r3_5s_ndvi %>%
  mutate(Label = "5s")

r3_5s_ndvi$Canal<-2

colnames(r3_5s_ndvi)[colnames(r3_5s_ndvi) == "r3_5s_ndvi"] <- "ndvi_value"

r3_10s_ndvi <- r3_10s_ndvi %>%
  mutate(Label = "10s")

r3_5s_ndvi = subset(r3_5s_ndvi, select=-Canal)

r3_5s_ndvi

colnames(r3_10s_ndvi)[colnames(r3_10s_ndvi) == "r3_10s_ndvi"] <- "ndvi_value"


colnames(r3_5s_ndvi)



r3_combined_ndvi <- rbind(r3_10s_ndvi, r3_5s_ndvi)

r3_combined_ndvi$ndvi_value <- r3_combined_ndvi$ndvi_value*100

r3_combined_ndvi$Label <- factor(r3_combined_ndvi$Label, levels = c("5s", "10s"))

library(tidyverse)
r3_ndvi_plot<-ggplot(data=r3_combined_ndvi, aes(x=Label, y=ndvi_value, fill=Label)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) +
  labs(fill="Watering duration / 0.5 hour") +
  ylim(0,1) + 
  geom_jitter(width=0.2) +
  xlab("Wavelength of light (nm)") + 
  ggtitle("Fig 2.c: Repeat 3 NDVI values") + 
  theme(axis.text.y = element_blank())




r3_combined_ndvi
mean(r3_combined_ndvi$ndvi_value)

r2_combined_ndvi

all_ndvi_data

colnames(r1_combined_ndvi)

all_ndvi_data<-rbind(r3_combined_ndvi, r2_combined_ndvi, r1_combined_ndvi)
nrow(all_ndvi_data)
all_ndvi_data
all_ndvi_data<-data.frame(all_ndvi_data)

out_of_range <- df$ndvi_value < 0 | df$ndvi_value > 1 | is.na(df$ndvi_value)
df$ndvi_value[out_of_range] <- NA
all_ndvi_data

df$ndvi_value <- pmin(pmax(df$ndvi_value, 0), 1)


colnames(all_ndvi_data)[colnames(all_ndvi_data) == "label"] <- "NDVI_VALUE"

rm(all_ndvi_data)


library(openxlsx)

NDVIa$ndvi_value <- ifelse(NDVI$ndvi_value < 0, 0, ifelse(NDVI$ndvi_value > 1, 1, NDVI$ndvi_value))
NDVI

NDVI$Watering_regime <- factor(NDVI$Watering_regime, levels = c("1", "2"))

colnames(NDVI)

total_ndvi<-ggplot(data=NDVI, aes(x=Watering_regime, y=Ndvi_value, fill=Watering_regime)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#6c1f31", "#0e496c")) + 
  geom_jitter(width=0.2) +
  ylim(0,1) + 
  ylab("NDVI values") +
  xlab("Watering regime") + 
  ggtitle("Condtion + Repeat: NDVI value")+
  labs(fill = "Watering regime") + 
  facet_wrap(~Repeat)+ 
  theme(axis.text.y = element_blank())
  
  
NDVI$Watering_regime<-as.factor(NDVI$Watering_regime)

total_ndvi

colnames(r1_combined_ndvi)  
colnames(r1_combined_ndvi) <- c("ndvi_value", "Label")



  
all_ndvi_data  

all_ndvi_data <- all_ndvi_data[all_ndvi_data$ndvi_value >= 0.1, ]

  
  
write.xlsx(all_ndvi_data, "C:\\Users\\HP EliteBook\\Documents\\ndvi_values.xlsx")
  

total_ndvi

all_ndvi_data$ndvi_value<-all_ndvi_data$ndvi_value[all_ndvi_data$ndvi_value >=0 ]
all_ndvi_data$ndvi_value <- all_ndvi_data$ndvi_value[all_ndvi_data$ndvi_value >= 0]


t_test<-t.test(ndvi_value~Label, data=all_ndvi_data)
t_test

summary_data <- all_ndvi_data %>%
  group_by(Label) %>%
  summarise(
    Mean_ndvi = mean(ndvi_value),
    SD_ndvi = sd(ndvi_value)
  )



summary_data

all_ndvi_data

all_ndvi_data$canal <- ifelse(all_ndvi_data$Label == "5s", "canal_2",
                   ifelse(all_ndvi_data$Label == "10s", "canal_5", NA))


colnames(all_ndvi_data)

library(nlme)

model <- lme(ndvi_value ~ Label + canal, data = all_ndvi_data, random = ~1 | canal)


install.packages("gridExtra")
library(gridExtra)
grid.arrange(ref_r1_plot, ref_r2_plot, ref_r3_plot, r1_ndvi_plot, r2_ndvi_plot, r3_ndvi_plot, ncol = 3, nrow=3)
ref_r1_plot

grid.arrange(ref_r1_plot, r1_ndvi_plot, repeat_1, ncol = 1, nrow=3)
grid.arrange(ref_r1_plot, r1_ndvi_plot, repeat_1, ncol=1, nro1=3)
###########################################################
################## GLME model on data ndvi ################
###########################################################
d 'df'
# Fit a linear mixed-effects model
model <- lmer(ndvi_value ~ Label + Canal + (1 | _10s_ndvi), data = r3_ndvi_plot)

# View the model summary
summary(model)

install.packages




library(nlme)



###########################################################
############ removing column_1 ############################
###########################################################

trans_no_col_1 <- read_excel("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Transmittance\\Transmittance_Repeat_1_10s_post_edit_rm_col_1.xlsx")

abs_no_col_1 <- read_excel ("C:\\Users\\HP EliteBook\\Documents\\pak_choi_pp\\Updated\\Repeat_1\\Absorbance\\Absorbance_Repeat_1_10s_post_edit_rm_col_1.xlsx")


trans_df <- data.frame(trans_no_col_1)
abs_no_col_1 <- data.frame(abs_no_col_1)
# Load the required packages
install.packages("lme4")
library(lme4)

repeat_1


# Assuming your data is in a data frame calle

df <- cbind(trans_df, abs_no_col_1)
df <- data.frame(df)

colnames(df)

trans_df

r1_10s_band <- subset(trans_df, Color == "Red")
r1_10s_band_mean <- aggregate(Re)


trans_no_col_1


################################################################
########### MULTIVARIATE ANALYSIS - MANOVA #####################
###############################################################

repeat_1<-repeat_1 + ylim(-0.75, 0.75)
repeat_1

