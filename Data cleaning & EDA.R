#Describe datasets

##### DATA PREP #####

setwd("~/Documents/My Documents/University of Bath/Year 4/Semester 2/Dissertation")
data_1 <- read.csv("FinalData.csv",stringsAsFactors = TRUE)
tapping_data <- read.csv("TappingData.csv",stringsAsFactors = TRUE)

# Removing duplicate subjects and NA values
tapping_data = tapping_data[!duplicated(tapping_data$SubjectNum),]
tapping_data = na.omit(tapping_data)

#Exploring data
head(data_1)
tail(data_1)
dim(data_1)
str(data_1)
summary(is.na(data_1))

#Recoding subject number as a factor 
data_1$SubjectNum = as.factor(data_1$SubjectNum)
length(levels(data_1$SubjectNum)) #total subjects: 117

#Load packages
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("car")) {install.packages("car"); require("car")}
if (!require("tidyr")) {install.packages("tidyr"); require("tidyr")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("rstatix")) {install.packages("rstatix"); require("rstatix")}
if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
if (!require("datarium")) {install.packages("datarium"); require("datarium")}
if (!require("lmerTest")) {install.packages("lmerTest"); require("lmerTest")}
if (!require("hunspell")) {install.packages("hunspell"); require("hunspell")}
if (!require("devtools")) {install.packages("devtools"); require("devtools")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("pastecs")) {install.packages("pastecs"); require("pastecs")}
if (!require("DHARMa")) {install.packages("DHARMa"); require("DHARMa")}
if (!require("MVN")) {install.packages("MVN"); require("MVN")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}
if (!require("performance")) {install.packages("performance"); require("performance")}
if (!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}
if (!require("olsrr")) {install.packages("olsrr"); require("olsrr")}

#Fixing gender
data_1$Gender = as.character(data_1$Gender)
data_1$Gender[data_1$Gender== "1,2"] = 3
data_1$Gender[data_1$Gender== "4"] = 3
data_1$Gender = as.numeric(data_1$Gender)


#Spell check & spelling corrections
data_1$InputDepunct = as.character(data_1$InputDepunct)
data_1$InputDepunct <- trimws(data_1$InputDepunct, which = c("left"))
data_1$InputDepunct <- trimws(data_1$InputDepunct, which = c("right"))
data_1$InputDepunct <- tolower(data_1$InputDepunct)
data_1$InputDepunct <- str_replace_all(data_1$InputDepunct, "[[:punct:]]", "")
data_1$InputDepunct <- str_replace_all(data_1$InputDepunct, " ", "")
data_1$CorrectDepunct = as.character(data_1$CorrectDepunct)
data_1$CorrectDepunct <- trimws(data_1$CorrectDepunct, which = c("left"))
data_1$CorrectDepunct <- trimws(data_1$CorrectDepunct, which = c("right"))
data_1$CorrectDepunct <- tolower(data_1$CorrectDepunct)
data_1$CorrectDepunct <- str_replace_all(data_1$CorrectDepunct, "[[:punct:]]", "")
data_1$CorrectDepunct <- str_replace_all(data_1$CorrectDepunct, " ", "")

data_1$acc_check = 0
data_1$acc_check[data_1$InputDepunct == data_1$CorrectDepunct] = 1

#Check for remaining mispelled words
incorrect = as.data.frame(hunspell_check(data_1$InputDepunct))
names(incorrect)[names(incorrect) == 'hunspell_check(data_1$InputDepunct)'] = 'spell_check'

data_1$spell_check = hunspell_check(data_1$InputDepunct)

spell_check_df = data_1 %>%
  select(SubjectNum, Item, InputDepunct, CorrectDepunct, spell_check) %>%
  filter(spell_check == "FALSE") 

suggest = hunspell_suggest(spell_check_df$InputDepunct)

#Append suggest as column to spell_check_df
spell_check_df$suggest = as.character(suggest)

#Check if CorrectDepunct in 'suggest'
spell_check_df$correct = str_detect(spell_check_df$suggest, spell_check_df$CorrectDepunct)

#Merge into data_1
spell_check_df = spell_check_df %>%
  filter(correct == "TRUE") %>%
  select(SubjectNum, Item, correct)

final_data = merge(data_1, spell_check_df, by = c("SubjectNum", "Item"), all.x=TRUE)

#Creating new column for final accuracy scores
final_data$FinalAcc = 0

final_data$FinalAcc[final_data$AutAcc == 1 |
                      final_data$Acc == 1 |
                      final_data$acc_check == 1 |
                      final_data$correct == "TRUE"] = 1

#Joining main data with tapping data
final_data = merge(final_data, tapping_data, by = "SubjectNum")

#Getting reaction time (RT) in seconds
final_data = final_data %>%
  mutate(RT_s = RT/1000) %>%
  mutate(RT_write_s = RT_write/1000)


#Creating new summary df for analysis
data_2 = final_data %>%
  group_by(SubjectNum, Cloze, Group) %>%
  summarise(Accuracy = (sum(FinalAcc)/22), 
            VST = mean(VST_Score), 
            AQ = mean(AQ_Score), 
            VST_16 = mean(VST_Score_16),
            Age = mean(Age),
            Gender = mean(Gender),
            RT = mean(RT_s),
            RT_write = mean(RT_write_s),
            t1.slower = mean(t1.slower),
            t2.slower = mean(t2.slower),
            t3.slower = mean(t3.slower),
            t1_t2_diff.slower = mean(PCR1.slower),
            t0_diff.slower = abs(t1.slower-500),
            t1.faster = mean(t1.faster),
            t2.faster = mean(t2.faster),
            t3.faster = mean(t3.faster),
            t1_t2_diff.faster = mean(PCR1.faster),
            t0_diff.faster = abs(t1.faster-600))

#Creating new column with average MET score
data_2 = data_2 %>%
  mutate(MET = (t0_diff.slower + t0_diff.faster)/2)

#Creating summary of gender by subject group
gender_summary = data_2%>%
  group_by(Group) %>%
  get_summary_stats(Gender, type = "full")

#Remove subjects with >0.25 in low cloze condition & <8 in VST_16 as per exclusions defined in pre-registration
subjects_to_keep = data_2 %>%
  filter(Cloze == "Low" & Accuracy < 0.20) %>%
  filter(VST_16 > 8) 

subjects_to_keep = subjects_to_keep$SubjectNum
subjects_to_keep = droplevels(subjects_to_keep)
length(subjects_to_keep) 

#Creating new summary df only without excluded subjects
data_3 = filter(data_2, SubjectNum %in% subjects_to_keep)

data_3$SubjectNum = droplevels(data_3$SubjectNum)

length(levels(data_3$SubjectNum)) 

#Removing subjects to exclude from complete dataset - final_data df
final_data = filter(final_data, SubjectNum %in% subjects_to_keep)
final_data$RT = as.numeric(final_data$RT)

#Removing RT outliers
max_RT_full = mean(final_data$RT) + (2*sd(final_data$RT))
min_RT_full = mean(final_data$RT) - (2*sd(final_data$RT))

max_RT_write_full = mean(final_data$RT_write) + (2*sd(final_data$RT_write))

#Removing low cloze condition from RT data - not itnerested in RT data for low cloze condition
RT_data_full = final_data %>%
  filter(Cloze != "Low")

RT_data_full = RT_data_full %>%
  filter(RT < max_RT_full)

#Getting RT only for correct trials
RT_data_correct = RT_data_full %>%
  filter(FinalAcc == 1)

RT_data_correct_2 = RT_data_correct %>%
  filter(RT < max_RT_full)

check = anti_join(RT_data_correct, RT_data_correct_2, by=c("SubjectNum", "Item"))

  sqldf('SELECT * FROM RT_data_correct EXCEPT SELECT * FROM RT_data_correct_2')

##### Full RT Analysis #####
RT_model_1 <- lmer(RT ~ Cloze * Group + (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                 data= RT_data_full) #Does not converge

RT_model_2 <- lmer(RT ~ Cloze * Group + (1 + Cloze | SubjectNum) + (1 + Cloze | Item),
                data= RT_data_full) #Does not converge

RT_model_3 <- lmer(RT ~ Cloze * Group + (1 + Cloze | SubjectNum) + (1 | Item),
                 data= RT_data_full) #Singular but converges
summary(model_3) 

#Creating new DF grouped by cloze condition and subject group
summary_data = data_3 %>%
  group_by(Cloze, Group) %>%
  summarise(Accuracy = mean(Accuracy))

##### MET SUMMARY STATS & GRAPHS
MET_summary_stats = data_3 %>%
  group_by(Group) %>%
  get_summary_stats(MET, type = "mean_sd")

#MET boxplot
ggboxplot(data_3, x = "Group", y = "MET", palette = NULL)

#MET Comparison
MET_data = data_3 %>% 
  group_by(SubjectNum) %>% 
  summarise(MET = mean(MET), Group = Group) %>% 
  distinct

t.test(MET_data$MET ~ MET_data$Group, alternative = "two.sided", var.equal = TRUE)

##### AQ SUMMARY STATS & GRAPHS #####
AQ_summary_stats = data_3 %>%
  group_by(Group) %>%
  get_summary_stats(AQ, type = "mean_sd")

#AQ boxplot
ggboxplot(data_3, x = "Group", y = "AQ", palette = NULL)

#AQ Comparison
AQ_data = data_3 %>% 
  group_by(SubjectNum) %>% 
  summarise(AQ = mean(AQ), Group = Group) %>% 
  distinct

t.test(AQ_data$AQ ~ AQ_data$Group, alternative = "two.sided", var.equal = TRUE) #Significant difference in AQ - ASD is higher

##### VST SUMMARY STATS & GRAPHS #####
VST_summary_stats = data_3 %>%
  group_by(Group) %>%
  get_summary_stats(VST, type = "mean_sd")

#VST boxplot
ggboxplot(data_3, x = "Group", y = "VST", palette = NULL)

#VST Comparison
VST_data = data_3 %>% 
  group_by(SubjectNum) %>% 
  summarise(VST = mean(VST), Group = Group) %>% 
  distinct

t.test(VST_data$VST ~ VST_data$Group, alternative = "two.sided", var.equal = TRUE) #Significant difference in VST - ASD is higher

##### AGE SUMMARY STATS & GRAPHS #####
Age_summary_stats = data_3 %>%
  group_by(Group) %>%
  get_summary_stats(Age, type = "mean_sd")

#Age boxplot
ggboxplot(data_3, x = "Group", y = "Age", palette = NULL)

ggplot(data_3, aes(x=Age, fill = Group)) + 
  geom_histogram() + 
  theme_classic()

Age_data = data_3 %>%
  group_by(SubjectNum) %>%
  summarise(Age = mean(Age), Group = Group) %>% 
  distinct

t.test(Age_data$Age ~ Age_data$Group, alternative = "two.sided", var.equal = TRUE) #No significant difference in ages

##### ACCURACY SUMMARY STATS & GRAPHS #####
accuracy_summary_stats = data_3 %>%
  group_by(Cloze, Group) %>%
  get_summary_stats(Accuracy, type = "mean_sd")

data_0 = data_3 %>%
  group_by(SubjectNum)

#Boxplot
ggboxplot(
  data_3, x = "Cloze", y = "Accuracy",
  color = "Group", palette = NULL)

ggboxplot(high_cloze, x = "Group", y = "Accuracy", palette = NULL)

#Bar graph
ggplot(accuracy_summary_stats, aes(Cloze, mean, fill = Group)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3")

#Difference between high and med cloze
high_med_diff = data_3 %>%
  select(SubjectNum, Cloze, Accuracy, AQ, VST, Group, t0_diff.slower, t0_diff.faster, t1_t2_diff.slower, t1_t2_diff.faster)

high_med_diff = spread(high_med_diff, Cloze, Accuracy)

high_med_diff = high_med_diff %>%
  mutate(Diff = High - Med)

##### ACCURACY ANOVA #####
#Outliers
outliers = data_3 %>%
  group_by(Cloze, Group) %>%
  identify_outliers(Accuracy) #3 extreme outliers in medium cloze condition

#Normality
normality = data_3 %>%
  group_by(Cloze, Group) %>%
  shapiro_test(Accuracy) #Not normally distributed

ggqqplot(data_3, "Accuracy", ggtheme = theme_bw()) +
  facet_grid(Cloze ~ Group) #Some data points lie outside the line of reference

#Creating DFs for each combination of subject group (ASD & TD) and cloze condition (High, Medium, Low)
high_cloze_ASD = data_3 %>%
  filter(Cloze == "High" & Group == "ASD")
hist(high_cloze_ASD$Accuracy)

high_cloze_TD = data_3 %>%
  filter(Cloze == "High" & Group == "TD")
hist(high_cloze_TD$Accuracy)

med_cloze_ASD = data_3 %>%
  filter(Cloze == "Med" & Group == "ASD")
hist(med_cloze_ASD$Accuracy)

med_cloze_TD = data_3 %>%
  filter(Cloze == "Med" & Group == "TD")
hist(med_cloze_TD$Accuracy)

low_cloze_ASD = data_3 %>%
  filter(Cloze == "Low" & Group == "ASD")
hist(low_cloze_ASD$Accuracy)

low_cloze_TD = data_3 %>%
  filter(Cloze == "Low" & Group == "TD")
hist(low_cloze_TD$Accuracy)

#Homogeneity of variance
data_3 %>%
  group_by(Cloze) %>%
  levene_test(Accuracy ~ Group) #Only for low and medium

#Homogeneity of covariances
box_m(data_3[, "Accuracy", drop = FALSE], data_3$Group) #Passes assumption

#Accuracy Anova
anova_model <- data_3 %>%
  group_by(Cloze) %>%
  anova_test(dv = Accuracy, wid = SubjectNum, between = Group, within = Group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_model

#Paper on robust ANOVA: https://cran.r-project.org/web/packages/WRS2/vignettes/WRS2.pdf

##### AQ CORRELATIONS ACROSS GROUPS #####

#Low Cloze df
low_cloze = data_3 %>%
  filter(Cloze == "Low")

ggplot(data = low_cloze, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze")

#Medium Cloze
med_cloze = data_3 %>%
  filter(Cloze == "Med") 

med_cloze_2 = data_3 %>%
  filter(Cloze == "Med") %>%
  filter(Accuracy > 0.4)

lm1 = lm(Accuracy ~ AQ, data = med_cloze_1)
lm2 = lm(Accuracy ~ AQ, data = med_cloze_2)
sum = aov(lm1, lm2)
summary(lm2)

ggplot(data = med_cloze, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze")

#High Cloze
high_cloze = data_3 %>%
  filter(Cloze == "High")

ggplot(data = high_cloze, aes(x = Accuracy, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze")

#High - Med Cloze
ggplot(data = high_med_diff, aes(x = Diff, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")
lm1 = lm(Diff ~ AQ, data = high_med_diff)
summary(lm1)

##### VST CORRELATIONS ACROSS GROUPS #####

#Low Cloze df
ggplot(data = low_cloze, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze")

#Medium Cloze
ggplot(data = med_cloze, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze")

#High Cloze
ggplot(data = high_cloze, aes(x = Accuracy, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze")

#High - Med Cloze
ggplot(data = high_med_diff, aes(x = Diff, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### T0 CORRELATIONS ACROSS GROUPS #####

#Low Cloze df
ggplot(data = low_cloze, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze")

ggplot(data = low_cloze, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze")

#Medium Cloze
ggplot(data = med_cloze, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze")

ggplot(data = med_cloze, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze")

#High Cloze
ggplot(data = high_cloze, aes(x = Accuracy, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze")

ggplot(data = high_cloze, aes(x = Accuracy, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze")

#High - Med Cloze
ggplot(data = high_med_diff, aes(x = Diff, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

ggplot(data = high_med_diff, aes(x = Diff, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### AQ CORRELATIONS - ASD #####
ggplot(data = low_cloze_ASD, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze ASD")

#Medium Cloze
ggplot(data = med_cloze_ASD, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze ASD")
identify()

med_cloze_ASD_2 = med_cloze_ASD %>%
  filter(Accuracy > 0.55)

lm1 = lm(Accuracy ~ AQ, data = med_cloze_ASD)
lm2 = lm(Accuracy ~ AQ, data = med_cloze_ASD_2)
sum = aov(lm1, lm2)
summary(lm1)

#High Cloze
ggplot(data = high_cloze_ASD, aes(x = Accuracy, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze ASD")

#High - Med Cloze
high_med_diff_ASD = high_med_diff %>%
  filter(Group == "ASD")

ggplot(data = high_med_diff_ASD, aes(x = Diff, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### AQ CORRELATIONS - TD #####

#Low Cloze df
ggplot(data = low_cloze_TD, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze TD")

#Medium Cloze
ggplot(data = med_cloze_TD, aes(x = Accuracy, y = AQ)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze TD")

#High Cloze
ggplot(data = high_cloze_TD, aes(x = Accuracy, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze TD")

#High - Med Cloze
high_med_diff_TD = high_med_diff %>%
  filter(Group == "TD")

ggplot(data = high_med_diff_TD, aes(x = Diff, y = AQ)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### VST CORRELATIONS - ASD #####

#Low Cloze df
ggplot(data = low_cloze_ASD, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze ASD")

#Medium Cloze
ggplot(data = med_cloze_ASD, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze ASD")

#High Cloze
ggplot(data = high_cloze_ASD, aes(x = Accuracy, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze ASD")

#High - Med Cloze
ggplot(data = high_med_diff_ASD, aes(x = Diff, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### VST CORRELATIONS - TD #####

#Low Cloze df
ggplot(data = low_cloze_TD, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze TD")

#Medium Cloze
ggplot(data = med_cloze_TD, aes(x = Accuracy, y = VST)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze TD")

#High Cloze
ggplot(data = high_cloze_TD, aes(x = Accuracy, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze TD")

#High - Med Cloze
ggplot(data = high_med_diff_TD, aes(x = Diff, y = VST)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### T0 CORRELATIONS - ASD #####

#Low Cloze df
ggplot(data = low_cloze_ASD, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze ASD")

ggplot(data = low_cloze_ASD, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze ASD")

#Medium Cloze
ggplot(data = med_cloze_ASD, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze ASD")

ggplot(data = med_cloze_ASD, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze ASD")

#High Cloze
ggplot(data = high_cloze_ASD, aes(x = Accuracy, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze ASD")

ggplot(data = high_cloze_ASD, aes(x = Accuracy, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze ASD")

#High - Med Cloze
ggplot(data = high_med_diff_ASD, aes(x = Diff, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

ggplot(data = high_med_diff_ASD, aes(x = Diff, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### T0 CORRELATIONS - TD #####

#Low Cloze df
ggplot(data = low_cloze_TD, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze TD")

ggplot(data = low_cloze_TD, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Low cloze TD")

#Medium Cloze
ggplot(data = med_cloze_TD, aes(x = Accuracy, y = t0_diff.slower)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze TD")

ggplot(data = med_cloze_TD, aes(x = Accuracy, y = t0_diff.faster)) +
  geom_point() + geom_smooth(method='lm', se = FALSE) + 
  ggtitle("Medium cloze TD")

#High Cloze
ggplot(data = high_cloze_TD, aes(x = Accuracy, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze TD")

ggplot(data = high_cloze_TD, aes(x = Accuracy, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High cloze TD")

#High - Med Cloze
ggplot(data = high_med_diff_TD, aes(x = Diff, y = t0_diff.slower)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

ggplot(data = high_med_diff_TD, aes(x = Diff, y = t0_diff.faster)) + 
  geom_point() + geom_smooth(method='lm', se = FALSE) +
  ggtitle("High - Med Cloze")

##### RT SUMMARY STATS & GRAPHS #####

RT_data = RT_data_correct %>%
  group_by(SubjectNum, Cloze, Group) %>%
  summarise(Accuracy = (sum(FinalAcc)/22), 
            VST = mean(VST_Score), 
            AQ = mean(AQ_Score), 
            VST_16 = mean(VST_Score_16),
            Age = mean(Age),
            Gender = mean(Gender),
            RT = mean(RT_s, na.rm = T))

RT_med_high = RT_data %>%
  filter(Cloze != "Low")

#Boxplot of RTs in seconds by subject
boxplot(RT ~ SubjectNum, data = RT_med_high,
        main = "RT by Subject", xlab = "Subject", ylab = "RT (s)",
        axes = TRUE, medcol = "blue",
        ylim = c(0, 18))
abline(h = median(data_1$RT), col = "blue", lty = 2)
abline(h = mean(data_1$RT), col = "red", lty = 2)

#Boxplot of RTs in seconds by group
ggboxplot(RT_med_high, x = "Group", y = "RT", palette = NULL)

#Boxplot of RTs in seconds by cloze condition
ggboxplot(RT_med_high, x = "Cloze", y = "RT", palette = NULL)

#Bar graph
ggplot(RT_med_high, aes(Cloze, RT, fill = Group)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3")

#RT summary stats
RT_summary_stats = RT_data_full %>%
  group_by(Group, Cloze) %>%
  get_summary_stats(RT, show = c("min", "max"))

RT_summary_stats = RT_summary_stats %>%
  mutate(range = max - min)

#RT Group Comparison
RT_data_summary = RT_data %>% 
  group_by(SubjectNum, Cloze) %>% 
  summarise(RT = mean(RT), Group = Group) %>% 
  distinct

t.test(RT_data$RT ~ RT_data$Group, alternative = "two.sided")

#RT Cloze Comparison
RT_anova = aov(RT_data$RT ~ RT_data$Cloze)
summary(RT_anova)

##### RT ANOVA #####
#Outliers
outliers = RT_med_high %>%
  group_by(Cloze, Group) %>%
  identify_outliers(RT) #No extreme outliers

#Normality
normality = RT_med_high %>%
  group_by(Cloze, Group) %>%
  shapiro_test(RT) #Not normally distributed

ggqqplot(RT_med_high, "RT", ggtheme = theme_bw()) +
  facet_grid(Cloze ~ Group) #Some data points lie outside the line of reference

hist(high_cloze_ASD$RT)
hist(high_cloze_TD$RT)
hist(med_cloze_ASD$RT)
hist(med_cloze_TD$RT)
hist(low_cloze_ASD$RT)
hist(low_cloze_TD$RT)

#Homogeneity of variance
RT_med_high %>%
  group_by(Cloze) %>%
  levene_test(RT ~ Group) #All groups pass assumption

#Homogeneity of covariances
box_m(RT_data[, "RT", drop = FALSE], RT_data$Group) #Passes assumption

#RT Anova
anova_model_RT <- RT_med_high %>%
  group_by(Cloze) %>%
  distinct %>%
  anova_test(dv = RT, wid = SubjectNum, between = Group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_model_RT

#RT lm
model_RT = lm(RT ~ Cloze * Group, data = RT_data)
summary(model_RT)

##### RT high med ANOVA #####
#Outliers
outliers = RT_med_high %>%
  group_by(Cloze, Group) %>%
  identify_outliers(RT) #1 extreme outlier in medium cloze condition

#Normality
normality = RT_med_high %>%
  group_by(Cloze, Group) %>%
  shapiro_test(RT) #Not normally distributed

ggqqplot(RT_med_high, "RT", ggtheme = theme_bw()) +
  facet_grid(Cloze ~ Group) #Some data points lie outside the line of reference

#Homogeneity of variance
RT_med_high %>%
  group_by(Group) %>%
  levene_test(RT ~ Cloze) #All groups pass assumption

#Homogeneity of covariances
box_m(RT_med_high[, "RT", drop = FALSE], RT_med_high$Group) #Passes assumption

#RT Anova
anova_model_RT_med_high <- RT_med_high %>%
  group_by(Cloze) %>%
  distinct %>%
  anova_test(dv = RT, wid = SubjectNum, between = Group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_model_RT_med_high

#RT lm
model_RT_med_high = lm(RT ~ Cloze * Group, data = RT_med_high)
summary(model_RT_med_high)
check_model(model_RT_med_high)
check_collinearity(model_RT_med_high)
residmodel = rstandard(model_RT_med_high)
summary(residmodel)

par(mfrow=c(2,2))
plot(model_RT_med_high)

#Linearity
ggplot(RT_med_high, aes(x=Cloze, y=RT)) + geom_point()
ggplot(RT_med_high, aes(x=Group, y=RT)) + geom_point()

##### RT_write high med ANOVA #####
#Outliers
outliers = RT_med_high %>%
  group_by(Cloze, Group) %>%
  identify_outliers(RT_write) #2 extreme outlier, one in med one in high

#Normality
normality = RT_med_high %>%
  group_by(Cloze, Group) %>%
  shapiro_test(RT_write) #Not normally distributed

ggqqplot(RT_med_high, "RT_write", ggtheme = theme_bw()) +
  facet_grid(Cloze ~ Group) #Some data points lie outside the line of reference

#Homogeneity of variance
RT_med_high %>%
  group_by(Cloze) %>%
  levene_test(RT_write ~ Group) #Med does not pass assumption

#Homogeneity of covariances
box_m(RT_med_high[, "RT_write", drop = FALSE], RT_med_high$Group) #Does not pass assumption

#RT Anova
anova_model_RT_write_med_high <- RT_med_high %>%
  group_by(Cloze) %>%
  distinct %>%
  anova_test(dv = RT_write, wid = SubjectNum, between = Group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_model_RT_write_med_high

#RT lm
model_RT_write_med_high = lm(RT_write ~ Cloze * Group, data = RT_med_high)
summary(model_RT_write_med_high)

##### RT WRITE SUMMARY STATS & GRAPHS #####

#Boxplot of RT_write in seconds by subject
boxplot(RT_write ~ SubjectNum, data = RT_data,
        main = "RT by Subject", xlab = "Subject", ylab = "RT (s)",
        axes = TRUE, medcol = "blue",
        ylim = c(0, 40))
abline(h = median(data_1$RT), col = "blue", lty = 2)
abline(h = mean(data_1$RT), col = "red", lty = 2)

#Boxplot of RT_write in seconds by group
ggboxplot(RT_data, x = "Group", y = "RT_write", palette = NULL)

#Boxplot of RT_write in seconds by cloze condition
ggboxplot(RT_data, x = "Cloze", y = "RT_write", palette = NULL)

#Bar graph
ggplot(RT_data, aes(Cloze, RT_write, fill = Group)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3")

#RT_write summary stats
RT_write_summary_stats = data_3 %>%
  group_by(Group, Cloze) %>%
  get_summary_stats(RT_write, type = "mean_sd")

#RT_write Group Comparison
RT_write_data = RT_data %>% 
  group_by(SubjectNum, Cloze) %>% 
  summarise(RT_write = mean(RT_write), Group = Group) %>% 
  distinct

t.test(RT_data$RT_write ~ RT_data$Group, alternative = "two.sided")

#RT_write Cloze Comparison
RT_write_anova = aov(RT_data$RT_write ~ RT_data$Cloze)
summary(RT_write_anova)

##### RT WRITE ANOVA #####
#Outliers
outliers = RT_data %>%
  group_by(Cloze, Group) %>%
  identify_outliers(RT_write) #1 in high and 1 in med

#Normality
normality = RT_data %>%
  group_by(Cloze, Group) %>%
  shapiro_test(RT_write) #Not normally distributed

ggqqplot(RT_data, "RT_write", ggtheme = theme_bw()) +
  facet_grid(Cloze ~ Group) #Some data points lie outside the line of reference

hist(high_cloze_ASD$RT_write)
hist(high_cloze_TD$RT_write)
hist(med_cloze_ASD$RT_write)
hist(med_cloze_TD$RT_write)
hist(low_cloze_ASD$RT_write)
hist(low_cloze_TD$RT_write)

#Homogeneity of variance
RT_data %>%
  group_by(Cloze) %>%
  levene_test(RT_write ~ Group) #Med does not pass assumption

#Homogeneity of covariances
box_m(RT_data[, "RT_write", drop = FALSE], RT_data$Group) #Does not pass assumption

#RT Write Anova
anova_model_RT_write <- RT_data %>%
  group_by(Cloze) %>%
  distinct %>%
  anova_test(dv = RT_write, wid = SubjectNum, between = Group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_model_RT_write

#RT_write lm
model_RT_write = lm(RT_write ~ Cloze * Group, data = RT_data)
summary(model_RT_write)

##### REGRESSION MODELS ACROSS GROUPS #####
#High cloze condition
model_1 <- lm(Accuracy ~ AQ, high_cloze) 
summary(model_1) #ASD significant
model_2 = lm(Accuracy ~ AQ + VST, high_cloze) 
summary(model_2) #Only VST is significant
model_3 = lm(Accuracy ~ AQ + VST + MET, high_cloze) 
summary(model_3) #Only VST is significant

#Medium cloze condition
model_4 = lm(Accuracy ~ AQ, med_cloze) 
summary(model_4) #AQ significant
model_5 = lm(Accuracy ~ AQ + VST, med_cloze) 
summary(model_5) #Only VST is significant
model_6 = lm(Accuracy ~ AQ + VST + MET, med_cloze) 
summary(model_6) #Only VST is significan

#High - Medium Cloze 
model_7 = lm(Diff ~ AQ, high_med_diff) 
summary(model_7) #AQ significant
model_8 = lm(Diff ~ AQ + VST, high_med_diff) 
summary(model_8)
model_9 = lm(Diff ~ AQ + VST + t0_diff.slower +t0_diff.faster, high_med_diff) 
summary(model_9)

##### REGRESSION MODELS - ASD #####
#High cloze condition
model_10 = lm(Accuracy ~ AQ, high_cloze_ASD) 
summary(model_10) #AQ not significant
model_11 = lm(Accuracy ~ AQ + VST, high_cloze_ASD) 
summary(model_11) #Only VST is significant
model_12 = lm(Accuracy ~ AQ + VST + t0_diff.slower +t0_diff.faster, high_cloze_ASD) 
summary(model_12)

#Medium cloze condition
model_13 = lm(Accuracy ~ AQ, med_cloze_ASD) 
summary(model_13) #AQ significant 
model_14 = lm(Accuracy ~ AQ + VST, med_cloze_ASD) 
summary(model_14) #Only VST significant 
model_15 = lm(Accuracy ~ AQ + VST + t0_diff.slower +t0_diff.faster, med_cloze_ASD) 
summary(model_15)

#High - Medium Cloze 
model_16 = lm(Diff ~ AQ, high_med_diff_ASD) 
summary(model_16) #AQ significant
model_17 = lm(Diff ~ AQ + VST, high_med_diff_ASD) 
summary(model_17)
model_18 = lm(Diff ~ AQ + VST + t0_diff.slower +t0_diff.faster, high_med_diff_ASD) 
summary(model_18)

high_med_cloze = data_3[which(data_3$Cloze != "Low"),]
med_cloze = data_3[which(data_3$Cloze == "Med"),]
high_cloze = data_3[which(data_3$Cloze == "High"),]
asd = high_med_cloze[which(high_med_cloze$Group == "ASD"),]
td = high_med_cloze[which(high_med_cloze$Group == "TD"),]
asd$Cloze = droplevels(asd$Cloze)
td$Cloze = droplevels(td$Cloze)
asd_med = med_cloze[which(med_cloze$Group == "ASD"),]
med_cloze$SubjectNum = droplevels(med_cloze$SubjectNum)


model_x = lmer(Accuracy ~ Age + (AQ + VST + MET)*Gender + (AQ + VST + MET)*Cloze + (1 | SubjectNum), data = high_med_cloze)
summary(model_x)

model_y = lmer(Accuracy ~ Age + (AQ + VST + MET)*Cloze + (1 | SubjectNum), data = high_med_cloze)
summary(model_y)

em3 = emmeans(model_y, pairwise ~ Cloze | VST)
em3$contrasts

model_med = lm(Accuracy ~ Age + AQ + VST + MET, data = med_cloze)
summary(model_med)

model_high = lm(Accuracy ~ Age + AQ + VST + MET, data = high_cloze)
summary(model_high)

model_asd = lmer(Accuracy ~ Cloze* (AQ + VST + MET + Age + Gender) + (1|SubjectNum), data = asd)
summary(model_asd)

model_td = lmer(Accuracy ~ Cloze* (AQ + VST + MET + Age) + (1|SubjectNum), data = td)
summary(model_td)

model_asd_med = lm(Accuracy ~ AQ + VST + MET + Age, data = asd_med)
summary(model_asd_med)

em1 = emmeans(model_y, pairwise ~ Cloze | VST)
em1$contrasts
em2 = emmeans(model_3, pairwise ~ VST | Cloze)
em2$contrasts

##### REGRESSION MODELS - TD #####
#High cloze condition
model_19 = lm(Accuracy ~ AQ, high_cloze_TD) 
summary(model_19) #AQ not significant 
model_20 = lm(Accuracy ~ AQ + VST, high_cloze_TD) 
summary(model_20) #Only VST significant
model_21 = lm(Accuracy ~ AQ + VST + t0_diff.slower +t0_diff.faster, high_cloze_TD) 
summary(model_21) 

#Medium cloze condition
model_22 = lm(Accuracy ~ AQ, med_cloze_TD) 
summary(model_22) #AQ not significant 
model_23 = lm(Accuracy ~ AQ + VST, med_cloze_TD) 
summary(model_23) #Only VST significant
model_24 = lm(Accuracy ~ AQ + VST + t0_diff.slower +t0_diff.faster, med_cloze_TD) 
summary(model_24)

#High - Medium Cloze 
model_25 = lm(Diff ~ AQ, high_med_diff_TD) 
summary(model_25) #AQ significant
model_26 = lm(Diff ~ AQ + VST, high_med_diff_TD) 
summary(model_26)
model_27 = lm(Diff ~ AQ + VST + t0_diff.slower +t0_diff.faster, high_med_diff_TD) 
summary(model_27)

##### Export CSV #####

#Selecting only relevant columns
data_4 <- data_3[ -c(10:21) ]

#Transposing df to create final df for analysis
data_5 = spread(data_4, Cloze, Accuracy) 

tapping_data_5 = tapping_data %>%
  select(SD_Asy_500, SubjectNum)

data_5 = merge(data_5, tapping_data_5, by = "SubjectNum")

data_5$SubjectNum = droplevels(data_5$SubjectNum)

data_5$GroupDummy[data_5$Group  == "ASD"] = 1
data_5$GroupDummy[data_5$Group == "TD"] = 2

#Creating final RT df for analysis
RT_include = RT_data %>%
  filter(Cloze != "Low")

RT_include = RT_include %>%
  select(SubjectNum, Cloze, Group, RT)

RT_include = spread(RT_include, Cloze, RT) 

RT_data_3 = RT_med_high %>%
  select(!Accuracy)
RT_data_3 = spread(RT_data_3, Cloze, RT) 

write.csv(data_5,"~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation/data_5.csv", row.names = FALSE)
write.csv(final_data,"~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation/final_data.csv", row.names = FALSE)
write.csv(RT_data,"~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation/RT_data.csv", row.names = FALSE)
write.csv(RT_data_3,"~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation/RT_data_3.csv", row.names = FALSE)
write.csv(RT_include,"~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation/RT_include.csv", row.names = FALSE)