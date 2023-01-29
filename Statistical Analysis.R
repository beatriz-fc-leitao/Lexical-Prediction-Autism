setwd("~/Documents/My Documents/1. University/Year 4/Semester 2/Dissertation")
final_data_1 <- read.csv("final_data.csv",stringsAsFactors = TRUE)

# Removing low cloze
final_data_1 = filter(final_data_1, Cloze == "High" | Cloze == "Med")

#Contrast code cloze
final_data_1$Cloze = as.character(final_data_1$Cloze)
final_data_1$Cloze[final_data_1$Cloze == "Med"] = -1
final_data_1$Cloze[final_data_1$Cloze =="High"] = 1
final_data_1$Cloze = as.numeric(final_data_1$Cloze)

#Contrast code group
final_data_1$Group = as.character(final_data_1$Group)
final_data_1$Group[final_data_1$Group == "ASD"] = -1
final_data_1$Group[final_data_1$Group == "TD"] = 1
final_data_1$Group = as.numeric(final_data_1$Group)

asd_data = final_data_1[which(final_data_1$Group == 1),]

#Model 1
model_1 <- glmer(FinalAcc ~ Cloze +
                   (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                 data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_1) #AIC = 3497.0

model_1a <- glmer(FinalAcc ~ Cloze +
                   (1 | SubjectNum) + (1 | Item),
                 data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_1a) 

anova(model_1a, model_1)

#Model 2
model_2 <- glmer(FinalAcc ~ Cloze + Group + (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                 data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_2) #AIC = 3493.5

model_2a <- glmer(FinalAcc ~ Cloze + Group +
                    (1 | SubjectNum) + (1 | Item),
                  data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_2a)

#Model 3
model_3 <- glmer(FinalAcc ~ Cloze*Group + (1 | SubjectNum) + (1 | Item),
                 data= final_data_1, family = binomial())
summary(model_3) #AIC = 3490.2

anova(model_1, model_2) #Lower AIC significant
anova(model_2, model_3) #Lower AIC but not significant?

model_3 <- glmer(FinalAcc ~ Cloze * Group + (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                 data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_3) #AIC = 3493.5

#Model 4
model_4 <- glmer(FinalAcc ~ Cloze * Group +
                    (1 | SubjectNum) + (1 | Item),
                  data= final_data_1, family = binomial(), control = glmerControl(optimizer="bobyqa"))

summary(model_4)

#Running models on medium adn high cloze separately
medium_cloze = final_data_1[which(final_data_1$Cloze == -1),]
high_cloze = final_data_1[which(final_data_1$Cloze == 1),]

model_med <- glmer(FinalAcc ~ Group +
                   (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                 data= medium_cloze, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_med) #AIC = 3497.0

model_high <- glmer(FinalAcc ~ Group +
                     (1 + Cloze | SubjectNum) + (1 + Cloze + Group | Item),
                   data= high_cloze, family = binomial(), control = glmerControl(optimizer="bobyqa"))
summary(model_high) #AIC = 3497.0

#Post-hoc tests
emmip(model_3, Cloze ~ Group)
emmeans(model_3, ~ Group * Cloze)
em1 = emmeans(model_3, pairwise ~ Cloze | Group)
em1$contrasts
em2 = emmeans(model_3, pairwise ~ Group | Cloze)
em2$contrasts

sjp.glmer(model_3, type = "pred", vars = c("Group", "Cloze"))
plot_model(model_3, type = "eff", terms = c("Cloze", "Group"))

#Assumptions https://stats.stackexchange.com/questions/524376/testing-glmer-model-assumptions-optionally-in-r
#no over or under-dispersion of residuals in your model
#DHARMa nonparametric dispersion test 

simulationOutput <- simulateResiduals(fittedModel = model_3)
plot(simulationOutput)
testDispersion(simulationOutput)

check_overdispersion(model_3)

#random coefficients and intercepts follow a multivariate normal distribution
#qqplot on the random coefficients and intercepts should give good insights
check_model(model_3) 

#check for multicolinearity between predictors to ensure that no variable is able to perfectly predict your response.
check_collinearity(model_3)