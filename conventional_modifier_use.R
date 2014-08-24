# modifier use for conventional referents
library(lme4)

crefs <- readRDS(file="crefs.rds")
crefs_mit <- subset(crefs, ModInTrain) # get rid of trials where modifier
# was not used during training

# cell means
with(crefs_mit, 
     aggregate(Modifier ~ Novelty+Addressee+Feedback, FUN=mean))

crefs_mit2 <- transform(crefs_mit, N=ifelse(Novelty=="New",1,0),
                    A=ifelse(Addressee=="New",1,0),
                    F=ifelse(Feedback=="Yes",1,0))
crefs_mit2c <- transform(crefs_mit2,
                     Nc=N-mean(N),
                     Ac=A-mean(A),
                     Fc=F-mean(F))                          

# warning: this will take a long time, but doesn't converge
crefs_mit2c_m1 <- glmer(Modifier ~ Ac*Nc*Fc +
                        (1 + Nc*Fc | SessionID) + (1 + Ac*Nc | ItemID),
                        crefs_mit2c, family=binomial(link="logit"),
                        control=glmerControl(optimizer="bobyqa"))

# no-random-correlations model
crefs_mit2c_m2 <- glmer(Modifier ~ Ac*Nc*Fc + 
                        (1 | SessionID) +
                        (0 + Nc | SessionID) + (0 + Fc | SessionID) +
                        (0 + Nc:Fc | SessionID) +
                        (1 | ItemID) +
                        (0 + Ac | ItemID) + (0 + Nc | ItemID) +
                        (0 + Ac:Nc | ItemID),
                        crefs_mit2c, family=binomial(link="logit"),
                        control=glmerControl(optimizer="bobyqa")) # converges

crefs_mit2c_m2_noA <- update(crefs_mit2c_m2, . ~ . - Ac) 
crefs_mit2c_m2_noN <- update(crefs_mit2c_m2, . ~ . - Nc) # didn't converge
crefs_mit2c_m2_noF <- update(crefs_mit2c_m2, . ~ . - Fc) 
crefs_mit2c_m2_noAN <- update(crefs_mit2c_m2, . ~ . - Ac:Nc)
crefs_mit2c_m2_noAF <- update(crefs_mit2c_m2, . ~ . - Ac:Fc)
crefs_mit2c_m2_noNF <- update(crefs_mit2c_m2, . ~ . - Nc:Fc)
crefs_mit2c_m2_noANF <- update(crefs_mit2c_m2, . ~ . - Ac:Nc:Fc) 

anova(crefs_mit2c_m2, crefs_mit2c_m2_noA)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noN)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noF)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noAN)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noAF)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noNF)
anova(crefs_mit2c_m2, crefs_mit2c_m2_noANF)

# very low modifier rates for "New Referent" condition
#
# let's drop that condition, analyze only OldRef condition
crefs_newonly <- subset(crefs_mit2c, Novelty=="Old")

crefs_newonly_m1 <-
    glmer(Modifier ~ A*F +
          (1 + F | SessionID) +
          (1 + A| ItemID),
          data=crefs_newonly, family=binomial(link="logit"),
          control=glmerControl(optimizer="bobyqa"))

summary(crefs_newonly_m1)
