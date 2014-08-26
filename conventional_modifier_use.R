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
m_max <- glmer(Modifier ~ Ac*Nc*Fc +
               (1 + Nc*Fc | SessionID) + (1 + Ac*Nc | ItemID),
               crefs_mit2c, family=binomial(link="logit"),
               control=glmerControl(optimizer="bobyqa"))

# "diagonal" model; estimate variances only, covariances fixed to zero
m_diag <- glmer(Modifier ~ Ac*Nc*Fc +
                (1 + Nc*Fc || SessionID) + (1 + Ac*Nc || ItemID),
                crefs_mit2c, family=binomial(link="logit"),
                control=glmerControl(optimizer="bobyqa"))

m_diag_noA <- update(m_diag, . ~ . - Ac) 
m_diag_noN <- update(m_diag, . ~ . - Nc)
m_diag_noF <- update(m_diag, . ~ . - Fc) 
m_diag_noAN <- update(m_diag, . ~ . - Ac:Nc)
m_diag_noAF <- update(m_diag, . ~ . - Ac:Fc)
m_diag_noNF <- update(m_diag, . ~ . - Nc:Fc)
m_diag_noANF <- update(m_diag, . ~ . - Ac:Nc:Fc) 

anova(m_diag, m_diag_noA)
anova(m_diag, m_diag_noN)
anova(m_diag, m_diag_noF)
anova(m_diag, m_diag_noAN)
anova(m_diag, m_diag_noAF)
anova(m_diag, m_diag_noNF)
anova(m_diag, m_diag_noANF)

# very low modifier rates for "New Referent" condition
#
# let's drop that condition, analyze only OldRef condition
crefs_newonly <- subset(crefs_mit2c, Novelty=="Old")

newonly_max <-
    glmer(Modifier ~ A*F +
          (1 + F | SessionID) +
          (1 + A| ItemID),
          data=crefs_newonly, family=binomial(link="logit"),
          control=glmerControl(optimizer="bobyqa"))

summary(newonly_max)
