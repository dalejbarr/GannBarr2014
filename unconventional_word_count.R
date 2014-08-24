# analyze word count for unconventional referents
library(lme4)

urefs <- readRDS(file="urefs.rds")

                                        # look at the distribution
hist(urefs$WC)

                                        # truncate extremely wordy trials
wctrunc <- quantile(urefs$WC, .975)
urefs$WCt <- ifelse(urefs$WC>wctrunc, wctrunc, urefs$WC)

hist(urefs$WCt)

                                        # look at the means
with(urefs, aggregate(WCt ~ Novelty + Addressee + Feedback, FUN=mean))

# look at the design; which factors are within and which are between?
xtabs(~SessionID+Novelty, urefs)
xtabs(~SessionID+Addressee, urefs)
xtabs(~SessionID+Feedback, urefs)
xtabs(~ItemID+Novelty, urefs)
xtabs(~ItemID+Addressee, urefs)
xtabs(~ItemID+Feedback, urefs)

# make quantitative predictors for independent variables
urefs2 <- transform(urefs, N=ifelse(Novelty=="New",1,0),
                    A=ifelse(Addressee=="New",1,0),
                    F=ifelse(Feedback=="Yes",1,0))
urefs2c <- transform(urefs2,
                     Nc=N-mean(N),
                     Ac=A-mean(A),
                     Fc=F-mean(F))
head(urefs2c)

# fit a maximal model
# default fit is "Laplace"
urefs2c_wc_m1 <- # takes a long time, but converges
    glmer(WCt ~ Ac*Nc*Fc + (Nc*Fc | SessionID) + (Ac*Nc | ItemID), 
    data=urefs2c, family=poisson,
          glmerControl(optimizer="bobyqa"))

# no-random-correlations model
urefs2c_wc_m2 <- # converges
    glmer(WCt ~ Ac*Nc*Fc + 
          (1 | SessionID) +
          (0 + Nc | SessionID) + (0 + Fc | SessionID) + (0 + Nc:Fc | SessionID) +
          (1 | ItemID) +
          (0 + Ac | ItemID) + (0 + Nc | ItemID) + (0 + Ac:Nc | ItemID),
          data=urefs2c, family=poisson)

urefs2c_wc_m1_noA <- update(urefs2c_wc_m1, . ~ . - Ac)
urefs2c_wc_m1_noN <- update(urefs2c_wc_m1, . ~ . - Nc)
urefs2c_wc_m1_noF <- update(urefs2c_wc_m1, . ~ . - Fc)
urefs2c_wc_m1_noAN <- update(urefs2c_wc_m1, . ~ . - Ac:Nc)
urefs2c_wc_m1_noAF <- update(urefs2c_wc_m1, . ~ . - Ac:Fc)
urefs2c_wc_m1_noNF <- update(urefs2c_wc_m1, . ~ . - Nc:Fc)
urefs2c_wc_m1_noANF <- update(urefs2c_wc_m1, . ~ . - Ac:Nc:Fc)

anova(urefs2c_wc_m1, urefs2c_wc_m1_noA)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noN)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noF)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noAN)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noAF)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noNF)
anova(urefs2c_wc_m1, urefs2c_wc_m1_noANF)
