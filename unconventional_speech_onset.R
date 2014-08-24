# analyze speech onset latency for unconventional referents
library(lme4)

urefs <- readRDS(file="urefs.rds")
hist(urefs$SOT)

# truncate at top .975 of the distribution
sot.tval <- quantile(urefs$SOT, .975)
urefs$SOTt <- ifelse(urefs$SOT > sot.tval, sot.tval, urefs$SOT)
hist(urefs$SOTt)

                                        # look at the means
with(urefs, aggregate(SOTt ~ Novelty + Addressee + Feedback, FUN=mean))

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
# note: REML=FALSE needed for maximum likelihood estimation
urefs2c_sot_m1 <- # did not converge (lmer 1.1-7)
    lmer(SOTt ~ Ac*Nc*Fc + (Nc*Fc | SessionID) + (Ac*Nc | ItemID), data=urefs2c,
         REML=FALSE)

urefs2c_sot_m2 <- # no-random-correlation model
    lmer(SOTt ~ Ac*Nc*Fc + 
         (1 | SessionID) +
         (0 + Nc | SessionID) + (0 + Fc | SessionID) + (0 + Nc:Fc | SessionID) +
         (1 | ItemID) +
         (0 + Ac | ItemID) + (0 + Nc | ItemID) + (0 + Ac:Nc | ItemID),
         data=urefs2c,
         REML=FALSE)

urefs2c_sot_m2_noA <- update(urefs2c_sot_m2, . ~ . - Ac) 
urefs2c_sot_m2_noN <- update(urefs2c_sot_m2, . ~ . - Nc)
urefs2c_sot_m2_noF <- update(urefs2c_sot_m2, . ~ . - Fc)
urefs2c_sot_m2_noAN <- update(urefs2c_sot_m2, . ~ . - Ac:Nc)
urefs2c_sot_m2_noAF <- update(urefs2c_sot_m2, . ~ . - Ac:Fc)
urefs2c_sot_m2_noNF <- update(urefs2c_sot_m2, . ~ . - Nc:Fc) 
urefs2c_sot_m2_noANF <- update(urefs2c_sot_m2, . ~ . - Ac:Nc:Fc) 

anova(urefs2c_sot_m2, urefs2c_sot_m2_noA)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noN)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noF)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noAN)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noAF)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noNF)
anova(urefs2c_sot_m2, urefs2c_sot_m2_noANF)
