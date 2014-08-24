# analyze speech onset latency for conventional referents
library(lme4)

crefs <- readRDS(file="crefs.rds")
crefs_mit <- subset(crefs, ModInTrain) # get rid of trials where modifier

hist(crefs_mit$SOT)

sottrunc <- quantile(crefs_mit$SOT, .975)
crefs_mit$SOTt <- ifelse(crefs_mit$SOT>sottrunc, sottrunc, crefs_mit$SOT)
hist(crefs_mit$SOTt)

with(crefs_mit, aggregate(SOTt~Novelty+Addressee+Feedback, FUN=mean))

# make quantitative predictors
crefs_mit2 <- transform(crefs_mit, N=ifelse(Novelty=="New",1,0),
                        A=ifelse(Addressee=="New",1,0),
                        F=ifelse(Feedback=="Yes",1,0))
crefs_mit2c <- transform(crefs_mit2,
                         Nc=N-mean(N),
                         Ac=A-mean(A),
                         Fc=F-mean(F))                          

crefs_sot_m1 <- lmer(SOTt ~ Ac*Nc*Fc +
                     (1 + Nc*Fc | SessionID) +
                     (1 + Ac*Nc | ItemID),
                     data=crefs_mit2c,
                     REML=FALSE)
