library(psych)
library(tidyverse)
library(car)
library(ggplot2)
library(haven)
library(jtools)
library(stats)
library(ggpubr)
library(dbplyr)
library(lm.beta)
library(investr)
library(MASS)
library(lmSupport)
library(nortest)
library(broom)

#dprelim <- as.data.frame(read_sav("/Volumes/Hariri/DNS.01/Data/ALL_DATA_TO_USE/BATTERY_SCORED.sav"))
dprelim <- as.data.frame(read_sav("H:/Database/DNS/BATTERY_SCORED.sav"))

dsubset <- subset(dprelim, select = c(DNSID, CTQTot, STAXIAng, NEON, PSSTOT))

dprelim2 <- as.data.frame(read_sav("ISEL_MASQFuture_n545.sav"))
dprelim2 <- subset(dprelim2,select=c(DNSID, Sex, Age, AnyDx, DaysFollow, MASQtot_T3, MASQdep_TOT_T3, 
                                   MASQanx_TOT_T3, ISEL, MASQTot_T1, MASQDepTot_T1, MASQAnxTot_T1,
                                   ERQ_Sup, ERQ_Reap))
d <- inner_join(dsubset, dprelim2, by = c("DNSID"))
d <- na.omit(d)

#Explore Variable 1
summary(d$MASQTot_T1)
hist_MASQ1_org <- hist(d$MASQTot_T1)
d$MASQTot_T1_sq <- sqrt(d$MASQTot_T1-72)
hist(d$MASQTot_T1_sq)

hist(d$MASQTot_T1_sq)

MASQTot_T1_test <- BoxCoxTrans(d$MASQTot_T1)
print(MASQTot_T1_test)

d <- cbind(d, MASQTot_T1_bc=predict(MASQTot_T1_test, d$MASQTot_T1))
hist(d$MASQTot_T1_bc)
d$MASQTot_T1z <- scale(d$MASQTot_T1_bc)[,1]
#Which test to use? Who knows...
ks.test(d$MASQTot_T1_bc,"pnorm", mean=mean(d$MASQTot_T1_bc), sd=sd(d$MASQTot_T1_bc))
ad.test(d$MASQTot_T1_bc)
shapiro.test(d$MASQTot_T1_bc)

shapiro_test_results <-  shapiro.test(d$MASQTot_T1_bc) %>% tidy()


#outliers
d$MASQTot_T1_bc_mean <- mean(d$MASQTot_T1_bc)
d$MASQTot_T1_bc_sd <- sd(d$MASQTot_T1_bc)
d$MASQ_outlier <- d$MASQTot_T1_bc > (d$MASQTot_T1_bc_mean + 3*d$MASQTot_T1_bc_sd)
d %>%
  filter(d$MASQ_outlier == TRUE)


#Explore Variable 2 ISEL
summary(d$ISEL)
hist(d$ISEL)
shapiro.test(d$ISEL)

d$ISEL_ref <- (37-d$ISEL)
hist(d$ISEL_ref)
summary(d$ISEL_ref)
d$ISEL_ref_sq <- sqrt(d$ISEL_ref)
hist(d$ISEL_ref_sq)
summary(d$ISEL_ref_sq)
d$ISEL_sq <- (6.657 - d$ISEL_ref_sq)
hist(d$ISEL_sq)

ISEL_test <- BoxCoxTrans(d$ISEL)
print(ISEL_test)

d <- cbind(d, ISEL_bc=predict(ISEL_test, d$ISEL))
hist(d$ISEL_bc)

d$ISEL_z <- scale(d$ISEL_sq)[,1]

#Explore Variable 3
summary(d$MASQtot_T3)
hist(d$MASQtot_T3)

d$MASQtot_T3_sq <- sqrt(d$MASQtot_T3-61)
hist(d$MASQtot_T3_sq)

MASQT3_test <- BoxCoxTrans(d$MASQtot_T3)
print(MASQT3_test)

d <- cbind(d, MASQTot_T3_bc=predict(MASQT3_test, d$MASQtot_T3))
hist(d$MASQTot_T3_bc)

d$MASQtot_T3z <- scale(d$MASQtot_T3_sq)[,1]


#Explore Variable 5
summary(d$CTQTot)
hist(d$CTQTot)
d$CTQTot_sq <- sqrt(d$CTQTot-24)
hist(d$CTQTot_sq)
ks.test(d$CTQTot_sq,"pnorm", mean=mean(d$CTQTot_sq), 
        sd=sd(d$CTQTot_sq))

CTQ_test <- BoxCoxTrans(d$CTQTot)
print(CTQ_test)

d <- cbind(d, CTQTot_bc=predict(CTQ_test, d$CTQTot))
hist(d$CTQTot_bc)



#Explore variable PSS
hist(d$PSSTOT)
PSS_test <- BoxCoxTrans(d$PSSTOT)
d <- cbind(d, PSSTOT_bc=predict(PSS_test, d$PSSTOT))
hist(d$PSSTOT_bc)


#Explore Variable ERQ
hist(d$ERQ_Reap)
R_test <- BoxCoxTrans(d$ERQ_Reap)
d <- cbind(d, ERQReap_bc=predict(R_test, d$ERQ_Reap))
hist(d$ERQReap_bc)
d$ERQRep_z <- scale(d$ERQReap_bc)[,1]


hist(d$ERQ_Sup)
d$ERQSup_z <- scale(d$ERQ_Sup)[,1]

#NEON
hist(d$NEON)
#looks fine...whatever.



#Subscales

d$DepTot_z <- scale(log(d$MASQDepTot_T1))[,1]
d$DepTot_T2z <- scale(log(d$MASQdep_TOT_T3))[,1]

Anx <- BoxCoxTrans(d$MASQAnxTot_T1)
print(Anx)
d <- cbind(d, MASQAnx_bc=predict(Anx, d$MASQAnxTot_T1))
hist(d$MASQAnx_bc)
d$AnxTot_z <- scale(d$MASQAnx_bc)[,1]


Anx2 <- BoxCoxTrans(d$MASQanx_TOT_T3)
print(Anx2)
d <- cbind(d, Anx2_bc=predict(Anx2, d$MASQanx_TOT_T3))
d$AnxTot_T2z <- scale(d$Anx2_bc)[,1]
