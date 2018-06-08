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
library(caret)
source("ISEL_descriptives.R")

#Descriptives for all Variables
descMASQT1 <- describe(d$MASQtot_T3)
descMASQT2 <- describe(d$MASQTot_T1)
descERQr <- describe(d$ERQ_Reap)
descERQs <- describe(d$ERQ_Sup)
descISEL <- describe(d$ISEL)

#variables for relationship between current symptoms and future symptoms of Depression.
fit1 <- lm(MASQtot_T3 ~ MASQTot_T1, d) 
betas1 <- tidy(fit1)
func1 <- glance(fit1)
#code for p values if they are less than .001
betas1_pval <- ifelse(betas1$p.value[2]<.001,"*p*<.001","*p*=fit1$p.value[2]")


d$Sex_rc <- recode(d$Sex, "1 = '1'; 2 = '0'")

d$Sex_fac <- factor(d$Sex_rc, levels = c("0", "1"))



#Gender differences within the Variables
d$ISEL_c <- d$ISEL - mean(d$ISEL)
d$ERQRep_c <- d$ERQ_Reap - mean(d$ERQ_Reap)
d$ERQSup_c <- d$ERQ_Sup - mean(d$ERQ_Sup)
d$MASQtot_T3_c <- d$MASQtot_T3 - mean(d$MASQtot_T3)
d$MASQTot_T1_c <- d$MASQTot_T1 - mean(d$MASQTot_T1)


ERQRep_Gen <- lm(d$ERQ_Reap ~ Sex_fac, d)
RepGen <- tidy(ERQRep_Gen)
funRepGen <- glance(ERQRep_Gen)

ERQSup_Gen <- lm(ERQ_Sup ~ Sex_fac, d) 
SupGen <- tidy(ERQSup_Gen)
funSupGen <- glance(ERQSup_Gen)


MASQT1_Gen <- lm(MASQTot_T1 ~ Sex_fac, d)
T1Gen <- tidy(MASQT1_Gen)
funT1Gen <- glance(MASQT1_Gen)

MASQT3_Gen <- lm(MASQtot_T3 ~ Sex_fac, d)
T2Gen <- tidy(MASQT3_Gen)
funT2Gen <- glance(MASQT3_Gen)

ISEL_Gen <- lm(ISEL ~ Sex_fac, d)
ISELGen <- tidy(ISEL_Gen)
funISELGen <- glance(ISEL_Gen)


#Predicting future depression with ERQ
MASQreg <- lm(MASQtot_T3z ~ MASQTot_T1z + Sex_fac + Age + AnyDx, d)
betas_MASQ <- tidy(MASQreg)
func_MASQ <- glance(MASQreg)

ERQr_MASQ <- lm(MASQtot_T3z ~ ERQRep_z + MASQTot_T1z + Sex_fac + Age + AnyDx, d)
Betas_erqR <- tidy(ERQr_MASQ)
func_erqR <- glance(ERQr_MASQ)
ERQrMASQ_pval <- ifelse(Betas_erqR$p.value[2]<.001,"*p*<.001","*p*=fit1$p.value[2]")

modelCompare(MASQreg, ERQr_MASQ)

ERQs_MASQ <- lm(MASQtot_T3z ~ ERQSup_z + MASQTot_T1z + Sex_fac + Age + AnyDx, d) 
betas_erqS <- tidy(ERQs_MASQ)
func_erqS <- glance(ERQs_MASQ)
ERQsMASQ_pval <- ifelse(betas_erqS$p.value[2]<.001,"*p*<.001","*p*=fit1$p.value[2]")



#Path A for Reappraisal and Suppression separately
PathA_rep <- lm(ISEL_z ~ ERQRep_z*Sex_fac + Age + AnyDx, d)
betasA <- tidy(PathA_rep)
FuncA <- glance(PathA_rep)
FuncA

PathA_sup <- lm(ISEL_z ~ ERQSup_z*Sex_fac + Age + AnyDx, d)
betasA_sup <- tidy(PathA_sup)
funcA_sup <- glance(PathA_sup)

#Path B
PathB <- lm(MASQtot_T3z ~ ISEL_z + MASQTot_T1z + Age + Sex_fac + AnyDx, d) 
betasB <- tidy(PathB)
funcB <- glance(PathB)

#Split datasets by gender
d_fem <- subset(d, Sex_fac == 0)
d_male <- subset(d, Sex_fac==1)



#Lets look at women vs Men with reappraisal
FemRep_A <- lm(ISEL_z ~ ERQRep_z + Age + AnyDx, d_fem)
summary(FemRep_A)
MaleRep_A <- lm(ISEL_z ~ ERQRep_z + Age + AnyDx, d_male)
summary(MaleRep_A)


 

#Lets look at women vs men with suppression
FemSup_A <- lm(ISEL_z ~ ERQSup_z + Age + AnyDx, d_fem)
summary(FemSup_A)
MaleSup_A <- lm(ISEL_z ~ ERQSup_z + Age + AnyDx, d_male)
summary(MaleSup_A)


summary(Fem_B)
summary(Male_B)





#Split by Dx, see any changes?

d_dx <- subset(d, AnyDx==1)
dx_c <- lm(MASQtot_T3z ~ ERQRep_z + MASQTot_T1z + Sex + Age, d_dx)
summary(dx_c)
dx_a <- lm(ISEL_z ~ ERQRep_z + Sex + Age, d_dx)
summary(dx_a)
dx_med <- lm(MASQtot_T3z ~ ISEL_z + MASQTot_T1z+ Sex_fac + Age, d_dx)
summary(dx_med)



#Find individual variances
reg <- lm(MASQtot_T3z ~ MASQTot_T1z + Age, d)
d$Resid_Var <- reg$residuals
hist(d$Resid_Var)

Reap <- lm(Resid_Var ~ ERQRep_z + Sex, d) %>% tidy()
summary(Reap)

Sup <- lm(Resid_Var ~ ERQSup_z + Sex, d) %>% tidy()
summary(Sup)

Isel <- lm(Resid_Var~ ISEL_z + Sex, d) %>% tidy()
summary(Isel)





