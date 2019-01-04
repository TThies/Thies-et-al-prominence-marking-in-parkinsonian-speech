##############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of vowel formants F1 and F2 and vowel articulation index

## (1) Mean vowel formants and VAI
## (2) Statistical anaylses

## insert working directory
setwd("")

## read data file (csv)
data=read.csv("vowel_articulation.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 4: Measurements for the formants F1 and F2 and the related vowel articulation index (VAI).

## Vowel formants /a/
aggregate(F1_a ~ acc_condition + group, data, mean)
aggregate(F1_a ~ acc_condition + group, data, sd)

aggregate(F2_a ~ acc_condition + group, data, mean)
aggregate(F2_a ~ acc_condition + group, data, sd)

## Vowel formants /i/
aggregate(F1_i ~ acc_condition + group, data, mean)
aggregate(F1_i ~ acc_condition + group, data, sd)

aggregate(F2_i ~ acc_condition + group, data, mean)
aggregate(F2_i ~ acc_condition + group, data, sd)

## Vowel formants /o/
aggregate(F1_o ~ acc_condition + group, data, mean)
aggregate(F1_o ~ acc_condition + group, data, sd)

aggregate(F2_o ~ acc_condition + group, data, mean)
aggregate(F2_o ~ acc_condition + group, data, sd)

## Vowel articulation index (VAI)
aggregate(VAI_meas ~ acc_condition + group, data, mean)
aggregate(VAI_meas ~ acc_condition + group, data, sd)


##############################################################

## (2)
## statistical analyses

## load package
library(lme4)

## z-standadise parameters of potential further influencing factors (UPDRS, TMT, disease duration, intelligibility)

data$TMT_z <- scale(data$TMT)
data$UPDRS_z <- scale(data$UPDRS)
data$Dur_z <- scale(data$Dur)
data$Intell_z <- scale(data$Intell)


################################
## (i) group - based analysis ##
################################

## Testing fixed factor: group
VAI.group.full <- lmer(VAI_meas ~ group + acc_condition + (1| speaker), data = data, REML=FALSE)
VAI.group.null <- lmer(VAI_meas ~ 1 + acc_condition + (1 |speaker), data = data, REML=FALSE)

anova(VAI.group.full, VAI.group.null)  ## effect of group X2(1) = 11.796 p= 0.0005935 ***
summary(VAI.group.full) ## smaller VAI in patients


## Testing fixed factor: accent condition
VAI.acc_condition.full <- lmer(VAI_meas ~ group + acc_condition + (1| speaker), data = data, REML=FALSE)
VAI.acc_condition.null <- lmer(VAI_meas ~ group + 1 + (1|speaker), data = data, REML=FALSE)

anova(VAI.acc_condition.full, VAI.acc_condition.null)  ## effect of accent conditions X2(1) = 13.324 p = 0.0002749 ***
summary(VAI.acc_condition.full) ## VAI greater in accented condition



############################################
## (ii) individual - differences analysis ##
############################################

## subset per group

PAT = subset(data, group == "Pat")
CON = subset(data, group == "Con")

##############
## patients ##
##############

## influence trail making test
patient.TMT <- lmer(VAI_meas ~  TMT_z + acc_condition + (1|speaker) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(VAI_meas ~  1 + acc_condition + (1|speaker), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMT, patient.null) ## X2(1)=2.2089 ; p=0.1372 --> no influence of TMT


## influence duration of disease
patient.Dur <- lmer(VAI_meas ~  Dur_z + acc_condition + (1|speaker), data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=0.524; p=0.4691 --> no influence of duration


## influence of UPDRS
patient.UPDRS <- lmer(VAI_meas ~  UPDRS_z + acc_condition + (1|speaker) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=9.3221 ; p=0.002264 ** --> influence of motor impairment
summary(patient.UPDRS) ## decreasing the vowel space of the patients by -0.06 ± 0.02 (standard errors). 


## influence of intelligibility rating
patient.intell <- lmer(VAI_meas ~  Intell_z + acc_condition + (1|speaker) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=1.8546 ; p=0.1732 --> no influence of intelligibility


##############
## controls ##
##############

## influence of trail making test
control.TMT <- lmer(VAI_meas ~  TMT_z + acc_condition + (1|speaker) , data = CON,  REML=FALSE)
control.null <- lmer(VAI_meas ~  1 + acc_condition + (1|speaker), data = CON, REML=FALSE)

anova(control.TMT, control.null) ## X2(1)=1.0226  ; p=0.3119 --> no influence of TMT


