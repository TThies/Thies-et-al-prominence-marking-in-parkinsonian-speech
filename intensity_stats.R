#############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of intensity measures

## (1) Mean intensity
## (2) Statistical anaylses
## (3) Testing correlation with and without outlier (TMT)

## insert working directory
setwd("")

## read data file (csv)
data=read.csv("intensity_new.csv",header= T, strip.white=TRUE)


##############################################################################################


## (1)
## means and sd for table 5: values of intensity

aggregate(mean_vowel ~ acc_condition + group, data, mean)
aggregate(mean_vowel ~ acc_condition + group, data, sd)


##############################################################


## (2)
## statistical analyses

## load package
library(lme4)



## new column "TMT2" = exclude TMT value of PD 13 (TMT = 280)

### TMT ändern - exclude PD 13 mit TMT = 280

## neuen Faktor erstellen: TMT Differenz B-A

data[,"TMT2"] <- data[,"speaker"]
## Level umbenennen
levels(data[,"TMT2"]) <- list("76" = "P01", "51" = "P02",
                             "61" = "P03", "28" = "P04", "99" = "P05", 
                             "54" = "P06", "110" = "P07",
                             "126" = "P08", "28" = "P09", "45" = "P10",
                             "90" = "P11", "52" = "P12", "NA" = "P13",
                             "134" = "P14", "29" = "P15", "58" = "P16",
                             "24" = "P17", "182" = "P18",  "NA" = "P19",
                             "41" = "C01", "18" = "C02",
                             "35" = "C03", "35" = "C04", "27" = "C05", 
                             "64" = "C06", "52" = "C07", "14" = "C08",
                             "24" = "C09", "53" = "C10", "21" = "C11",
                             "9" = "C12", "27" = "C13", "16" = "C14",
                             "43" = "C15", "106" = "C16", "49" = "C17",
                             "106" = "C18", "14" = "C19")

## conert factor to numeric
data$TMT2 = as.numeric(as.character(data$TMT2))

## z-standadise parameters of potential further influencing factors (UPDRS, TMT, disease duration, intelligibility)

data$TMT2_z <- scale(data$TMT2)
data$UPDRS_z <- scale(data$UPDRS)
data$Dur_z <- scale(data$Dur)
data$Intell_z <- scale(data$Intell)


################################
## (i) group - based analysis ##
################################

## Testing fixed factor: group
intensity.group.full <- lmer(mean_vowel ~ group + acc_condition + (1| speaker) + (1|word), data = data, REML=FALSE)
intensity.group.null <- lmer(mean_vowel ~ 1 + acc_condition + (1|speaker) + (1|word), data = data, REML=FALSE)

anova(intensity.group.full, intensity.group.null)  ## no effect of group X2(1) = 1.5732  p = 0.2097


## Testing fixed factor: accent condition
intensity.acc_condition.full <- lmer(mean_vowel ~ group + acc_condition + (1| speaker) + (1|word), data = data, REML=FALSE)
intensity.acc_condition.null <- lmer(mean_vowel ~ group + 1 + (1| speaker) + (1|word), data = data, REML=FALSE)

anova(intensity.acc_condition.full, intensity.acc_condition.null)  ## effect of accent condition X2(1) = 520.26  p < 2.2e-16 ***
summary(intensity.acc_condition.full) ## intensity lower in unaccented vowels


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
patient.TMT <- lmer(mean_vowel ~  TMT2_z + acc_condition + (1+acc_condition|speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(mean_vowel ~  1 + acc_condition + (1+acc_condition|speaker)+ (1|word), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMT, patient.null) ## X2(1)=8.1019 ; p=0.004422 ** --> influence of TMT
summary(patient.TMT)

## influence duration of disease
patient.Dur <- lmer(mean_vowel ~  Dur_z + acc_condition + (1+acc_condition |speaker)+ (1|word) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=0.1181; p=0.7311 --> no influence of duration 


## influence of UPDRS
patient.UPDRS <- lmer(mean_vowel ~  UPDRS_z + acc_condition + (1+acc_condition |speaker)+ (1|word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.0148  ; p=0.9033 --> no influence of motor impairment


## influence of intelligibility rating
patient.intell <- lmer(mean_vowel ~  Intell_z + acc_condition + (1+acc_condition |speaker)+ (1|word), data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=1.5784  ; p=0.209 --> no influence of intelligibility



##############
## controls ##
##############


## influence of trail making test
control.TMT <- lmer(mean_vowel ~  TMT2_z + acc_condition + (1+acc_condition |speaker)+ (1|word), data = CON,  REML=FALSE)
control.null <- lmer(mean_vowel ~  1 + acc_condition + (1+acc_condition |speaker)+ (1|word), data = CON, REML=FALSE)

anova(control.TMT, control.null) ## X2(1)=0.0907 ; p=0.0.7633 --> no influence of TMT



###############################################################################################


## (3)
## Correlation TMT & intensity with and without outliers

## mixed model:
## with outlier (PD 13)
patient.TMTwith <- lmer(mean_vowel ~  TMT_z  + (1|speaker) + (1| word), data = PAT, REML=FALSE)

## without outlier
patient.TMTwithout <- lmer(mean_vowel ~  TMT2_z + (1|speaker)+ (1| word), data = PAT,  REML=FALSE)

## comparison via AIC
summary(patient.TMTwith) ## AIC = 2565.8
summary(patient.TMTwithout) ## AIC = 2442.2 ## --> better model fit for linear relationship


## Correlation

## with outlier (PD 13)
cor.test(PAT$TMT_z, PAT$mean_vowel, method = "pearson", conf.level = 0.95) ## r = 0.24

## without outlier
cor.test(PAT$TMT2_z, PAT$mean_vowel, method = "pearson", conf.level = 0.95) ## r = 0.36

