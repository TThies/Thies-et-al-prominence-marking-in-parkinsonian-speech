##############################
## Prominence Marking in PD ##
##           PLOS ONE       ##
## 04.01.2018 - Tabea Thies ##
##############################

## Analysis of tonal height of rising pitch accents

## (1) Mean tonal height measures
## (2) Statistical anaylses

## insert working directory
setwd("C:/Users/Tabea Thies/Desktop/Plos One Paper_PD_Daten/Submission/Data on GitHub")

## read data file (csv)
data=read.csv("tonal_height.csv",header= T, strip.white=TRUE)


##############################################################################################

names(data)[24]<-"speaker"
write.csv(data, file = "TH.csv")

data$speaker<- as.factor(data$speaker)

## (1)
## means and sd for table 5: Values of tonal height

aggregate(TH ~ group, data, mean)
aggregate(TH ~ group, data, sd)


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
TH.full <- lmer(TH ~ group + (1| speaker), data = data, REML=FALSE)
TH.null <- lmer(TH ~ 1 + (1|speaker), data = data, REML=FALSE)

anova(TH.full, TH.null)  ## no effect of group X2(1) = 0.3632 p= 0.5467




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
patient.TMT <- lmer(TH ~  TMT2_z + (1|spk) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(TH ~  1 + (1|speaker), data = na.exclude(PAT), REML=FALSE)

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






#########
## CON ##
#########


### Alter (lme 4)
control.age <- lmer(TH ~  age_z + (1 |spk_age) , data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.age, control.null) ## X2(1)=0.5417; p=0.4617 --> Keine Einfluss von Alter

### Geschlecht (lme 4)
control.sex <- lmer(TH ~  sex.x + (1 |spk_age) , data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.sex, control.null) ## X2(1)=0.7809 ; p=0.3769 --> Keine Einfluss von Geschlecht

### Interaktion Geschlecht + Alter (lme 4)
control.sex.alter <- lmer(TH ~  sex.x + age_z + (1 |spk_age) , data = CON, REML=FALSE)
control.sex.alter.int <- lmer(TH ~  sex.x * age_z +(1 |spk_age), data = CON, REML=FALSE)

anova(control.sex.alter, control.sex.alter.int)  ## X2(1)=0.1027 ; p= 0.7486 --> Keine Interaktion von Geschlecht und Alter

### Panda (lme 4)
control.panda <- lmer(TH ~  Panda_z + (1 |spk_age) , data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.panda, control.null) ## X2(1)=0.5826 ; p=0.4453 --> Keine Einfluss von panda

### TMT (lme 4)
control.TMT <- lmer(TH ~  TMT_z + (1 |spk_age) , data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.TMT, control.null) ## X2(1)=0.531 ; p=0.4662 --> Keine Einfluss von TMT

### BTA (lme 4)
control.BTA <- lmer(TH ~  BTA_z + (1 |spk_age) , data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.BTA, control.null) ## X2(1)=1.6376; p=0.2007 --> Keine Einfluss von BTA

## Full vs null (lme4)
control.full <- lmer(TH ~ Panda_z + TMT_z + BTA_z + age_z + sex.x + (1 |spk_age), data = CON, REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age), data = CON, REML=FALSE)

anova(control.full, control.null)

summary (control.null) ## Anstieg bei Con 7.7 st +/- 0,6

#########
## PAT ##
#########


### Alter (lme 4)
patient.age <- lmer(TH ~  age_z + (1 |spk_age) , data = PAT, REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = PAT, REML=FALSE)

anova(patient.age, patient.null) ## X2(1)=0.0331 ; p=0.8557--> Keine Einfluss von Alter

### Geschlecht (lme 4)
patient.sex <- lmer(TH ~  sex.x + (1 |spk_age) , data = PAT, REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = PAT, REML=FALSE)

anova(patient.sex, patient.null) ## X2(1)=0.9507 ; p=0.3295 --> Keine Einfluss von Geschlecht

### Interaktion Geschlecht + Alter (lme 4)
patient.sex.alter <- lmer(TH ~  sex.x + age_z + (1 |spk_age) , data = PAT, REML=FALSE)
patient.sex.alter.int <- lmer(TH ~  sex.x * age_z +(1 |spk_age), data = PAT, REML=FALSE)

anova(patient.sex.alter, patient.sex.alter.int) ## X2(1)=2.6865 ; p=0.1012 --> Keine Interaktion von Alter & Geschlecht

### Panda (lme 4)
patient.panda <- lmer(TH ~  Panda_z + (1 |spk_age) ,  data = na.exclude(PAT), REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age),  data = na.exclude(PAT), REML=FALSE)

anova(patient.panda, patient.null) ## X2(1)=0.0277 ; p=0.8678 --> Keine Einfluss von panda


### TMT (lme 4)
patient.TMT <- lmer(TH ~  TMT+ (1 |spk_age) , data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMT, patient.null) ## X2(1)=7.0156 ; p=0.00808 ** --> Einfluss von TMT: TH wird um 0.04 st ± 0.01 (standard error) angehoben
summary (patient.TMT)

### BTA (lme 4)
patient.BTA <- lmer(TH ~  BTA_z + (1 |spk_age) , data = na.exclude(PAT), REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)

anova(patient.BTA, patient.null) ## X2(1)=5.9247; p=0.01493 * --> Einfluss BTA: TH wird um -1.33 st ± 0.5 (standard error) abgesenkt
summary(patient.BTA)

### Interaktion BTA + TMT (lme 4)
patient.BTA.TMT <- lmer(TH ~  BTA_z + TMT_z + (1 |spk_age) , data = na.exclude(PAT), REML=FALSE)
patient.BTA.TMT.int <- lmer(TH ~  BTA_z * TMT_z + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)

anova(patient.BTA.TMT, patient.BTA.TMT.int) ## X2(1)=0.0024; p= 0.9608  --> keine Interaktion


### UPDRS (lme 4)
patient.UPDRS <- lmer(TH ~ UPDRS_z + (1 |spk_age) , data = na.exclude(PAT), REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.0374; p=0.8466 --> kein Einfluss UPDRS


## Full vs null (lme4)
patient.full <- lmer(TH ~ Panda_z + TMT_z + BTA_z + age_z + sex.x + (1 |spk_age), data = PAT, REML=FALSE)
patient.BTA.TMT <- lmer(TH ~  BTA_z + TMT_z+ (1 |spk_age), data = PAT, REML=FALSE)

anova(patient.full, patient.BTA.TMT)

summary (patient.BTA.TMT)

### Einfluss bei PAT auf TH haben BTA und TMT. wobei Effekt von BTA grÃ¶ÃŸer ist.
### durchschnittlicher Anstieg 7 st +/- 0.7
## Je hÃ¶her BTA desto geringer TH -1.03 st Â± 0.7 (standard error) 
## je hÃ¶her TMT desto geringer TH -0.1 st Â± 0.6 (standard error) 



######## PAT - Dysarthrie hinzufÃ¼gen

### Sprecher nach Alter umbenennen


## neuen Faktor erstellen: Inhalte von Spalte "Sprecher" zu neuer SPalte "spk" kopieren - Controls
PAT[,"Dys"] <- PAT[,"spk_age"]

PAT$Dys = as.factor(PAT$Dys)

## Level umbenennen
levels(PAT[,"Dys"]) <- list("7.25" = "P01", "8.50" = "P02",
                            "9.00" = "P03", "8.50" = "P04", "8.75" = "P05", 
                            "8.25" = "P06", "8.00" = "P07", "7.75" = "P08",
                            "8.00" = "P09", "9.00" = "P10", "8.00" = "P11",
                            "8.50" = "P12", "6.50" = "P13", "8.75" = "P14",
                            "7.25" = "P15", "8.00" = "P16", "8.75" = "P17",
                            "7.75" = "P18", "7.25" = "P19")


PAT$Dys = as.numeric(as.character(PAT$Dys))

PAT$Dys_z <- scale(PAT$Dys)

### Dysarthrie (lme 4)
patient.Dys <- lmer(TH ~Dys_z + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)
patient.null <- lmer(TH ~  1 + (1 |spk_age), data = na.exclude(PAT), REML=FALSE)

anova(patient.Dys, patient.null) ## X2(1)=1.2379 ; p= 0.2659 --> Kein Einfluss Dys



library(coefplot)
coefplot(patient.BTA.TMT)


plot(fitted(patient.BTA.TMT), residuals(patient.BTA.TMT), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(patient.BTA.TMT), residuals(patient.BTA.TMT))

      
      
      
## Stats Paper: Brain & Language

## define function for correction of multiple testing
dunn.sidak<-function(alpha,n) 1-(1-alpha)^(1/n)
dunn.sidak(0.05, 3) ## so the new threshold for significance is 0.0170 in the B&L paper
dunn.sidak(0.05, 4) ## 0.0127

## Spalte Wortart erstellen

TH[,"Wort"] <- TH[,"utt"]

TH$Wort = as.factor(TH$Wort)

## Level umbenennen
levels(TH[,"Wort"]) <- list("Hose" = "1", "Dose" = "5",
                              "Hose" = "7", "Wiese" = "11", "Vase" = "12", 
                              "Mine" = "13", "Wade" = "14",
                              "Nase" = "15", "Dose" = "17", "Wiese" = "19",
                              "Nase" = "20", "Biene" = "23", "Biene" = "26",
                              "Bohne" = "28", "Mine" = "29", "Vase" = "30",
                              "Bohne" = "33", "Wade" = "35")



###########################
## neue Stats für Paper ###
###########################

## Tabelle Dur-Onglide minimieren
TH <- TH[,-(30)]   

##################
## Pakete laden ##
##################

library(lme4)


## UPDRS III - Werte einfügen

TH[,"UPDRSIII"] <-TH[,"spk_age"]

TH$UPDRSIII = as.factor(TH$UPDRSIII)

TH$UPDRSIII = as.factor(as.character(TH$UPDRSIII))


## Level umbenennen
levels(TH[,"UPDRSIII"]) <- list("18" = "P01", "17" = "P02",
                                 "15" = "P03", "7" = "P04", "8" = "P05", 
                                 "28" = "P06", "22" = "P07",
                                 "32" = "P08", "35" = "P09", "3" = "P10",
                                 "29" = "P11", "25" = "P12", "40" = "P13",
                                 "16" = "P14", "20" = "P15", "48" = "P16",
                                 "28" = "P17", "31" = "P18",  "30" = "P19",
                                 "NA" = "C01", "NA" = "C02",
                                 "NA" = "C03", "NA" = "C04", "NA" = "C05", 
                                 "NA" = "C06", "NA" = "C07", "NA" = "C08",
                                 "NA" = "C09", "NA" = "C10", "NA" = "C11",
                                 "NA" = "C12", "NA" = "C13", "NA" = "C14",
                                 "NA" = "C15", "NA" = "C16", "NA" = "C17",
                                 "NA" = "C18", "NA" = "C19")


## Spalten in Numeric umwandeln
TH$UPDRSIII = as.numeric(as.character(TH$UPDRSIII))


### TMT ändern - exclude PD 13 mit TMT = 280

## neuen Faktor erstellen: TMT Differenz B-A

TH[,"TMT2"] <- TH[,"spk_age"]

TH$TMT2 = as.factor(as.character(TH$TMT2))

## Level umbenennen
levels(TH[,"TMT2"]) <- list("76" = "P01", "51" = "P02",
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

## Spalten in Numeric umwandeln
TH$TMT2 = as.numeric(as.character(TH$TMT2))

## Krankheitsdauer
TH[,"Dur"] <- TH[,"spk_age"]

TH$Dur= as.factor(as.character(TH$Dur))

levels(TH[,"Dur"]) <- list("1" = "P01", "3" = "P02",
                            "2" = "P03", "9" = "P04", "1" = "P05", 
                            "5" = "P06", "10" = "P07",
                            "9" = "P08", "5" = "P09", "1" = "P10",
                            "10" = "P11", "7" = "P12", "8" = "P13",
                            "5" = "P14", "5" = "P15", "3" = "P16",
                            "14" = "P17", "5" = "P18",  "9" = "P19",
                            "NA" = "C01", "NA" = "C02",
                            "NA" = "C03", "NA" = "C04", "NA" = "C05", 
                            "NA" = "C06", "NA" = "C07", "NA" = "C08",
                            "NA" = "C09", "NA" = "C10", "NA" = "C11",
                            "NA" = "C12", "NA" = "C13", "NA" = "C14",
                            "NA" = "C15", "NA" = "C16", "NA" = "C17",
                            "NA" = "C18", "NA" = "C19")

TH$Dur = as.numeric(as.character(TH$Dur))


## Intelligibility Ratings 
TH[,"Intell"] <- TH[,"spk_age"]

TH$Intell = as.factor(as.character(TH$Intell))

levels(TH[,"Intell"]) <- list("7.25" = "P01", "8.5" = "P02",
                               "9.0" = "P03", "8.5" = "P04", "8.75" = "P05", 
                               "8.25" = "P06", "8" = "P07",
                               "7.75" = "P08", "8.0" = "P09", "9.0" = "P10",
                               "8.0" = "P11", "8.5" = "P12", "6.5" = "P13",
                               "8.75" = "P14", "7.25" = "P15", "8.0" = "P16",
                               "8.75" = "P17", "7.75" = "P18",  "7.25" = "P19",
                               "NA" = "C01", "NA" = "C02",
                               "NA" = "C03", "NA" = "C04", "NA" = "C05", 
                               "NA" = "C06", "NA" = "C07", "NA" = "C08",
                               "NA" = "C09", "NA" = "C10", "NA" = "C11",
                               "NA" = "C12", "NA" = "C13", "NA" = "C14",
                               "NA" = "C15", "NA" = "C16", "NA" = "C17",
                               "NA" = "C18", "NA" = "C19")

TH$Intell = as.numeric(as.character(TH$Intell))


## z-standadisieren

TH$TMT2_z <- scale(TH$TMT2)
TH$UPDRSIII_z <- scale(TH$UPDRSIII)
TH$Dur_z <- scale(TH$Dur)
TH$Intell_z <- scale(TH$Intell)


##### Info zu weiteren Parametern zufÃ¼gen

## Stats Paper: Brain & Language

## define function for correction of multiple testing
dunn.sidak<-function(alpha,n) 1-(1-alpha)^(1/n)
dunn.sidak(0.05, 3) ## so THe new THreshold for significance is 0.0170 in THe B&L paper
dunn.sidak(0.05, 4) ## 0.0127


## (i) group - based analysis
## Modell Gruppe
TH.Gruppe.full <- lmer(TH ~ Gruppe + (1| spk_age) + (1| Wort), data = TH, REML=FALSE)
TH.Gruppe.null <- lmer(TH ~ 1 + (1 |spk_age) + (1| Wort), data = TH, REML=FALSE)

anova(TH.Gruppe.full, TH.Gruppe.null)  ## Effekt der Gruppe X2(1) = 0.3345 p= 0.5573

## how good is data and model?

plot(fitted(TH.Gruppe.null),residuals(TH.Gruppe.null))

hist(residuals(TH.Gruppe.null))

qqnorm(residuals(TH.Gruppe.null))


############################################
## (ii) individual - differences analysis
############################################

PAT = subset(TH, Gruppe =="Pat")
CON = subset(TH, Gruppe =="Con")

### TMT - PATIENTEN
patient.TMT <- lmer(TH ~  TMT2_z + (1 |spk_age)+ (1| Wort), data = na.exclude(PAT),  REML=FALSE)
patient.null <- lmer(TH ~  1  + (1 |spk_age) + (1| Wort), data = na.exclude(PAT), REML=FALSE)

anova(patient.TMT, patient.null) ## X2(1)=7.2313 ; p=0.0.007164 ** --> Einfluss von TMT

summary(patient.TMT)

### TMT - Controls
control.TMT <- lmer(TH ~  TMT2_z + (1 |spk_age)+ (1| Wort) , data = CON,  REML=FALSE)
control.null <- lmer(TH ~  1 + (1 |spk_age)+ (1| Wort), data = CON, REML=FALSE)

anova(control.TMT, control.null) ## X2(1)=0.556  ; p=0.4559 --> Kein Einfluss von TMT


### Sex - PATIENTEN
patient.sex <- lmer(TH ~  sex.x + (1 |spk_age)+ (1| Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.sex, patient.null) ## X2(1)=1.3782 ; p=0.2404 --> kein Einfluss von Geschlecht


### Sex - Controls
control.sex <- lmer(TH ~  sex.x + (1 |spk_age) + (1| Wort) , data = CON,  REML=FALSE)

anova(control.sex, control.null) ## X2(1)=0.7687  ; p=0.3806 --> Kein Einfluss von Geschlecht


### Dauer - PATIENTEN
patient.Dur <- lmer(TH  ~  Dur_z + (1 |spk_age) + (1| Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.Dur, patient.null) ## X2(1)=2.0515; p=0.1521 --> kein Einfluss von Krankheitsdauer


### UPDRS - PATIENTEN
patient.UPDRS <- lmer(TH  ~  UPDRSIII_z +(1 |spk_age) + (1| Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.UPDRS, patient.null) ## X2(1)=0.0154 ; p=0.9012 --> kein Einfluss von Motor impairment

summary(patient.UPDRS)


### Intelligibility - PATIENTEN
patient.intell <- lmer(TH ~  Intell_z +(1 |spk_age)+ (1| Wort) , data = na.exclude(PAT),  REML=FALSE)

anova(patient.intell, patient.null) ## X2(1)=1.0991; p=0.2945 --> Kein Einfluss intell


#######
## comparing model output: TMT - tonal height
######

### TMT - PATIENTEN
patient.TMTohne <- lmer(TH ~  TMT2_z + (1 |spk_age)+ (1| Wort), data = PAT,  REML=FALSE)

PAT$TMT_z <- scale(PAT$TMT)

patient.TMTmit <- lmer(TH ~  TMT_z  + (1 |spk_age) + (1| Wort), data = PAT, REML=FALSE)

summary(patient.TMTohne) ## AIC = 1030.9 ## ohne ist besser (aber auch lm!)
summary(patient.TMTmit) ## AIC = 1077.4

#### Korrelationen

cor.test(PAT$TMT2_z, PAT$TH, method = "pearson", conf.level = 0.95) ## r = 0.44

cor.test(PAT$TMT_z, PAT$TH, method = "pearson", conf.level = 0.95) ## r = 0.22
