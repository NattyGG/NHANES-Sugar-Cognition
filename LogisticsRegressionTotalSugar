library(survey)
library(questionr)

#quasibinomial test acommodates weighted data

#Memory tests
#Unadjusted
RLCeradBinarioSugarTercil <- svyglm(CeradBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioSugarTercil) #results
odds.ratio(RLCeradBinarioSugarTercil, level=0.95) #odds ratio


#Model 1
RLCeradBinarioSugarTercilD <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCeradBinarioSugarTercilD) #results
odds.ratio(RLCeradBinarioSugarTercilD, level=0.95) #odds ratio


#Model 2
RLCeradBinarioSugarTercilDC <- svyglm(CeradBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCeradBinarioSugarTercilDC) #results
odds.ratio(RLCeradBinarioSugarTercilDC, level=0.95) #odds ratio


#Verbal Fluency
#Unadjusted
RLAnimalBinarioSugarTercil <- svyglm(AnimalBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioSugarTercil) #results
odds.ratio(RLAnimalBinarioSugarTercil, level=0.95) #odds ratio


#Model 1
RLAnimalBinarioSugarTercilD <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLAnimalBinarioSugarTercilD) #results
odds.ratio(RLAnimalBinarioSugarTercilD, level=0.95) #odds ratio


#Model 2
RLAnimalBinarioSugarTercilDC <- svyglm(AnimalBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+HipertensaoYN+
AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLAnimalBinarioSugarTercilDC) #results
odds.ratio(RLAnimalBinarioSugarTercilDC, level=0.95) #odds ratio


#Digit Symbol
#Unadjusted
RLDigitSymbolBinarioSugarTercil <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19) #logistics regression
summary(RLDigitSymbolBinarioSugarTercil) #results
odds.ratio(RLDigitSymbolBinarioSugarTercil, level=0.95) #odds ratio


#Model 1
RLDigitSymbolBinarioSugarTercilD <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial,
design = survey19) #logistics regression
summary(RLDigitSymbolBinarioSugarTercilD) #results
odds.ratio(RLDigitSymbolBinarioSugarTercilD, level=0.95) #odds ratio


#Model 2
RLDigitSymbolBinarioSugarTercilDC <- svyglm(DigitSymbolBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19)  #logistics regression
summary(RLDigitSymbolBinarioSugarTercilDC) #results
odds.ratio(RLDigitSymbolBinarioSugarTercilDC, level=0.95) #odds ratio


#Global cognition
#Unadjusted
RLCognicaoBinarioSugarTercil <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil), family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioSugarTercil) #results
odds.ratio(RLCognicaoBinarioSugarTercil, level=0.95) #odds ratio

#Model 1
RLCognicaoBinarioSugarTercilD <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL, family = quasibinomial, 
design = survey19) #logistics regression
summary(RLCognicaoBinarioSugarTercilD) #results
odds.ratio(RLCognicaoBinarioSugarTercilD, level=0.95) #odds ratio


#Model 2
RLCognicaoBinarioSugarTercilDC <- svyglm(CognicaoBinario ~ as.factor(AcucarTercil)+Gender+Idade+Raca+Marital+Education+DR1TKCAL+BMXBMI+DiabetesYN+
HipertensaoYN+AlcoholYN+SmokeYN+Exercise, family = quasibinomial, design = survey19) #logistics regression
summary(RLCognicaoBinarioSugarTercilDC) #results
odds.ratio(RLCognicaoBinarioSugarTercilDC, level=0.95) #odds ratio
