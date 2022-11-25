library(survey)

# exclude participants without dietary data (dietary day 1 weights will be used)
survey1m <- subset(nhanes36m, !is.na(WTDRD1))

# create new column with 2 cycles weights (divides 1 cycle weight by 2)
survey2m <-  survey1m %>% rowwise() %>% mutate(PesoDieta4YR =WTDRD1 / 2) 

# apply weights
survey3m <- svydesign(data = survey2m, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, 
                     weights = ~PesoDieta4YR) 


survey4m <- subset(survey3m,  RIDAGEYR > 59) #excludes those younger than 60 years
survey5m <- subset(survey4m,  MCQ160F==2) #excludes those with stroke
survey6m <- subset(survey5m,  DR1DRSTZ==1) #excludes those without complete dietary recall
survey7m <- subset(survey6m,  DR1TKCAL > 500 & DR1TKCAL < 10000) #excludes those with extreme calorie intake 
survey8m <- subset(survey7m,  DMDEDUC2 <= 5) #excludes those with incomplete education data
survey10m <- subset(survey8m,  BMXBMI <= 83)  #excludes those with incomplete BMI data
survey11m <- subset(survey10m,  DiabetesYN <= 2)  #excludes those with incomplete diabetes data
survey12m <- subset(survey11m,  HypertensionYN <= 2)  #excludes those with incomplete hypertension data
survey13m <- subset(survey12m,  AlcoholYN == 0 |  AlcoholYN == 1) #excludes those with incomplete alcohol consumption data
survey14m <- subset(survey13m,  SmokeYN == "never" | SmokeYN == "current" | SmokeYN == "former") #excludes those with incomplete smoking data
survey15m <- subset(survey14m,  Exercise >= 0) #excludes those with incomplete exercise data
survey16m <- subset(survey15m, SumCerad < 40) #excludes those with incomplete memory data
survey17m <- subset(survey16m, CFDAST < 40) #excludes those with incomplete verbal fluency data
survey18m <- subset(survey17m, CFDDS < 106) #excludes those with incomplete digit symbol data
survey19m <- subset(survey18m, CFDCSR < 11) #excludes those with incomplete delayed recall data

print(summary(survey19m)) #prints summary of survey design
