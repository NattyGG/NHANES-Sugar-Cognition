library(survey)

Total Sample
#Age
svymean(~RIDAGEYR, survey19m, na = TRUE)
sqrt(svyvar(~RIDAGEYR, design = survey19m, na = TRUE))

#gender
svytable(~Gender, design = survey19m)

# Race
svytable(~Raca, design = survey19m)

#education
svytable(~Education, design = survey19m)

#BMI
svymean(~BMXBMI, design = survey19m, na = TRUE)
sqrt(svyvar(~BMXBMI, design = survey19m, na = TRUE))

# Exercise
svytable(~Exercise, survey19m)

#hypertension
svytable(~HypertensionYN, design = survey19m)

#diabetes
svytable(~DiabetesYN, design = survey19m)

#alcohol consumption
svytable(~AlcoholYN, design = survey19m) 

#smoking
svytable(~SmokeYN, design = survey19m)

#Kcal
svymean(~DR1TKCAL, design = survey19m, na = TRUE)
sqrt(svyvar(~DR1TKCAL, design = survey19m, na = TRUE))


# Immediate recall
svymean(~SumCerad, design = survey19m, na = TRUE)
sqrt(svyvar(~SumCerad, design = survey19m, na = TRUE))

# Delayed recall
svymean(~CFDCSR, design = survey19m, na = TRUE)
sqrt(svyvar(~CFDCSR, design = survey19m, na = TRUE))

# Animal
svymean(~CFDAST, design = survey19m, na = TRUE)
sqrt(svyvar(~CFDAST, design = survey19m, na = TRUE))

# Digit Symbol
svymean(~CFDDS, design = survey19m, na = TRUE)
sqrt(svyvar(~CFDDS, design = survey19m, na = TRUE))


# By cognitive status
# age
svyby(~RIDAGEYR, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~RIDAGEYR, ~GlobalImpaired, survey19m, svyvar))

#gender
svytable(~Gender+GlobalImpaired, survey19m)

# Race
svytable(~Raca+GlobalImpaired, survey19m)

#education
svytable(~Education+GlobalImpaired, survey19m)

#BMI
svyby(~BMXBMI, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~BMXBMI, ~GlobalImpaired, survey19m, svyvar))

# Exercise
svytable(~Exercise+GlobalImpaired, survey19m)

#hypertension
svytable(~HypertensionYN+GlobalImpaired, survey19m)

#diabetes
svytable(~DiabetesYN+GlobalImpaired, survey19m) 

#alcohol consumption
svytable(~AlcoholYN+GlobalImpaired, survey19m) 

#smoking
svytable(~SmokeYN+GlobalImpaired, survey19m)

# Kcal
svyby(~DR1TKCAL, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~DR1TKCAL, ~GlobalImpaired, survey19m, svyvar))

# Immediate recall
svyby(~SumCerad, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~SumCerad, ~GlobalImpaired, survey19m, svyvar))

# Delayed recall
svyby(~CFDCSR, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~CFDCSR, ~GlobalImpaired, survey19m, svyvar))

# Animal
svyby(~CFDAST, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~CFDAST, ~GlobalImpaired, survey19m, svyvar))

# Digit Symbol
svyby(~CFDDS, ~GlobalImpaired, survey19m, svymean)
sqrt(svyby(~CFDDS, ~GlobalImpaired, survey19m, svyvar))

# Statistics
# chisquare categorical variables
svychisq(~Gender + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~Raca + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~Education + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~Exercise + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~HypertensionYN + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~DiabetesYN + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~AlcoholYN + GlobalImpaired, survey19m, statistic = "adjWald")
svychisq(~SmokeYN + GlobalImpaired, survey19m, statistic = "adjWald")

# T-test continuous variables
svyttest(RIDAGEYR~GlobalImpaired, survey19m)
svyttest(BMXBMI~GlobalImpaired, survey19m)
svyttest(DR1TKCAL~GlobalImpaired, survey19m)
svyttest(SumCerad~GlobalImpaired, survey19m)
svyttest(CFDCSR~GlobalImpaired, survey19m)
svyttest(CFDAST~GlobalImpaired, survey19m)
svyttest(CFDDS~GlobalImpaired, survey19m)
