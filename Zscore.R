# immediate recall
nhanes29m$ZscoreCerad <- (nhanes29m$SumCerad - mean(nhanes29m$SumCerad))/
                           sd(nhanes29m$SumCerad)

# delayed recall
nhanes29m$ZscoreDelCerad <- (nhanes29m$CFDCSR - mean(nhanes29m$CFDCSR))/
                           sd(nhanes29m$CFDCSR)

# verbal fluency
nhanes29m$ZscoreAnimal <- (nhanes29m$CFDAST - mean(nhanes29m$CFDAST))/
                           sd(nhanes29m$CFDAST)

# executive function
nhanes29m$ZscoreDigitSymbol <- (nhanes29m$CFDDS - mean(nhanes29m$CFDDS))/
                           sd(nhanes29m$CFDDS)

# memory z-score
nhanes30m <- nhanes29m %>% 
  rowwise() %>% 
  mutate(MeanCerad = mean(c(ZscoreCerad,ZscoreDelCerad))) #mean z-score of all Cerad tests

nhanes30m$ZscoreCeradTotal <- (nhanes30m$MeanCerad - mean(nhanes30m$MeanCerad)) /
                                sd(nhanes30m$MeanCerad) #z-score of MeanCerad


nhanes31m <- nhanes30m %>% 
  rowwise() %>% 
  mutate(MeanCognicao = mean(c(ZscoreCerad, ZscoreDelCerad, ZscoreAnimal,
                               ZscoreDigitSymbol))) #mean z-score of all cognitive tests

nhanes31m$ZscoreCognicao <- (nhanes31m$MeanCognicao - mean(nhanes31m$MeanCognicao)) /
                                sd(nhanes31m$MeanCognicao) #global cognitive score


# merge to original dataset
nhanes32m <- merge(x = nhanes12a, y = nhanes31m[, c("SEQN", "ZscoreCerad","ZscoreDelCerad", 
                                                 "ZscoreAnimal", "ZscoreDigitSymbol", 
                                                 "ZscoreCeradTotal", "ZscoreCognicao")], 
                  by= "SEQN", all.x = TRUE) 
                  
                  
# Cognitive impairment classification

nhanes33m <- nhanes32m %>%
  mutate(MemoryImpaired = ifelse(ZscoreCeradTotal < -1.0, 1, 
                                 ifelse(ZscoreCeradTotal >= -1.0, 0, NA)))

nhanes34m <- nhanes33m %>%
  mutate(AnimalImpaired = ifelse(ZscoreAnimal < -1.0, 1, 
                                 ifelse(ZscoreAnimal >= -1.0, 0, NA)))

nhanes35m <- nhanes34m %>%
  mutate(ExecImpaired = ifelse(ZscoreDigitSymbol < -1.0, 1, 
                                 ifelse(ZscoreDigitSymbol >= -1.0, 0, NA)))

nhanes36m <- nhanes35m %>%
  mutate(GlobalImpaired = ifelse(ZscoreCognicao < -1.0, 1, 
                                 ifelse(ZscoreCognicao >= -1.0, 0, NA)))
