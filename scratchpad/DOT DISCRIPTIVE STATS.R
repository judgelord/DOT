
rules <- dotRIN

# HOW MANY WITH REASON (NOTE NEED TO CORRECT WHYDELAY CONCAT)
# rules$whydelay1 <- gsub("N/A",NA,rules$whydelay1)
# sum(!is.na(rules$whydelay1))
n_distinct(reasons$RIN)  %>%
  print() %>%
write("stats/RINsWithReasonsForDelay.tex")

# HOW MANY DELAYED
filter(rules, color %in% c("Red", "Yellow")) %>%
  n_distinct(.$RIN) %>%
  print() %>%
  write("stats/RINsDelayed.txt")
# HOW MANY NOT DELAYED 
filter(rules, color %in% c("Black", "Green")) %>%
  n_distinct(.$RIN) %>%
  print() %>%
  write("stats/RINsNotDelayed.tex")

# HOW MANY AT EACH STAGE 
n_distinct(rules$RIN)  %>% print %>% write("stats/uniqueRINs.tex")

sum(!is.na(rules$ANPRMpublished))  %>% print %>% write("stats/ANPRMs.tex")

sum(!is.na(rules$ANPRM_RECIEVED)) %>% print %>% write("stats/ANPRMs recieved by OMB.tex")

sum(!is.na(rules$ANPRMtoOST)) %>% print %>% write("stats/ANPRMs to OST.tex")

sum(!is.na(rules$NPRMpublished))  %>% print %>% write("stats/NPRMs.tex")

sum(!is.na(rules$NPRMtoOST)) %>% print %>% write("stats/NPRMs to OST.tex")

sum(!is.na(rules$NPRM_RECIEVED)) %>% print %>% write("stats/NPRMs recieved by OMB.tex")

sum(!is.na(rules$SNPRMpublished))  %>% print %>% write("stats/SNPRMs.tex")

sum(!is.na(rules$SNPRM_RECIEVED)) %>% print %>% write("stats/SNPRMs recieved by OMB.tex")

sum(!is.na(rules$IFRpublished))  %>% print %>% write("stats/IFRs.tex")

sum(!is.na(rules$IFR_RECIEVED)) %>% print %>% write("stats/IFRs recieved by OMB.tex")

sum(!is.na(rules$FinalRulePublished))  %>% print %>% write("stats/Final Rules.tex")

sum(!is.na(rules$FINAL_RECIEVED)) %>% print %>% write("stats/Final Rules recieved by OMB.tex")

sum(!is.na(rules$WithdrawalPublished)) %>% print %>% write("stats/Withdrawals.tex")



#############################################################
# DAYS UNTIL EVENT 
#############################################################
rules %>% 
  filter(!is.na(DaysUntilANPRM)) %>% 
  summarise(mean(DaysUntilANPRM)) %>%
  first()  %>% print %>% write("days until ANPRM.tex")

rules %>% 
  filter(!is.na(DaysUntilNPRM)) %>% 
  summarise(mean(DaysUntilNPRM)) %>%
  first()  %>% print %>% write("days until NPRM.tex")



rules %>% 
  filter(!is.na(DaysUntilFinalRule)) %>% 
  summarise(mean(DaysUntilFinalRule)) %>%
  first()  %>% print %>% write("stats/days until final.tex")


rules %>% 
  filter(!is.na(DaysUntilOSTANPRM)) %>% 
  summarise(mean(DaysUntilOSTANPRM)) %>%
  first()  %>% print %>% write("stats/days until ANPRM to OST.tex")

rules %>% 
  filter(!is.na(DaysUntilOSTNPRM)) %>% 
  summarise(mean(DaysUntilOSTNPRM)) %>%
  first()  %>% print %>% write("stats/days until NPRM to OST.tex")



rules %>% 
  filter(!is.na(DaysUntilOSTFinalRule)) %>% 
  summarise(mean(DaysUntilOSTFinalRule)) %>%
  first()  %>% print %>% write("stats/days until Final to OST.tex")



rules %>% 
  filter(!is.na(DaysUntilWithdrawal)) %>% 
  summarise(mean(DaysUntilWithdrawal)) %>%
  first() %>% print %>% write("stats/days until Withdrawal.tex")

# stage histogram 

ggplot(DOTmonthly%>% filter(stage %in% c("Withdrawal","Proposed Rule", "Other", "Final Rule", "Interim Final Rule")), aes(DOTdate, na.rm = T)) +
  geom_histogram(na.rm = T, bins = 116)  + 
  facet_grid(stage~.)
  scale_y_date(breaks=date_breaks(width = "1 year"), labels = date_format("%y"),
               minor_breaks=NULL)



# publication histogram 

  ggplot(dotRIN %>% filter(stage %in% c("Withdrawal","Proposed Rule", "Final Rule", "Interim Final Rule")), 
         aes(publicationactual, na.rm = T)) +
    geom_histogram(na.rm = T, bins = 116)  + 
    facet_grid(stage~.)
  scale_y_date(breaks=date_breaks(width = "1 year"), labels = date_format("%y"),
               minor_breaks=NULL)
  
# propmt 
  unique(dotRIN$prompt)
  
  dotRIN %>% filter(prompt %in% c("Secretarial/Head of Operating Administration Decision","2011 Retrospective Regulatory Review" , 
                                  "International Agreement",
                                  "Settlement Agreement" 
  )) %>% 
  ggplot() +  
    geom_histogram(aes(initiated), bins = 20, na.rm = T ) + 
    facet_grid(STAGE~.)
  

  
# how many post 2008
  dotRIN %>% 
    filter(initiated > as.Date("2008-01-01")) %>% 
    #filter(!is.na(FinalRulePublished)) %>%  
    n_distinct(.$RIN)

n_distinct(dotcomplete2008$RIN)

