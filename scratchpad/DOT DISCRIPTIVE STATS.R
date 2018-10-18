
# HOW MANY WITH REASON (NOTE NEED TO CORRECT WHYDELAY CONCAT)
# dotRIN$whydelay1 <- gsub("N/A",NA,dotRIN$whydelay1)
# sum(!is.na(dotRIN$whydelay1))
sum(!is.na(dotRIN$whydelay))  %>%
  print() %>%
write("stats/RINsWithReasonsForDelay.tex")

# HOW MANY DELAYED
filter(dotRIN, color %in% c("Red", "Yellow")) %>%
  n_distinct(.$RIN) %>%
  print() %>%
  write("stats/RINsDelayed.txt")

# HOW MANY NOT DELAYED WHEN PUBLISHED
filter(dotRIN, color %in% c("Black", "Green")) %>%
  n_distinct(.$RIN) %>%
  print() %>%
  write("stats/RINsNotDelayed.tex")

# HOW MANY AT EACH STAGE 
n_distinct(dotRIN$RIN)  %>% print %>% write("stats/uniqueRINs.tex")

sum(!is.na(dotRIN$ANPRMpublished))  %>% print %>% write("stats/ANPRMs.tex")

sum(!is.na(dotRIN$ANPRM_RECIEVED)) %>% print %>% write("stats/ANPRMs-recieved-by-OMB.tex")

sum(!is.na(dotRIN$ANPRMtoOST)) %>% print %>% write("stats/ANPRMs-to-OST.tex")

sum(!is.na(dotRIN$NPRMpublished))  %>% print %>% write("stats/NPRMs.tex")

sum(!is.na(dotRIN$NPRMtoOST)) %>% print %>% write("stats/NPRMs-to-OST.tex")

sum(!is.na(dotRIN$NPRM_RECIEVED)) %>% print %>% write("stats/NPRMs-recieved-by-OMB.tex")

sum(!is.na(dotRIN$SNPRMpublished))  %>% print %>% write("stats/SNPRMs.tex")

sum(!is.na(dotRIN$SNPRM_RECIEVED)) %>% print %>% write("stats/SNPRMs-recieved-by-OMB.tex")

sum(!is.na(dotRIN$IFRpublished))  %>% print %>% write("stats/IFRs.tex")

sum(!is.na(dotRIN$IFR_RECIEVED)) %>% print %>% write("stats/IFRs-recieved-by-OMB.tex")

sum(!is.na(dotRIN$FinalRulePublished))  %>% print %>% write("stats/FinaldotRIN.tex")

sum(!is.na(dotRIN$FINAL_RECIEVED)) %>% print %>% write("stats/FinaldotRIN-recieved-by-OMB.tex")

sum(!is.na(dotRIN$WithdrawalPublished)) %>% print %>% write("stats/Withdrawals.tex")



#############################################################
# days-until-EVENT 
#############################################################
dotRIN %>% 
  filter(!is.na(DaysUntilANPRM)) %>% 
  summarise(mean(DaysUntilANPRM)) %>%
  first()  %>% print %>% write("stats/days-until-ANPRM.tex")

dotRIN %>% 
  filter(!is.na(DaysUntilNPRM)) %>% 
  summarise(mean(DaysUntilNPRM)) %>%
  first()  %>% print %>% write("stats/days-until-NPRM.tex")

dotRIN %>% 
  filter(!is.na(DaysUntilFinalRule)) %>% 
  summarise(mean(DaysUntilFinalRule)) %>%
  first()  %>% print %>% write("stats/days-until-final.tex")


dotRIN %>% 
  filter(!is.na(DaysUntilOSTANPRM)) %>% 
  summarise(mean(DaysUntilOSTANPRM)) %>%
  first()  %>% print %>% write("stats/days-until-ANPRM-to-OST.tex")

dotRIN %>% 
  filter(!is.na(DaysUntilOSTNPRM)) %>% 
  summarise(mean(DaysUntilOSTNPRM)) %>%
  first()  %>% print %>% write("stats/days-until-NPRM-to-OST.tex")

dotRIN %>% 
  filter(!is.na(DaysUntilOSTFinalRule)) %>% 
  summarise(mean(DaysUntilOSTFinalRule)) %>%
  first()  %>% print %>% write("stats/days-until-Final-to-OST.tex")

dotRIN %>% 
  filter(!is.na(DaysUntilWithdrawal)) %>% 
  summarise(mean(DaysUntilWithdrawal)) %>%
  first() %>% print %>% write("stats/days-until-Withdrawal.tex")

# how many initiated post 2008
dotRIN %>% 
  filter(initiated > as.Date("2008-01-01")) %>% 
  #filter(!is.na(FinalRulePublished)) %>%  
  n_distinct(.$RIN) %>%
  first() %>% print %>% write("stats/initiatedSince2008.tex")



# stage histogram 
ggplot(dotMonthly %>% filter(stage %in% c("Withdrawal","Proposed Rule", "Other", "Final Rule", "Interim Final Rule")), aes(DOTdate, na.rm = T)) +
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
                                  "Settlement Agreement") ) %>% 
  ggplot() +  
    geom_histogram(aes(initiated), bins = 20, na.rm = T ) + 
    facet_grid(STAGE~.)

  



