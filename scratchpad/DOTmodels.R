############################
# method to take in a set of dates, melt data, identify order, identify transitions, and model by stage

# First, identify transitions by RIN, merge back in with covariates later

# one observation per RIN + selected events
d <- filter(dotRIN, !is.na(initiated)) 
d %<>% 
  select(RIN, initiated, NPRMpublished, FinalRulePublished, WithdrawalPublished)

# events <- c("NPRMpublished", "FinalRulePublished", "WithdrawalPublished")

# Melt to be one observation per event
d %<>% 
  melt(id = "RIN", na.rm = T, value.name = "transition_date") %>% 
  distinct()

d$transition_date %<>% as.character()
# Group by RIN and identify sequence
d %<>% 
  group_by(RIN) %>% 
  arrange(transition_date) %>% 
  rename(event = variable) %>%
  mutate(transitions = paste(event, collapse = " to ")) 

# Parse transitions from sequence
d %<>% 
  mutate(trans1 = str_match(transitions, "([A-z]*? to [A-z]*){1}")[1] ) %>% 
  mutate(trans2 = str_match(transitions, "([A-z]*? to [A-z]*){2}")[1] ) %>%
  mutate(trans2 = str_replace(trans2, "(.*? to ){1}", "") ) %>%
  mutate(trans3 = str_match(transitions, "([A-z]*? to [A-z]*){3}")[1] ) %>%
  mutate(trans3 = str_replace(trans3, "(.*? to ){2}", "") )

d %<>%  select(-transitions) 

d %<>% melt(id = c("RIN", "event", "transition_date"), na.rm = T, value.name = "transition") %>% 
  rename(transition_count = variable)

d %<>% 
  group_by(RIN, transition) %>% 
  mutate(exit_date = ifelse(event == gsub(".* to ", "", transition),
                            transition_date, NA)) %>% 
  mutate(exit_date = paste(unique(na.omit(exit_date)), collapse = ";")) %>%
  mutate(entry_date = ifelse(event == gsub(" to .*", "", transition),
                            transition_date, NA)) %>% 
  mutate(entry_date = paste(unique(na.omit(entry_date)), collapse = ";"))

# no longer need event and transition_date, now that we have entry and exit
d %<>% select(-event, -transition_date) %>% distinct()

# d %<>% filter(event == gsub(".* to ", "", transition) )


# MERGE BACK WITH DATA

d %<>% left_join(dotRIN) %>% ungroup() %>% arrange(RIN) 

# Days until event 
d %<>% 
  mutate(exit = difftime(exit_date, initiated, units="days") ) %>% 
  mutate(entry = difftime(entry_date, initiated, units="days") ) 

# days till initiated is obviously 0, but difftime has odd rounding
d$entry[grepl("^init", d$transition)] <- 0
  
##########################################################
# INDEX TRANSITIONS 
d$transition %<>% as.factor()

d %<>% mutate(trans = factor(transition) ) %>% 
  mutate(trans = factor(trans, levels = rev(levels(trans))))

levels(d$trans)
d$trans %<>% as.numeric()


# Transition Matrix 
tmat <- transMat(list(c(2, 4), #transitions from prerule
                      c(3,4), 		 		  #transitions from nprm
                      c(), 		 		  #transitions from withdrawal
                      c()),        # trans from final 
                 names = c("preNPRM", "NPRM", "Withdrawal", "Final"))


# USE INDEX TO ID ENTRY AND EXIT DATES  
# prerule stage: entry = initiated, exit = NPRM published, status = 1 if DOT report date = NPRM published date

# obs entering at date initiated
d %<>% mutate(entry = ifelse(trans %in% c(1,2), 0, NA))

# obs entering at NPRM date 
d %<>% mutate(entry = ifelse(trans %in% c(3,4), DaysUntilNPRM, entry))

# obs exiting at Final Rule published date
d %<>% mutate(exit = ifelse(trans %in% c(2,4), DaysUntilFinalRule, NA))

# obs exiting at Withdrawal published date
d %<>% mutate(exit = ifelse(trans %in% c(3), DaysUntilWithdrawal, exit))

# obs exiting at NPRM published date
d %<>% mutate(exit = ifelse(trans %in% c(1), DaysUntilNPRM, exit))

# NOTE THERE ARE SOME ERRORS THAT MAY NEED TO BE INVESTIGATED IF THEY PERSIST AFTER PROPER CODING OF TRANSITIONS 
d %<>% filter(exit > entry) # FIXME

# ALL OBS ARE TRANSITIONS, HOWEVER, WE CAN INCLUDE NON TRANSITION OBS (i.e. months where no transition happened) -- DOES IT HELP? Perhaps only with covariats that have values for those non-transition months?
d$status <- 1
# ```

# In estimation, the multi-stage cox hazard model is a cox hazard model stratified by transition. 

# MODEL WITH NO COVARIATES 
mod.1 <- coxph(Surv(entry, exit, status) ~ strata(trans), data = d, method = "breslow")

# # INSPECT 
# summary(mod.1)
# class(mod.1$xlevels)

# # Extract fitted values
fit <- msfit(mod.1, trans=tmat, variance=TRUE)

# CAUTION NOTE: in one run, Withdrawals got dropped, making 4 (NPRM to Final) the 3rd factor. Not cool.
f <- fit$Haz

# Name stage for trans 1 and 2
f %<>% mutate(Stage = ifelse(trans %in% c(1,2), " Pre NPRM Stage", "Post NPRM Stage") )

# Name transitions
f$trans %<>% {gsub(1, "Inititiated to NPRM", .)}
f$trans %<>% {gsub(2, "Direct to Final", .)}
f$trans %<>% {gsub(3, "NPRM to Withdrawal", .)} 
f$trans %<>% {gsub(4, "NPRM to Final", .)} 

# Name variables
names(f) <- c("Days", "Hazard", "Transition", "Stage")


# plot
ggplot(f) +
  geom_line(aes(x = Days, y = Hazard, color = Transition)) + 
  facet_grid(. ~ Stage) + 
  theme_minimal()










#### OLD STUFF 



model <- glm(HasANPRM ~ TotalDays, family=binomial(link='logit'), data=dotRIN)
model2 <- glm(HasANPRM ~ NPRMtoRule, family=binomial(link='logit'), data=dotRIN)

#summary(model)
#summary(model2)
#stargazer(model, model2, title="Do Rules With ANPRMs Take Longer?")
visreg(model, type="effect",scale=c("response"), xlab = "Days from Project Initiated to Final Rule", ylab = "Predicted Probablity of ANPRM")
visreg(model2, type="effect",scale=c("response"), xlab = "Days from NPRM to Final Rule", ylab = "Predicted Probablity of ANPRM")


which(dot$STAGE=="Other")
unique(dot$STAGE)


sum(is.na(dotRIN$initiated))
length(unique(dotRIN$Terminated))
