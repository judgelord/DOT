# move to merge 
dotRIN %<>% mutate(Same_President = ifelse(initiated>as.Date(2009-01-01) & enddate<as.Date(20017-01-01), "Yes", "No")) 
dotRIN %<>% mutate(Same_President = ifelse(is.na(Same_President), "No", Same_President))
dotRIN %<>% mutate(MAJOR = ifelse(MAJOR == "Undetermined", "No", MAJOR))

############################
# Here we take event data with rows indexed by ID and columns for relevent dates and covariates.
# The method takes date variables, melts the data to identify the order in which various transitions occur, and, finally, models transitions in and out of each stage as a multi-state survival model. 
# THis is a slightly more generic approach than the mstate package vinettes as it does not require one to specifiy which transitions are possible or actually present in the data. Instead, the transition matrix is derived from the data.
# It also attempts to keep data in a tidy format (rather than special class objects). 

# First we identify the order in which events occured in order to identify different types of transitions. 

# STEP 1: IDENTIFY TRANSITIONS AND DATES 

# These data start as one observation per ID (in this case, "RIN")
# Every observation must have a start date (here the date the rulemaking project was "initiated").
d <- filter(dotRIN, !is.na(initiated)) 

# We split out transitions observed for each ID (We will merge back in with covariates later.)
# Select the events we want to model:
d %<>% select(RIN, initiated, NPRMpublished, FinalRulePublished, WithdrawalPublished)

# Melt data to be one observation per event
d %<>% 
  melt(id = "RIN", na.rm = T, value.name = "transition_date") %>% 
  distinct()

# Convert dates to strings for now to make things easier (specifically, the super-useful mutate(ifelse()) method). 
d$transition_date %<>% as.character()
# Group by RIN and identify sequence
d %<>% 
  group_by(RIN) %>% 
  arrange(transition_date) %>% 
  rename(event = variable) %>%
  mutate(transitions = paste(event, collapse = " to ")) 

# Parse transitions from sequence (could be made more general?)
# FIXME
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

# Now that we have identified transitions and the entry and exit dates, we no longer need the `event` and `transition_date` variables 
# However, we will need the event names later, so we save them
events <- as.character(unique(d$event))
d %<>% select(-event, -transition_date) %>% distinct()



# STEP 2: MERGE BACK WITH DATA

d %<>% left_join(dotRIN) %>% ungroup() %>% arrange(RIN) 

# Count days until event with `difftime()`
d %<>% 
  mutate(exit = difftime(exit_date, initiated, units="days") ) %>% 
  mutate(entry = difftime(entry_date, initiated, units="days") ) 

# Days until initiated is obviously 0, but difftime has odd rounding, so let us fix that:
d$entry[grepl("^init", d$transition)] <- 0
  
##########################################################
# INDEX TRANSITIONS 
d$transition %<>% as.factor()

d %<>% mutate(trans = factor(transition) ) %>% 
  mutate(trans = factor(trans, levels = rev(levels(trans))))

# See the levels before converting them to numbers
trans <- tlevels <- levels(d$trans)
events

# Possible transitions from state i
for(i in 1:length(events)){
  trans <- gsub(events[i], i, trans)
}

# Make a matrix of from and to
trans <- str_split(trans, " to ", simplify = TRUE)

# Initilize a list
from <- list()

# Use the matrix to populate the list for the transMat()
for(i in 1:length(events)){
  from[[i]] <- as.numeric(trans[which(trans[,1]==i),2])
}

# Transition Matrix
tmat <- transMat(from, 
                 names = events)


# NOTE THERE ARE SOME ERRORS (at least one left) THAT MAY NEED TO BE INVESTIGATED IF THEY PERSIST AFTER PROPER CODING OF TRANSITIONS 
d %<>% filter(exit > entry) # FIXME

# ALL OBS ARE TRANSITIONS, HOWEVER, WE CAN INCLUDE NON TRANSITION OBS (i.e. months where no transition happened) -- DOES IT HELP? Perhaps only with covariats that have values for those non-transition months?
d$status <- 1
# ```

# In estimation, the multi-stage cox hazard model is a cox hazard model stratified by transition. 

# MODEL WITH NO COVARIATES 
mod.1 <- coxph(Surv(entry, exit, status) ~ strata(trans), data = d, method = "breslow")

library(ggfortify)
library(survival)

d$stage <- gsub(" to .*", "", d$trans)
fit <- survfit(Surv(entry, exit, status) ~ strata(trans), data = d)
autoplot(fit, fun = 'event') 

install.packages("survminer")
library(survminer)
fit$stage <- fit$strata
names(fit$stage) <- gsub(" to .*", "", names(fit$stage))
ggsurvplot(fit, data = d, fun = "event") 
facet_grid(. ~ stage)

library(broom)

fit <- survfit(Surv(entry, exit, status) ~ MAJOR + strata(trans), data = d)
summary(fit)
tidy(fit) 
tidyfit <- fit %>% 
  tidy() %>%
  separate(strata, c("Major", "strata"), ", ") %>% 
  #filter(estimate > 0) %>%
  filter(time<3650) %>%
  mutate(strata = gsub(".*=|published|Published| *$","", strata) ) %>%
  mutate(from = factor(paste("from",gsub(".*=| to .*","", strata)) ) ) %>% 
  mutate(from = factor(from, levels = rev(levels(from)))) %>%
  mutate(to = gsub(".*=|.* to ","to ", strata) ) # %>% 
ggplot(tidyfit, aes(time/365, estimate)) + 
  geom_line(aes(color = Major)) +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  geom_point(aes(color = Major), shape = "+") + 
  geom_ribbon(aes(fill = Major, ymin = conf.low, ymax = conf.high), alpha=.25) + 
  facet_grid(to~ from, scales = "free_x") + 
  labs(x = "Years" , y = "KM Probability that transition will take longer than t")

# The Cuminc() function in the mstate package calculates non-parametric CIF (aka Aalen–Johansen estimates) and associated standard errors for the competing events.
# https://rpubs.com/alecri/258589
cif <- Cuminc(time = "time", status = "event", data = d)
head(cif)


# multi-state
fitCI <- survfit(Surv(entry, exit, status * as.numeric(trans), type = "mstate") ~ 1,
                 data = d)
td_multi <- tidy(fitCI)
td_multi

ggplot(td_multi, aes(time, estimate, group = state)) +
  geom_line(aes(color = state)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25)

unique(d$trans)


# # Extract fitted values
fit <- msfit(mod.1, trans=tmat, variance=TRUE)

# Extract transition probabilities from each state
# pred = # A positive number indicating the prediction time--the time at which the prediction is made

pt0 <- probtrans(fit,  predt = 1) 
summary(pt0, from=2)
plot(pt0[1])

p <- pt0[[1]]
p$from <- 1
for(i in 2:length(events)){
  pt <- pt0[[i]]
  pt$from <- i
  p <- rbind(p, pt)
}

p %<>% melt(id = time, value.name = "Probability", keep = from)

p %>% 
  ggplot(aes(x = time, color = factor(from))) + geom_line(aes(y = pstate2))
# CAUTION NOTE: check to make transitions were not dropped
# in one run, Withdrawals got dropped, making 4 (NPRM to Final) the 3rd factor. Not cool.
f <- fit$Haz

f %<>% mutate(Transition = factor(trans) ) %>% 
  mutate(Transition = factor(Transition, labels = tlevels))

f %<>% mutate(Stage = factor(gsub(" to .*", "", Transition))) %>%
  mutate(Stage = factor(Stage, levels = rev(levels(Stage))))

# Rename model output
f %<>% rename("Days" = "time", "Hazard" = "Haz")

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
