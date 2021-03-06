load(here("data/DOT-monthly.Rdata"))

load(here("data/DOT-perRule-perStage.Rdata"))

load(here("data/DOT-perRule.Rdata"))
# PREP DATA ###########################
# Here we take event data with rows indexed by ID and columns for relevent dates and covariates.
# The method takes date variables, melts the data to identify the order in which various transitions occur, and, finally, models transitions in and out of each stage as a multi-state survival model. 
# THis is a slightly more generic approach than the mstate package vinettes as it does not require one to specifiy which transitions are possible or actually present in the data. Instead, the transition matrix is derived from the data.
# It also attempts to keep data in a tidy format (rather than special class objects). 

# First we identify the order in which events occured in order to identify different types of transitions. 

# STEP 1: IDENTIFY TRANSITIONS AND DATES 

# These data start as one observation per ID (in this case, "RIN"). I will be extending this approach to data frames of montly observations for each RIN
load(here("data/DOT-perRule.Rdata"))

# First, identify an ID variable so that we can split out transitions observed for each ID 
d <- original <- rename(dotRIN, id = RIN)

# Select a set of events to model (We will merge back in with covariates later.)
d %<>% select(id, Initiated, NPRMpublished, FinalRulePublished, WithdrawalPublished)

# Melt data to be one observation per event
d %<>% 
  melt(id = "id", na.rm = T, value.name = "transition_date") %>% 
  distinct()

# Convert dates to strings for now to make things easier (specifically, the super-useful mutate(ifelse()) method). 
d$transition_date %<>% as.character()

# Group by RIN and identify sequence
d %<>% 
  group_by(id) %>% 
  arrange(transition_date) %>% 
  rename(event = variable) %>%
  # clean up text for this example
  mutate(event = str_replace(event, " *$|published|Published", "")) %>%
  mutate(event = str_replace(event, "FinalRule", "Final Rule")) %>%
  # make a variable describing the path each took
  mutate(transitions = paste(event, collapse = " to ")) 
  
# Parse transitions from sequence (how do I make this more general?)
d %<>% separate(transitions, into= c("t1", "t2", "t3", "t4", "t5"), sep=" to ") %>% 
  #mutate(Initiated = t1) %>% 
  mutate(t1 = paste(t1, "to", t2)) %>% 
  mutate(t2 = paste(t2, "to", t3)) %>% 
  mutate(t3 = paste(t3, "to", t4)) %>% 
  mutate(t4 = paste(t4, "to", t5))

# melt back down to one observation per id per transition event
d %<>% melt(id = c("id", "event", "transition_date"), na.rm = T, value.name = "transition") %>% 
  rename(transition_count = variable) %>% 
  filter( !grepl(" to NA", transition) )

# identify entry and exit dates for each state 
d %<>% 
  group_by(id, transition) %>% 
  mutate(exit_date = ifelse(event == gsub(".* to ", "", transition),
                            transition_date, 
                            NA)) %>% 
  mutate(exit_date = max(na.omit(exit_date))) %>%
  mutate(entry_date = ifelse(event == gsub(" to .*", "", transition),
                            transition_date, 
                            NA)) %>% 
  mutate(entry_date = max(na.omit(entry_date) ) )

# Now that we have identified transitions and the entry and exit dates, we no longer need the `event` and `transition_date` variables 
# However, we will need the event names for the tranition matrix, so we save them
events <- as.character(unique(d$event))
d %<>% select(-event, -transition_date) %>% distinct()

###############################
# STEP 2: MERGE BACK WITH DATA
d %<>% left_join(original) %>% ungroup() %>% arrange(id) 

# Count days until event with `difftime()`
d %<>% 
  mutate(exit = as.numeric(difftime(exit_date, Initiated, units="days") ) )%>% 
  mutate(entry = as.numeric(difftime(entry_date, Initiated, units="days") ) ) 

# Days until initiated is obviously 0, but difftime has odd rounding, so let us fix that:
d$entry[grepl("^Init", d$transition)] <- 0

# NOTE THERE ARE SOME ERRORS IN THESE DATA (at least one left) THAT MAY NEED TO BE INVESTIGATED IF THEY PERSIST AFTER PROPER CODING OF TRANSITIONS 
d %<>% filter(exit > entry) # FIXME

# Finally, we define the time until the event
d %<>% mutate(time = exit - entry)


  
##########################################################
# INDEX TRANSITIONS 
d$transition %<>% as.factor()
trans <- tlevels <- levels(d$transition)

# Possible transitions from state i
for(i in 1:length(events)){
  trans <- gsub(events[i], i, trans)
}

# Make a trasition matrix (indexing transitions to and from each state)
trans <- str_split(trans, " to ", simplify = TRUE)

# Use the matrix to populate a list for the transMat()
from <- list()
for(i in 1:length(events)){
  from[[i]] <- as.numeric(trans[which(trans[,1]==i),2])
}

# Transition Matrix
tmat <- transMat(from, names = events)



# ALL OBS ARE TRANSITIONS, HOWEVER, WE CAN INCLUDE NON TRANSITION OBS (i.e. months where no transition happened) -- DOES IT HELP? Perhaps only with covariats that have values for those non-transition months?
d$status <- 1
# ```

# In estimation, the multi-stage cox hazard model is a cox hazard model stratified by transition. 

# Simple example
fit <- survfit(Surv(entry, exit, status) ~ strata(transition), data = d)
# ggfortify allows autoplotting of suvfit objects
autoplot(fit) # km estimate per strata
# autoplot(fit, fun = 'event') # cumulative incidents per strata

# Survminer also easy, but options are limited without a tidy object
ggsurvplot(fit, data = d, fun = "event") 

# Now with tidy data
fit <- survfit(Surv(entry, exit, status) ~  strata(transition), data = d) %>%
  tidy() 

fit %<>% # clean up text 
  mutate(from = factor(paste("from",gsub(".*=| to .*","", strata)) ) ) %>% 
  mutate(to = gsub(".*=|.* to ","to ", strata) ) %>%
  mutate(to = gsub(" *$","", to) ) 

fit %>%   #filter(estimate > 0) %>%
  filter(time<3650) %>% # limit to 10 year timeframe
  ggplot(aes(x = time/365, estimate)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  geom_point(shape = "+") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.25) + 
  facet_grid(to ~ from, scales = "free_x") + 
  labs(x = "Years" , y = "KM probability transition takes longer than t")

# Now with tidy data and covariates
fit <- survfit(Surv(entry, exit, status) ~ MAJOR + strata(transition), data = d) %>%
  tidy() 

fit %<>%
  separate(strata, c("Major", "strata"), ", ") %>% 
  #filter(estimate > 0) %>%
  filter(time<3650) %>% # limit to 10 year timeframe
  mutate(from = factor(paste("from",gsub(".*=| to .*","", strata)) ) ) %>% 
  mutate(to = gsub(".*=|.* to ","to ", strata) ) %>%
  mutate(to = gsub(" *$","", to) ) 

fit %>% 
ggplot(aes(x = time/365, estimate)) + 
  geom_line(aes(color = Major)) +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  geom_point(aes(color = Major), shape = "+") + 
  geom_ribbon(aes(fill = Major, ymin = conf.low, ymax = conf.high), alpha=.25) + 
  facet_grid(to ~ from, scales = "free_x") + 
  labs(x = "Years" , y = "KM probability transition takes longer than t")


################################################# REDUNDENT 
#######################################
# Cumulative incidence for each state #
#######################################
# The Cuminc() function in the mstate package calculates non-parametric CIF (aka Aalen–Johansen estimates) and associated standard errors for the competing events.
# https://rpubs.com/alecri/258589
# Cuminc is now simply a wrapper around survfit of the survival package with type="mstate", only maintained for backward compatibility.
# nonparametric cumulative incidence functions and associated standard errors
# It is not clear how to input non-event status (as with the survfit method below (the downside of using survfit directly is that output only has numeric transition values))
fit <- Cuminc(time = "time", status  = "transition", data = d)

# tidy
fit %<>%   select(-Surv, -seSurv) %>%
  gather(variable, value, -time) %>% 
  separate(variable, into = c("stat", "Transition"), sep = "\\.") %>% 
  spread(key = stat, value =  value)

fit %<>% 
  # make faceting variables for each current and future state
  mutate(to = gsub(".*=|.* to ","to ", Transition) ) %>%
  mutate(from = factor(paste("from", gsub(".*=| to .*","", Transition)) ) ) 

# Normalize by state (because we care about the competing risk within each state.)
fit %<>% 
  group_by(Transition) %>% 
  mutate(max = max(CI)) %>% 
  ungroup() %>% 
  group_by(from, time) %>% 
  mutate(CI = CI/(sum(max))) %>%
  mutate(max = max/sum(max)) %>% 
  ungroup()

# plot
ggplot(fit, aes(x = time/365)) +
  geom_step(aes(y = CI, color = Transition)) +
  geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2) +
  # geom_point(aes(y = CI, color = Transition), shape = "+") +
  geom_ribbon(aes(ymax = CI + 1.96*seCI, 
              ymin = CI - 1.96*seCI, 
              fill = Transition),
              alpha = .2) + 
  labs(x = "Years" , y = "Cumulative Incidence Function") +
  facet_grid(. ~ from) + # for comparing states without covariates 
  #facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()
################################################# /REDUNDENT 





# Now using the multi-state method (I am not yet clear on exactly how type = "mstate" is different):

# Status is now multinomial by trasition. This will help add non-event time observations (i.e. status !=1)?
fit <- survfit(Surv(time, status * as.numeric(transition), type = "mstate") ~ 1,
                 data = d) %>% tidy()

# replace numeric transition index with transition names (note: levels with too few observations may be dropped)
fit$state %<>% as.factor()
levels(fit$state) <- levels(d$transition)[as.numeric(levels(fit$state))]
fit$Transition <- fit$state

# make faceting variables for each current and future state
fit %<>% 
  mutate(to = gsub(".*=|.* to ", "to ", Transition) ) %>%
  mutate(from = factor(paste("from", gsub(".*=| to .*","", Transition)) ) ) 

# plot
fit %>% 
  ggplot(aes(x = time/365, color = Transition, fill = Transition) ) +
  geom_step(aes(y = estimate)) +
  geom_ribbon(aes(ymax = estimate + 1.96*std.error, 
                  ymin = estimate - 1.96*std.error),
              alpha = .2, color = NA) + 
  # geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2, color = "black") +
  labs(x = "Years" , y = "Cumulative incidence of all transitions") +
  facet_grid(. ~ from) + # for comparing states without covariates 
  #facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()

# Normalize by state (because we care about the competing risk within each state.)
fit %<>% 
  group_by(Transition) %>% 
  mutate(max = max(estimate)) %>% 
  ungroup() %>% 
  group_by(from, time) %>% 
  mutate(estimate = estimate/(sum(max))) %>%
  mutate(max = max/sum(max)) %>% 
  ungroup()


# plot
fit %>% filter(time < 3650) %>% 
ggplot(aes(x = time/365, color = Transition, fill = Transition) ) +
  geom_step(aes(y = estimate)) +
  geom_ribbon(aes(ymax = estimate + 1.96*std.error, 
                  ymin = estimate - 1.96*std.error),
              alpha = .2, color = NA) + 
  geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2, color = "black") +
  labs(x = "Years" , y = "Cumulative Incidence Function
       normalized by stage") +
  facet_grid(. ~ from) + # for comparing states without covariates 
  #facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()



# Now the mstate model with covariates: 
fit <- survfit(Surv(time, status * as.numeric(transition), type = "mstate") ~ MAJOR,
               data = d) %>% tidy()

# replace numeric transition index with transition names (note: levels with too few observations may be dropped)
fit$state %<>% as.factor()
levels(fit$state) <- levels(d$transition)[as.numeric(levels(fit$state))]
fit$Transition <- fit$state

# make faceting variables for each current and future state
fit %<>% 
  mutate(to = gsub(".*=|.* to ", "to ", Transition) ) %>%
  mutate(from = factor(paste("from", gsub(".*=| to .*","", Transition)) ) ) 

# plot
fit %>% filter(time < 3650) %>%
  ggplot(aes(x = time/365, color = strata, fill = strata) ) +
  geom_step(aes(y = estimate)) +
  geom_ribbon(aes(ymax = estimate + 1.96*std.error, 
                  ymin = estimate - 1.96*std.error),
              alpha = .2, color = NA) + 
  # geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2, color = "black") +
  labs(x = "Years" , y = "Cumulative incidence of all transitions") +
  #facet_grid(. ~ from) + # for comparing states without covariates 
  facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()

# Normalize by state (because we care about the competing risk within each state.)
fit %<>% 
  group_by(Transition, strata) %>% 
  mutate(max = max(estimate)) %>% 
  ungroup() %>% 
  group_by(from, time, strata) %>% 
  mutate(estimate = estimate/(sum(max))) %>%
  mutate(max = max/sum(max)) %>% 
  ungroup()


# plot
fit %>% filter(time < 3650) %>%
  ggplot(aes(x = time/365, color = strata, fill = strata) ) +
  geom_step(aes(y = estimate)) +
  geom_ribbon(aes(ymax = estimate + 1.96*std.error, 
                  ymin = estimate - 1.96*std.error),
              alpha = .2, color = NA) + 
  #geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2, color = "black") +
  labs(x = "Years" , y = "Cumulative Incidence Function
       normalized by stage") +
  #facet_grid(. ~ from) + # for comparing states without covariates 
  facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()














# Approach using msfit() to extract transition probabilities and fitted values (hazards)
# What is the relationship between transition probabilities and cumulative incidence?
# MODEL WITH NO COVARIATES 
d$trans <-d$transition
mod.1 <- coxph(Surv(time, status) ~ strata(trans), data = d, method = "breslow")

# # Extract fitted values
fit <- msfit(mod.1, trans=tmat)

# Extract transition probabilities from each state
# pred = # A positive number indicating the prediction time--the time at which the prediction is made
pt0 <- probtrans(fit,  predt = 1) 

# tidy the mstate object
trans <- as.data.frame(pt0[[5]]) # transition matrix
p <- pt0[[1]] # estimate matrix
p$from <- paste(1, names(trans)[1]) # names from transition matrix
for(i in 2:length(events)){
  pt <- pt0[[i]]
  pt$from <- paste(i, names(trans)[i])
  p <- rbind(p, pt)
}

# p$pstate2[which(p$from == "1 Initiated")] <- p$pstate2[which(p$from == "1 Initiated")] + p$pstate1[which(p$from == "2 NPRM")] + 
#   p$pstate3[which(p$from == "2 NPRM")] + 
#   p$pstate4[which(p$from == "2 NPRM")]

# melt into one observation per transition per time
p %<>% melt(id = c("time", "from"), value.name = "Probability")

# separate state from variable (estimate, error) name
p %<>% separate(variable, into = c("variable", "to"), sep = "e") %>%
  # spread estimates and errors into two variables
  spread(key = "variable", value = "Probability") %>%
  # name estimate and error variables
  rename(estimate = pstat, std.error = s)

# name transitions
p$to %<>% as.factor()
p$from %<>% as.factor()
levels(p$to) <- levels(p$from)


p %<>% 
  mutate(from = factor(paste("At state", from))) %>% 
  mutate(to = factor(paste("next state =", to))) %>%
  mutate(Transition = paste(from, to, sep = ", ")) 


# plot
p %>% filter(time <3650, !estimate %in% c(0,1) ) %>% 
  ggplot(aes(x = time/365, color = Transition, fill = Transition) ) +
  geom_step(aes(y = estimate)) +
  geom_ribbon(aes(ymax = estimate + 1.96*std.error, 
                  ymin = estimate - 1.96*std.error),
              alpha = .2, color = NA) + 
  #geom_text(aes(y = max, x = max(time/365), label = paste0(round(max*100), "%")), hjust = 1, vjust = -.2, color = "black") +
  labs(x = "Years" , y = "Transition probability
from Aalen-Johansen estimator") +
  facet_grid(. ~ from) + # for comparing states without covariates 
  #facet_grid(to ~ from) + # for comparing covariates within states
  theme_bw()


# QUESTION 
# msfit probtrans with covariate does not work without newdata argument?
mod.1 <- coxph(Surv(time, status) ~ MAJOR + strata(trans), data = d, method = "breslow")

# # Extract fitted values
# # Requires "newdata" - why - documentation does not say
fit <- msfit(mod.1, trans=tmat)
















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






# these models are wrong because they estimate overall rate, not rate per transition 
Simple example with no covariates:
  `fit <- survfit(Surv(entry, exit, status) ~ strata(transition), data = d)`

`autoplot(fit)` will quickly plot km estimate per strata with the `ggfortify` package.
```{r autoplot}
fit <- survfit(Surv(entry, exit, status) ~ strata(transition), data = d)
autoplot(fit)
```


Now with tidy data:
  ```{r survfit}
fit <- survfit(Surv(entry, exit, status) ~  strata(transition), data = d) %>%
  tidy() 

fit %<>% # clean up text 
  mutate(from = factor(paste("from",gsub(".*=| to .*","", strata)) ) ) %>% 
  mutate(to = gsub(".*=|.* to ","to ", strata) ) %>%
  mutate(to = gsub(" *$","", to) ) 

fit %>%   #filter(estimate > 0) %>%
  filter(time<3650) %>% # limit to 10 year timeframe
  ggplot(aes(x = time/365, estimate)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  geom_point(shape = "+") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.25) + 
  facet_grid(to ~ from, scales = "free_x") + 
  labs(x = "Years" , y = "KM probability transition takes longer than t") + 
  theme_bw()
```    

Now with tidy data and a covariate:
  ```{r survfit-major}
fit <- survfit(Surv(entry, exit, status) ~ MAJOR + strata(transition), data = d) %>%
  tidy() 

fit %<>%
  separate(strata, c("Major", "strata"), ", ") %>% 
  #filter(estimate > 0) %>%
  filter(time<3650) %>% # limit to 10 year timeframe
  mutate(from = factor(paste("from",gsub(".*=| to .*","", strata)) ) ) %>% 
  mutate(to = gsub(".*=|.* to ","to ", strata) ) %>%
  mutate(to = gsub(" *$","", to) ) 

fit %>% 
  ggplot(aes(x = time/365, estimate)) + 
  geom_line(aes(color = Major)) +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  geom_point(aes(color = Major), shape = "+") + 
  geom_ribbon(aes(fill = Major, ymin = conf.low, ymax = conf.high), alpha=.25) + 
  facet_grid(to ~ from, scales = "free_x") + 
  labs(x = "Years" , y = "KM probability transition takes longer than t") + 
  theme_bw()
```    


















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
