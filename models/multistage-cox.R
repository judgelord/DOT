
# in estimation, the multi-stage cox model is a cox stratified by transition

# prerule stage: entry = initiated, exit = NPRM published, status = 1 if DOT report date = NPRM published date

# NEED TO ID TRANSITIONS 
# trans Initiated to NPRM <- 1,  NPRM published to FINAL published stage <- 2

load(here("data/DOT-perRule-perStage.Rdata"))

# select vars for minimal model 
d <- select(dotStage, DOTdate, RIN, STAGE, initiated, DaysUntilNPRM, DaysUntilFinalRule, DaysUntilWithdrawal) %>%
  filter(!is.na(initiated), !is.na(DOTdate), STAGE %in% c("Proposed Rule", "Final Rule", "Withdrawal")) %>% 
  distinct() %>% 
  arrange(DOTdate) %>% arrange(RIN)

# note, this is not always correct, FIXME
d %<>% mutate(DaysUntilNPRM = ifelse(is.na(DaysUntilNPRM) & STAGE == "Proposed Rule", DOTdate - initiated, DaysUntilNPRM))
d %<>% mutate(DaysUntilFinalRule = ifelse(is.na(DaysUntilFinalRule) & STAGE == "Final Rule", DOTdate - initiated, DaysUntilFinalRule))
d %<>% mutate(DaysUntilWithdrawal = ifelse(is.na(DaysUntilWithdrawal) & STAGE == "Withdrawal", DOTdate - initiated, DaysUntilWithdrawal))


# identify previous stages in the data (need to use previous stage var to supplement this)
d %<>% group_by(RIN) %>%
  mutate(stages = paste(STAGE, collapse = ";")) %>% ungroup() 

head(d$stages)
unique(d$stages)

d$trans <- NA
# to proposed rule = trans 1 
d$trans[grepl("^Proposed Rule", d$stages) & d$STAGE == "Proposed Rule"] <- 1

# direct to final = trans 2 
d$trans[d$stages =="Final Rule"] <- 2

# withdrawal = trans 3
d$trans[d$stages == "Proposed Rule;Withdrawal" & d$STAGE == "Withdrawal"] <- 3

# proposed to final = trans 4 
d$trans[grepl("Proposed Rule;Final Rule", d$stages) & d$STAGE == "Final Rule"] <- 4

#Use Matric Omitting Other Category
tmat <- transMat(list(c(2, 4), #transitions from prerule
                      c(3,4), 		 		  #transitions from nprm
                      c(), 		 		  #transitions from withdrawal
                      c()),        # trans from final 
                 names = c("preNPRM", "NPRM", "Withdrawal", "Final"))



d %<>% mutate(entry = ifelse(trans %in% c(1,2), 0, NA))
d %<>% mutate(entry = ifelse(trans %in% c(3,4), DaysUntilNPRM, entry))

d %<>% mutate(exit = ifelse(trans %in% c(2,4), DaysUntilFinalRule, NA))
d %<>% mutate(exit = ifelse(trans %in% c(3), DaysUntilWithdrawal, exit))
d %<>% mutate(exit = ifelse(trans %in% c(1), DaysUntilNPRM, exit))

d %<>% filter(exit > entry)

d$status <- 1


# NOTE: THIS IS WHITH NO COVARIATES 
mod.1 <- coxph(Surv(entry, exit, status) ~ strata(trans), data = d, method = "breslow")
summary(mod.1)

fit <- msfit(mod.1, trans=tmat, variance=TRUE)
# NOTE: Withdrawals got dropped, making 3 the 3rd factor
f <- fit$Haz
f$trans %<>% {gsub(1, "Inititiated to NPRM", .)}
f$trans %<>% {gsub(2, "Direct to Final", .)}
f$trans %<>% {gsub(3, "NPRM to Final", .)}

names(f) <- c("Days", "Hazard", "Transition")

ggplot(f) +
  geom_line(aes(x = Days, y = Hazard, color = Transition))







##### Convert Data Type?
attr(d, "trans") <- tmat
class(d) <- c("msdata", "data.frame")
events(d)
