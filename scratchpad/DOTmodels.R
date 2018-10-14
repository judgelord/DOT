


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
