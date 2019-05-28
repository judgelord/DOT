source("setup.R")

load(here("data/DOT-monthly.Rdata"))
load(here("data/DOT-perRule.Rdata"))
d <- dotMonthly
DOTmonthly <- dotMonthly
summary(d)

d %<>% mutate(agency = RIN,
              stage = STAGE)

d$agency %<>% substr(3, 4) %>% 
{gsub("20", "FAA", .)} %>%
{gsub("05", "SEC", .)} %>%
{gsub("25", "FHA", .)} %>%
{gsub("26", "FMCSA", .)} %>%
{gsub("30", "FRA", .)} %>%
{gsub("32", "FTA", .)} %>%
{gsub("27", "NHTSA", .)} %>%
{gsub("37", "PHMSA", .)}%>%
{gsub("39", "BTS", .)}%>%
{gsub("35", "SLSDC", .)}%>%
{gsub("33", "MA", .)}

#######################################################################################################
# TIMELINE BY COLOR 
####################
ggplot(d, aes(x=RIN, y=DOTdate)) + 
  # facet_grid(agency~.)+
  geom_point(aes(color = factor(color), 
                 #fill = factor(color), 
                 shape = factor(stage)), size = .5) + 
  # scale_colour_manual(values = c("black", "green","red","yellow"), 
  #                     labels = c("No Timeline",
  #                     "On Time",
  #                     "Delayed",
  #                     "Likely Delayed") )+
  coord_flip() +
  scale_y_date(lim = c(as.Date("2008-01-01"), as.Date("2018-01-01")),
             breaks=date_breaks(width = "1 year"), 
             labels = date_format("%y"),
             minor_breaks=NULL) + 
  #ggtitle('Significant DOT Rulemaking Projects Initiated Between 2008 and 2017') + 
  ggtitle('Significant DOT Rulemaking Projects Active Between 2008 and 2017') + 
  xlab("RIN")+ylab("Year")+
  theme(axis.text.y = element_text(size = 2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1),
        axis.ticks = element_blank()) 
  










#############################################
######### TIMELINES #####################
dotcomplete <- filter(d, !is.na(FinalRulePublished) | !is.na(WithdrawalPublished))

dotcomplete2008 <- subset(dotcomplete, initiated>"2008-01-01")


# TOY Timeline
ggplot(dotcomplete, aes(x=RIN)) +
  # facet_grid(agency ~ .) +
  geom_linerange(aes(ymin=initiated, ymax=enddate, color="black"), size=.1) +
  #geom_point(aes(y=WithdrawalPublished, color="red", shape = 4), size=.4) + 
  geom_point(aes(y=WithdrawalPublished, color="red"), shape = 15, size=.4) + 
  geom_point(aes(y=FinalRulePublished, color="black"), shape = 15, size=.4) + 
  coord_flip() +  
  scale_y_date(lim = c(as.Date("1988-01-01"), as.Date("2018-01-01")),
               breaks=date_breaks(width = "1 year"), labels = date_format("%y"),
               minor_breaks=NULL)+
  ggtitle('Complete Cases (Final or Withdrawal Published, N = 163)')+ xlab("RIN")+ylab("Year")+
  scale_color_identity("",guide="legend", labels = c("Final Rule", "Withdrawal"))+
  theme(axis.text.y =
          element_text(size  =2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1)) 


# old code: 
# # Where missing initiated, use ANPRM date 
# d$initiated[which(is.na(d$initiated))] <- d$ANPRMpublished[which(is.na(d$initiated))]
# 
# # Where still missing initiated, use NPRM date 
# d$initiated[which(is.na(d$initiated))] <- d$NPRMpublished[which(is.na(d$initiated))]
# 
# # where missing end date, use report date 
# #d$enddate[which(is.na(d$enddate) & d$DOTdate==max(d$DOTdate, na.rm = T))] <- max(d$DOTdate, na.rm = T)
# d$enddate[which(is.na(d$enddate))] <- d$Terminated[which(is.na(d$enddate))]
# d$enddate[which(is.na(d$enddate))] <- d$reportdate[which(is.na(d$enddate))]

# Where missing initiated, use ANPRM date 
d %<>% 
  mutate(initiated = if_else(is.na(initiated), ANPRMpublished, initiated)) %>%
  # Where still missing initiated, use NPRM date 
  mutate(initiated = if_else(is.na(initiated), NPRMpublished, initiated)) %>% 
  # where missing end date, use report date 
  # mutate(enddate = ifelse(is.na(enddate), Terminated, enddate)) %>% 
  mutate(enddate = if_else(is.na(enddate), Terminated, enddate)) 

  
  
# Timeline
ggplot(d %>% filter(initiated > as.Date("2008-01-01")), aes(x=RIN)) +
#  ggplot(d%>% filter(initiated > as.Date("1988-01-01")), aes(x=RIN)) +
  #facet_grid(agency ~ .) +
  geom_linerange(aes(ymin=initiated, ymax=enddate, color="bisque"), size=.4, show.legend = F) +
  geom_point(aes(y=DOTdate, color=color), size=.1) + 
  geom_linerange(aes(ymin=ANPRMtoOMB, ymax=ANPRMclearOMB, color="pink"), size=.2) +
  geom_linerange(aes(ymin=NPRMtoOMB, ymax=NPRMclearOMB, color="magenta"), size=.2) +
  geom_linerange(aes(ymin=SNPRMtoOMB, ymax=SNPRMclearOMB, color="purple"), size=.2) +
  geom_linerange(aes(ymin=IFRtoOMB, ymax=IFRclearOMB, color="Sky Blue"), size=.2) +
  geom_linerange(aes(ymin=FinalRuletoOMB, ymax=FinalRuleclearOMB, color="blue"), size=.2) +
  geom_text(aes(y=ANPRMtoOST, color="pink"), label = "SEC", size=.8) +  
  geom_text(aes(y=NPRMtoOST, color="magenta"), label = "SEC", size=.8) +  
  geom_text(aes(y=SNPRMtoOST, color="purple"), label = "SEC", size=.8) +  
  geom_text(aes(y=IFRtoOST), color="Sky Blue", label = "SEC", size=.8) +  
  geom_text(aes(y=FinalRuletoOST), label = "SEC", color="blue", size = .8) +  
  geom_text(aes(y=ANPRMtoOMB, color="pink"), label = "OMB",size=.8) +  
  geom_text(aes(y=NPRMtoOMB, color="magenta"), label = "OMB",size=.8) +  
  geom_text(aes(y=SNPRMtoOMB, color="purple"), label = "OMB",size=.8) +  
  geom_text(aes(y=IFRtoOMB, color="Sky Blue"), label = "OMB",size=.8) +  
  geom_text(aes(y=FinalRuletoOMB, color="blue"), label = "OMB",size=.8) + 
  #geom_text(aes(y=ANPRMclearOMB, color="pink"), label = "OMB",size=.8) +  
  #geom_text(aes(y=NPRMclearOMB, color="magenta"), label = "OMB",size=.8) +  
  #geom_text(aes(y=SNPRMclearOMB, color="purple"), label = "OMB",size=.8) +  
  #geom_text(aes(y=IFRclearOMB, color="Sky Blue"), label = "OMB",size=.8) +  
  #geom_text(aes(y=FinalRuleclearOMB, color="blue"), label = "OMB",size=.8) + 
  geom_point(aes(y=ANPRMpublished, color="pink"), shape = 15, size=.4) + 
  geom_point(aes(y=NPRMpublished, color="magenta"), shape = 15, size=.4) + 
  geom_point(aes(y=SNPRMpublished, color="purple"), shape = 15, size=.4) + 
  geom_point(aes(y=IFRpublished), color="Sky Blue", shape = 15, size=.4) + 
  geom_point(aes(y=WithdrawalPublished), color="red", shape = 4, size=1) + 
  geom_point(aes(y=Terminated), color="red4", shape = 4, size=1) + 
  #geom_point(aes(y=red, color="red"), shape = "D", size=.6) + 
  #geom_point(aes(y=yellow, color="yellow"), shape = "D", size=.6) + 
  #geom_point(aes(y=green, color="green"), shape = "D", size=.6) + 
  geom_point(aes(y=FinalRulePublished, color="blue"), shape = 15, size=.8, show.legend = F) + 
  coord_flip() +  
  #scale_y_date(lim = c(as.Date("1988-01-01"), as.Date("2018-01-01")),
  scale_y_date(lim = c(as.Date("2008-01-01"), as.Date("2018-01-01")),
               breaks=date_breaks(width = "1 year"), labels = date_format("%y"),
               minor_breaks=NULL)+
  ggtitle('Significant DOT Rulemaking Projects Initiated Between 2008 and 2017') + 
  # ggtitle('Significant DOT Rulemaking Projects Active Between 2008 and 2017') + 
    
    xlab("RIN")+ylab("Year")+

  theme(axis.text.y =
          element_text(size  =2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1))  +

scale_color_identity(""
                     ,guide="legend",
labels=c("Project Active", 
         "No Schedule", 
         "Final Rule Stage", 
         "On Schedule",
         "NPRM Stage",
         "ANPRM Stage",
         "SNPRM Stage",
         "X = Withdrawn",
         "Delayed",
         "X = Terminated",
         "Interm Final Rule Stage",
         "Likely Delayed")) 

min(d$initiated)


# Initiated * Published Fit (takes 4 years from initiated)
ggplot(dotRIN%>% filter(enddate > as.Date("2008-01-01"))) + 
  geom_point(aes(x=FinalRulePublished, y= initiated, color=color)) +
  geom_point(aes(WithdrawalPublished, initiated), color = "red", shape = 4) +
  geom_smooth(aes(x=FinalRulePublished, y = initiated), method='auto')+
  scale_y_date(breaks=date_breaks(width = "1 year"), labels = date_format("%Y"),
               minor_breaks=NULL)+
  scale_x_date(breaks=date_breaks(width = "1 year"), labels = date_format("\'%y"),
               minor_breaks=NULL) + 
  scale_color_identity("",guide="legend", labels = c("No Timeline",
                                                     "On Time",
                                                     "Delayed",
                                                     "Likely Delayed"))+ 
  ggtitle('Significant DOT Final Rules and Withdrawals Published 2008 - 2017
          (The average Final Rule takes 4 years, but is scheduled for less)') + 
  xlab("Publication")+ylab("Initiated")

# Initiated * NPRM
# FIXME
ggplot(dotRIN%>% filter(NPRMpublished > as.Date("2008-01-01"))) + 
  geom_point(aes(x=NPRMpublished, y= initiated, color=color)) +
  geom_smooth(aes(x=NPRMpublished, y = initiated), method='auto')+
  scale_y_date(breaks=date_breaks(width = "1 year"), labels = date_format("%Y"),
               minor_breaks=NULL)+
  scale_x_date(breaks=date_breaks(width = "1 year"), labels = date_format("\'%y"),
               minor_breaks=NULL) + 
  scale_color_identity("",guide="legend", labels = c("No Timeline",
                                                     "On Time",
                                                     "Delayed",
                                                     "Likely Delayed"))+ 
  ggtitle('Significant DOT NPRMs Published 2008 - 2017
(The average NPRM takes 3 years, but is scheduled for less)') + 
  xlab("Publication")+ylab("Initiated")






# DELAY 
# FIXME
DOTmonthly %<>% 
  mutate(coordinationDelay = {1*grepl("Additional coordination necessar", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7))}) %>% 
  mutate(staffingDelay = 1*grepl("Lack of staffing", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7))) %>% 
  mutate(issuesDelay = 1*grepl("Unanticipated issues requiring further analysis", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(researchDelay = 1*grepl("Additional research and data analysis necessar", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(prioritiesDelay = 1*grepl("Other, higher priorities", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(dataDelay = 1*grepl("Awaiting development of additional data", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(legislationDelay = 1*grepl("Awaiting outcome of pending legislation", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(resourcesDelay = 1*grepl("Lack of resources", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(coordinationLegislationDelay = 1*grepl("Additional Coordination and New Legislation", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(appointmentDelay = 1*grepl("Awaiting appointment/confirmation of political appointees", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))%>% 
  mutate(regulatorycoordinationDelay = 1*grepl("Additional coordination needed for regulatory evaluation", paste(.$whydelay, .$whydelay1, .$whydelay2, .$whydelay3, .$whydelay4, .$whydelay5, .$whydelay6, .$whydelay7)))

reasons <- DOTmonthly %>% dplyr::select(DOTdate, initiated, RIN, coordinationDelay, staffingDelay, issuesDelay, researchDelay, prioritiesDelay, dataDelay, legislationDelay, 
                                        resourcesDelay, coordinationLegislationDelay, appointmentDelay, regulatorycoordinationDelay)
reasons %<>% melt(id=c("DOTdate", "initiated", "RIN"))
reasons %<>% filter(value>0) %<>% group_by(DOTdate, RIN)
reasons$value %<>% as.factor()
reasons %<>% mutate(year = substr(DOTdate, 1,4))

reasons$variable  %<>% 
  {gsub("regulatorycoordinationDelay", "Additional coordination needed for regulatory evaluation",.)} %>% 
  {gsub("coordinationDelay", "Additional coordination necessary",.)} %>% 
  {gsub("staffingDelay", "Lack of staffing",.)} %>% 
  {gsub("issuesDelay", "Unanticipated issues requiring further analysis",.)} %>% 
  {gsub("researchDelay", "Additional research and data analysis necessary",.)} %>% 
  {gsub("prioritiesDelay", "Other, higher priorities",.)} %>% 
  {gsub("dataDelay", "Awaiting additional data",. )} %>% 
  {gsub("legislationDelay", "Awaiting pending legislation",.)} %>% 
  {gsub("resourcesDelay", "Lack of resources",.)} %>% 
  {gsub("coordinationLegislationDelay", "Additional coordination and new legislation",.)} %>% 
  {gsub("appointmentDelay", "Awaiting appointment/confirmation of political appointees",.)}


ggplot(reasons, aes(DOTdate)) + geom_bar(aes(fill = variable))+ facet_grid( ~ year, scales="free_x") +
  ggtitle('Reasons for Delay in Significant DOT Rulemaking Projects Active 2008 - 2017') + 
  # ggtitle('Significant DOT Rulemaking Projects Active Between 2008 and 2017') + 
  
  xlab("")+ylab("Number of Rulemaking Projects")+
  
  theme(axis.text.x = element_blank(), legend.title = element_blank())

# reasons for delay by date initiated
ggplot(reasons) +
  geom_point(aes(x=DOTdate, y=initiated),color="bisque") + 
  geom_jitter(aes(x=DOTdate, y= initiated, color=factor(variable)), height = 100, alpha = .4) +
  scale_y_date(breaks=date_breaks(width = "1 year"), labels = date_format("%Y"),
               minor_breaks=NULL)+
  scale_x_date(breaks=date_breaks(width = "1 year"), labels = date_format("\'%y"),
               minor_breaks=NULL) + 
  ggtitle('DOT Reasons For Delay') + 
  xlab("Month")+ylab("Initiated") 

# reasons for delay by RIN
ggplot(reasons) +
  geom_point(aes(x=DOTdate, y=RIN),color="bisque") + 
  geom_jitter(aes(x=DOTdate, y= RIN, color=factor(variable)), height = 1, alpha = .4) +
  scale_x_date(breaks=date_breaks(width = "1 year"), labels = date_format("\'%y"),
               minor_breaks=NULL) + 
  ggtitle('DOT Reasons For Delay by RIN') + 
  
  ylab("RIN")+xlab("Year")+
  
  theme(axis.text.y =
          element_text(size  =2,
                       angle = 0,
                       hjust = 1,
                       vjust = 1)) 






class(d$initiated)
class(d$ANPRMtoOMB)
class(d$NPRMtoOMB)
class(d$SNPRMtoOMB)
class(d$IFRtoOMB)
class(d$FinalRuletoOMB)
class(d$ANPRMtoOST) 
class(d$NPRMtoOST)
class(d$SNPRMtoOST) 
class(d$IFRtoOST) 
class(d$FinalRuletoOST)
class(d$ANPRMtoOMB)
class(d$NPRMtoOMB)
class(d$SNPRMtoOMB)
class(d$IFRtoOMB) 
class(d$FinalRuletoOMB)
class(d$ANPRMclearOMB)  
class(d$NPRMclearOMB) 
class(d$SNPRMclearOMB) 
class(d$IFRclearOMB)
class(d$FinalRuleclearOMB)
class(d$ANPRMpublished)
class(d$NPRMpublished)
class(d$SNPRMpublished)
class(d$IFRpublished)
class(d$WithdrawalPublished)
class(d$FinalRulePublished)
class(d$FinalRuleDeadline)
