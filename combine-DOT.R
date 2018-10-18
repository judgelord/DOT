
# This script combines .csv files processed with the DOTrules.R script into two data frames, one of all observations and one of just the most recent observation for each RIN. It also cleans, formats, and organizes these data.
# It then merges them with data from OIRA via reginfo.gov


# Load required packages and functions
source("setup.R")
Sys.setlocale('LC_ALL','C') 

## Load all ORIA + UA object named "regs"
load( url("https://github.com/judgelord/reginfo.gov/raw/master/data/OIRAandUA.Rdata") )
## or
# regs <- read.csv("Data/OIRA and UA.csv")

#########################################################################

# Read in first report "reports/DOTclean/200801DOTclean.csv"
dot <- read.csv("reports/DOTclean/200801DOTclean.csv")


# Read in the rest
dot <- dot[0,] # start with empty data frame
for( i in list.files(path = "reports/DOTclean/") ){
  print(c(i, dim(read.csv(here("reports/DOTclean/", i)))))
  dot <- rbind(dot, read.csv(here("reports/DOTclean/", i)))
}

# MISSING:
  # read.csv("200808DOTclean.csv", stringsAsFactors = F)
  #   read.csv("200808bDOTclean.csv", stringsAsFactors = F)
  #  read.csv("200912bDOTclean.csv", stringsAsFactors = F)
  

# MISSING OR YET TO BE CLEANED 
#read.csv("201606DOTclean.csv", stringsAsFactors = F),
#read.csv("201607DOTclean.csv", stringsAsFactors = F),
#read.csv("201608DOTclean.csv", stringsAsFactors = F),
#read.csv("201609DOTclean.csv", stringsAsFactors = F),
#read.csv("201610DOTclean.csv", stringsAsFactors = F),
#read.csv("201611DOTclean.csv", stringsAsFactors = F),
# + 2018 other than Sept

# order by date decending
# dot <- dot[order(-dot$date),]
original <- dot
#replace NA with blank
dot <- dot[!is.na(dot$RIN),] 
dot[is.na(dot)] <- ""

# copy to correct location
# all effects
dot$effects <- gsub("Prompting action.*", "", apply(dot[ , 11:17 ] , 1 , paste , collapse = ", " ))
dot$effects <- gsub(", , ", "", dot$effects)
dot$effects <- gsub(", , ", "", dot$effects)
dot$effects <- gsub(", , ", "", dot$effects)

# prompt to legal deadline
for(i in 1:dim(dot)[1]){
  if(grepl("Legal Deadline", dot$prompt[i], ignore.case = T)){
    dot$legaldeadline1[i] <- dot$prompt[i]
  }}
dot$legaldeadline1 <- apply(dot[ , 19:24 ] , 1 , paste , collapse = " " )
dot$legaldeadline1 <- gsub(pattern = "^.*Legal Deadline: ","", dot$legaldeadline1, ignore.case=TRUE)

#legal deadline to initiated
for(i in 1:dim(dot)[1]){
  if(grepl("Rulemaking Project Initiated", dot$legaldeadline1[i], ignore.case = T)){
    dot$initiated[i] <- dot$legaldeadline1[i]
  }}
dot$initiated <- gsub(pattern = "^.*Rulemaking Project Initiated: ","", dot$initiated, ignore.case=TRUE)

# initiated to docket
for(i in 1:dim(dot)[1]){
  if(grepl("Docket Number", dot$legaldeadline1[i], ignore.case = T)){
    dot$docket[i] <- dot$legaldeadline1[i]
  }}
dot$docket <- gsub(pattern = "^.*Docket Number: ","", dot$docket, ignore.case=TRUE)
dot$docket <- gsub(pattern = "<d0>","-", dot$docket, ignore.case=TRUE)

# remove discriptors ("RIN: " "Prompting Action: " etc.) 
dot$RIN <- gsub("^.*RIN ", "", dot$RIN)
dot$title <- gsub("^.*Popular Title: ", "", dot$title)
dot$stage <- gsub("^.*Stage: ", "", dot$stage)
dot$PrevStage <- gsub("^.*(Previous Stage: |Previous Stage:)", "", dot$PrevStage)
dot$abstract <- gsub("^.*Abstract: ", "", dot$abstract)
dot$prompt <- gsub("^.*Prompting action: ", "", dot$prompt)
dot$legaldeadline1 <- gsub("^.*Legal Deadline: ", "", dot$legaldeadline1)
dot$initiated <- gsub("^.*Rulemaking Project Initiated: ", "", dot$initiated)
dot$docket <- gsub("^.*(Docket Number: |Docket Number:)", "", dot$docket)
dot$whydelay1 <- gsub("^.*Explanation for any delay: ", "", dot$whydelay1)
dot$fedreg <- gsub("^.*Federal Register Citation for ", "", dot$fedreg)

# delete extra text after  
dot$prompt<-gsub(pattern = " Legal Deadline.*","", dot$prompt, ignore.case=TRUE)
dot$legaldeadline1 <- gsub(pattern = "(     | Rulemaking Project Initiated: .*)","", dot$legaldeadline1, ignore.case=F)
dot$initiated <-gsub(pattern = "( Docket Number|Docket Number).*","", dot$initiated, ignore.case=TRUE)
dot$docket <-gsub(pattern = "(Dates for| Dates for).*","", dot$docket, ignore.case=TRUE)
dot$whydelay1 <- gsub(" Federal Register Citation .*","", dot$whydelay1)

# copy aggregated things to new location
# previous dates
# ANPRM
dot$ANPRMpublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("ANPRM", dot$PrevStage[i], ignore.case = T)){
    dot$ANPRMpublished[i] <- dot$PrevStage[i]
  }}
dot$ANPRMpublished <-gsub(pattern = "^.*(ANPRM Publication Date |ANPRM: Publication Date: |ANPRM: Publication Date |ANPRM: Publication Approved |ANPRM: Publication |ANPRM: |ANPRM )","", dot$ANPRMpublished, ignore.case=TRUE)
dot$ANPRMpublished <-gsub(pattern = "( |;).*","", dot$ANPRMpublished, ignore.case=TRUE)

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="ANPRM" && !grepl("/", dot$ANPRMpublished[i])
    ){
    dot$ANPRMpublished[i] <- dot$publicationactual[i]
  }}

# SNPRM
dot$SNPRMpublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("SNPRM", dot$PrevStage[i], ignore.case = T)){
    dot$SNPRMpublished[i] <- dot$PrevStage[i]
  }}
dot$SNPRMpublished <-gsub(pattern = "^.*(SNPRM Publication Date |SNPRM: Publication Date: |SNPRM: Publication Date |SNPRM: Publication Approved |SNPRM: Publication |SNPRM: |SNPRM )","", dot$SNPRMpublished, ignore.case=TRUE)
dot$SNPRMpublished <-gsub(pattern = "( |;).*","", dot$SNPRMpublished, ignore.case=TRUE)

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="SNPRM" && !grepl("/", dot$SNPRMpublished[i])
  ){
    dot$SNPRMpublished[i] <- dot$publicationactual[i]
  }}

dot$PrevStage <- gsub("SNPRM", "S.N.P.R.M.", dot$PrevStage)
dot$PrevStage <- gsub("ANPRM", "A.N.P.R.M.", dot$PrevStage)
dot$stage <- gsub("ANPRM", "A.N.P.R.M.", dot$stage)
dot$stage <- gsub("ANPRM", "A.N.P.R.M.", dot$stage)

#NPRM
dot$NPRMpublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("NPRM", dot$PrevStage[i], ignore.case = T)){
    dot$NPRMpublished[i] <- dot$PrevStage[i]
  }}
dot$NPRMpublished <- gsub(pattern = "^.*(NPRM Publication Date |NPRM: Publication Date: |NPRM: Publication Date |NPRM: Publication Approved |NPRM: Publication |NPRM: |NPRM )","", dot$NPRMpublished, ignore.case=TRUE)
dot$NPRMpublished <- gsub(pattern = "( |;).*","", dot$NPRMpublished, ignore.case=TRUE)

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="NPRM" && !grepl("/", dot$NPRMpublished[i])
  ){
    dot$NPRMpublished[i] <- dot$publicationactual[i]
  }}

# Interim Final
dot$IFRpublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("Interim Final Rule", dot$PrevStage[i], ignore.case = T)){
    dot$IFRpublished[i] <- dot$PrevStage[i]
  }}
dot$IFRpublished <- gsub(pattern = "^.*(Interim Final Rule Publication Date |IFR: Date of Publication |IFR: Publication Date |Interim Final Rule: Publication Date: |Interim Final Rule: Publication Date |Interim Final Rule: Publication Approved |Interim Final Rule: Publication |Interim Final Rule: |Interim Final Rule )","", dot$IFRpublished, ignore.case=TRUE)

dot$IFRpublished <-gsub(pattern = "( |;|\\.).*","", dot$IFRpublished, ignore.case=TRUE)

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="Interim Final Rule" && !grepl("/", dot$IFRpublished[i])
  ){
    dot$IFRpublished[i] <- dot$publicationactual[i]
  }}

dot$PrevStage <- gsub("Interim Final Rule", "Interim FinalRule", dot$PrevStage)
dot$stage <- gsub("Interim Final Rule", "Interim FinalRule", dot$stage)

# Final
dot$FinalRulePublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("(Final Rule)", dot$PrevStage[i], ignore.case = T)){
    dot$FinalRulePublished[i] <- dot$PrevStage[i]
  }}
dot$FinalRulePublished <- gsub(pattern = "^.*(Final Rule Publication Date |Final Rule: Publication Date: |Final Rule: Publication Date |Final Rule: Publication Approved |Final Rule: Publication |Final Rule: |Final Rule )","", dot$FinalRulePublished, ignore.case=TRUE)
dot$FinalRulePublished <- gsub(pattern = "( |;|\\.).*","", dot$FinalRulePublished, ignore.case=TRUE)

sum(grepl("/", dot$FinalRulePublished))

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="Final Rule" && !grepl("/", dot$FinalRulePublished[i])
  ){
    dot$FinalRulePublished[i] <- dot$publicationactual[i] 
  }}
sum(grepl("/", dot$FinalRulePublished))

# Withdrawal
dot$WithdrawalPublished = NA
for(i in 1:dim(dot)[1]){
  if(grepl("Withdrawal", dot$PrevStage[i], ignore.case = T)){
    dot$WithdrawalPublished[i] <- dot$PrevStage[i]
  }}
dot$WithdrawalPublished <- gsub(pattern = "^.*(Withdrawal Publication Date |Withdrawal: Publication Date: |Withdrawal: Publication Date |Withdrawal: Publication Approved |Withdrawal: Publication |Withdrawal: |Withdrawal of HoS Supp Docs NPRM: )","", dot$WithdrawalPublished, ignore.case=TRUE)
dot$WithdrawalPublished <- gsub(pattern = "( |;|\\.).*","", dot$WithdrawalPublished, ignore.case=TRUE)

for(i in 1:dim(dot)[1]){
  if(
    dot$stage[i]=="Withdrawal" && !grepl("/", dot$WithdrawalPublished[i])
  ){
    dot$WithdrawalPublished[i] <- dot$publicationactual[i]
  }}
sum(grepl("/", dot$WithdrawalPublished))

dot$Terminated <- NA
for(i in 1:dim(dot)[1]){
  if(
    grepl("Terminat.*", dot$stage[i])
  )
    dot$Terminated[i] <- as.character(dot$publicationactual[i])
}

# return modified text to orgiginal
dot$PrevStage <- gsub("Interim FinalRule", "Interim Final Rule", dot$PrevStage)
dot$stage <- gsub("Interim FinalRule", "Interim Final Rule", dot$stage)

# deadlines
dot$FinalRuleDeadline = NA
for(i in 1:dim(dot)[1]){
  if(grepl("(FR|final rule)", dot$legaldeadline1[i], ignore.case = T)){
    dot$FinalRuleDeadline[i] <- dot$legaldeadline1[i]
    }}
dot$FinalRuleDeadline <- gsub(pattern = "^.*(FR|final rule).*?: ","", dot$FinalRuleDeadline, ignore.case=TRUE)
dot$FinalRuleDeadline <- gsub(pattern = " .*","", dot$FinalRuleDeadline, ignore.case=TRUE)

dot$IFRDeadline = NA
for(i in 1:dim(dot)[1]){
  if(grepl("temporary rule", dot$legaldeadline1[i], ignore.case = T)){
    dot$IFRDeadline[i] <- dot$legaldeadline1[i]
  }}
dot$IFRDeadline <- gsub(pattern = "^.*temporary rule.*?: ","", dot$IFRDeadline, ignore.case=TRUE)
dot$IFRDeadline <- gsub(pattern = " .*","", dot$IFRDeadline, ignore.case=TRUE)

dot$NPRMDeadline = NA
for(i in 1:dim(dot)[1]){
  if(grepl("NPRM", dot$legaldeadline1[i], ignore.case = T)){
    dot$NPRMDeadline[i] <- dot$legaldeadline1[i]
  }}
dot$NPRMDeadline <- gsub(pattern = "^.*NPRM.*?: ","", dot$NPRMDeadline, ignore.case=TRUE)
dot$NPRMDeadline <- gsub(pattern = " .*","", dot$NPRMDeadline, ignore.case=TRUE)




# delete misplaced things
dot$prompt <- gsub(" Legal Deadline: .*", "", dot$prompt)





###################################################################



# create data frame of only last observations
#dotlast <- dot[!duplicated(dot$RIN),]
# select the most recent observation for each RIN at each Stage
dotlast <- dot %>% 
  dplyr::group_by(RIN, stage) %>% 
  dplyr::top_n(1, date)

#replace incorrect titles etc. with those from dotlast
for(i in 1:dim(dot)[1])
{
  for(j in c(2,4,5,25,26))
  {
      dot[i,j] <- dotlast[which(dot$RIN[i]==dotlast$RIN),j]
  }
}


#########################################################################

dot$PrevStage <- gsub("S.N.P.R.M.", "SNPRM", dot$PrevStage)
dot$PrevStage <- gsub("A.N.P.R.M.", "ANPRM", dot$PrevStage)
dot$stage <- gsub("A.N.P.R.M.", "ANPRM", dot$stage)
dot$stage <- gsub("A.N.P.R.M.", "ANPRM", dot$stage)

# format dates

dot$docket <- gsub(pattern = "<..>","-", dot$docket, ignore.case=TRUE)


for(x in c(27:77, 25,88:96)){
  dot[,x] <- gsub("\\/198","\\/8", dot[,x])
  dot[,x] <- gsub("\\/199","\\/9", dot[,x])
  dot[,x] <- gsub("\\/200","\\/0", dot[,x])
  dot[,x] <- gsub("\\/201","\\/1", dot[,x])
dot[,x] <- as.Date(
  #gsub("[^0-9,\\/]", "", dot[,x])
  dot[,x]
  , "%m/%d/%y")
}

for(i in 1:dim(dot)[1]){dot$date[i] <- paste0(dot$date[i],"01")}
dot$DOTdate<-as.Date(as.character(dot$date), "%Y%m%d")

# Removed extra columns
dot$effect1 = NULL
dot$effect2 = NULL
dot$effect3 = NULL
dot$effect4 = NULL
dot$effect5 = NULL
dot$effect6 = NULL
dot$effect7 = NULL
dot$legaldeadline2 = NULL
dot$legaldeadline3 = NULL
dot$legaldeadline4 = NULL
dot$legaldeadline5 = NULL
dot$legaldeadline6 = NULL
dot$whydelay <- paste(dot$whydelay2, dot$whydelay2, dot$whydelay3, dot$whydelay4, dot$whydelay5, dot$whydelay6, dot$whydelay7, dot$whydelay8, sep = ", ")
dot$whydelay <- gsub(", , ",", ", dot$whydelay)
dot$whydelay <- gsub(", , ",", ", dot$whydelay)
dot$whydelay <- gsub(", , ",", ", dot$whydelay)
dot$whydelay <- gsub(", , ",", ", dot$whydelay)
dot$whydelay <- gsub(", , ",", ", dot$whydelay)
dot$whydelay <- gsub("^,|^, |^ ,", "", dot$whydelay)






###########################################################################################
# change name of stage to match Unified Agenda
names(dot) <- gsub("stage", "dotstage", names(dot))
dot$STAGE <- dot$dotstage # retain original

dot$STAGE <- gsub("^ ", "", dot$STAGE) # delete extra space 
unique(dot$STAGE)

dot$STAGE <- gsub(".*ANPRM.*", "Prerule", dot$STAGE)
dot$STAGE <- gsub("NPRM.*", "Proposed Rule", dot$STAGE)
dot$STAGE <- gsub("SProposed Rule","SNPRM", dot$STAGE)
dot$STAGE <- gsub("Final Rule .*", "Final Rule", dot$STAGE)
dot$STAGE <- gsub(".*of Final Rule", "Final Rule", dot$STAGE)
dot$STAGE <- gsub("Request for Comments", "Other", dot$STAGE) # FIXME? should this be NPRM? 
dot$STAGE <- gsub("Disposition of Comments", "Other", dot$STAGE)
dot$STAGE <- gsub("Supplemental Notice of Intent", "Prerule", dot$STAGE)
dot$STAGE[which(dot$dotstage=="Interim Final Rule.*")]<-"Interim Final Rule"
dot$STAGE <- gsub("Terminat.*", "Termination", dot$STAGE)



# correct mislabled stages
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2009-02-01" & dot$RIN=="2120-AJ37")] <- "Final Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2010-10-01" & dot$RIN=="2120-AJ51")] <- "Final Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2013-02-01" & dot$RIN=="2120-AJ83")] <- "Proposed Rule"
dot$STAGE[which(dot$STAGE=="Interim Final Rule" & dot$DOTdate=="2016-05-01" & dot$RIN=="2120-AK82")] <- "Final Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2012-03-01" & dot$RIN=="2125-AF40")] <- "Proposed Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2016-05-01" & dot$RIN=="2126-AA64")] <- "Final Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2008-01-01" & dot$RIN=="2130-AB72")] <- "Final Rule"
dot$STAGE[which(dot$STAGE=="Undetermined" & dot$DOTdate=="2015-03-01" & dot$RIN=="2105-AD90")] <- "Termination"

# Is it working? 
dot$toOMBactual[which(dot$toOMBactual == "2011-02-02" & dot$RIN=="2127-AE13")] <- NA # this date is clearly wrong as OMB reports recieveing it earlier and DOT reports sending it back to OMB before this date
dot$toOMBactual[which(dot$toOMBactual == "2014-07-21" & dot$RIN=="2137-AF08")] <- NA # this date is clearly wrong as it was already approved by OMB by then, but OIRA reports recieving it 1 day after initiated, which is suspicious


for(i in 1:dim(dot)[1]){
  if(
    !is.na(dot$dotstage[i]) &
    dot$dotstage[i]=="Final Rule/2"
    ){
    dot$STAGE[i] <- "Final Rule/2"
  }}


for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i]=="Final Rule/2"){
    dot$toOMBactual[i] <- dot$resubmitOMB2actual[i]
  }}


dotALL <- dot


# select the most recent observation for each RIN at each Stage
dot <- dplyr::group_by(dot, RIN, STAGE)
dotlast <- dplyr::top_n(dot, 1, DOTdate)
dot <- dplyr::ungroup(dot)



































#############################################################
# Merge OIRA/UA with DOT #
#############################################################

# merge data
dot <- full_join(dotlast,
                 # subset OIRA and Unified Agenda  to only DOT RINs
                 regs %>% filter( grepl("^21", RIN) ) ) 

dot <- unique.data.frame(dot)


dot$reportdate <- dot$DOTdate
for(i in 1:dim(dot)[1]){
  if(!grepl("-", dot$reportdate[i])){dot$reportdate[i] <- as.character(dot$UnifiedAgendaDate[i])}
  if(!grepl("-", dot$reportdate[i])){dot$reportdate[i] <- as.character(dot$DATE_RECEIVED[i])}
}



# copy info to across obs of the same initialization date # CHECK THIS, perhaps use group_by() # FIXME
dot %<>% arrange((reportdate)) %>%
  transform(initiated = ave(initiated, RIN, FUN = CopyIfNA)) %>%
  transform(agency = ave(agency, RIN, FUN = CopyIfNA)) %>%
  transform(prompt = ave(prompt, RIN, FUN = CopyIfNA)) %>%# UA vars
  transform(title = ave(title, RIN, FUN = CopyIfNA)) %>%
  transform(abstract = ave(abstract, RIN, FUN = CopyIfNA)) %>%
  transform(fulltitle = ave(fulltitle, RIN, FUN = CopyIfNA))
dot %<>% arrange(desc(reportdate)) %>%
  transform(initiated = ave(initiated, RIN, FUN = CopyIfNA)) %>%
  transform(agency = ave(agency, RIN, FUN = CopyIfNA)) %>%
  transform(prompt = ave(prompt, RIN, FUN = CopyIfNA)) %>%# UA vars
  transform(title = ave(title, RIN, FUN = CopyIfNA)) %>%
  transform(abstract = ave(abstract, RIN, FUN = CopyIfNA)) %>%
  transform(fulltitle = ave(fulltitle, RIN, FUN = CopyIfNA))

# Replace missing titles and agency names with UA data (for now, until replacing with mutate(if_else) which works with dates)
# To OST Scheduled 
dot$ANPRMtoOSTscheduled = NA
dot$NPRMtoOSTscheduled = NA
dot$SNPRMtoOSTscheduled = NA
dot$IFRtoOSTscheduled = NA
dot$FinalRuletoOSTscheduled = NA
for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i] == "Prerule"){dot$ANPRMtoOSTscheduled[i] <- as.character(dot$toOSTscheduled[i])}
  if(dot$STAGE[i] == "Proposed Rule"){dot$NPRMtoOSTscheduled[i] <- as.character(dot$toOSTscheduled[i])}
  if(dot$STAGE[i] == "SNPRM"){dot$SNPRMtoOSTscheduled[i] <- as.character(dot$toOSTscheduled[i])}
  if(dot$STAGE[i] == "Interim Final Rule"){dot$IFRtoOSTscheduled[i] <- as.character(dot$toOSTscheduled[i])}
  if(dot$STAGE[i] == "Final Rule"){dot$FinalRuletoOSTscheduled[i] <- as.character(dot$toOSTscheduled[i])}
}
# To OST 
dot$ANPRMtoOST = NA
dot$NPRMtoOST = NA
dot$SNPRMtoOST = NA
dot$IFRtoOST = NA
dot$FinalRuletoOST = NA
for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i] == "Prerule"){dot$ANPRMtoOST[i] <- as.character(dot$toOSTactual[i])}
  if(dot$STAGE[i] == "Proposed Rule"){dot$NPRMtoOST[i] <- as.character(dot$toOSTactual[i])}
  if(dot$STAGE[i] == "SNPRM"){dot$SNPRMtoOST[i] <- as.character(dot$toOSTactual[i])}
  if(dot$STAGE[i] == "Interim Final Rule"){dot$IFRtoOST[i] <- as.character(dot$toOSTactual[i])}
  if(dot$STAGE[i] == "Final Rule"){dot$FinalRuletoOST[i] <- as.character(dot$toOSTactual[i])}
}
# Clear OST
dot %<>%
  mutate(ANPRMclearOST = ifelse(dot$STAGE == "Prerule", as.character(dot$toModeactual), NA)) %>%
  mutate(NPRMclearOST = ifelse(dot$STAGE == "Proposed Rule", as.character(dot$toModeactual), NA)) %>%
  mutate(SNPRMclearOST = ifelse(dot$STAGE == "SNPRM", as.character(dot$toModeactual), NA)) %>%
  mutate(IFRclearOST = ifelse(dot$STAGE == "Interim Final Rule", as.character(dot$toModeactual), NA)) %>%
  mutate(FinalRuleclearOST = ifelse(dot$STAGE == "Final Rule", as.character(dot$toModeactual), NA))

# OST
dot %<>% arrange((reportdate)) %>%
  transform(ANPRMtoOST = ave(ANPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOST = ave(SNPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOST = ave(NPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOST = ave(IFRtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOST = ave(FinalRuletoOST, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMtoOSTscheduled = ave(ANPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOSTscheduled = ave(SNPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOSTscheduled = ave(NPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOSTscheduled = ave(IFRtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOSTscheduled = ave(FinalRuletoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMclearOST = ave(ANPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMclearOST = ave(SNPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMclearOST = ave(NPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(IFRclearOST = ave(IFRclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleclearOST = ave(FinalRuleclearOST, RIN, FUN = CopyIfNA))
dot %<>% arrange(desc(reportdate)) %>%
  transform(ANPRMtoOST = ave(ANPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOST = ave(SNPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOST = ave(NPRMtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOST = ave(IFRtoOST, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOST = ave(FinalRuletoOST, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMtoOSTscheduled = ave(ANPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOSTscheduled = ave(SNPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOSTscheduled = ave(NPRMtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOSTscheduled = ave(IFRtoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOSTscheduled = ave(FinalRuletoOSTscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMclearOST = ave(ANPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMclearOST = ave(SNPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMclearOST = ave(NPRMclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(IFRclearOST = ave(IFRclearOST, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleclearOST = ave(FinalRuleclearOST, RIN, FUN = CopyIfNA))
  
  
# To OMB 
  #Scheduled 
  dot$ANPRMtoOMBscheduled = NA
dot$NPRMtoOMBscheduled = NA
dot$SNPRMtoOMBscheduled = NA
dot$IFRtoOMBscheduled = NA
dot$FinalRuletoOMBscheduled = NA
for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i] == "Prerule"){dot$ANPRMtoOMBscheduled[i] <- as.character(dot$toOMBscheduled[i])}
  if(dot$STAGE[i] == "Proposed Rule"){dot$NPRMtoOMBscheduled[i] <- as.character(dot$toOMBscheduled[i])}
  if(dot$STAGE[i] == "SNPRM"){dot$SNPRMtoOMBscheduled[i] <- as.character(dot$toOMBscheduled[i])}
  if(dot$STAGE[i] == "Interim Final Rule"){dot$IFRtoOMBscheduled[i] <- as.character(dot$toOMBscheduled[i])}
  if(dot$STAGE[i] == "Final Rule"){dot$FinalRuletoOMBscheduled[i] <- as.character(dot$toOMBscheduled[i])}
}
  # Dates To OMB
dot$ANPRMtoOMB = NA
dot$NPRMtoOMB = NA
dot$SNPRMtoOMB = NA
dot$IFRtoOMB = NA
dot$FinalRuletoOMB = NA
for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i] == "Prerule"){dot$ANPRMtoOMB[i] <- as.character(dot$toOMBactual[i])}
  if(!grepl("-", dot$ANPRMtoOMB[i])){dot$ANPRMtoOMB[i] <- as.character(dot$ANPRM_RECIEVED[i])}
  if(dot$STAGE[i] == "Proposed Rule"){dot$NPRMtoOMB[i] <- as.character(dot$toOMBactual[i])}
  if(!grepl("-", dot$NPRMtoOMB[i])){dot$NPRMtoOMB[i] <- as.character(dot$NPRM_RECIEVED[i])}
  if(dot$STAGE[i] == "SNPRM"){dot$SNPRMtoOMB[i] <- as.character(dot$toOMBactual[i])}
  if(!grepl("-", dot$SNPRMtoOMB[i])){dot$SNPRMtoOMB[i] <- as.character(dot$SNPRM_RECIEVED[i])}
  if(dot$STAGE[i] == "Interim Final Rule"){dot$IFRtoOMB[i] <- as.character(dot$toOMBactual[i])}
  if(!grepl("-", dot$IFRtoOMB[i])){dot$IFRtoOMB[i] <- as.character(dot$IFR_RECIEVED[i])}
  if(dot$STAGE[i] == "Final Rule"){dot$FinalRuletoOMB[i] <- as.character(dot$toOMBactual[i])}
  if(!grepl("-", dot$FinalRuletoOMB[i])){dot$FinalRuletoOMB[i] <- as.character(dot$FINAL_RECIEVED[i])}
}
  # Clear OMB
dot$ANPRMclearOMB = NA
dot$NPRMclearOMB = NA
dot$SNPRMclearOMB = NA
dot$IFRclearOMB = NA
dot$FinalRuleclearOMB = NA
for(i in 1:dim(dot)[1]){
  if(dot$STAGE[i] == "Prerule"){dot$ANPRMclearOMB[i] <- as.character(dot$clearOMBactual[i])}
  if(!grepl("-", dot$ANPRMclearOMB[i])){dot$ANPRMclearOMB[i] <- as.character(dot$ANPRM_COMPLETED[i])}
  if(dot$STAGE[i] == "Proposed Rule"){dot$NPRMclearOMB[i] <- as.character(dot$clearOMBactual[i])}
  if(!grepl("-", dot$NPRMclearOMB[i])){dot$NPRMclearOMB[i] <- as.character(dot$NPRM_COMPLETED[i])}
  if(dot$STAGE[i] == "SNPRM"){dot$SNPRMclearOMB[i] <- as.character(dot$clearOMBactual[i])}
  if(!grepl("-", dot$SNPRMclearOMB[i])){dot$SNPRMclearOMB[i] <- as.character(dot$SNPRM_COMPLETED[i])}
  if(dot$STAGE[i] == "Interim Final Rule"){dot$IFRclearOMB[i] <- as.character(dot$clearOMBactual[i])}
  if(!grepl("-", dot$IFRclearOMB[i])){dot$IFRclearOMB[i] <- as.character(dot$IFR_COMPLETED[i])}
  if(dot$STAGE[i] == "Final Rule"){dot$FinalRuleclearOMB[i] <- as.character(dot$clearOMBactual[i])}
  if(!grepl("-", dot$FinalRuleclearOMB[i])){dot$FinalRuleclearOMB[i] <- as.character(dot$FINAL_COMPLETED[i])}
}
# OMB
dot %<>% arrange((reportdate)) %>%
  transform(ANPRMtoOMB = ave(ANPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOMB = ave(SNPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOMB = ave(NPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOMB = ave(IFRtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOMB = ave(FinalRuletoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMtoOMBscheduled = ave(ANPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOMBscheduled = ave(SNPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOMBscheduled = ave(NPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOMBscheduled = ave(IFRtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOMBscheduled = ave(FinalRuletoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMclearOMB = ave(ANPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMclearOMB = ave(SNPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMclearOMB = ave(NPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(IFRclearOMB = ave(IFRclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleclearOMB = ave(FinalRuleclearOMB, RIN, FUN = CopyIfNA))
dot %<>% arrange(desc(reportdate)) %>%
  transform(ANPRMtoOMB = ave(ANPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOMB = ave(SNPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOMB = ave(NPRMtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOMB = ave(IFRtoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOMB = ave(FinalRuletoOMB, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMtoOMBscheduled = ave(ANPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMtoOMBscheduled = ave(SNPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMtoOMBscheduled = ave(NPRMtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(IFRtoOMBscheduled = ave(IFRtoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuletoOMBscheduled = ave(FinalRuletoOMBscheduled, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMclearOMB = ave(ANPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMclearOMB = ave(SNPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMclearOMB = ave(NPRMclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(IFRclearOMB = ave(IFRclearOMB, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleclearOMB = ave(FinalRuleclearOMB, RIN, FUN = CopyIfNA))


################################## FIX ################################
  # Dates Published
for(i in 1:dim(dot)[1]){
  if(!grepl("-", dot$ANPRMpublished) & dot$STAGE[i] == "Prerule"){dot$ANPRMpublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$ANPRMpublished[i])){dot$ANPRMpublished[i] <- as.character(dot$ANPRM_PUBLISHED[i])}
  if(!grepl("-", dot$NPRMpublished[i]) & dot$STAGE[i] == "Proposed Rule"){dot$NPRMpublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$NPRMpublished[i])){dot$NPRMpublished[i] <- as.character(dot$NPRM_PUBLISHED[i])}
  if(!grepl("-", dot$SNPRMpublished[i]) & dot$STAGE[i] == "SNPRM"){dot$SNPRMpublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$SNPRMpublished[i])){dot$SNPRMpublished[i] <- as.character(dot$SNPRM_PUBLISHED[i])}
  if(!grepl("-", dot$IFRpublished[i]) & dot$STAGE[i] == "Interim Final Rule"){dot$IFRpublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$IFRpublished[i])){dot$IFRpublished[i] <- as.character(dot$IFR_PUBLISHED[i])}
  if(!grepl("-", dot$FinalRulePublished[i]) & dot$STAGE[i] == "Final Rule"){dot$FinalRulePublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$FinalRulePublished[i])){dot$FinalRulePublished[i] <- as.character(dot$FINAL_PUBLISHED[i])}
  if(!grepl("-", dot$WithdrawalPublished[i]) & dot$STAGE[i] == "Withdrawal"){dot$WithdrawalPublished[i] <- as.character(dot$publicationactual[i])}
  if(!grepl("-", dot$WithdrawalPublished[i])){dot$WithdrawalPublished[i] <- as.character(dot$WITHDRAWAL[i])}
}

dot %<>%
  mutate(ANPRMpublished = ifelse(!grepl("-", ANPRMpublished) & STAGE=="Prerule", as.character(publicationactual), NA)) %>%
  mutate(ANPRMpublished = ifelse(!grepl("-", ANPRMpublished), as.character(ANPRM_PUBLISHED), NA)) %>%
  mutate(NPRMpublished = ifelse(!grepl("-", NPRMpublished) & STAGE=="Proposed Rule", as.character(publicationactual), NA)) %>%
  mutate(NPRMpublished = ifelse(!grepl("-", NPRMpublished), as.character(NPRM_PUBLISHED), NA)) %>%
  mutate(SNPRMpublished = ifelse(!grepl("-", SNPRMpublished) & STAGE=="SNPRM", as.character(publicationactual), NA)) %>%
  mutate(SNPRMpublished = ifelse(!grepl("-", SNPRMpublished), as.character(SNPRM_PUBLISHED), NA)) %>%
  mutate(IFRpublished = ifelse(!grepl("-", IFRpublished) & STAGE=="Interim Final Rule", as.character(publicationactual), NA)) %>%
  mutate(IFRpublished = ifelse(!grepl("-", IFRpublished), as.character(IFR_PUBLISHED), NA)) %>%
  mutate(FinalRulePublished = ifelse(!grepl("-", FinalRulePublished) & STAGE=="Final Rule", as.character(publicationactual), NA)) %>%
  mutate(FinalRulePublished = ifelse(!grepl("-", FinalRulePublished), as.character(FINAL_PUBLISHED), NA))

dot %<>% arrange((reportdate)) %>%
  transform(ANPRMpublished = ave(ANPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMpublished = ave(SNPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMpublished = ave(NPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(IFRpublished = ave(IFRpublished, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRulePublished = ave(FinalRulePublished, RIN, FUN = CopyIfNA)) %>%
  transform(WithdrawalPublished = ave(WithdrawalPublished, RIN, FUN = CopyIfNA))
dot %<>% arrange(desc(reportdate)) %>%
  transform(ANPRMpublished = ave(ANPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMpublished = ave(SNPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMpublished = ave(NPRMpublished, RIN, FUN = CopyIfNA)) %>%
  transform(IFRpublished = ave(IFRpublished, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRulePublished = ave(FinalRulePublished, RIN, FUN = CopyIfNA)) %>%
  transform(WithdrawalPublished = ave(WithdrawalPublished, RIN, FUN = CopyIfNA))
  
dot$enddate <- dot$FinalRulePublished
dot$enddate[which(is.na(dot$enddate))] <- dot$WithdrawalPublished[which(is.na(dot$enddate))]


# Deadlines
for(i in 1:dim(dot)[1]){
  if(!grepl("-", dot$NPRMDeadline[i])){dot$NPRMDeadline[i] <- as.character(dot$StatutoryNPRM[i])}
  if(!grepl("-", dot$NPRMDeadline[i])){dot$NPRMDeadline[i] <- as.character(dot$JudicialNPRM[i])}
  if(!grepl("-", dot$FinalRuleDeadline[i])){dot$FinalRuleDeadline[i] <- as.character(dot$StatutoryFinal[i])}
  if(!grepl("-", dot$FinalRuleDeadline[i])){dot$FinalRuleDeadline[i] <- as.character(dot$JudicialFinal[i])}
}

dot%<>% 
  mutate(NPRMDeadline = ifelse(!grepl("-", NPRMDeadline), as.character(StatutoryNPRM), NA)) %>%
  mutate(NPRMDeadline = ifelse(!grepl("-", NPRMDeadline), as.character(JudicialNPRM), NA)) %>%
  mutate(FinalRuleDeadline = ifelse(!grepl("-", FinalRuleDeadline), as.character(StatutoryFinal), NA)) %>%
  mutate(FinalRuleDeadline = ifelse(!grepl("-", FinalRuleDeadline), as.character(JudicialFinal), NA)) 
# deadline 
dot %<>% arrange((reportdate)) %>%
  transform(FinalRuleDeadline = ave(FinalRuleDeadline, RIN, FUN = CopyIfNA)) %>%
  transform(IFRDeadline = ave(IFRDeadline, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMDeadline = ave(NPRMDeadline, RIN, FUN = CopyIfNA)) 
dot %<>% arrange(desc(reportdate)) %>%
  transform(FinalRuleDeadline = ave(FinalRuleDeadline, RIN, FUN = CopyIfNA)) %>%
  transform(IFRDeadline = ave(IFRDeadline, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMDeadline = ave(NPRMDeadline, RIN, FUN = CopyIfNA)) 
  
##########################################################################################################################################################################################
# try again with new UA 
##########################################################################################################################################################################################

# Comment
dot$ANPRMcomment[which(is.na(dot$ANPRMcomment) & dot$STAGE=="Prerule")] <- dot$endcommentactual[which(is.na(dot$ANPRMcomment) & dot$STAGE=="Prerule")]
dot$NPRMcomment[which(is.na(dot$NPRMcomment) & dot$STAGE=="Proposed Rule")] <- dot$endcommentactual[which(is.na(dot$NPRMcomment) & dot$STAGE=="Proposed Rule")]
dot$SNPRMcomment[which(is.na(dot$SNPRMcomment) & dot$STAGE=="SNPRM")] <- dot$endcommentactual[which(is.na(dot$SNPRMcomment) & dot$STAGE=="SNPRM")]
dot$IFRcomment[which(is.na(dot$IFRcomment) & dot$STAGE=="Interim Final Rule")] <- dot$endcommentactual[which(is.na(dot$IFRcomment) & dot$STAGE=="Interim Final Rule")]
dot$FinalRulecomment=NA
dot$FinalRulecomment[which(is.na(dot$FinalRulecomment) & dot$STAGE=="Final Rule")] <- dot$endcommentactual[which(is.na(dot$FinalRulecomment) & dot$STAGE=="Final Rule")]
# extended
dot %<>%
  ungroup() %>%
  mutate(ANPRMextended = ifelse(dot$STAGE=="Prerule" & grepl("[0-9]", dot$endcomment2actual), 1, 0)) %>% 
  mutate(NPRMextended = ifelse(dot$STAGE=="Proposed Rule" & grepl("[0-9]", dot$endcomment2actual), 1, 0)) %>%
  mutate(SNPRMextended = ifelse(dot$STAGE=="SNPRM" & grepl("[0-9]", dot$endcomment2actual), 1, 0)) %>% 
  mutate(IFRextended = ifelse(dot$STAGE=="Interim Final Rule" & grepl("[0-9]", dot$endcomment2actual), 1, 0)) %>% 
  mutate(FinalRuleextended = ifelse(dot$STAGE=="Final Rule" & grepl("[0-9]", dot$endcomment2actual), 1, 0))
#comment
dot %<>% arrange((reportdate)) %>%
  transform(ANPRMcomment = ave(ANPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMcomment = ave(SNPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMcomment = ave(NPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(IFRcomment = ave(IFRcomment, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRulecomment = ave(FinalRulecomment, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMextended = ave(ANPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMextended = ave(SNPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMextended = ave(NPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(IFRextended = ave(IFRextended, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleextended = ave(FinalRuleextended, RIN, FUN = CopyIfNA))
dot %<>% arrange(desc(reportdate)) %>%
  transform(ANPRMcomment = ave(ANPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMcomment = ave(SNPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMcomment = ave(NPRMcomment, RIN, FUN = CopyIfNA)) %>%
  transform(IFRcomment = ave(IFRcomment, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRulecomment = ave(FinalRulecomment, RIN, FUN = CopyIfNA)) %>%
  transform(ANPRMextended = ave(ANPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(SNPRMextended = ave(SNPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(NPRMextended = ave(NPRMextended, RIN, FUN = CopyIfNA)) %>%
  transform(IFRextended = ave(IFRextended, RIN, FUN = CopyIfNA)) %>%
  transform(FinalRuleextended = ave(FinalRuleextended, RIN, FUN = CopyIfNA))



################################################################################
# Format Dates 
dot %<>% mutate(ANPRM_PUBLISHED = ifelse(ANPRM_PUBLISHED == "", NA, ANPRM_PUBLISHED))
dot %<>% mutate(NPRM_PUBLISHED = ifelse(NPRM_PUBLISHED == "", NA, NPRM_PUBLISHED))
dot %<>% mutate(FINAL_PUBLISHED = ifelse(FINAL_PUBLISHED == "", NA, FINAL_PUBLISHED))
dot %<>% mutate(DATE_PUBLISHED = ifelse(DATE_PUBLISHED == "", NA, DATE_PUBLISHED))
#FIXME
# dot %<>% mutate(enddate = ifelse(enddate == "", NA, enddate))

# published 
dot %<>% mutate(ANPRMpublished = ifelse(ANPRMpublished == "", NA, ANPRMpublished))
dot %<>% mutate(ANPRMpublished = as.Date(ANPRMpublished))
dot %<>% mutate(NPRMpublished = as.Date(NPRMpublished))
dot %<>% mutate(SNPRMpublished = as.Date(SNPRMpublished))
dot %<>% mutate(IFRpublished = as.Date(IFRpublished))
dot %<>% mutate(FinalRulePublished = ifelse(FinalRulePublished == "", NA, FinalRulePublished))
dot %<>% mutate(FinalRulePublished = as.Date(FinalRulePublished)) # why so many "" ? 
# clear OMB
dot %<>% mutate(ANPRMclearOMB = as.Date(ANPRMclearOMB))
dot %<>% mutate(NPRMclearOMB = as.Date(NPRMclearOMB))
dot %<>% mutate(SNPRMclearOMB = as.Date(SNPRMclearOMB))
dot %<>% mutate(IFRclearOMB = as.Date(IFRclearOMB))
dot %<>% mutate(FinalRuleclearOMB = as.Date(FinalRuleclearOMB))
dot %<>% mutate(Terminated = as.Date(Terminated))

dot %<>% mutate(enddate = ifelse(enddate == "", NA, enddate))
dot %<>% mutate(enddate = as.Date(enddate))


# # more dates 
for(x in c(76:86, 151, 166:180, 182:212)){
  dot[,x] <- as.Date(dot[,x])
}
x
names(dot)[x]
# sort(dot[, x])








#################################################################################
# EXTRA VARS (vars that are direct functions of others, not conditional on stage)
# maybe move this to use add needed? 


  

#names(dot)

# Differences between reported dates
dot$toOMBdiff <- difftime(dot$DATE_RECEIVED, dot$toOMBactual, units="days")
dot$clearOMBdiff <- difftime(dot$DATE_COMPLETED, dot$clearOMBactual, units="days")
dot$publisheddiff <- difftime(dot$DATE_PUBLISHED, dot$publicationactual, units="days")
dot$UAdiff <- difftime(dot$DOTdate, dot$UnifiedAgendaDate, units="weeks")

# Time variables
# Until OST
dot$DaysUntilOSTANPRM <- round(difftime(dot$ANPRMtoOST, dot$initiated, units="days"))
dot$DaysUntilOSTNPRM <- round(difftime(dot$NPRMtoOST, dot$initiated, units="days"))
dot$DaysUntilOSTSNPRM <- round(difftime(dot$SNPRMtoOST, dot$initiated, units="days"))
dot$DaysUntilOSTIFR <- round(difftime(dot$IFRtoOST, dot$initiated, units="days"))
dot$DaysUntilOSTFinalRule <- round(difftime(dot$FinalRuletoOST, dot$initiated, units="days"))
dot$DaysUntilOSTWithdrawal <- round(difftime(dot$withdrawnOSTactual, dot$initiated, units="days"))
# Until OMB
dot$DaysUntilOMBANPRM <- round(difftime(dot$ANPRMtoOMB, dot$initiated, units="days"))
dot$DaysUntilOMBNPRM <- round(difftime(dot$NPRMtoOMB, dot$initiated, units="days"))
dot$DaysUntilOMBSNPRM <- round(difftime(dot$SNPRMtoOMB, dot$initiated, units="days"))
dot$DaysUntilOMBIFR <- round(difftime(dot$IFRtoOMB, dot$initiated, units="days"))
dot$DaysUntilOMBFinalRule <- round(difftime(dot$FinalRuletoOMB, dot$initiated, units="days"))
dot$DaysUntilOMBWithdrawal <- round(difftime(dot$withdrawnOMBactual, dot$initiated, units="days"))
# at OMB
dot$DaysANPRMatOMB <- round(difftime(dot$ANPRMclearOMB, dot$ANPRMtoOMB, units="days"))
dot$DaysNPRMatOMB <- round(difftime(dot$NPRMclearOMB, dot$NPRMtoOMB, units="days"))
dot$DaysFRatOMB <- round(difftime(dot$FinalRuleclearOMB, dot$FinalRuletoOMB, units="days"))
dot$DaysIFRatOMB <- round(difftime(dot$IFRclearOMB, dot$IFRtoOMB, units="days"))
# Until Stage End (published/withdrawn))
dot$DaysUntilANPRM <- round(difftime(dot$ANPRMpublished, dot$initiated, units="days"))
dot$DaysUntilNPRM <- round(difftime(dot$NPRMpublished, dot$initiated, units="days"))
dot$DaysUntilSNPRM <- round(difftime(dot$SNPRMpublished, dot$initiated, units="days"))
dot$DaysUntilIFR <- round(difftime(dot$IFRpublished, dot$initiated, units="days"))
dot$DaysUntilFinalRule <- round(difftime(dot$FinalRulePublished, dot$initiated, units="days"))
dot$DaysUntilWithdrawal <- round(difftime(dot$WithdrawalPublished, dot$initiated, units="days"))
dot$TotalDays <- round(difftime(dot$enddate, dot$initiated, units="days"))
dot$ANPRMtoRule <- round(difftime(dot$FinalRulePublished, dot$ANPRMpublished, units="days"))
dot$NPRMtoRule <- round(difftime(dot$FinalRulePublished, dot$NPRMpublished, units="days"))




dot %<>% 
  mutate(acronym = strtrim(dot$RIN, 4))
  
#?stringtrim

unique(dot$acronym)
dot$acronym %<>% 
  gsub("2137","PHMSA", .) %>%
  gsub("2127","NHTSA", .) %>%
  gsub("2105","OTS", .) %>%
  gsub("2120","FAA", .) %>%
  gsub("2125","FHA", .) %>%
  gsub("2126","FMCSA", .) %>%
  gsub("2130","FRA", .) %>%
  gsub("2132","FTA", .) %>%
  gsub("2133","MA", .) %>%
  gsub("2139","RITA", .) %>%
  gsub("2135","St Lawernce", .)
  


dot %<>% dplyr::select(initiated, toOMBactual, resubmitOMB2actual, DATE_RECEIVED, toOMBdiff, clearOMBactual, DATE_COMPLETED, clearOMBdiff, publicationactual, DATE_PUBLISHED, publisheddiff, DOTdate, UnifiedAgendaDate, UAdiff, RIN,  STAGE, PrevStage, TIMETABLE_LIST, agency, fulltitle,  ANPRMpublished, ANPRM, NPRMpublished, NPRM, SNPRM, IFRpublished, IFR, FinalRulePublished, FINAL,  fedreg, ANPRMfedreg, NPRMfedreg, SNPRMfedreg, IFRfedreg, FINALfedreg, docket,  whydelay, color, everything())

dot %<>%
  ungroup() %>%
  mutate(HasANPRM = ifelse(is.na(dot$ANPRMpublished),0, 1))
# dot$HasANPRM

# dot %<>% dplyr::select(RIN, agency, fulltitle, initiated, WithdrawalPublished, FinalRulePublished, DaysUntilANPRM, DaysANPRMatOMB, DaysUntilNPRM, DaysNPRMatOMB, DaysUntilSNPRM, DaysUntilIFR, DaysIFRatOMB, DaysUntilFinalRule, DaysFRatOMB, DOTdate, UnifiedAgendaDate, STAGE, PrevStage, TIMETABLE_LIST, toOMBactual, DATE_RECEIVED, ANPRMpublished, ANPRM, NPRMpublished, NPRM, SNPRM, IFRpublished, IFR, FinalRulePublished, FINAL,  fedreg, ANPRMfedreg, NPRMfedreg, SNPRMfedreg, IFRfedreg, FINALfedreg, docket,  whydelay, color, everything())


dot %<>% dplyr::select(RIN, docket, agency, acronym, fulltitle, initiated, enddate, reportdate, STAGE, PrevStage, TIMETABLE_LIST, #TotalDays,
                       DaysUntilANPRM, DaysUntilOSTANPRM, DaysUntilOMBANPRM, DaysANPRMatOMB,
                       DaysUntilNPRM, DaysUntilOSTNPRM, DaysUntilOMBNPRM, DaysNPRMatOMB,
                       DaysUntilIFR, DaysUntilOSTIFR, DaysUntilOMBIFR, DaysIFRatOMB,
                       DaysUntilFinalRule, DaysUntilOSTFinalRule, DaysUntilOMBFinalRule, DaysFRatOMB,
                       DaysUntilWithdrawal,
                       DOTdate, UnifiedAgendaDate, 
                       toOMBactual, DATE_RECEIVED, resubmitOMB2actual, 
                       ANPRMpublished, NPRMpublished, SNPRMpublished, IFRpublished, FinalRulePublished, WithdrawalPublished,
                       fedreg, ANPRMfedreg, NPRMfedreg, SNPRMfedreg, IFRfedreg, FINALfedreg, whydelay, color, everything())



















#################### MAKE SUBSETS ###############################
dot %<>%
  dplyr::group_by(RIN) %>%
  dplyr::arrange(desc(enddate))

# One observation per rule per stage 
dotStage <- dot

# One observation per RIN 
dotRIN <- dot %>%
  dplyr::group_by(RIN) %>%
  dplyr::top_n(n = 1, reportdate) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(reportdate))

# Merge with full DOT monthly dataset
# IS THIS ALL OF THE MONTHLY VARIANCE (DATES DON'T CHANGE, RIGHT?)
# Select things that vary by month
dotMonthly <- select(dotALL, RIN, STAGE, 
                     date, color, abstract, DOTdate, prompt, 
                     whydelay, whydelay1, whydelay2, whydelay3, whydelay4, whydelay5, whydelay6, whydelay7, whydelay8,
                     effects)

# merge on RIN and STAGE, delete everything that varies monthly:
dotMonthly %<>%  left_join(dotStage %>% select(-date, -color, -abstract, -DOTdate, -prompt, 
                                  -whydelay, -whydelay1, -whydelay2, -whydelay3, -whydelay4, -whydelay5, -whydelay6, -whydelay7, -whydelay8,
                                  -effects))
dotMonthly <- dplyr::arrange(dotMonthly, desc(RIN))

unique(dotMonthly$STAGE)




############################################################
# Write out
###########################################################

# write out entire DOT data as master
write.csv(dotMonthly, file = "data/DOT-monthly.csv")
save(dotMonthly, file = "data/DOT-monthly.Rdata")

# write out only last observations per stage 
write.csv(dotStage, file = "data/DOT-perRule-perStage.csv")
save(dotStage, file = "data/DOT-perRule-perStage.Rdata")

# write out one per RIN 
write.csv(dotRIN, file = "data/DOT-perRule.csv")
save(dotRIN, file = "data/DOT-perRule.Rdata")

# list of variables
write.csv(names(dot), file = "DOT variables.csv")

# save Rdata
# save.image("data/DOT.Rdata")


