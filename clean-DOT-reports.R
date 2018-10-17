rm(list=ls()) #(clean workspace) 
#################################################################################
# This code takes in .csv files with text coppied from DOT word documents, pasted into excel and saved as .csv

# PART 1 adds and re-orders rows so that each report has the same data on each of n lines

# PART 2 is code to check that it worked, identify erros and troubleshoot

# PART 3 creates a new dataframe such that each rule for that month is a row 

# Finally, when complete, the final line exports it as a cleanDOT[YYYYMM].csv for that month

# Use rbind code in "combine dot files.R" to combine seperate monthly outpust into master file
##################################################################################
##############       ==================================== 
##############      | load monthly report and name date: |
##############       ==================================== 

date <- 201709 # <---- REPORT DATE YYYYMMM--------------------------

###################################################################################
# load packages and functions
if(!exists("CopyIfNA")){source("setup.R")} 

rules <- NA # just to make sure we are getting new data
original <- rules <- read.csv(here("reports", "rawDOT", paste0("raw", substr(date,1,4), "dot"), paste0("raw", date, "dot.csv")), header=F)
        

# for now, deleting effective date, but could go back and change 
rules %<>% filter(!grepl("^Effective Date", V1))


rules$V1 <- gsub(" $", "", rules$V1)


anchor <-"To OST"  # some value that always occurs in the first column of each observation (used in Part 2)

#delete table of contents
for(i in 1:dim(rules)[1] ) {
  if(
    grepl("^[1-9][0-9]", rules$V1[i]) & 
    (grepl("^[1-9][0-9]", rules$V1[i + 1]) | grepl("^[1-9][0-9]", rules$V1[i + 2]))&
    rules$V1[i+3] == "" & 
    rules$V1[i+4] == "" ) {
      rules <- rules[(i + 5):dim(rules)[1], ]
    }
}

# remove blank rows
rules %<>% filter(!(V1 == "" & V2 == "" & V3 == "" & V4 == ""))

# add blank rows at end to allow looking ahead
for(i in 1:10){
rules <- rbind(rules, c("","","","")) 
}

for(i in 1:dim(rules)[1]) {
  if( grepl("^Federal|^National", rules$V1[i]) & rules$V1[i] == rules$V1[i+1]) {
    rules <- rules[-i,]
  }
}

# the remainder of Part 1 merges, adds, deletes, and re-orders rows - ### THIS CODE MAY NEED TO BE RUN TWICE ###

# change "No Schedule Available" to "Milestone" and insert blank schedule rows
i <- 1
while(i<length(rules$V1)){
  if (rules[i, "V1"] == "No Schedule Available"
  ) {
    rules[(i), "V1"]<-"Milestone"
    rules<-rbind(rules[0:(i),],
                 c("","","",""),
                 c("","","",""),
                 c("To OST","","",""),
                 c("Withdrawn from OST","","",""),
                 c("Returned to Mode","","",""),
                 c("Resubmitted To OST","","",""),
                 c("To OMB","","",""),
                 rules[(i+1):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}


i=1
while(i<length(rules$V1)){
  if(rules$V1[i]=="Resubmitted to OMB"){
    rules$V1[i]="Resubmitted to OMB/2"  
    i<- i + 1
  }
  i <- i + 1
}



i=1
while(i<length(rules$V1)){
  if(rules$V1[i]=="End of Reopened Comment Period"){
    rules$V1[i]="End of Extended Comment Period"  
    i<- i + 1
  }
  i <- i + 1
}

# change any Returned to Mode occuring after OMB to Returned to Mode/2
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Withdrawn from OMB" &
      rules[(i+1),"V1"] =="Returned to Mode"
      ) {
    rules[(i+1),"V1"]<-"Returned to Mode/2"
    i<- i + 1
  }
  i <- i + 1
}

# change"Resent to OST" or"Returned to OST" to"Resubmitted to OST"
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Resent to OST"
      |
      rules[i,"V1"] =="Returned to OST"
      ) {
    rules[(i),"V1"]<-"Resubmitted to OST"
    i<- i + 1
  }
  i <- i + 1
}

i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="To OST/2"
  ) {
    rules[(i),"V1"]<-"Resubmitted to OST/2"
    i<- i + 1
  }
  i <- i + 1
}

while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Publication Approved"
  ) {
    rules[(i),"V1"]<-"Publication Approved"
    i<- i + 1
  }
  i <- i + 1
}



# concat abstract and delete up to 10 rows between abstract and effects
Sys.setlocale('LC_ALL','C') 
x=0
while(x<15){
  i <- 1
  while(i<length(rules$V1)){
    if (grepl("Abstract", rules[i,"V1"]) &
        rules[(i+1),"V1"] !="Effects:") {
      rules[i,"V1"]=paste(rules[i,"V1"],rules[i,"V2"], rules[i,"V3"], rules[i,"V4"],rules[(i+1),"V1"],rules[(i+1),"V2"],rules[(i+1),"V3"], rules[(i+1),"V4"], sep =" ", collapse = NULL)
      rules<-rbind(rules[0:(i),],
                   rules[(i+2):length(rules$V1),]
      )
      i <- i + 1
    }
    i <- i + 1
  }
  x = x + 1
}

# copy color and full title to title row
i=1
while(i<length(rules$V1)){
  if(grepl("Popular Title:", rules[i, "V1"])){
    rules[i,"V2"]=rules[(i-1),"V2"]
    rules[i,"V3"]=rules[(i-1),"V3"]
    rules[i,"V4"]=rules[(i-1),"V4"]
    i = i + 1
  }
  i = i + 1
}

# remove rows between agency and title
x=0
while(x<5){
  i <- 3
  while(i<length(rules$V1)){
    if ( grepl("Popular Title", rules[i, "V1"]) &
        grepl("^[0-9]", rules[(i-1), "V1"]) ) {
      rules<-rbind(rules[0:(i-2),],
                   rules[(i):length(rules$V1),]
      )
      i <- i + 1
    }
    i <- i + 1
  }
  x = x + 1
}

# remove duplicate titles
for(i in 1:dim(rules)[1] ) {
  if ( rules$V1[i] == rules$V1[i+1] & grepl("Popular Title", rules$V1[i+2]) 
       ) {
    rules<-rbind(rules[0:(i),],
                 rules[(i+2):dim(rules)[1],]
    )
  }
}


# remove rows between "prvious stage" and "abstract"
x=0
while(x<5){
  i <- 3
  while(i<length(rules$V1)){
    if (grepl("Abstract", rules[i, "V1"]) &
        !(grepl("Previous Stage", rules[(i-1), "V1"]))
        ) {
      rules<-rbind(rules[0:(i-2),],
                   rules[(i):length(rules$V1),]
      )
      i <- i + 1
    }
    i <- i + 1
  }
  x = x + 1
}




# add rows for up to 7 effects 

new<-c("","","","")
x=0
while(x < 7){
  i <- 1
  while(i<length(rules$V1)){
    if (rules[i, "V1"] == "Effects:" &
        rules[(i+x+1), "V1"] != "") {
      rules<-rbind(rules[0:(i + x),],
                   new,
                   rules[(i+x+1):length(rules$V1),]
      )
      i<- i + 1
    }
    i <- i + 1
  }
x = x + 1
}

# add 5 rows where only one legal deadline listed
x=0
while(x<5){
  new<-c("","","","")
  i <- 1
  while(i<length(rules$V1)){
    if (grepl("Legal Deadline", rules[i, "V1"]) &
        rules[(i+1+x), "V1"] != "") {
      rules<-rbind(rules[0:(i+x),],
                   new,
                   rules[(i+1+x):length(rules$V1),]
      )
      i<- i + 1
    }
    i <- i + 1
  }
  x=x+1
}

i <- 1
while(i<length(rules$V1)){
  if (grepl("Legal Deadline:", rules[i, "V1"]) &
      !(grepl("Rulemaking Project Initiated", rules[(i+6), "V1"]))
      ) {
    rules<-rbind(rules[0:(i+5),],
                 rules[(i+7):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}


# insert "to OST " where missing 
new<-c("To OST","","","")
i <- 1
while( i < (length(rules$V1))){  # NOTE: MAY NEED TO EDIT THE LAST ENTRY IF IT DOES NOT HAVE OST AND MILESTONE IS < 10 lines from the bottom
  if (rules[i, "V1"] == "Milestone" & # anchor on milestone 
      sum(rules[i:(i+10), "V1"] == "To OST") == 0 # make sure "to OST" does not appear in the next 10 lines
      ){
    rules=rbind(
      rules[(0:(i+2)),],
      new,
      rules[(i+3):length(rules$V1),]
    )
    i<- i + 1
    }
  i <- i + 1
}

# remove anything between Milestone and "To Ost"
x=0
while(x<5){
  i <- 1
  while(i<length(rules$V1)){
    if (rules[i,"V1"]=="Milestone" &
        rules[(i+3),"V1"]!="To OST" &
        sum(rules[(i+4):(i+10),"V1"] =="To OST") > 0 
             ){
               rules<-rbind(rules[0:(i+2),],
                   rules[(i+4):length(rules$V1),]
                   )
      i <- i + 1
    }
    i <- i + 1
  }
  x = x + 1
}


# standardize table of dates (note, a few may have dates out of order and thus will need to be duplicated manually)

#start with"Milestone
blank<-c("","","","")
new<-c("To OST","","","")
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Milestone" &
      rules[(i+3),"V1"] !="To OST") {
    rules<-rbind(rules[0:(i),],
                 blank,
                 blank,
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}

#adding row for"Withdrawn from OST" 

new<-c("Withdrawn from OST","","","")
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="To OST" &
      rules[(i+1),"V1"] !="Withdrawn from OST") {
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}

new<-c("Returned to Mode","","","")
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Withdrawn from OST" &
      rules[(i+1),"V1"] !="Returned to Mode") {
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}

new<-c("Resubmitted To OST","","","")
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Returned to Mode" &
     rules[(i+1),"V1"] !="Resubmitted To OST") {
    rules<-rbind(rules[0:i,],
          new,
          rules[(i+1):length(rules$V1),]
          )
    i<- i + 1
  }
  i <- i + 1
}
######################################################## FIXME 
# GOBBLES UP LINES WHEN NOT MATCHING 
rules$V1 <- gsub("Publication date","Publication Date", rules$V1)

x = 0
while(x<5){
  i <- 1
  while(i<length(rules$V1)){
    if (rules[i,"V1"] =="Resubmitted To OST" &
        rules[(i+1),"V1"] !="To OMB" &
        rules[(i+1),"V1"] !="Publication Date" &
        rules[(i+1),"V1"] !="Publication Approved" &
        rules[(i+1),"V1"] !="Explanation for any delay:"
        ) {
      rules<-rbind(rules[0:i,],
                   rules[(i+2):length(rules$V1),]
      )
      i<- i + 1
    }
    i <- i + 1
  }
  x = x + 1
}

#####################################################

new<-c("To OMB","","","")
i <- 1
while(i<length(rules$V1)){
  if (rules[i,"V1"] =="Resubmitted To OST" &
      rules[(i+1),"V1"] !="To OMB") {
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<- i + 1
  }
  i <- i + 1
}



new<-c("Withdrawn from OMB","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"]=="To OMB" &
     rules[(i+1),"V1"]!="Withdrawn from OMB"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}



  new<-c("Returned to Mode/2","","","")
  i <- 1
  while(i<length(rules$V1)){
    if(rules[i,"V1"]=="Withdrawn from OMB" &
       rules[(i+1),"V1"]!="Returned to Mode/2"){
      rules<-rbind(rules[0:i,],
                   new,
                   rules[(i+1):length(rules$V1),]
      )
      i<-i+1
    }
    i <- i + 1
  }  

    new<-c("Resubmitted to OST/2","","","")
    i <- 1
    while(i<length(rules$V1)){
      if(rules[i,"V1"]=="Returned to Mode/2" &
         rules[(i+1),"V1"]!="Resubmitted to OST/2"){
        rules<-rbind(rules[0:i,],
                     new,
                     rules[(i+1):length(rules$V1),]
        )
        i<-i+1
      }
      i <- i + 1
    }
      
      

new<-c("Returned to Mode/3","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Resubmitted to OST/2" &
     rules[(i+1),"V1"]!="Returned to Mode/3"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}


new<-c("Resubmitted to OST/3","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Returned to Mode/3" &
     rules[(i+1),"V1"]!="Resubmitted to OST/3"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}


new<-c("Resubmitted to OMB/2","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Resubmitted to OST/3" &
     rules[(i+1),"V1"]!="Resubmitted to OMB/2"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}
i

new<-c("OMB Clearance","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Resubmitted to OMB/2" &
     rules[(i+1),"V1"]!="OMB Clearance"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

new<-c("Publication Approved","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="OMB Clearance" &
     rules[(i+1),"V1"]!="Publication Approved"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

new<-c("Publication Date","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Publication Approved" &
     rules[(i+1),"V1"]!="Publication Date"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

new<-c("End of Comment Period","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Publication Date" &
     !grepl("End of Comment Period", rules[(i+1),"V1"])){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

new<-c("Extension of Comment Period","","","")
i <- 1
while(i<length(rules$V1)){
  if((rules[i,"V1"]=="End of Comment Period") &
     rules[(i+1),"V1"]!="Extension of Comment Period"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}




new<-c("End of Extended Comment Period","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="Extension of Comment Period" &
     rules[(i+1),"V1"]!="End of Extended Comment Period"){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

new<-c("Explanation for any delay:","","","")
i <- 1
while(i<length(rules$V1)){
  if(rules[i,"V1"] =="End of Extended Comment Period" &
     sum(grepl("Explanation for any delay:", rules[(i+1):(i+3),"V1"])) == 0 # look ahead 3 for explaination
     ){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}



#add rows for Explanation for any delay:

new<-c("","","","")
x=1
while(x<8){
i <- 1
while(i<length(rules$V1)){
  if(grepl("Explanation for any delay:", rules[i,"V1"]) &&
     rules[(i+x),"V1"]!=""){
    rules<-rbind(rules[0:(i+x-1),],
                 new,
                 rules[(i+x):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}
x=x+1}


new <- c("Federal Register Citation: None","","","")
i <- 1
while(i<length(rules$V1)){
  if(grepl("Explanation for any delay:", rules[i,"V1"]) &
     !(grepl("Federal Register",rules[i+8,"V1"]))
     ){
    rules<-rbind(rules[0:(i+7),],
                 new,
                 rules[(i+8):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

# add a line after fed reg cite 
new<-c("","","","")
i <- 1
while(i<length(rules$V1)){
  if(grepl("Federal Register Citation",rules[i,"V1"])
     &
     rules[(i+1),"V1"]!=""
   ){
    rules<-rbind(rules[0:(i),],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i<-i+1
}

i <- 1
while(i<length(rules$V1)){
  if(grepl("Federal Register Citation",rules[i,"V1"])
     &
     rules[(i+2),"V1"]!=""
  ){
    rules<-rbind(rules[0:(i+1),],
                 new,
                 rules[(i+2):length(rules$V1),]
    )
    i<-i+1
  }
  i<-i+1
}
  
i <- 1
while(i<length(rules$V1)){
  if(
    grepl("Federal Register Citation", rules[i,"V1"])
    &
    rules[(i+3),"V1"] !=""
  ){
    rules<-rbind(rules[0:(i+2),],
                 new,
                 rules[(i+3):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

#delete extra blank rows at end 
x=0
while(x<5){
  i <- 1
  while(i<(length(rules$V1)-5)){
    if(
      grepl("Federal Register Citation", rules[i,"V1"])
      &
      rules[(i+4),"V1"] ==""
    ){
      rules<-rbind(rules[0:(i+3),],
                   rules[(i+5):length(rules$V1),]
      )
      i<-i+1
    }
    i <- i + 1
  }
  x = x + 1
}


# delete agency headings 

i <- 1
while(i<length(rules$V1)){
  if(rules[i, "V1"] == rules[(i+4), "V1"] &
     rules[(i), "V1"]!=""){
    rules<-rbind(rules[0:i,],
                 rules[(i+5):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

i <- 1
while(i<length(rules$V1)){
  if(rules[i, "V1"] == rules[(i+3), "V1"] &
     rules[(i), "V1"]!=""){
    rules<-rbind(rules[0:i,],
                 rules[(i+4):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}

#add docket row
new <- c("Docket Number:","","","")
i <- 1
while(i<length(rules$V1)){
  if(grepl("Rulemaking Project Initiated:",rules[i,"V1"]) &
     grepl("Dates for",rules[(i+1),"V1"]) &
     !grepl("Docket Number:",rules[(i+1),"V1"])
     ){
    rules<-rbind(rules[0:i,],
                 new,
                 rules[(i+1):length(rules$V1),]
    )
    i<-i+1
  }
  i <- i + 1
}






####################################################################################

############################ RUN ABOVE ##############################################

#####################################################################################

# PART 2: code to check that it worked, identify erros and troubleshoot

################################################################

# find errors

################################################################

# define n = the correct number of lines for each rule 
firstanchor<-9999
i <- 1
while(i!=(firstanchor+1)){
  if(rules[i, "V1"] == anchor
  ){
    firstanchor<-i
    #i=i+1
  }
  i=i+1
}
firstanchor

secondanchor<-9999
while(i!=(secondanchor+1)){
  if(rules[i, "V1"] == anchor
  ){
    secondanchor<-i
    #i=i+1
  }
  i=i+1
}
secondanchor
n=secondanchor-firstanchor
n

# find errors by numbering each rule 1:n

linenumber<-rep(seq(1:n), length(rules$V1))
linenumber<-linenumber[1:length(rules$V1)]
rules$linenumber<-linenumber

i <- 1
while(!(rules[i, "linenumber"] == firstanchor &
     rules[(i), "V1"]!= anchor)){
    i<-i+1
  }
problem=i
Problem<- problem!=(length(rules$V1)+1)
Problem
problemArea <- rules[(problem - n*3):(problem + 3),]
done <- is.na(rules[i,"V1"])
done#?
#####################################################################

######################## RUN ABOVE ##################################

####################################################################

#####################################################################

###################################################################

####################################################################

# If rules[i,"V1"] is NA, then data are clean, all are aligned, skip to Part 3

# if not, use below to troublshoot:
#################################################################################

rules[(i-firstanchor),"V1"] #title of problem rule
error=rules$V1[(i-n):(i)] # general location of the error
correct=rules$V1[(firstanchor):(secondanchor)] # what it should look like
finderror=cbind(correct, error)
# finderror
# view finderror to see problem, or
# find location of mismatch
i <- 1
while(correct[i] == error[i]
        ){
  i<-i+1
}
#correct:
correct[(i-1):(i+1)]
#error
error[(i-1):(i+1)]
done#?

#################################################################################






















##################################################################################

# PART: 3 creates a new dataframe such that each rule for that month is a row 

#################################################################################

# define new dataframe "out"
out<- data.frame(matrix(vector(), sum(rules$linenumber==1), 0)) # no vars, sum(rules$linenumber==1) = # of rules

#add vars
out$agency<-NA # line 1
out$date<-date
out$title<-NA #2
out$fulltitle<-NA #2
out$color<-NA #2
out$RIN<-NA #3
out$stage<-NA #4
out$PrevStage<-NA #5
out$abstract<-NA #6
out$effect1<-NA #8
out$effect2<-NA #9
out$effect3<-NA #10
out$effect4<-NA #11
out$effect5<-NA #12
out$effect6<-NA #13
out$effect7<-NA #14
out$prompt<-NA #15
out$legaldeadline1<-NA #16
out$legaldeadline2<-NA #17
out$legaldeadline3<-NA #18
out$legaldeadline4<-NA #19
out$legaldeadline5<-NA #20
out$legaldeadline6<-NA #21
out$initiated<-NA
out$docket<-NA #23
out$toOSTscheduled<-NA #28
out$toOSTprojected=NA
out$toOSTactual=NA
out$withdrawnOSTscheduled=NA # 29
out$withdrawnOSTprojected=NA
out$withdrawnOSTactual=NA
out$toModescheduled=NA #30
out$toModeprojected=NA
out$toModeactual=NA
out$resubmitOSTscheduled=NA#31
out$resubmitOSTprojected=NA
out$resubmitOSTactual=NA
out$toOMBscheduled=NA #32
out$toOMBprojected=NA
out$toOMBactual=NA
out$withdrawnOMBscheduled=NA #33
out$withdrawnOMBprojected=NA
out$withdrawnOMBactual=NA
out$toMode2scheduled=NA #34
out$toMode2projected=NA
out$toMode2actual=NA
out$resubmitOST2scheduled=NA #35
out$resubmitOST2projected=NA
out$resubmitOST2actual=NA
out$toMode3scheduled=NA #36
out$toMode3projected=NA
out$toMode3actual=NA
out$resubmitOST3scheduled=NA #37
out$resubmitOST3projected=NA
out$resubmitOST3actual=NA
out$resubmitOMB2scheuled=NA #38
out$resubmitOMB2projected=NA
out$resubmitOMB2actual=NA
out$clearOMBscheduled=NA #39
out$clearOMBprojected=NA
out$clearOMBactual=NA
out$publicationapprovedscheduled=NA #40
out$publicationapprovedprojected=NA
out$publicationapprovedactual=NA
out$publicationscheduled=NA #41
out$publicationprojected=NA
out$publicationactual=NA
out$endcommentscheduled=NA #42
out$endcommentprojected=NA
out$endcommentactual=NA
out$extcommentscheduled=NA #43
out$extcommentprojected=NA
out$extcommentactual=NA
out$endcomment2scheduled=NA #44
out$endcomment2projected=NA
out$endcomment2actual=NA
out$whydelay1=NA #45
out$whydelay2=NA #46
out$whydelay3=NA #47
out$whydelay4=NA #48
out$whydelay5=NA #49
out$whydelay6=NA#50
out$whydelay7=NA #51
out$whydelay8=NA #52
out$fedreg=NA #53

# put rule data in out

i <- 0
j <- 1
while(i<length(rules$V1)){
  out$agency[j]<-rules[i+1,"V1"] #1
  out$color[j]<-rules[i+2,"V4"] #2
  out$title[j]<-rules[i+2,"V1"] 
  out$fulltitle[j]<-rules[i+2,"V3"] 
  out$RIN[j]<-rules[i+3,"V1"] #3
  out$stage[j]<-rules[i+4,"V1"] #4
  out$PrevStage[j]<-rules[i+5,"V1"] #5
  out$abstract[j]<-rules[i+6,"V1"] #6
  out$effect1[j]<-rules[i+8,"V2"] #8
  out$effect2[j]<-rules[i+9,"V2"] #9
  out$effect3[j]<-rules[i+10,"V2"] #10
  out$effect4[j]<-rules[i+11,"V2"] #11
  out$effect5[j]<-rules[i+12,"V2"] #12
  out$effect6[j]<-rules[i+13,"V2"] #13
  out$effect7[j]<-rules[i+14,"V2"] #14
  out$prompt[j]<-rules[i+15,"V1"] #15
  out$legaldeadline1[j]<-rules[i+16,"V2"] #16
  out$legaldeadline2[j]<-rules[i+17,"V2"] #17
  out$legaldeadline3[j]<-rules[i+18,"V2"] #18
  out$legaldeadline4[j]<-rules[i+19,"V2"] #19
  out$legaldeadline5[j]<-rules[i+20,"V2"] #20
  out$legaldeadline6[j]<-rules[i+21,"V2"] #21
  out$initiated[j]<-rules[i+22,"V1"] #22
  out$docket[j]<-rules[i+23,"V1"] #23
  out$toOSTscheduled[j]<-rules[i+28,"V2"] #28
  out$toOSTprojected[j]<-rules[i+28,"V3"]
  out$toOSTactual[j]<-rules[i+28,"V4"]
  out$withdrawnOSTscheduled[j]<-rules[i+29,"V2"] # 29
  out$withdrawnOSTprojected[j]<-rules[i+29,"V3"]
  out$withdrawnOSTactual[j]<-rules[i+29,"V4"]
  out$toModescheduled[j]<-rules[i+30,"V2"] #30
  out$toModeprojected[j]<-rules[i+30,"V3"]
  out$toModeactual[j]<-rules[i+30,"V4"]
  out$resubmitOSTscheduled[j]<-rules[i+31,"V2"] #31
  out$resubmitOSTprojected[j]<-rules[i+31,"V3"]
  out$resubmitOSTactual[j]<-rules[i+31,"V3"]
  out$toOMBscheduled[j]<-rules[i+32,"V2"] #32
  out$toOMBprojected[j]<-rules[i+32,"V3"]
  out$toOMBactual[j]<-rules[i+32,"V4"]
  out$withdrawnOMBscheduled[j]<-rules[i+33,"V2"] #33
  out$withdrawnOMBprojected[j]<-rules[i+33,"V3"]
  out$withdrawnOMBactual[j]<-rules[i+33,"V3"]
  out$toMode2scheduled[j]<-rules[i+34,"V2"] #34
  out$toMode2projected[j]<-rules[i+34,"V3"]
  out$toMode2actual[j]<-rules[i+34,"V4"]
  out$resubmitOST2scheduled[j]<-rules[i+35,"V2"] #35
  out$resubmitOST2projected[j]<-rules[i+35,"V3"]
  out$resubmitOST2actual[j]<-rules[i+35,"V4"]
  out$toMode3scheduled[j]<-rules[i+36,"V2"] #36
  out$toMode3projected[j]<-rules[i+36,"V3"]
  out$toMode3actual[j]<-rules[i+36,"V4"]
  out$resubmitOST3scheduled[j]<-rules[i+37,"V2"] #37
  out$resubmitOST3projected[j]<-rules[i+37,"V3"]
  out$resubmitOST3actual[j]<-rules[i+37,"V4"]
  out$resubmitOMB2scheuled[j]<-rules[i+38,"V2"] #38
  out$resubmitOMB2projected[j]<-rules[i+38,"V3"]
  out$resubmitOMB2actual[j]<-rules[i+38,"V4"]
  out$clearOMBscheduled[j]<-rules[i+39,"V2"] #39
  out$clearOMBprojected[j]<-rules[i+39,"V3"]
  out$clearOMBactual[j]<-rules[i+39,"V4"]
  out$publicationapprovedscheduled[j]<-rules[i+40,"V2"] #40
  out$publicationapprovedprojected[j]<-rules[i+40,"V3"]
  out$publicationapprovedactual[j]<-rules[i+40,"V4"]
  out$publicationscheduled[j]<-rules[i+41,"V2"] #41
  out$publicationprojected[j]<-rules[i+41,"V3"]
  out$publicationactual[j]<-rules[i+41,"V4"]
  out$endcommentscheduled[j]<-rules[i+42,"V2"] #42
  out$endcommentprojected[j]<-rules[i+42,"V3"]
  out$endcommentactual[j]<-rules[i+42,"V4"]
  out$extcommentscheduled[j]<-rules[i+43,"V2"] #43
  out$extcommentprojected[j]<-rules[i+43,"V3"]
  out$extcommentactual[j]<-rules[i+43,"V4"]
  out$endcomment2scheduled[j]<-rules[i+44,"V2"] #44
  out$endcomment2projected[j]<-rules[i+44,"V3"]
  out$endcomment2actual[j]<-rules[i+44,"V4"]
  out$whydelay1[j]<-rules[i+45,"V2"] #45
  out$whydelay2[j]<-rules[i+46,"V2"] #46
  out$whydelay3[j]<-rules[i+47,"V2"] #47
  out$whydelay4[j]<-rules[i+48,"V2"] #48
  out$whydelay5[j]<-rules[i+49,"V2"] #49
  out$whydelay6[j]<-rules[i+50,"V2"] #50
  out$whydelay7[j]<-rules[i+51,"V2"] #51
  out$whydelay8[j]<-rules[i+52,"V2"] #52
  out$fedreg[j]<-rules[i+53,"V1"] #53
  i <- i + n
  j <- j + 1
}

#############################################################
################# EXPORT CLEAN DATA ########################
############################################################
if(Problem==F){
  write.csv(out, 
            file = paste0("reports/DOTclean/", date, "DOTclean.csv") 
  )
  # distribution of delay status 
  summary(as.factor(out$color))
  # another check for failures 
  which(out$color=="")
  }

if(Problem==T){
  print(error)
  print(rules$V1[(problem-2):(problem+2)])
  }

Problem#?
done#?
######################### END ###############################

rm(anchor, blank, correct, firstanchor, i, j, linenumber, new, problem, requires, secondanchor, to_install, x)