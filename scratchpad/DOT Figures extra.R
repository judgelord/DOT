##################################
# COLORS 
###################################
dotRIN$enddate
dotRIN$initiated

dotRIN$red <- NA
dotRIN$yellow <- NA
dotRIN$green <- NA
dotRIN$black <- NA
for(i in 1:dim(dotRIN)[1]){
  if(dotRIN$color[i]=="Red"){
    dotRIN$red[i] <- as.character(dotRIN$enddate[i])
  }
}
dotRIN$red

for(i in 1:dim(dotRIN)[1]){
  if(dotRIN$color[i]=="Yellow"){
    dotRIN$yellow[i] <- as.character(dotRIN$enddate[i])
  }
}
for(i in 1:dim(dotRIN)[1]){
  if(dotRIN$color[i]=="Green"){
    dotRIN$green[i] <- as.character(dotRIN$enddate[i])
  }
}
for(i in 1:dim(dotRIN)[1]){
  if(dotRIN$color[i]=="Black"){
    dotRIN$black[i] <- as.character(dotRIN$enddate[i])
  }
}
dotRIN$red <- as.Date(dotRIN$red)
dotRIN$yellow <- as.Date(dotRIN$yellow)
dotRIN$green <- as.Date(dotRIN$green)
dotRIN$black <- as.Date(dotRIN$black)


####  TIMELINE 
class(DOTmonthly$DOTdate)
delay <- ggplot(DOTmonthly, aes(x=RIN)) +
  #facet_grid(agency ~ .) +
  #geom_linerange(aes(ymin=initiated, ymax=enddate, color="black"), size=.1) +
  geom_point(aes(y=WithdrawalPublished, color="red", shape = 4), size=.8) + 
  geom_point(aes(y=FinalRulePublished, color="blue", shape = 15, size=.6) + 
               geom_point(aes(y=DOTdate, color=color), shape = 15, size=.4) + 
               coord_flip() 
             scale_y_date(lim = c(as.Date("2008-01-01"), as.Date("2017-01-01")),
                          breaks=date_breaks(width = "1 year"), labels = date_format("%y"),
                          minor_breaks=NULL) + 
               ggtitle('Significant DOT Rulemaking Projects Active Between 2008 and 2016')+ xlab("RIN")+ylab("Year")+
               
               theme(axis.text.y =
                       element_text(size  =2,
                                    angle = 0,
                                    hjust = 1,
                                    vjust = 1)) 
             delay +
               scale_fill_manual(values=c("black", "green","red","yellow","blue", "orange")) +
               scale_colour_manual(values=c("black", "green","red","yellow","blue", "orange")) 
             #
             
             
             
             
             
             
             
             
             
             
             
             #################
             dotdelay=cbind(dot$date,dot[,87:96])
             dotdelay[is.na(dotdelay)] <- 0
             dotRINcolor<-dot[c(6,7)]
             
             
             
             counts <- table(as.factor(dot$color), dot$date)
             barplot(counts, main="Delay Status by Month Jan 2008-May 2016",
                     xlab="Month", ylab="Number of Active Rulemaking Projects", col=c("black","green","red","yellow"),
                     border=NA)
             
             ########################## again by date - BEST ONE
             dotdatecolor=dot[,c(3,6)]
             dotdatecolor=transform(dotdatecolor, datefreq = ave(seq(nrow(dotdatecolor)), date, FUN=length))
             
             dotdatecolor=dotdatecolor[order(dotdatecolor$datefreq),]
             
             dotdatecolor$year=trunc(dotdatecolor$date / 10 ^ nchar(dotdatecolor$date) * 10000) 
             
             ggplot(dotdatecolor, aes(x= date, fill=color)) + 
               geom_bar() + 
               facet_grid( ~ year, scales="free_x") +
               scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) +   labs(x="",y="Number of Rulemaking Projects") +
               ggtitle("DOT Active Rulemaking Projects") + scale_x_discrete(breaks = c(""))  
             
             
             # DOT COLOR MONTHS BY RIN
             
             # method 1 (using counts table)- not ordered 
             dotRINcolor<-dot[c(6,7)]
             counts <- table(as.factor(dotRINcolor$color), dotRINcolor$RIN)
             barplot(counts, main="DOT Rules By Delay Status Jan 2008-May 2016",
                     xlab="RIN", ylab="Months of Rulemaking", col=c("black","green","red","yellow"),
                     border=NA)
             
             
             #method 2 (using ggplot) - orderd but hidden
             md <- melt(dotRINcolor, id=(c("RIN")))
             ggplot(dotRINcolor, aes(x=RIN, fill=color) ) +geom_bar() + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
               coord_flip()
             
             ggplot(dotRINcolor) +geom_bar(aes(x=RIN, fill=color)) + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
               coord_flip()
             
             ######################### this one works 
             dotRINcolor=transform(dotRINcolor, RINfreq = ave(seq(nrow(dotRINcolor)), RIN, FUN=length))
             
             dotRINcolor=dotRINcolor[order(dotRINcolor$RINfreq),]
             
             
             ggplot(dotRINcolor, aes(x= RIN, fill=color)) + 
               geom_bar() + 
               scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
               coord_flip() +  
               labs(x="RIN",y="Months as Active Rulemaking Project") + 
               ggtitle("DOT Rulemaking Projects by RIN and Monthly Status 2008-2016") + 
               theme(axis.text.y =
                       element_text(size  =2,
                                    angle = 0,
                                    hjust = 1,
                                    vjust = 1))  
             
             +
               scale_x_continuous(breaks= seq(200801,201701,by=100))
             
             
             + scale_x_discrete(breaks = c("200801")) 
             
             
             #+ theme(axis.text.x = element_text(face="bold", color="#993333", size=10, angle=45))
             
             labels== c("2008", "2009", "2010","2011", "2012", "2013","2014","2015", "2016", "2016-05"))




summary(dot$RIN)

























































summary(dotdelay)
summary(dotdelay$datadelay)



#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##############################################################################
#################



dotpdelay=as.data.frame(na.omit((cbind(dot$date,dot$whydelay))))
dotpdelay
names(dotpdelay)=c("date","prioritiesdelay")




p1 = ggplot(as.data.frame(na.omit((cbind(dot$date,dot$prioritiesdelay)))),
            aes(V1)) +
  stat_bin(geom="line") +
  labs(x="Month",y="Number of Rulemaking Projects")  # or "line"


p2 = ggplot(as.data.frame(na.omit((cbind(dot$date,dot$staffingdelay)))),
            aes(V1)) +
  stat_bin(geom="line") +
  labs(x="Month",y="Number of Rulemaking Projects")  # or "line"


multiplot(p1, p2, cols=2)

plot(p1)
plot(p2, add)




ggplot(dotdelay,
       aes(date)) +
  stat_bin(geom="bar") +
  labs(x="Month",y="Number of Rulemaking Projects") 


test_data_long <- melt(dotdelay, id="dot$date")  # convert to long format

ggplot(dotdelay, aes(dot$date)) +
  geom_line(aes(y = datedelay, colour = "red")) + 
  geom_line(aes(y = staffingdelay, colour = "blue"))




p=ggplot(data=dotdelay)  
p+geom_histogram(binwidth=0.5,stat="identity")+  
  aes(x=reorder(date,-value,sum),y=value,label=value,fill=value)+
  theme()

names(dotdelay[1])=c("date")

dotdelay$date=as.numeric(dotdelay$dot$date)
summary(dotdelay)

sum(dotdelay$staffingdelay[which(200801==dot$date):which(200812==dot$date)])


sum(dotdelay$staffingdelay)















# DOT COLOR MONTHS BY RIN

# method 1 (using counts table)- not ordered 
dotRINcolor<-dot[c(6,7)]
counts <- table(as.factor(dotRINcolor$color), dotRINcolor$RIN)
barplot(counts, main="DOT Rules By Delay Status Jan 2008-May 2016",
        xlab="RIN", ylab="Months", col=c("black","green","red","yellow"))


#method 2 (using ggplot) - orderd but hidden
md <- melt(dotRINcolor, id=(c("RIN")))
ggplot(dotRINcolor, aes(x=RIN, fill=color) ) +geom_bar() + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
  coord_flip()

ggplot(dotRINcolor) +geom_bar(aes(x=RIN, fill=color)) + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
  coord_flip()




write.csv(dotRINcolor, file="dotRINcolor.csv")
#write.csv(rules, file="Feb2013DOTcleaner.csv")
dotRINcolor=read.csv("dotRINcolor.csv")









totals <- dotRINcolor
group_by(RIN) 
summarize(total = sum(color))
#Then adding a geom_text layer to your plot, using totals as the dataset:
p=ggplot(data=dotRINcolor) 

p + geom_bar(binwidth = 0.5, stat="identity") +  
  aes(x = reorder(RIN, -color, sum), y = color, label = value, fill = color) +
  theme() +
  geom_text(aes(RIN, total, label = total, fill = NULL), data = totals)
#You can make the text higher or lower than the top of the bars using the vjust argument, or just by adding some value to total:

p + geom_bar(binwidth = 0.5, stat = "identity") +  
  aes(x = reorder(class, -value, sum), y = value, label = value, fill = year) +
  theme() +
  geom_text(aes(class, total + 20, label = total, fill = NULL), data = totals)



data <- read.table(header = TRUE, text = "name    value1  value2
                   1   A       1118    239
                   2   B       647     31
                   3   C       316     1275
                   4   D       2064    230
                   5   E       231     85")

library('reshape2')
library('ggplot2')

melted <- melt(data, id.vars=c("name"))

melted <- within(melted, {
  name <- factor(name, levels = names(sort(tapply(value, name, sum))))
})

levels(melted$name)
# [1] "E" "B" "A" "C" "D"

ggplot(melted, aes(x= name, y = value, fill = variable, order = variable)) + 
  geom_bar(stat = "identity") +
  coord_flip()

#####################################
counts <- table(as.factor(dotRINcolor$color), dotRINcolor$RIN)
#?as.data.frame
counts=t(counts)
as.data.frame(counts, optional=True)
colnames(counts)[1]="RIN" 
counts
cbind(counts, newColumn = labels)
counts$Yellow




melted <- melt(counts, id.vars=c("RIN"))
melted
melted <- within(melted, {
  RIN <- factor(RIN, levels = names(sort(tapply(value, RIN, sum))))
})
levels(melted$RIN)
# [1] "E" "B" "A" "C" "D"

ggplot(melted, aes(x= RIN, y = value, fill=color)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
  coord_flip()




####
#  dotRINcolor=dotRINcolor[1:4765,]


dotRINcolor=transform(dotRINcolor, RINfreq = ave(seq(nrow(dotRINcolor)), RIN, FUN=length))

dotRINcolor=dotRINcolor[order(dotRINcolor$RINfreq),]


ggplot(dotRINcolor, aes(x= RIN, fill=color)) + 
  geom_bar() + 
  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + coord_flip()

>
  
  
  ggplot(data = dotRINcolor, aes(x = RIN, fill = color)) + 
  geom_bar()

############################


md <- melt(dotRINcolor, id=(c("RIN")))
ggplot(dotRINcolor, aes(x=RIN, fill=color) ) +geom_bar() + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
  coord_flip()

ggplot(dotRINcolor) +geom_bar(aes(x=RIN, fill=color)) + scale_x_discrete(limits=dotRINcolor$RIN) +  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow", "Black"="black","Green"="green")) + 
  coord_flip()













##########################################
# WORKING #
##########
DOTmonthly$whydelay1

# reasons <- DOTmonthly %>% dplyr::select(DOTdate, whydelay1, whydelay2, whydelay3, whydelay4, whydelay5, whydelay6, whydelay7, whydelay8)
reasons %<>% melt(id="DOTdate")
#reasons$value %<>% substr(0,60) 
# reasons$value %<>% {gsub("  ", " ",.)}
# reasons %<>% arrange(DOTdate)
# reasons %<>% filter(nchar(value)>5)
reasons %<>% filter(value>0) %<>% group_by(DOTdate)
reasons$value %<>% as.factor()
reasons %<>% dplyr::count(reasons, variable)
ggplot(reasons, aes(DOTdate)) + geom_bar(aes(fill = variable))
reasons 

