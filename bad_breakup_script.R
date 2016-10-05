# How does when you look, and how long, affect the conclusions you reach about your data?
# Are short term studies more likely to yield significant results?
# Are short term studies more likely to find *erroneous* significant trends?
# This script will perform a simple moving window analysis to answer these questions
# for long term data across a variety of domains- essentially, data gets subsetted, 
# we run a simple linear regression on each subset and record summary stats, trends

#assume data is coming in in the form year, response variable
#use this test data set to build stuff
test<-read.csv(file="test.csv", header=TRUE)

# set it so we get our decimal places rather than sci notation in our outputs
options(scipen=10)

# technical things that need to be done:

# data needs to be normalized in some way because we will be operating across domains 
# and likely with dramatically different values. Let's use a standardization approach so
# we don't give undue influence to outliers but also gets responses in the same magnitude
# something like x(standard)=(x-mean)/Stdev

standardize<-function(data){
  #operate on the second column in data, where our response variable is
  data$stand.response<-(data[,2]-mean(data[,2]))/sd(data[,2])
  #name the columns consistently so we don't get headaches later
  names(data)<-c("year", "response", "stand.response")
  #spit back our new data frame
  return(data)
}

#try it on test data
test1<-standardize(test)
#seems to be functioning

# next we need a function that runs a simple linear model of x=year, y=response variable

linefit<-function (data){
  #fit the model
  model<-lm(stand.response~year, data=data)
  #create a vector of relevant outputs. We want slope, error, P value
  output<-c(min(data$year), #year the analysis started on
            nrow(data), #number of data points the analysis includes
            length(unique(data$year)), #number of years the analysis includes
            summary(model)$coefficients[2,1], # slope
            summary(model)$coefficients[2,2], # se for slope
            summary(model)$coefficients[2,4]) #p value
  return(output)
}

#and try this on test data
linefit(test1)
# functional!

#now we need to think about how to iterate through the dataset. We want a
#function that starts at the first year, counts the number of rows specified
#and then feeds that rsultant data frame to the fittling function. 
#then we want to discard the first row of the data set, and repeat until fewer than
#the number of rows specified remains

breakup<-function(data, window){ #window is the size of the window we want to use
  remaining<-data #create dummy data set to operate on
  output<-data.frame(year=integer(0), #create empty data frame to put our output variables in
                     length=integer(0), 
                     years=integer(0),
                     slope=numeric(0), 
                     slope_se=numeric(0), 
                     p_value=numeric(0))
  numyears<-length(unique(data$year))
  while (numyears>(window-1)){ #while there's still more years of data than in the window
    chunk<-subset(remaining, year<(min(year)+window)) #pull out a chunk as big as the window from the top of the data
    out<-linefit(chunk) #fit a linear model and get relevant statistics on chunk
    #add a conditional so that if there's missing data, it's not included in output
    if (window==length(unique(chunk$year))){
      output<-rbind(output, out) #append the stats to the output data frame
    }else{
      output<-output #leave it out if it has missing data
    }
    
    remaining<-subset(remaining, year>min(year)) #cut out the first year of the remaining data + repeat
    numyears<-length(unique(remaining$year))
  }
  names(output)<-c("start_year", "N_data", "N_years", "slope", "slope_se", "p_value")
  return(output)#output the data frame
}

#and now try this on the test data
breakup(test1, 3)

# now time to write the function that will iterate through our targetted windows
# let's make a decision rule that our test data set must be greater than 10y in length
# we can chose windows of 3, 4, 5, 6, 10, and actual study length
# let's make this idiot-proof and build the standardize function right into this function
# so we can literally run each properly prepared raw data set with a single line

multiple_breakups<-function(data){
  data1<-standardize(data) #standardize data
  out3<-breakup(data1, 3) #fit at each window length
  out4<-breakup(data1, 4)
  out5<-breakup(data1, 5)
  out6<-breakup(data1, 6)
  out10<-breakup(data1, 10)
  outall<-linefit(data1) #fit a line to the complete data set too
  out<-rbind(out3, out4, out5, out6, out10, outall)
  return(out)
}

multiple_breakups(test)
#fan-flipping-tastic! it looks like that works

#########################################################################################

#now, let's give this a try with some real data

#lets' start with the lampyrid dataset because I'm familiar with it
#and know there is enough data to do it

#bring data in from figshare
lampyrid<-read.csv(file="https://ndownloader.figshare.com/files/3686040",
                   header=T)

#pull in our data cleaning code from https://github.com/cbahlai/lampyrid/blob/master/lampyrid_analysis.R
#details of cleaning in the code in comments found at that link- in summary, get all the typoes
#out and make the date column usable
library(lubridate)
lampyrid$newdate<-mdy(lampyrid$DATE)
lampyrid$year<-year(lampyrid$newdate)
lampyrid$DOY<-yday(lampyrid$newdate)
lampyrid<-na.omit(lampyrid)
lampyrid$TREAT_DESC<-gsub("Early succesional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Early sucessional community", "Early successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Succesional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Sucessional", "Successional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Biologically based \\(organic\\)", "Organic", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-gsub("Conventional till", "Conventional", lampyrid$TREAT_DESC)
lampyrid$TREAT_DESC<-as.factor(lampyrid$TREAT_DESC)
lampyrid$HABITAT<-as.factor(lampyrid$HABITAT)
lampyrid$REPLICATE<-as.factor(lampyrid$REPLICATE)
lampyrid$STATION<-as.factor(lampyrid$STATION)

# data is currently structured as subsamples, replicates, across treatments. We know from 
# http://biorxiv.org/content/early/2016/09/11/074633 that the organisms were most abundant in
# Alfalfa and no-till treatments, so we should use data from these treatments if we're 
# interested in picking up trends over time. We also don't really care about within
# year dynamics for this experiment- we essentially want a summary measure of what was going on
#within a rep, within a year. So let's reshape the data, drop out irrelevant treatments.

library(reshape2)
#tell R where the data is by melting it, assigning IDs to the columns
lampyrid1<-melt(lampyrid, id=c("DATE","TREAT_DESC","HABITAT","REPLICATE","STATION","newdate", "year", "DOY"))
#cast the data to count up the fireflies
lampyrid2<-dcast(lampyrid1, year+TREAT_DESC+REPLICATE~., sum)
#cast the data to count the traps
lampyrid3<-dcast(lampyrid1, year+TREAT_DESC+REPLICATE~., length)
#let's rename these new vectors within the data frame
names(lampyrid2)[4]<-"ADULTS"
names(lampyrid3)[4]<-"TRAPS"

#rename the data frame and combine the number of traps we counted into it from lampyrid3
lampyrid_summary<-lampyrid2
lampyrid_summary$TRAPS<-lampyrid3$TRAPS

#create a new variable to account for trapping effort in a given year
lampyrid_summary$pertrap<-lampyrid_summary$ADULTS/lampyrid_summary$TRAPS

#get rid of columns we don't need for analysis
lampyrid_summary$REPLICATE<-NULL
lampyrid_summary$ADULTS<-NULL
lampyrid_summary$TRAPS<-NULL

# pull out relevant treatments, create a data frame for each
lampyrid_alfalfa<-subset(lampyrid_summary, TREAT_DESC=="Alfalfa")
lampyrid_notill<-subset(lampyrid_summary, TREAT_DESC=="No till")

#get rid of data that isn't needed for our analysis
lampyrid_alfalfa$TREAT_DESC<-NULL
lampyrid_notill$TREAT_DESC<-NULL

#ok, these data frames should be ready to go

#here goes nothing
multiple_breakups(lampyrid_alfalfa)
# there are some perculiarities because 2007 is missing, but I think we're working now.
# try it with other data too
multiple_breakups(lampyrid_notill)

