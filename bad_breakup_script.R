# How does when you look, and how long, affect the conclusions you reach about your data?
# Are short term studies more likely to yield significant results?
# Are short term studies more likely to find *erroneous* significant trends?
# This script will perform a simple moving window analysis to answer these questions
# for long term data across a variety of domains- essentially, data gets subsetted, 
# we run a simple linear regression on each subset and record summary stats, trends

#assume data is coming in in the form year, response variable
#use this test data set to build stuff
test<-read.csv(file="test.csv", header=TRUE)

# technical things that need to be done:

# data needs to be normalized in some way because we will be operating across domains 
# and likely with dramatically different values. Let's use a standardization approach so
# we don't give undue influence to outliers- something like x(standard)=(x-mean)/Stdev

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
  output<-c(summary(model)$coefficients[2,1], # slope
            summary(model)$coefficients[2,2], # se for slope
            summary(model)$coefficients[2,4]) #p value
  return(output)
}

#and try this on test data
linefit(test1)
# functional!



