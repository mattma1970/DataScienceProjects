## Webscraping the cherry blossom 10 mile race results for men 1999-2012

library(XML)
library(RColorBrewer)

scrapeData = 
  ## Get data from URL passed in. Drop heading and return character vector.
  #@intHeaderLines is the number (base 1) of the element marking the end of the header.
  function(url,year=1999){
  txt=''
  doc=htmlParse(url)
  
  ##deal with web page of results and handle formatting variations.
  if (year==2000){
    ## Year 2000 is not not properly formed html and so needs to be handled as an exception. 
    ff = getNodeSet(doc,"//font")
    txt = xmlValue(ff[[4]])
  }
  else if (year==2009){
    ## 2009 appears to have been copy/pasted from MS Office. 
    ## Each line is wrapped in a <pre></pre> tag rather than delimited with \r\n as in plain text.
    pp = getNodeSet(doc,"//pre")
    # need to get rid of all <html tags> in the lines. Use regex to get rid of <> tags.
    lines = lapply(pp,function(x){ return (gsub("<.+?>",'',xmlValue(x))) })
  } else
  {
    preNode = getNodeSet(doc,"//pre")
    txt=xmlValue(preNode[[1]])
  }
  if (txt!=''){
        lines = strsplit(txt,'\\r\\n')[[1]]
  }
  else {
    lines = unlist(lines)
  }
  
  print(t(c("year",year,"  #rows=",length(lines))))
  return (removePreamble(lines))
  }


removePreamble = 
  # Remove the preamble and set the header row ot lower case
  #@results is a character vector.
  # returns processed list.
  function(results){

    dividerRowNumber = grep("^={4}",results)
    if (dividerRowNumber>2){
        results = results[-(1:(dividerRowNumber-2))]
    }
    results[1]=tolower(results[1])
    return(results)
}


getColumnLocs = 
  #get the character positions for the columns.
  # returns the bounding locations of the columns in character location units.
  function(spacerRow){
    locs = gregexpr("\\s",spacerRow)[[1]]
    if (substring(spacerRow,nchar(spacerRow),nchar(spacerRow))!=" "){
      return(c(0,locs,nchar(spacerRow)+1))
    } else {
      return(c(0,locs))
    }
  }



getValByCol =
  # get the values from a column specified by the name 
  # @colNames is the names of the columns to retrieve vals for
  # @headerRow is the columnnames row in the data
  # @searchLocations are the bounding character positions delimiting the column
  # @returns a matrix with each column corresponding to a column name, row one is start char loc and row 2 is end char location of column
  function(colNames,headerRow,searchLocations){
    sapply(colNames,
           function(name, headerRow,searchLocations){
             startPos = regexpr(name,headerRow)[[1]]  #returns base 1 location of the column name.
             if(startPos==-1)
               return (c(NA,NA))
             index = sum(startPos>=searchLocations)  # find the index in the searchLocations
             c(searchLocations[index]+1,searchLocations[index+1])
           },
           headerRow = headerRow, searchLocations=searchLocations)  #note using sapply not mapply as the paras headerrow and searchlocations are not in applied sequentially to the parameter of the function
  }

removeStrayLines = 
  # helper functon to remove blankl lines from the data
  function(myData){
    #get the empty and footnote (#,*) lines
    emptyRowIndecies = grep("(^[[:blank:]]*$|^[#\\*])",myData)  # returns a vector of 1 based indicies.
    if (length(emptyRowIndecies)>0){
      myData = myData[-(emptyRowIndecies)]
    }
    return (myData) 
  }

extractVariables = 
  # take a vector char and split into a matrix of seperate variables. Data type remains char.
  # @ret - returns a list of split out data sets.
  function(myData,varNames = c("name","home","ag","gun","net","time")){
    
    #first clean the data
    myData = removeStrayLines(myData)
    # data will always have first row as the columns names and second row as the spacer row from which the column delimiter places will be foudn
    colDelimiters = getColumnLocs(myData[2])   #char locations of col boundaries
    liColBoundaries = getValByCol(varNames,myData[1],colDelimiters)   # the boundaries corresponding to the col names supplied
    
    Values = mapply(substr,list(myData[-(1:2)]),start=liColBoundaries[1,],stop=liColBoundaries[2,])
    colnames(Values)=varNames
    return(Values)
  }


getAllMensResults = 
  # Get all the mens results for the cherryblossom race 1999-2012.
  # Returns list of results, one element per year.
  function(intHeaderRows){
ubase = "http://www.cherryblossom.org/"
#### Handling multiple pages of results. 
##urls = paste(ubase,"results/",1999:2012,"/",1999:2012,"cucb10m-m.htm",sep="")
## FAILS DUE TO CHANGING URL AND XML FORMAT OVER TIME allMensResults = lapply(urls,scrapeData)

menURLs = c("cb99m.htm","cb003m.htm","results/2001/oof_m.html","results/2002/oofm.htm","results/2003/CB03-M.HTM",
            "results/2004/men.htm","results/2005/CB05-M.htm",
            "results/2006/men.htm","results/2007/men.htm",
            "results/2008/men.htm","results/2009/09cucb-M.htm",
            "results/2010/2010cucb10m-m.htm","results/2011/2011cucb10m-m.htm",
            "results/2012/2012cucb10m-m.htm")

urls = paste(ubase,menURLs,sep="")
years=1999:2012
allMensResults = mapply(scrapeData,url = urls,year=years)
names(allMensResults)=1999:2012
return(allMensResults)
  }

################### Deal with times #############################################################
timeToMinutes = 
  # convert a character vector in format hh:mm:ss or mm:ss to minutes.
  function(x){
    val=0
    components = as.vector(strsplit(x,':')[[1]])
    if (length(components)>0){
      for (i in 1:length(components)){
        val=val*60+as.numeric(gsub('[^[:digit:]]','',components[i])) #clear footnote markers
        # print(c(x,components[i],val,as.numeric(gsub('[^[:digit:]]','',components[i]))))
      }
    }
    #print(c(x,val))
    return (val/60.0) #
  }

isAllNA = function(vecData){
  if (sum(is.na(vecData))>length(as.vector(vecData))*0.85)
    return (TRUE)
  else
    return (FALSE)
}

getUseableTime = 
  # get the time from the data and convert it to minutes. 
  # return a list of times.
  function(myData){
    ret = list()
    if (!isAllNA(myData[,'gun'])) {
      ret = sapply(myData[,'gun'],timeToMinutes)
    } else if (!isAllNA(myData[,'time'])){
      ret = sapply(myData[,'time'],timeToMinutes)
    }
    else {
      # fall back to net (assumes it exists. must check if this is true)
      ret =sapply(myData[,'net'],timeToMinutes)
    }
    return(FinalTime=ret)
  }
######################## END deal with times #####################################


### Main   ########################

#get the data by scraping the web site. 
mensResults = getAllMensResults(intHeaderRows=0)

# get cols of interest only and return a char matrix
mensResultsMatrix = lapply(mensResults, extractVariables) 

## Ad hoc data cleaning
# Parse values to appropriate type.
age = sapply(mensResultsMatrix,function(x) as.numeric(x[,'ag']))
boxplot(age,ylab="Age",xlab="Year") #Observe that 2003 and 2006 have something wrong. Check data. Find that data is right shift on place relative to the delimiting markers in header.
#age2001 = mensResults[['2001']]
#grep("^[[:blank:]]*$",mensResults[['2001']])

# Remove small age values
mensResultsMatrix = sapply(mensResultsMatrix, 
                            function(x) {
                              b= which(as.numeric(x[,'ag'])<5)
                              if (length(b)>0)
                                return(x[-b,])
                              else
                                return (x)
                              })
intAge = sapply(mensResultsMatrix,function(x) as.numeric(x[,'ag']))
mensResultsMatrix = mapply(cbind,mensResultsMatrix,age=intAge)
boxplot(intAge,ylab="Age",xlab="Year")

######### Deal with time. time,gun, net is the order of preference for times.

useableTimes = sapply(mensResultsMatrix,getUseableTime) 
##found that 2001 has 2 instance where gun time is ommitted but available everywhere else
##decisions to leave the zero values in there as impact on averages, and tracking of data willl be negligable

##Add numerical time to results.
mensResultsMatrix=mapply(cbind,mensResultsMatrix,raceTime = useableTimes)
#check if the racetime data is okay. FOund lots of NA is 2007 which result from people doing the 1/2 race. 
sapply(mensResultsMatrix,function(x) which(is.na(x[,'raceTime'])))
#D Drop the non-finishers
mensResultsMatrix = mapply(function(m,t){  m[!is.na(t),]}, mensResultsMatrix,useableTimes)

prepareDF = 
  # pare down to just the data columns of interest and collapse list to single data frame.
  # @myData is matrix representing a single year.
  function(myData,year,sex){
    retDF= data.frame(year=rep(year,nrow(myData)),
                       sex = rep(sex,nrow(myData)),
                       name = myData[,'name'],
                       home=myData[,'home'],
                       age=as.numeric(myData[,'age']),
                       raceTime = as.numeric(myData[,'raceTime']),
                       stringsAsFactors = FALSE)
    invisible(retDF)
  }

mensDF = mapply(prepareDF,myData=mensResultsMatrix,year=1999:2012,sex=rep('male',14),SIMPLIFY=FALSE)
mensFinalData = do.call(rbind,mensDF)


#### Exploring the data.
plot(raceTime~age,data=mensFinalData,ylim=c(40,180))

Purples8A=paste(brewer.pal(9,"Purples")[8],"14",sep="")
#manual smoothed scatter plot using points.
mensFinalData$ageJitter= jitter(mensFinalData$age,amount=0.5)
plot(raceTime~ageJitter,data=mensFinalData, type='n')
points(mensFinalData$ageJitter,mensFinalData$raceTime,cex=0.5,pch=20,col=Purples8A)
# using smooth scatter
smoothScatter(y=mensFinalData$raceTime,x=mensFinalData$age,ylim=c(40,165),xlim=c(15,85))
#chunk down into categories of age and explore the averages
menFinalSub= subset(mensFinalData,raceTime>30 & !is.na(age) & age>5)
ageCat = cut(menFinalSub$age,breaks=c(seq(15,75,10),90))
plot(menFinalSub$raceTime~ageCat,xlab="age in years",ylab='run time(mins)')

#begin analysis of trends
smoothScatter(y=mensFinalData$raceTime,x=mensFinalData$age,ylim=c(40,165),xlim=c(15,85))
lmAge =lm(raceTime ~ age, data = menFinalSub)
#summary(lmAge)
smoothScatter(y=lmAge$residuals,x=menFinalSub$age,xlab='age (years)',ylab='residuals')
abline(h=0,col="purple",lwd=3)

#begin residual analysis of the linear model
#use loess (local regression)
resid.io = loess(resids ~ age, data=data.frame(resids = residuals(lmAge),age=menFinalSub$age))
age20to80 = 20:80
resid.io.pr = predict(resid.io,newdata=data.frame(age=age20to80))
lines(x=age20to80,y=resid.io.pr,col='green',lwd=3,lty=2)
# residual analysis shows that after 60 the residuals are increasingly positive which suggests that model breaks down after age=60.
# try non-linear regressions techniques.

menRes.io =  loess(raceTime~age, menFinalSub)
menRes.io.pr = predict(menRes.io, data.frame(age=age20to80))

over50 = pmax(0,menFinalSub$age-50)
lmOver50 = lm(raceTime~age+over50, data = menFinalSub)
summary(lmOver50)
over50Sub = pmax(0,age20to80-50)
pred = predict.lm(lmOver50,newdata=data.frame(age=age20to80,over50=over50Sub))
plot(y=pred,x=age20to80, type='n')
lines(y=pred,x=age20to80)

decades = seq(30,60,by=10)
overAge = lapply(decades, function(x) pmax(0,menFinalSub$age - x))
names(overAge)=paste("over",decades,sep="")
head(overAge)
lmPeicewise = lm(raceTime ~ ., data=cbind(menFinalSub[,c("raceTime","age")],overAge))

overAgeSub = lapply(decades, function(x) pmax(0,age20to80 - x))
names(overAgeSub)=paste("over",decades,sep="")
pred = predict(lmPeicewise, newdata=cbind(data.frame(age=age20to80),overAgeSub))
lines(y=pred,x=age20to80,lwd=3,col='red',lty=2)
lines(y=menRes.io.pr,x=age20to80,col='green',lwd=3,lty=5)
