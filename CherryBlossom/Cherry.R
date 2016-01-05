## Webscraping the cherry blossom 10 mile race results for men 1999-2012

library(XML)


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
age = sapply(mensResultsMatrix,function(x) as.numeric(x[,'ag']))
boxplot(age,ylab="Age",xlab="Year")

# deal with time. time,gun, net is the order of preference for times.
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

useableTimes = sapply(mensResultsMatrix,getUseableTime) 
##found that 2001 has 2 instance where gun time is ommitted but available everywhere else
##decisions to leave the zero values in there as impact on averages, and tracking of data willl be negligable

##Add numerical time to results.
mensResultsMatrix=mapply(cbind,mensResultsMatrix,useableTimes)






