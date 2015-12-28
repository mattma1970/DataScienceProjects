## Webscraping the cherry blossom 10 mile race results for men 1999-2012

library(XML)


scrapeData = 
  ## Get data from URL passed in. Drop heading and return character vector.
  #@intHeaderLines is the number (base 1) of the element marking the end of the header.
  function(url,year=1999,intHeaderLines=6){
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
    if (intHeaderLines>0){
        lines = strsplit(txt,'\\r\\n')[[1]][-(1:intHeaderLines)]
    } else {lines = strsplit(txt,'\\r\\n')[[1]] }
  }
  else {
    lines = unlist(lines)
    if (intHeaderLines>0)
    {
      lines = lines[-(1:intHeaderLines)]
    }
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
             c(searchLocations[index]+1,searchLocations[index+1]-1)
           },
           headerRow = headerRow, searchLocations=searchLocations)  #note using sapply not mapply as the paras headerrow and searchlocations are not in applied sequentially to the parameter of the function
  }


extractVariables = 
  # take a vector char and split into a matrix of seperate variables. Data type remains char.
  function(myData,varNames = c("name","home","ag","gun","net","time")){
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
allMensResults = mapply(scrapeData,url = urls,year=years, intHeaderLines=rep.int(intHeaderRows,length(years)))
names(allMensResults)=1999:2012
return(allMensResults)
  }


### Main   ########################

#get the data by scraping the web site. 
mensResults = getAllMensResults(intHeaderRows=0)

# get cols of interest only and return a char matrix
mensResultsMatrix = lapply(mensResults, extractVariables) 

# parse values to appropriate type.





