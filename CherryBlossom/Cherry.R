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
    lines = strsplit(txt,'\\r\\n')[[1]][-(1:intHeaderLines)]
  }
  else {
    lines = as.vector(lines)
    lines = lines[-(1:intHeaderLines)]
  }
  
  print(t(c("year",year,"  #rows=",length(lines))))
  return (lines)
}

getAllMensResults = 
  # Get all the mens results for the cherryblossom race 1999-2012.
  function(){
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
mensResults = getAllMensResults()
