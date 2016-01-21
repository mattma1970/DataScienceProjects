########################################################################################
## The purpose of this project is to create a Naive Bayes Spam filter based on ham/spam data set available from 
# spamassasin.apache.org/publiccorpus/. This project will initially be developed in R and will then be rebuilt in Python as a learning
# exercise. 

# The script below is a record of the learning sessions and not intended for production.

#Matt Ma 19/1/2016
########################################################################################
library(tm)
library(SnowballC)
setwd('~/Documents/GitRepos/RRepos/Spam')
dirMessageRoot = getwd()

dirNames = list.files(path=paste(dirMessageRoot,"Messages",sep=.Platform$file.sep))
messagePaths = paste(dirMessageRoot,"Messages",dirNames,sep=.Platform$file.sep)
messagesList = list.files(messagePaths)
print (length(messagesList))

print ("Message Data Files")
sapply(messagePaths, function(x) 
    print (paste(x,length(list.files(x))))
  )

## Read in the emails using readLines
# Trial 
##fileNames =list.files(messagePaths[1],full.names = TRUE)
##fileNames[1]
##msg = readLines(fileNames[1])   # read each line in an email and create a character vector to hold the email.
##head(msg)
##class(msg)
##length(msg)
##msg

#Firstly just pull a sample of unlabelled emailed that will be used for building the parsing and processing functions. 
#This is not a training set. 

emailIndecies = c(1:5, 14,27,68,69,329,404,427,516,852,971)  # emails from the easy_ham folder.
fn = list.files(messagePaths[1],full.names = TRUE)[emailIndecies] # get the fully qualified names of the sample emails
sampleEmails = sapply(fn, readLines)


## Functions to extract data from emails
# Seperate header from body

severHeader = 
  # seperate header and body. Delimiter is first occurance of blank line.
  # @vEmail a character vector wiht one entry per line of text
  # return list of 2 char vectors (header=1, body =2) or null if no header/body found
  function(vEmail){
    boundary = -1 # default for failure
    boundary = try(which(vEmail=="")[1], silent=TRUE)
    if (!is.na(boundary)){
      eHead = vEmail[1:(boundary-1)]
      eBod = vEmail[-(1:boundary)]
      ret =  list(header = eHead,body=eBod)
      return  (ret)
    }
    else
      stop()
  } 

# remove attachments
findAttachments = 
  # Remove attachments from the body fo text. Content-type of 'multipart/mixed'
  # indicates an attachment is present. BOUNDARY key contains delimitng text string for attachemtn boundaries.
  # accpets list of header vectors
  # return tuple of (bool,string) where bool indicates an attachment exists and string is the string delimiter
  
function(vHeader)  {
  # search header for Content-Type: key
  intContentLine = grep('content-type:',vHeader,ignore.case=TRUE)[1]
  if (length(intContentLine)>0){
    # found it. Then check the value is multipart.
    if (length(grep('multipart',vHeader[intContentLine],ignore.case = TRUE))>0)
    {
      # get the delimiting string 
        # strDelimiter = sub(".*boundary=\"(.*)\";.*","\\1",vHeader[intContentLine])  # failed as values for cntenttype key can wrap around lines and quotes mayb be missing
         
         # find the boundary key 
          intBoundaryLoc = grep("boundary=",vHeader,ignore.case = TRUE)[1] #boundary token line number
        # remove quotes and then extract the boundary token value.
          strTemp = gsub("\"","",vHeader[intBoundaryLoc])
          strDelimiter = sub(".*boundary=([^;]+);?.*","\\1",strTemp)
                return (list(blHasAttachment=TRUE,strDelim=strDelimiter))
    }
    else 
      return (list(blHasAttachment=FALSE,strDelim=""))
  }
  else
  {
    return (list(blHasAttachment=FALSE,strDelim=""))
  }
}

dropAttachments = 
  # remove attachments based on information boundary string data passed in
  # lAttachInfo tuple (flag,boundary string)
  # lBody single email bodies corresponding to the flag data
  function(lAttachInfo, lEmail){
    if (!lAttachInfo$blHasAttachment)
      {return(lEmail$body)}
    else
    {
      #find the location of the opening boundaries string occurence ("--"+strDelim)
      strDelimOpen = paste("--",lAttachInfo$strDelim,sep="")
      strDelimClose = paste(strDelimOpen,"--",sep="")
      lOpenLocs = which(lEmail$body==strDelimOpen)
      lCloseLocs = which(lEmail$body==strDelimClose)
      # check that boundaries are properly applied by checking that email has one opening boundary at start of body
      # at least one start of attachment and one close. If no opening one the format is faulty.
      ### special cases
      # length(lOpenLocs)==1 & length(lCloseLocs)==1)  -> No attachment despite delimiters present. Assume no attachment
      if (length(lOpenLocs)==1 & length(lCloseLocs)==1) {
        vRet = lEmail$body[-lCloseLocs[[1]]]
        vRet=vRet[-(1:lOpenLocs)]
      }
      else if (length(lOpenLocs)>1 & length(lCloseLocs)==0){
        ## opening delimiter but no closing. Assume everything after 2nd delimiter is an attachment.
        vRet = lEmail$body[-(lOpenLocs[[2]]:length(lEmail$body))]
      }
      else {
        vRet = lEmail$body[-(lOpenLocs[[2]]:lCloseLocs[[1]])]
        vRet = vRet[-lOpenLocs[[1]]]
      }
      return (vRet)
    }
  }



# Get words from body. Return a vector of unique words contained in a email message
# The tm package will be used for the preprocessing of the data for a single email and 
# on the corpus of test emails for consistancy sake. 

getMsgWordVector = 
  # get the vector of unique words in the single email passed in.
  # @vEmail is a character vector with one line of text from the email in each entry.
  # @return character vector containing unique words (no frequencies required)
function(vEmail){
  
  Email = paste(vEmail,sep="",collapse = " ")
  
  txtEmail = Corpus(VectorSource(Email))
  txtEmail = tm_map(txtEmail, removePunctuation)
  txtEmail = tm_map(txtEmail, tolower)
  txtEmail = tm_map(txtEmail, stemDocument)
  txtEmail = tm_map(txtEmail, stripWhitespace)
  txtEmail = tm_map(txtEmail, removeWords, stopwords("en"))
  txtEmail = tm_map(txtEmail, PlainTextDocument)

  # split the document on spaces and collect unique words
  vWords = unlist(strsplit(txtEmail[[1]]$content,"[[:blank:]]+"))
  vWords = vWords[nchar(vWords)>1]
  #vWords = unique(vWords)  DO THIS LATER TO AVOID EXTRA WORK
  invisible(vWords)
}



getMsgWordsDirectory = 
  # Get the combined word vector for all words in all messagesin a child directory of the Message directory
  # return list of character vectors,one list item for each document.
    function(DirName){
      messagePath = paste(dirMessageRoot,"Messages",DirName,sep=.Platform$file.sep) # fully qualified directory
      listFileNames = list.files(messagePath, full.names = TRUE)
      # clear stray cmd$ files 
      nonEmails = grep('cmd',listFileNames)
      if (length(nonEmails)>0) listFileNames = listFileNames[-nonEmails]
      
      #read in all messages. ImPORTANT - add the encode='latin1' to ensure things like pound signs aren't passed to the tm_map
      allMessages = lapply(listFileNames,readLines, encoding="latin1") 
      #split header from bodies
      splitMessages = lapply(allMessages, severHeader)
      # remove attachments
      ## get the list of tuples flagging presence of an attachment and the boundary string 
      flagAttachments = lapply(splitMessages, function(x) findAttachments(x$header))
      # then drop the attachments from the body of the emails. Note SIMPLE=FALSE used to prevent co-ersion into a vector or matrix.
      emailBodies = mapply(dropAttachments,flagAttachments, splitMessages, SIMPLIFY = FALSE)

      #finally get the word list by iterating over the messages and extracting the word vector for each.
      listWordVectors = lapply(emailBodies,getMsgWordVector)

      invisible(listWordVectors)
            
    }

#iterate of all directories and get the word vectors
msgWordLists = lapply(dirNames,getMsgWordsDirectory)


