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

#print ("Message Data Files")
#sapply(messagePaths, function(x) 
 #   print (paste(x,length(list.files(x))))
#  )

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
    {
      # found that there was a not email named as if it were email bu contained no header/body, just a series of commands (in spam directory)
      # Learning: ensure the graceful failure of any method used in the apply family.
      return(list(header=NA,body=NA))  
    }
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
          intBoundaryLoc = grep("boundary=[:space:]*",vHeader,ignore.case = TRUE)[1] #boundary token line number
        # remove quotes and then extract the boundary token value.
          strTemp = gsub("\"","",vHeader[intBoundaryLoc])
          strDelimiter = sub(".*boundary=([^;]+);?.*","\\1",strTemp, ignore.case = TRUE)
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
      else if (length(lOpenLocs)>=0 & length(lCloseLocs)==0){
        # there is most likely an opening boundary at the start of the email body so there is no way to tell if an attachemtn is present
        # If this occurs then drop the email from corpus.
        vRet = NA
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
  vWords = unique(vWords)  # do this as we will be keeping the words for each document seperate in order to calculate probs
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
#msgWordLists is a list of list of character vectors. 

msgWordLists = lapply(dirNames,getMsgWordsDirectory)
numMsgs = sapply(msgWordLists,length)

# cretae a single list where each item is a character vector of unqiue words in a single document from the corpus
msgWordLists = unlist(msgWordLists, recursive=FALSE)
# flags to label the corpus document as spam or not. Repeat by apply rep elements wise. Final vector has 1-1 correspondance with a documents unique word vector
flagIsSpam = rep(c(FALSE,FALSE,FALSE,TRUE,TRUE),numMsgs)

####################################################################################################
# Develop model for spam detection.
####################################################################################################

# Create train and test sample data using 1/3 of the spam  and ham messages from teh corpus

intTotalEmails = length(flagIsSpam)    # total number of emails in full corpus
intTotalSpam = sum(flagIsSpam==TRUE)
intTotalHam = sum(flagIsSpam==FALSE)
set.seed(418910)

# random sample of indecies in range of spam and ham subsets.
testSpamIds = sample(intTotalSpam,floor(intTotalSpam/3))
testHamIds = sample(intTotalHam,floor(intTotalHam/3))

testMsgWords = c((msgWordLists[flagIsSpam])[testSpamIds],(msgWordLists[!flagIsSpam])[testHamIds])
trainMsgWords = c((msgWordLists[flagIsSpam])[-testSpamIds],(msgWordLists[!flagIsSpam])[-testHamIds])
# and the correspoinding spam/ham flags
testFlagIsSpam = rep(c(TRUE,FALSE),c(length(testSpamIds),length(testHamIds)))
trainFlagIsSpam = rep(c(TRUE,FALSE),c(intTotalSpam-length(testSpamIds),intTotalHam - length(testHamIds)))

# Get training set complete list of unique words from spam and ham
#bow = unique(unlist(trainMsgWords))

write(as.vector(unlist(testMsgWords)), 'R_BOW.csv')


####################################################
# create probability tables for spam and ham emails.
####################################################
# Naive bayes requires the probs of both the presence and the abscence so returned table should contain both.
getProbTables=
# wordlist is list of document (ham and spam) representing the entiring feature subspace on which to create the probs
# blSpam a logical vector indicating whether the corresponding document in wordList is spam or ham
# bow - bag of words is the unlisted collection of unique words in the 'corpus'
# * uses vectorised approach rather than looping. 
# returns a matrix with all relevant probability
function(wordList, blSpam, bow = unique(unlist(wordList))){
  # matrix with unique words from corpus in columns. Add dummy data of 0.5
  wordTable = matrix(0.5, nrow = 4, ncol=length(bow), 
                     dimnames = list(c("spam","ham","presentLogOdds","missingLogOdds"), bow)
             )
  # start collecting the counts. Note that we ensured that when a word vector for a single document was returned the words were unique.
  # So, there is a one to one correspondance between the occurance of the word and the # of docs in which it is present.
  counts.spam = table(unlist(lapply(wordList[blSpam],unique)))
  wordTable["spam",names(counts.spam)] = counts.spam+0.5
  
  #ham equivalent
  counts.ham = table(unlist(lapply(wordList[!blSpam],unique)))
  wordTable["ham",names(counts.ham)] = counts.ham+0.5
  
  # get divisors
  totalSpam = sum(blSpam)
  totalHam = length(blSpam)-totalSpam
  
  # Naive Bayes uses the same trick as in MCVC whereby the normalisation constant is cancelled off by taking a ratio
  # of probabilities. Using Log to convert division to substration and stretching the ranges from 0-1 to -inf 0 is a natural step.
  # Moreover, in the Bayes expression we deal with P(content | spam). The Naive part is to assume that the content 
  #consists of entirely independant collections of words and thus the probability of the content can be written
  # as teh product of the probabilities of each word conditioned on being ham or spam. 
  
  # calc frequencies (probs)
  wordTable["spam",]=wordTable["spam",]/(totalSpam+0.5)
  wordTable["ham",]=wordTable["ham",]/(totalHam+0.5)
  
  #calc log odds
  wordTable["presentLogOdds",]= log(wordTable["spam",]) - log(wordTable["ham",])
  wordTable["missingLogOdds",]= log(1-wordTable["spam",]) - log(1-wordTable["ham",])
  
  invisible(wordTable)   # don't print the result to the screen when ths is running.

}


computeLLR=
  #calculat the LLR for Naive Bayes classifier
  # newMesg is a vector of UNIQUE WORDS extracted from a message
  #probTable is the MATRIX of probs and logOdds calc for training words (cols)
  function(newMesg,probTable){
    #remove words never encountered in corpus
    newMesg = newMesg[!is.na(match(newMesg,colnames(probTable)))]
    #get logical vector of those words present
    present = colnames(probTable) %in% newMesg
    # calc LLR
    ret = sum(probTable["presentLogOdds",present])+sum(probTable["missingLogOdds",!present])
    return(ret)
  }

#calculate the probabilities for the training set.
trainTable = getProbTables(trainMsgWords,trainFlagIsSpam)

# ad-hoc test on spam (returns 239)
newMesg = testMsgWords[[1]]
computeLLR(newMesg,trainTable)

#ad-hoc test on ham (rturn -70)
newMesg = testMsgWords[[length(testMsgWords)]]
computeLLR(newMesg,trainTable)

#Run LLR calcs on whole test set
testLLR = sapply(testMsgWords,computeLLR,trainTable)

myData = as.data.frame(cbind(LLR=testLLR,isSpam=testFlagIsSpam))
myData$isSpam = factor(myData$isSpam,levels=c(0,1),labels=c("Ham","Spam"))
par(mar=c(5,5,5,5))
boxplot(LLR~isSpam,data=myData)
title("LLR calc for Spam Classifier",ylab="LLR",xlab='Classification')

## Analysis of performance
# Select a threshold for the LLR above which it is considered to be Spam.
# Type 1 error in this case is incorrect classification as spam i.e its above threshold but is ham. The error rate is the
# % of ham classified as spam

typeIErrorRate=
function(threshold,LLR,blSpam){
  # number of ham classified as spam/#ham
  sum(LLR>threshold & !blSpam)/sum(!blSpam)
}
typeIIErrorRate=
  #incorrectly classifying spam as ham. Not such an important issue.
  function(threshold,LLR,blSpam){
    # number of ham classified as spam/#ham
    sum(LLR<threshold & blSpam)/sum(blSpam)
  }

t=seq(-300,300,by=1)
Type1 = sapply(t,typeIErrorRate,myData$LLR,(myData$isSpam=="Spam"))
Type2 = sapply(t,typeIIErrorRate,myData$LLR,(myData$isSpam=="Spam"))
perfData=data.frame(cbind(Threshold=t,typeIErrRate=Type1,typeIIErrRate=Type2))

plot.new()
plot(x=perfData$Threshold,y=perfData$typeIErrRate,type="n",xlab="Threshold for LLR",ylab="Error Rate", main="Type1 and Type 2 Error comparison")
lines(x=perfData$Threshold,y=perfData$typeIErrRate,type="l",col="red")
lines(x=perfData$Threshold,y=perfData$typeIIErrRate,type="l")
legend(10,-10,c("Type1Err Rate","Type II Err Rate"),col=c(3,5))
crossOverThresholdIndex = which(abs(perfData$typeIErrRate-perfData$typeIIErrRate)<0.001)
print (t[crossOverThresholdIndex])

