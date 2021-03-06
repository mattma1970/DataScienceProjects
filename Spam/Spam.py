import unicodecsv as unicsv
import numpy as np
import pandas as pd
import re
import os # misc OS functions
import random as rand
import math
import itertools
import csv
import time
from functools import partial
from matplotlib import pyplot as plt

###############################################################################################
# Naive Bayes Spam Filter using the spam assasin public corpus available on the Apache website.

# The purpose of this project was primarily to improve Python programing skills, work with Pandas objects and explore code optimisation.

# Note: Text preprocessing is performed through application of simple regular expression substituations. The package NLTK was not used.

### Classification Performance:
# The threshold value used to classifying spam/ham was selected to minimise the false positives rate. The rationale is that false positives correspond to ham classified as spam which from a user perspective represents the greatest risk ( a good email is never seen). The FPR = 1-TNR. The best performacen was found to have a threshold value of 38 resulting in a FPR = 2.3% or 2.3% of ham emails classified as spam. For this value 93.5% of spam emails are detected. While these results are promising neither 2.3% type 1 errors or 6.5% type 2 erros (FNR) would be acceptable in practice. Thus these preliminary results using 'off the self' technique could be used a benchmark for further refinement. This might include add header information to the analysis (currently ignored). Results for threshold=38
# Threshhold=38
# Accuracy 0.96572327044
# TPR/sensitivity/recall 0.932598039216
# TNR/specificity 0.977157360406
# precision 0.933742331288
# F1 score 0.933169834457

### Code optimisation.
# The most notable speed improvement was in the calculation of the Log Likelihood ratios.
# Three methods where used:
# i) For loop and set 'in' (O(1)) lookup
# ii) using pandas' apply function and a curried function that used set look up to extract the probabilities for the email unique words.
# iii) Observing that the list of unique words in emails was typically < 5% of the length of the Bag Of Words the contribution of missing words to the LLR
#      was calculated using the (one time calculation) total LLR for the absence of all words - the sum of probabilities of those words in the email being absent.
# The outcomes was a nearly 300 times improvement in calculation time for the LLR between method (i) and (iii)
 # The dramatic improvement was achieved by migrating pure python structures to instance methods on dataframes that are know to be optimised for speed. Specifically replacing for loops with apply constructs, replacing row operations using iterrows() with set and indexing opertions and finally seeking ways to minimise the number of calculations that needed to be performed. 
###############################################################################################



## Globals
data_path="/home/mattma/Documents/GitRepos/RRepos/Spam/Messages"
CONST_MIN_HEADER_SIZE=10  # Sanity check when splitting header and body to ensure a reasonable length header is present
DEBUG=False                # flag for verbose profiling
DEBUG_ATTACHMENTS=False   # flag for verbose profiling of attachment deletion.
MAX_WORD_LENGTH=60        # used to remove stray long words that are unlikely to be english words


# TODO: Find python package for text preprocessing.
# Preprocessing methods and word extraction from a single email.
class TextProcessor:
    def __init__(self):
        self.getStopWords()

    # Properties ##################
    dfEmails = pd.DataFrame(columns=('header','body'))     # hold the data being processed
    _rxIllegalChars = "[^a-zA-Z0-9\s]" #Regular expression patterns for admissible characters to be replaced with blanks
    _stopWords = np.array(None)

    _blIsSpam = False    # used to tag an email as either spam or ham

    def setTag(self,blIsSpam):
        self._blIsSpam = blIsSpam
    
    # Class methods ################
    def getStopWords(self):
        self._stopWords = np.genfromtxt('common-english-words.txt',dtype=str, delimiter=',')
        
    # set all body text to lower case
    def toLower(self):
        for i,myRow in self.dfEmails.iterrows():
            #myrow[0]=index value NOT row number; myrow[1]=header myrow[2]=body
            liNewLowerLines = map(lambda x: x.lower(),myRow['body'])
            self.dfEmails['body'][i]=liNewLowerLines
    
    #remove punctuation and other XML chars.
    def removePunc(self):
        for i,myRow in self.dfEmails.iterrows():
            liNewLines = [re.sub(self._rxIllegalChars,' ',line) for line in myRow['body']]
            self.dfEmails['body'][i]=liNewLines
    
    #basic stemming. Remove ed,ing, ly
    #operates on dfEmail in place
    #assumes punctuation removed.
    def stem(self):
        rxED = '(ed)[\s\b]'
        rxING = '(ing)[\s\b]'
        rxLY = '(ly)[\s\b]'

        for i,myRow in self.dfEmails.iterrows():
                liNewLines = [re.sub(rxED,' ',line) for line in myRow['body']]
                liNewLines = [re.sub(rxING,' ',line) for line in liNewLines]
                liNewLines = [re.sub(rxLY,' ',line) for line in liNewLines]
                self.dfEmails['body'][i]=liNewLines        
    
    #get a list of unique words occuring in a single email.
    # remove stop words
    #returns dataframe rows=email,columns=uniquewords, isSpam boolean
    def getUniqueWordsAndTag(self,maxLength=100):
        liUniqueWords = []
        for i,myRow in self.dfEmails.iterrows():
            setWords = set()
            for line in myRow['body']:
                for word in line.split(' '):
                    if len(word)>1 and word!=' ' and  not (word in self._stopWords) and len(word)<maxLength:
                        setWords.add(word)  #takes care of only adding unique words
            liUniqueWords.append({'words':setWords,'isSpam':self._blIsSpam})
        return pd.DataFrame(liUniqueWords)


# class container helper functions that operate collections of emails.
# Email data should be of one type only (ie.e either ham or spam)
class Emails:
    def __init(self):
        pass

    liEmails = []  # holds the list of email texts.
    dfSplitEmails = pd.DataFrame(columns=('header','body','filename'))# hold the header and body when seperated
    isSpam = False  #flag indicating if spam or not. Applied to all emails in dfSplitEmails.
    
    # Import emails
    # helper functions for file loading
    def blIsValid(self,strName):
        # Check that the file name conforms to the style we want.
        invalidChars = re.compile('[^\.0-9abcdef]')
        validFormat = re.compile('^\d{4,5}\.[0-9a-f]{10,}')
        if invalidChars.match(strName) is None and validFormat.match(strName):
            return True
        else:
            return False
        
    def getAllMessageFileNames(self,path=data_path):
        # recursively walk the message directory tree and get the fully qualified list of files.
        # return a dictionary with key=subdirectory name, 
        #      value is pandas DataFrame of unqualified file names (FQName) and unqualifies name 'Name')
        dicFileNames = {}
        pwd = os.getcwd()
        # if relative path specified get the the fully qualified name
        pwd = os.path.join(pwd,'Messages')
        if path==data_path:
            dirNames = os.listdir(pwd)    # get directories in the current dir (returns a list of dirs)
        else:
            dirNames = [path]           #else we have been passed a child directory so make a list of one element
            
        for dirName in dirNames:
            listFileNames = []
            listFQFileNames = []
            # compile list of names
            for fn in os.listdir(os.path.join(pwd,dirName)):
                if self.blIsValid(fn):  # targetin
                    listFQFileNames.append(os.path.join(pwd,dirName,fn))
                    listFileNames.append(os.path.join(dirName,fn))
            #convert to pandas dataframe
            df=pd.DataFrame({'FQNames':listFQFileNames,'Names':listFileNames})
            df=df.sort('Names')
            dicFileNames[dirName] = df
        return (dicFileNames)    
    
    # List the directories and file counts in each folder.
    def showMessageCounts(self,allFiles_dic):
        allFiles_dic = self.getAllMessageFileNames()
        for key in allFiles_dic.keys():
            print "dir={} file={}".format(key,len(allFiles_dic[key]['Names']))    

    def loadEmails(self,liFullDirectoryListing,liFileLocations=[]):
        # get some or all emails from the full list and return list of lists of email lines of text
        # return list of tuples consisting of (list of lines from email, file name)
        liEmails=[] #list of lists of text lines from each email
        if len(liFileLocations)>0:
            for i in liFileLocations: # is is row number not label
                #print liFullDirectoryListing['FQNames'].iloc[i]
                f=open(liFullDirectoryListing['FQNames'].iloc[i])
                lines=f.readlines()
                f.close()
                liEmails.append([map(lambda s: s.replace('\n','').replace('\t',''),lines),liFullDirectoryListing['Names'].iloc[i]])
        else:
            for i,myRow in liFullDirectoryListing.iterrows():   # is is label not row number
                f=open(myRow['FQNames'])
                lines=f.readlines()
                f.close()
                liEmails.append([map(lambda s: s.replace('\n','').replace('\t',''),lines),liFullDirectoryListing['Names'].loc[i]])            
        return liEmails   
   
    # Get the boundary string from headers in dfSplitEmails (via content-type key)
    # return a list of tuples containing row LABEL and boundary string where attachment is present)
    def getBoundaryStrings(self):
        liBoundaryKeys = []
        reContentKey = re.compile('^(content-type)\s*:\s*([A-Za-z\/]+);',re.I)
        reBoundaryKey = re.compile('.*(boundary)\s*=\s*([^;]*);?.*',re.I)
        for i,myRow in self.dfSplitEmails.iterrows(): # get index and row (series) seperately
            blHasAttachment=False
            strBoundaryKey=''
            try:
                for item in myRow['header']:
                    mContentKey = reContentKey.match(item)
                    mBoundaryKey = reBoundaryKey.match(item)
                    if mContentKey and "multi" in mContentKey.group(2).lower():
                        blHasAttachment=True
                    if mBoundaryKey:  
                        strBoundaryKey=mBoundaryKey.group(2).strip('" ')
            except:
                print "problem at i={}".format(i)
            if blHasAttachment:
                liBoundaryKeys.append([i,strBoundaryKey])
                if DEBUG_ATTACHMENTS:
                    print "File {}, Boundary={}".format(myRow['filename'],strBoundaryKey)
                    
        return liBoundaryKeys
                
    
    # Find the location of all occurances of the boundary string (from Content-Type key)
    # The number of occurences will be used as validation that matching pairs of boundary strings are present.
    def getBoundaryStringLocs(self,liBStrings):
        #loop through the emails that have attachments and get the line numbers where the boundary string appear.
        # note there should be an odd number of locations as the boundaries should appear in pairs plus there should be one the # #marks the start of the email
        # Returns a list of list tuples [Df index, [start locations],[end locations]]
        liLocations=[]
        for row in liBStrings:
            intLineCounter=0
            liStartBoundaryLocs = []
            liEndBoundaryLocs = []
            # start markers are preceeded by two -'s. Closing ones are preceeded and followed by two dashes.
            rxStartBoundary = re.compile("-{2}"+row[1])
            rxEndBoundary = re.compile("-{2}"+row[1]+"-{2}")
            for line in self.dfSplitEmails.loc[row[0]]['body']:
                if rxEndBoundary.search(line)!=None:
                    liEndBoundaryLocs.append(intLineCounter)   
                elif rxStartBoundary.search(line)!=None: 
                    liStartBoundaryLocs.append(intLineCounter)                    
                intLineCounter+=1
            liLocations.append([row[0],liStartBoundaryLocs,liEndBoundaryLocs,self.dfSplitEmails.loc[row[0]]['filename']])
       
        return liLocations

    #Remove the attachments that may be embedded in the body of the email.
    # Attachments marked by boundary strings.
    # operates on dfSplitEmails in place.
    # no return value
    def removeAttachments(self,liBoundaries):
        # @liBoundaries - list of tuples of lists. [ iloc in df,[list of start locations],[list of end loc]
        liEmailsToRemove = [] # iloc of emails to remove from dfSplitEmails.
        liTempEmails = []
        #print self.dfSplitEmails.head()
        for row in liBoundaries:
            # check that the pairing of the start end/markers make sense.
            # i.e there is an opening, a start and a closing occurence of the boundary string. 
            # If not, then the email should be discarded as we can't be sure the content is body of the email we care about. 
            if len(row[1])>=2 and len(row[2])>=1:
                intAttachStartLoc = row[1][1]
                intAttachEndLoc = row[2][-1]
                ##Learning: iLoc etc are select statements that create new objects and hence changes to new objects dont' propogate.
                ##Access object contents vi (reversed) indexation df[column][row index]
                ##Alternatively could use .xs method and set copy=False to prevent copy being created.
                del self.dfSplitEmails['body'][row[0]][intAttachStartLoc:(intAttachEndLoc+1)]
                del self.dfSplitEmails['body'][row[0]][row[1][0]]
            else:
                self.dfSplitEmails['body'][row[0]]=[] # delete the email. TODO ensure that empty emails are not included in further analysis.
                self.dfSplitEmails['header'][row[0]]=[]
                
            
    
    # Locate and remove attachments
    # Return a list of emails that should be dropped due to mismatched boundary strings.
    # Management function for removing attachements.
    def dealWithAttachments(self):
        if DEBUG_ATTACHMENTS:
            print "Start get boundary strings"
        liBStrings= self.getBoundaryStrings()
        if DEBUG_ATTACHMENTS:
            print liBStrings
            print "finished geting boundary strings"
            print "start locating boundary string in emails"
        liBStringLocs = self.getBoundaryStringLocs(liBStrings)
        if DEBUG_ATTACHMENTS:
            print liBStringLocs
            print "finished locating boundary strings"
            
        self.removeAttachments(liBStringLocs) 
        if DEBUG_ATTACHMENTS:
            print "Locating remaining boundary tags"
            for i,myRow in self.dfSplitEmails.iterrows():
                for lines in myRow['body']:
                    if 'boundary' in lines.lower():
                        print "index {}, filename {},\r\n text {} ".format(i,myRow['filename'],lines)
            print "End locating boundary tags"
            
    def _severHeader(self,tupText):
        # Get the seperator for header of a single email.
        # @liText is the lines of text for a single email.
        # Return a dictionary of two lists. The first containing the Header Lines list and the second containing the body lines list.
        loc = -1
        try:
            loc = tupText[0].index('')
        except:
            pass
        if loc!=-1 and loc>CONST_MIN_HEADER_SIZE: # if no split then throw the record away as its unusable
            return ({'header':list(tupText[0][0:loc]),'body':list(tupText[0][loc+1:len(tupText[0])]),'filename':tupText[1]})
        else:
            return ({'header':[],'body':[],'filename':''})

    def severHeaders(self):
        # liEmails is a list of list of email text lines.
        # sets pdSplitEmails, a dataframe with cols header and body and blIsSpam flag
        liSplitEmails = map(self._severHeader,self.liEmails)
        self.dfSplitEmails = pd.DataFrame(liSplitEmails)



# Class represents the Naive Bayes filter singleton.
class NaiveBayesFilter:
    def __init(self):
        pass
    
    #All emails pre-processed and ready for inputing into training alg,.
    _dfPreprocessedEmails = pd.DataFrame(columns=('words','isSpam'))
    
    #probability tables.
    dfProb = pd.DataFrame(columns=('words','pPres','pAbs'))
    liBOW = [] #Bag of words. A list of unique words in the whole corpus. A feature vector is a vector with one coordinate per  bow word.
    
    #The threshold for the LLR below which an email is considered spam.
    dThreshold = 0.0
    
    #totals
    _intTotalSpamEmails = 0
    _intTotalHamEmails = 0
    
    #Load get all the emails in the ./Message directory.
    #Deal with each Dir one at a time and tag emails as ham/spam
    #directoryList - list of relative directory names and correspondig tag isSpam.
    # indx a list of index labels to select emails to work with.
    def importParseCorpus(self, directoryList, indx=[]):
        liEmailDFs = [] # a list used to accumulate directories of emails before concatinating them.
        i =0
        for strDir in directoryList['dir']:
            eEmails = Emails()
            if DEBUG:
                print ("start load {}".format(strDir))
            #get file names in the current directory.
            allFiles_dic = eEmails.getAllMessageFileNames(directoryList['dir'][i])
            #eEmails.showMessageCounts(allFiles_dic)            
            
            if len(indx)>0:
                eEmails.liEmails = eEmails.loadEmails(allFiles_dic[directoryList['dir'][i]],indx)
            else:
                eEmails.liEmails = eEmails.loadEmails(allFiles_dic[directoryList['dir'][i]])
                
            blIsSpam = directoryList['isSpam'][i]
            
            # split header and body using the blank line delimiter that seperates them
            eEmails.severHeaders()
            #remove attachments from the bodies. Find the Content-Type key in the header, 
            eEmails.dealWithAttachments()
            #text pre-precessing 
            if DEBUG:
                print "Begin dealing with text preprocessing"
                t=time.time()
            
            Tp = TextProcessor()
            Tp.dfEmails = eEmails.dfSplitEmails
            Tp.setTag(blIsSpam)  
            Tp.toLower()
            Tp.removePunc()
            Tp.stem()
            #Extract the unique words from emails. Create a list of words for each email.
            dfWords = Tp.getUniqueWordsAndTag()
            #print len(dfWords)
            if DEBUG:
                print "Time to preprocess text {}".format(time.time()-t)
            liEmailDFs.append(Tp.getUniqueWordsAndTag(MAX_WORD_LENGTH)) ## add to df list until finished importing all emails.
            i+=1
            emailsAdded = len(liEmailDFs[-1])
            if blIsSpam:
                self._intTotalSpamEmails+=emailsAdded
            else:
                self._intTotalHamEmails+=emailsAdded
                
            if DEBUG:
                print "End loading {}. Emails processed {}".format(strDir,emailsAdded)
              
                       
        self._dfPreprocessedEmails = pd.concat(liEmailDFs)
        #print len(self._dfPreprocessedEmails)
        
    # calculate the dfProbability table based on the training data passed int.
    # flTrainingPercent is floating point value representing the portion of preprocessed data use for training. 
    # The balance to be used for testing. 
    # Returns a list of row numbers for _dfPreprocessed emails.
    def splitCorpus(self, flTrainingPercent):
     #Split the preprocessed data into training and test emails. 
        rand.seed(418910)
        totalEmails=self._intTotalSpamEmails+self._intTotalHamEmails
        trainHamIdx = rand.sample(range(0,self._intTotalHamEmails),int(math.floor(self._intTotalHamEmails*flTrainingPercent)))
        trainSpamIdx = rand.sample(range(self._intTotalHamEmails,totalEmails),int(math.floor(self._intTotalSpamEmails*flTrainingPercent)))

        trainIdx = trainHamIdx+trainSpamIdx
        setTestIdx= set(range(0,totalEmails))
        setTrainIdx = set(trainIdx)
        setTestIdx = setTestIdx-setTrainIdx
        testIdx = list(setTestIdx)

        if DEBUG:
            print "Length of training set {}: Length of test set {}".format(len(trainIdx),len(testIdx))
        
        return trainIdx,testIdx
    
    
          
         
         
    def trainFilter(self,trainIdx):
        # Method to create conditional probabilities tables for training data.
        # @trainIdx is a list of row numbers in dfPreprocessedEmails included inthe training set. 
        if DEBUG:
            print "Start Trainging Filter:"
        row = 0
        liUnlistedWords = [] # list of tuples (word from an email, isSpam bool)
        #unlist self.dfPreprocessedEmails which has rows structure of set of unique words,isspam bool
        if DEBUG:
            print "Start: Filter to training set"
            t=time.time()
        
        dfTrainingSet = self._dfPreprocessedEmails.iloc[trainIdx]
        
        if DEBUG:
            print "Time to filter:{}".format(time.time()-t)
            t=time.time()
        
        for i,anEmail in dfTrainingSet.iterrows():
            liWords=list(anEmail['words'])
            liFlag = list(itertools.repeat(anEmail['isSpam'],len(liWords)))
            liUnlistedWords[0:0]=zip(liWords,liFlag)
        
        if DEBUG:
            "time to unlist all emails unique word sets:{}".format(time.time()-t)
            t=time.time()
        
        # group by word to be able access the isspam counts.e
        dfUnlistedWords = pd.DataFrame(liUnlistedWords,columns=('words','isSpam')).groupby(('words','isSpam')).count()
      
        if DEBUG:
            print "Time to create counts={}".format(time.time()-t)

            
        #get total spam and ham counts
        intTotalSpamEmails= float(sum(dfTrainingSet['isSpam']))
        intTotalHamEmails = float(len(dfTrainingSet)-intTotalSpamEmails)
        
        liProbTables = [] #list of dictionaries to convert to df
        print "start calculating probabilities:"
        t=time.time()
        seriesProb = pd.Series([])
        
        for word in self.liBOW:
            #fall back values when word not present in andy spam or ham emails
            probPresSpam=0.5/intTotalSpamEmails
            probPresHam=0.5/intTotalHamEmails
            
            blIsInTrainingSet = False
            try:
                seriesProb = dfUnlistedWords['isSpam'][word]
                blIsInTrainingSet=True
            except:
                pass
            if blIsInTrainingSet:
                try:
                    probPresSpam =(float(seriesProb[True])+0.5)/(intTotalSpamEmails+0.5)
                except:
                    pass
                
                try:
                    probPresHam = (0.5+float(seriesProb[False]))/(intTotalHamEmails+0.5)
                except:
                    pass
            
                liProbTables.append((word,math.log(probPresHam)-math.log(probPresSpam),math.log(1.0-probPresHam)-math.log(1.0-probPresSpam)))
            
        if DEBUG:
            print "Time to calculate probabilities: {}".format(time.time()-t)
            
        ret= pd.DataFrame(liProbTables,columns=('word','pPres','pAbs'),index=self.liBOW)
       
        self.dfProb=ret
        return ret
    
    # Get bag of words.
    #if list specified then only process those row numbers (iloc)
    def createBOW(self,liIndecies=[]):
        #loop over _drPreprocessedEmails and get one consolidated list of unique words.
        setWords = set()
        t=time.time()
        if len(liIndecies)==0:
            for i,myRow in self._dfPreprocessedEmails.iterrows():
                if len(setWords)==0:
                    setWords = myRow['words']
                else:
                    setWords=setWords.union(myRow['words'])
                #print len(setWords)
        else:
            for i in liIndecies:
                if len(setWords)==0:
                    setWords=self._dfPreprocessedEmails.iloc[i]['words']
                else:
                    setWords=setWords.union(self._dfPreprocessedEmails.iloc[i]['words'])
        if DEBUG:
            print "time to creat BOW: {}".format(time.time()-t)
        self.liBOW=list(setWords)
        if DEBUG:
            print "BOW: Length of BOW {}".format(len(self.liBOW))
    
    def exportBOW(self):
        with open('BOW.csv','wb') as BOWFile:
            myFile = csv.writer(BOWFile,delimiter=',',quotechar='"')
            for line in self.liBOW:
                myFile.writerow([line])        


    # Calculate the log likelihood of the content
    # setLines collection of unique preprocessed words in the email
    def LLR(self,setWords):
        LLR=0.0
        t=time.time()
        
        # Method 1: for loop version. On 'ALL' on i5 Quad core took around 20sec's per LLR calc
            #for i, tProbs in self.dfProb.iterrows():
            #    if tProbs['word'] in setWords:
            #            LLR+=tProbs['pPres']
            #    else:
            #        LLR+=tProbs['pAbs']

        # Method 2: Use apply function on df. 
        # fLLRTerm = partial(self.isPres,setWords=setWords)  #create a partial with the setofwords from the test email passed in.
        # LLR = sum(self.dfProb.apply(fLLRTerm,axis=1))
        
        # Method 3: Try relying on indexing using the set of words. Also use netting of Total PAbs to calculate contribution of missing words.
        # Results: On an i5 quad core this method reduced the time to calc LLR from 20sec to 0.02seconds
        fTotalAbscent=self.dfProb['pAbs'].sum()
        fLLRPres = self.dfProb.loc[setWords,'pPres'].sum()
        fLLRAbs = self.dfProb.loc[setWords,'pAbs'].sum()
        LLR = fTotalAbscent -fLLRAbs+fLLRPres

        return LLR            

    # Classify the corpus of test emails.
    # Returns the results including the raw LL that can be used in further analysis.
    def evaluatePerformance(self, dfTest, threshold=40, samplePercent=1):
        testResults = []
        if samplePercent<1:
            sampleIdx = rand.sample(range(0,len(dfTest)),int(len(dfTest)*samplePercent))
            dfTest = dfTest.iloc[sampleIdx]                         
            
        for i,testEmail in dfTest.iterrows():
            if DEBUG:
                t=time.time()
            LLR = self.LLR(testEmail['words'])
            if DEBUG:
                procTime = time.time()-t
            if DEBUG:
                print "Spam? {}; LLR = {}; predict Spam?={} ProcTime={}".format(testEmail['isSpam'],LLR,LLR<threshold, procTime)
                testResults.append({'actual':testEmail['isSpam'],'predicted': LLR<threshold,'LLR':LLR,'processingTime':procTime})
            else:
                testResults.append({'actual':testEmail['isSpam'],'predicted': LLR<threshold,'LLR':LLR})
       
        return pd.DataFrame(testResults)

    # Display and return typical performance statistics.
    def printPerfStats(self,dfResults):

        accuracy = float(sum(dfResults['actual']==dfResults['predicted']))/float(len(dfTestResults))
        print "Accuracy {}".format(accuracy)
        # TPR,sensetivity,recall
        recall=float(sum((dfResults['actual']==True) & (dfResults['predicted']==True)))/float(sum(dfResults['actual']==True))
        print 'TPR/sensitivity/recall {}'.format(recall)
        #TNR, specificity
        specificity = float(sum((dfResults['actual']==False) & (dfResults['predicted']==False)))/float(sum(dfResults['actual']==False))
        print 'TNR/specificity {}'.format(specificity)
        precision = float(sum((dfResults['actual']==True) & (dfResults['predicted']==True)))/float(sum(dfResults['predicted']==True))
        print 'precision {}'.format(precision)
        f1=2.0*precision*recall/(precision+recall)
        print "F1 score {}".format(f1)
        
        return accuracy,recall,specificity,precision,f1

    
    #plot the recall performance vs threshold for range of thresholds
    def plotPerformance(self,dfData):
        
        plt.figure()
        dfData.plot()
        plt.ylabel('Rate or ratio(f1)')
        plt.title('Performance vs threshold value')
        plt.show()
        
    
    def exportPerformance(self,dfData):
        dfData.to_csv('./TrainingResults.csv', sep=",", na_rep='', float_format=None, cols=None, 
                      header=True, index=True, index_label=None, 
                      mode='w', nanRep=None, encoding=None, 
                      quoting=None, line_terminator='\n', 
                      chunksize=None, tupleize_cols=False, 
                      date_format=None)  
        
        
    # Used to create partial object (curried function) for method 2
    def isPres(self,sRow,setWords):
        if sRow['word'] in setWords:
            return sRow['pPres']
        else:
            return sRow['pAbs']


    
######## Main ###################################

SCENARIO="ALL"


#Get all the file names for the message corpus

# list of indecies of training emails to use while creating algorithm
if SCENARIO=='ALL':
    indx=[]
    directoryList = {'dir':['easy_ham','easy_ham_2','hard_ham','spam','spam_2'],'isSpam':[False,False,False,True,True]}    
elif SCENARIO=='SMALL_BATCH':
    indx=[0,1,2,3,4,14,11,12,16,17,27,30,40,50,100,101,102,26,67,68,328,403]
    directoryList = {'dir':['easy_ham','spam_2'],'isSpam':[False,True]}

# Create Naive Bayes Instance
NB=NaiveBayesFilter()
# Load the emails and preprocess them to produce a list of unque word sets per email.
print "Import Emails.."
NB.importParseCorpus(directoryList,indx)  # get jsut the subset for developing with. 
#get the list of training vs testing data sets. 
print "Split Emails into Training and Test sets.."
trainIdx,testIdx = NB.splitCorpus(0.66)
print "Create Bag of Words.."
NB.createBOW(trainIdx)
NB.exportBOW()
print "Train Filters.."
NB.trainFilter(trainIdx)
# Run tests

print "Evaluate performance on test email set.."

liResults = []  # store all performance metrics
dfTestResults = NB.evaluatePerformance(NB._dfPreprocessedEmails.iloc[testIdx],0,1) # returns dataframe with columns IsSpam,Predicted Spam, LLR

# Iterate of the threshold values and find where the Type 1 errors are minimised.
for i in range(0,80,1):
    print "Threshhold={}".format(i)
    dfTestResults['predicted']=pd.Series(dfTestResults['LLR']<i)
    accuracy,recall,specificity,precision,f1 = NB.printPerfStats(dfTestResults)
    liResults.append({'threshold':i,'acc':accuracy,'sens.':recall,'spec':specificity,'prec':precision,'f1':f1})

results = pd.DataFrame(liResults)
results2 = results.drop('threshold',axis=1)
results2.index.name='Threshold'
NB.plotPerformance(results2)




    
    
    