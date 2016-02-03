import unicodecsv as unicsv
import numpy as np
import pandas as pd
import re

import os # misc OS functions

## Globals
data_path="./Messages"

# helper functions for file loading
def blIsValid(strName):
    # Check that the file name conforms to the style we want.
    invalidChars = re.compile('[^\.0-9abcdef]')
    validFormat = re.compile('^\d{4,5}\.[0-9a-f]{10,}')
    if not invalidChars.match(strName) and validFormat.match(strName):
        return True
    else:
        return False
    
def getAllMessageFileNames(path='./Messages'):
    # recursively walk the message directory tree and get the fully qualified list of files.
    # return a dictionary with key=subdirectory name, 
    #      value is pandas DataFrame of unqualified file names (FQName) and unqualifies name 'Name')
    dicFileNames = {}
    pwd = os.getcwd()
    if path[0]=='.':
        pwd = pwd+path.lstrip('.')
    dirNames = os.listdir(pwd)    
    for dirName in dirNames:
        listFileNames = []
        listFQFileNames = []
        # compile list of names
        for fn in os.listdir(os.path.join(pwd,dirName)):
            if blIsValid(fn):  # targetin
                listFQFileNames.append(os.path.join(pwd,dirName,fn))
                listFileNames.append(fn)
        #convert to pandas dataframe
        df=pd.DataFrame({'FQNames':listFQFileNames,'Names':listFileNames})
        df=df.sort('Names')
        dicFileNames[dirName] = df
    return (dicFileNames)

# List the directories and file counts in each folder.
def showMessageCounts():
    allFiles_dic = getAllMessageFileNames()
    for key in allFiles_dic.keys():
        print "dir={} file={}".format(key,len(allFiles_dic[key]['Names']))



# TODO: Find python package for text preprocessing.
# preprocessing methods and word extraction from a single email.
class TextProcessing:
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
        for myRow in self.dfEmails.itertuples():
            #myrow[0]=index value NOT row number; myrow[1]=header myrow[2]=body
            liNewLowerLines = map(lambda x: x.lower(),myRow[2])
            self.dfEmails.loc[myRow[0]]['body']=liNewLowerLines
    
    #remove punctuation and other XML chars.
    def removePunc(self):
        for myRow in self.dfEmails.itertuples():
            liNewLines = [re.sub(self._rxIllegalChars,' ',line) for line in myRow[2]]
            self.dfEmails.loc[myRow[0]]['body']=liNewLines
    
    #basic stemming. Remove ed,ing, ly
    #operates on dfEmail in place
    #assumes punctuation removed.
    def stem(self):
        rxED = '(ed)[\s\b]'
        rxING = '(ing)[\s\b]'
        rxLY = '(ly)[\s\b]'

        for myRow in self.dfEmails.itertuples():
                liNewLines = [re.sub(rxED,'',line) for line in myRow[2]]
                liNewLines = [re.sub(rxING,'',line) for line in liNewLines]
                liNewLines = [re.sub(rxLY,'',line) for line in liNewLines]
                self.dfEmails.loc[myRow[0]]['body']=liNewLines        
    
    
    #get a list of unique words occuring in a single email.
    # remove stop words
    #returns dataframe rows=email,columns=uniquewords, isSpam boolean
    def getUniqueWords(self):
        dfUniqueWords = pd.DataFrame(columns=['words','isSpam'])
        for myRow in self.dfEmails.itertuples():
            setWords = set()
            for line in myRow[2]:
                for word in line.split(' '):
                    if len(word)>1 and word!=' ' and  not (word in self._stopWords):
                        setWords.add(word)  #takes care of only adding unique words
            dfUniqueWords.add({'words':setWords,'isSpam':self._blIsSpam})
        return dfUniqueWords



# class container helper functions that operate collections of emails.
class Emails:
    def __init(self):
        pass

    liEmails = []  # holds the list of email texts.
    dfSplitEmails = pd.DataFrame(columns=('header','body'))# hold the header and body when seperated
    
    def subsetEmails(self,liFullDirectoryListing,liFileLocations):
        # get a sample of emails from the full list and return list of lists of email lines of text
        liEmails=[] #list of lists of text lines from each email
        for i in liFileLocations:
            f=open(liFullDirectoryListing['FQNames'].iloc[i])
            lines=f.readlines()
            f.close()
            liEmails.append(map(lambda s: s.replace('\n',''),lines))
        return liEmails   

    def removeEmailsByiLoc(self,liEmailsToDrop):
        self.dfSplitEmails = self.dfSplitEmails.drop(liEmailsToDrop)
   
    # Get the boundary string from headers in dfSplitEmails (via content-type key)
    # return a list of tuples containing rownumber and boundary string where attachment is present)
    def getBoundaryStrings(self):
        liBoundaryKeys = []
        reContentKey = re.compile('^(content-type)\s*:\s*([A-Za-z\/]+);',re.I)
        reBoundaryKey = re.compile('.*(boundary)\s*=\s*([^;]+);?.*',re.I)
        for i,myRow in self.dfSplitEmails.iterrows(): # get index and row (series) seperately
            blHasAttachment=False
            strBoundaryKey=''
            for item in myRow['header']:
                mContentKey = reContentKey.match(item)
                mBoundaryKey = reBoundaryKey.match(item)
                if mContentKey and "multi" in mContentKey.group(2).lower():
                    blHasAttachment=True
                if mBoundaryKey:  
                    strBoundaryKey=mBoundaryKey.group(2).strip('" ')
            if blHasAttachment:
                liBoundaryKeys.append([i,strBoundaryKey])
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
            for line in self.dfSplitEmails.iloc[row[0]]['body']:
                if rxEndBoundary.search(line)!=None:
                    liEndBoundaryLocs.append(intLineCounter)   
                elif rxStartBoundary.search(line)!=None: 
                    liStartBoundaryLocs.append(intLineCounter)                    
                intLineCounter+=1
            liLocations.append([row[0],liStartBoundaryLocs,liEndBoundaryLocs])
        return liLocations

    #Remove the attachments that may be embedded in the body of the email.
    # Attachments marked by boundary strings.
    # operates on dfSplitEmails in place.
    # returns list of emails that have invalid boundarys and need to be discarded.
    def removeAttachments(self,liBoundaries):
        # @liBoundaries - list of tuples of lists. [ iloc in df,[list of start locations],[list of end loc]
        liEmailsToRemove = [] # iloc of emails to remove from dfSplitEmails.
        for row in liBoundaries:
            # check that the pairing of the start end/markers make sense.
            # i.e there is an opening, a start and a closing occurence of the boundary string. 
            # If not, then the email should be discarded as we can't be sure the content is body of the email we care about. 
            if len(row[1])>=2 and len(row[2])>=1:
                intAttachStartLoc = row[1][1]
                intAttachEndLoc = row[2][-1]
                liTempEmailBody = self.dfSplitEmails.iloc[row[0]]['body'][row[1][0]+1:intAttachStartLoc]
                liTempEmailBody.append(self.dfSplitEmails.iloc[row[0]]['body'][intAttachEndLoc+1:])
                self.dfSplitEmails.iloc[row[0]]['body']=liTempEmailBody
            else:
                liEmailsToRemove.append(row[0])
        return liEmailsToRemove
    
    # Locate and remove attachments
    # Return a list of emails that should be dropped due to mismatched boundary strings.
    # Management function for removing attachements.
    def dealWithAttachments(self):
        liBStrings= self.getBoundaryStrings()
        liBStringLocs = self.getBoundaryStringLocs(liBStrings)
        liEmailsToDrop = self.removeAttachments(liBStringLocs)        
        return liEmailsToDrop
    
        
    def _severHeader(self,liText):
        # Get the seperator for header of a single email.
        # @liText is the lines of text for a single email.
        # Return a dictionary of two lists. The first containing the Header Lines list and the second containing the body lines list.
        loc = -1
        try:
            loc = liText.index('')
        except:
            pass
        if loc!=-1: # if no split then throw the record away as its unusable
            return ({'header':liText[0:loc],'body':liText[loc+1:len(liText)]})

    def severHeaders(self):
        # liEmails is a list of list of email text lines.
        # sets pdSplitEmails, a dataframe with cols header and body and blIsSpam flag
        liSplitEmails = map(self._severHeader,self.liEmails)
        self.dfSplitEmails = pd.DataFrame(liSplitEmails)
    
######## Main ###################################

#Get all the file names for the message corpus

allFiles_dic = getAllMessageFileNames()
showMessageCounts()

Train = Emails() ## Object to hold emails while preparing them.

# list of indecies of training emails to use while creating algorithm
indx = [0,1,2,3,4,14,26,67,68,328,403,426,515,851,970]
# get the sample emails from easy_ham folder
Train.liEmails = Train.subsetEmails(allFiles_dic['easy_ham'],indx)

# split header and body using the blank line delimiter that seperates them
Train.severHeaders()

#remove attachments from the bodies. Find the Content-Type key in the header, 
#extract the boundary stringa and then find the boundary strings in the text. 
liEmailsToDrop = Train.dealWithAttachments()
Train.removeEmailsByiLoc(liEmailsToDrop)
#text pre-porcessing 

Tp = TextProcessing()
Tp.dfEmails = Train.dfSplitEmails
Tp.setTag(False)   # not spame
Tp.toLower()
Tp.removePunc()
Tp.stem()
dfUWords = Tp.getUniqueWords()
#Extract the unique words from emails. Create a list of words for each email.

#Add spam/ham tags to the email word lists for use in calculating probabilities.



