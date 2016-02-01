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
    def __intin(self):
        pass
    
    # set all text to lower case
    def toLower(self,liStrLines):
        pass
    
    #remove punctuation and other XML chars.
    def removePunc(self,liStrLines):
        pass
    
    #basic stemming. Remove es,ing,
    def stem(self,liStrLines):
        pass

    #remove stop words
    def removeStopWords(self,liStrLines):
        pass
    
    #get a list of unique words occuring in a single email.
    def getUniqueWords(self,liStrLines):
        pass


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
                
    
    # find the location of all occurances of the boundary string (from Content-Type key)
    # the number of occurences will be used as validation that matching pairs of boundary strings are present.
    def getBoundaryStringLocs(self,liBStrings):
        #loop through the emails that have attachments and get the line numbers where the boundary string appear.
        # note there should be an odd number of locations as the boundaries should appear in pairs plus there should be one the # #marks the start of the email
        liLocations=[]
        for row in liBStrings:
            intLineCounter=0
            liBoundaryLocs = []
            rxBoundary = re.compile(row[1])
            for line in self.dfSplitEmails.iloc[row[0]]['body']:
                if rxBoundary.search(line)!=None:
                    liBoundaryLocs.append(intLineCounter)
                intLineCounter+=1
            liLocations.append([row[0],liBoundaryLocs])
        return liLocations

    def blHasAttachment(self,dfEmails):
        pass
    
    def removeAttachments(self,dfEmails):
        pass
    
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
            return ({'header':liText[0:loc-1],'body':liText[loc+1:len(liText)]})

    def severHeaders(self):
        # liEmails is a list of list of email text lines.
        # sets pdSplitEmails, a dataframe with cols header and body and blIsSpam flag
        liSplitEmails = map(self._severHeader,self.liEmails)
        self.dfSplitEmails = pd.DataFrame(liSplitEmails)
    
### Main###################################

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
liBStrings= Train.getBoundaryStrings()
a = Train.getBoundaryStringLocs(liBStrings)
print a
#remove attachments from the bodies. Find the Content-Type key in the header, 
#extract the boundary stringa and then find the boundary strings in the text. 

#text pre-porcessing 

#Extract the unique words from emails. Create a list of words for each email.

#Add spam/ham tags to the email word lists for use in calculating probabilities.



