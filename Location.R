setwd('~/Documents/GitRepos/RRepos')

library(codetools)
library(lattice)
library(fields)


### Miscellaneous data and functions ###################
numNames = c("time","posx","posy","posz","orientation","signal")

#quantise orientation to remove random error in this measurement.
roundOrientation = function(x)
{
  refs = seq(0,by=45,length=9)
  q=sapply(x,function(o) which.min(abs(o-refs)))
  c(refs[1:8],0)[q]
}

#####################################################################
## Read data and do some basic cleaning up and class conversion and angle quantisation
#####################################################################
readData = function(fileName)
{
# Load the data from plain txt file
  txtData = readLines(fileName)

# Parse a single line of the text file into a matrix with the columns orders with label followed immediately by the relevant data.
## returns that output of the last operation executed ( just like a SQL stored proc) [if return not explicitly entered]
  processLine = function(x)
  {
    tokens = strsplit(x,"[;=,]")[[1]]
    if (length(tokens)==10)
      return(NULL)
    tmp=matrix(tokens[-(1:10)], ncol=4,byrow=TRUE)
    cbind(matrix(tokens[c(2,4,6:8,10)], nrow=nrow(tmp),ncol=6,byrow=TRUE),tmp)
  }
  
  txtLines = txtData[substr(txtData,1,1)!="#"]          # remove comment lines
  tmp = lapply(txtLines,processLine)   #returns a list of matrices where the type of each element is chr (all elements of matrices must be same type)
  
  offline = as.data.frame(do.call("rbind",tmp),stringsAsFactors=FALSE) #convert matrix of chrs to dataframe so we can change the types of the entries
  names(offline)=c("time","scanMac","posx","posy","posz","orientation","mac","signal","channel","type")
  offline[numNames]=lapply(offline[numNames],as.numeric)  #offline is a dataframe. lapply coerces the data frame into a list of vectors??

# Ret only the access point readings () and drop the type column
  offline = offline[offline$type==3,]
  offline$type=NULL
  
# reformat time from millseconds to seconds from 1/1/1970 (POSIXt)
  offline$rawtime=offline$time
  offline$time= offline$rawtime/1000;
  class(offline$time)=c("POSIXt","POSIXct")
  

  offline$angle=roundOrientation(offline$orientation)
  
#return offline
offline  
  }

#### Start Data Preparation###############################################

offline  = readData("Data/offline.final.trace.txt")

# Check the types of each ID in offline DF
  #print ("check: Types of each ID in DF")
  #unlist(lapply(offline,class))

# Count the unique values in the vector passed in to determine if any have no variance ( can be removed)
## Need to be aware of the type of objected being passed in. Note a dataframe is a wrapper for a list of vectors.
countUnique = function(x)
{
  c(names(x),length(unique(x)))
}

print ("check:show number of unique entries in each ID")
  unlist(lapply(offline,countUnique))    #recall that a data.frame is a wrapper around a list of vectors. lapply iterates over the column vectors and applies the passed in function
  summary(offline[numNames])

#explore orientation variable
    plot(ecdf(offline$orientation))

# Exploring the mac addresses
    testMacs = names(sort(table(offline$mac),decreasing=TRUE)[1:7])
    offline=offline[offline$mac %in% testMacs,]
    # confirm that the high frequency mac addresses have been preserved. Note that this will contain a mac address of the device not in the test area but appers to have useful data.
    table(offline[offline$mac %in% testMacs,]["mac"])
    # explore mac and channel relationship. Why? if there is 1-1 relationship then channel is not useful (and may impeed regression models)
    tMacChan= with(offline, table(mac,channel))
    offline$channel=NULL #remove channel as its redundant

# review the location data and reconcile with the documentation
# ..split the offline df into a list of subsets partitioned by the list of factors list(posx,posy). Results is a list of data frames.
    locDF=with(offline,by(offline,list(posx,posy),function(x) x))
    print ("number of null subsets based on position")
    sum(sapply(locDF,is.null))  # sapply returns type is a vector,matrix (data type storign a homogeneous type) so that sum can be applied (operates on a vector)
    # remove the null ones as NULL will get in the way of co-ercing the list into a data frame.
    locDF = locDF[!sapply(locDF,is.null)]
  
    print ("assert length of DF is 166")
    length(locDF)

#plot the reading coutns
    locCounts = sapply(locDF, function(x) c(x[1, c("posx","posy")],count=nrow(x)))   #returns a matrix (lapply would coerce the result into a list)
    
    locCounts = t(locCounts)  # transposeso data is in rows
    plot(locCounts,type="n",xlab="",ylab="")
    text(locCounts,labels=locCounts[,3],cex=0.5, srt=45)

## Plot various views of samples of the data. 
bwplot(signal~factor(angle) | mac, data = offline, subset = posx==2 & posy==12 &mac!="00:0f:a3:39:dd:cd",layout=c(2,3),xlab="Orientation (deg)",ylab="sign strength in dBm")
densityplot(~ signal | mac+factor(angle), data=offline,subset=posx==24 &posy==4 & mac!="00:0f:a3:39:dd:cd",bw=0.5,plot.points=FALSE)
# how many points are there in each density plot? -> c 110.
with(offline, sum(angle==315&mac=="00:0f:a3:39:e1:c0"&posx==24&posy==4))

##### visualisations of summary data for all location/angle/mac address.
# because of the large number of unique combinations of these tuples we will jsut display means and sd.

# add unique position descriptoins
  offline$posXY = paste(offline$posx,offline$posy, sep="-")
  # split DF into list of DF's, one per descriptor tuple.
  byLocAngleAP = with(offline,by(offline,list(posXY,angle,mac),function(x) x))
  print ("The number of data tuples:")
  length(byLocAngleAP)

# prep: gather the summary statistics into new list.
  signalSummary = lapply(byLocAngleAP,
                         function(x){
                           ans=x[1,]        #get the first row of the data frame.
                           ans$medSignal = median(x$signal)
                           ans$avSignal = mean(x$signal)
                           ans$numSignal = length(x$signal)
                           ans$sdSignal = sd(x$signal)
                           ans$iqrSignal = IQR(x$signal)
                           ans
                         })
  
  dfSummary= do.call("rbind",signalSummary)
#various plots: bucketised signal
  breaks = seq(-90,-25,by=5)
  bwplot(sdSignal ~ cut(avSignal,breaks=breaks),
         data=dfSummary,
         subset=mac!="00:0f:a3:dd:cd",
         xlab = "Mean Signal",ylab="SD Signal")
# skewness as function of number of readings.
  with(dfSummary,smoothScatter((avSignal-medSignal)~numSignal,
              xlab="Number of observations",
            ylab="skewness = mean-median"))
  abline(h=0,col="#984ea3", lwd=2)
# plot the mean of the skew vs signal counts to see if skew is an artifact of reading number variations. 
  lo.obj= with(dfSummary, loess(diff~numSignal, data=data.frame(diff=(avSignal-medSignal),numSignal=numSignal)))
  lo.obj.pr = predict(lo.obj,newdata=data.frame(numSignal=(70:120)))
  lines(x=(70:120),y=lo.obj.pr,col="#4daf4a",lwd=2)
#OBSERVATIONS: skew near zero for all observation counts.
  
  
## Signal and distance exploration
  subMac = names(sort(table(offline$mac),decreasing=TRUE)[1:7])

  smoothSS = function(macDisplay, myAngle,data){
    #exploration for only a single AP and angle.
    oneAPAngle = subset(data,mac==macDisplay & angle==myAngle)
    ## create topological map
    # create the surface model
    smoothSS = Tps(oneAPAngle[,c("posx","posy")],oneAPAngle$avSignal)
    #predict the surface 
    vizSmooth = predictSurface(smoothSS)
    plot.surface(vizSmooth,type="C",xlab=macDisplay)
    points(oneAPAngle$posx,oneAPAngle$posy,pch=19,cex=0.5)
  } 
  #set plot parameters to display a 2x2 grid with margins reduced to 1.
  #parCur = par(mfrow=c(2,2),mar=rep(1,4))
  ##mapply(smoothSS,macDisplay=subMac[rep(c(5,1),each=2)],angle=rep(c(0,90),2),data=list(data=dfSummary))
  #mapply(smoothSS,macDisplay=subMac[rep(c(5,1),each=2)],myAngle=rep(c(0,135),2),data=list(data=dfSummary))

  #Set plot params to display a 3 x 3 grid.Set margins to 4 so that the xlab can be displayed.
  #plot angle zero charts to help associate the mac with the location of the AP in the experiment documentation.
  parCur = par(mfrow=c(3,3),mar=rep(4,4))
  mapply(smoothSS,macDisplay=subMac[1:7],myAngle=rep(0,7),data=list(data=dfSummary))
  par(parCur)  
  # OBSERVATION: subMac[1] and subMac[2] appear almost co-located. But there should only be one. Possible that
  #              one is a corruption of the other, or they are subsets of one larger set. 
  ##counting the number of observations doesn't reveal and discrepancies. 
  #sum(dfSummary$mac=="00:0f:a3:39:e1:c0" & dfSummary$angle==0)
  #Hypothesis. Mac device was replaced and experiment repeated. Perhaps suspicious of malfunction???
  
  #drop data for subMac[2]
  dfSummary = subset(dfSummary, mac!=subMac[2])
  
  AP = matrix(c(7.5,6.3,2.5,-0.8,12.8,-2.8,1,14,33.5,9.3,33.5,2.8),
                ncol=2,
                byrow=TRUE,
                dimnames=list(subMac[-2],c("x","y")))
  
  
  ## take co-ordinate diffs. Note that the element wise calculations uses dfSummary$mac to line up corresponding rows 
  # results returned preserves the order of dfSummary (first operand) and so can be used to append new variable to dfSummary
  diffs = dfSummary[,c("posx","posy")]-AP[dfSummary$mac,]
  dfSummary$dist = sqrt(diffs[,1]^2+diffs[,2]^2)
  xyplot(signal~dist | factor(mac)+factor(angle),
         data = dfSummary,pch=19,cex=0.3,xlab="distance")
  distData = with(dfSummary,by(dfSummary,list(factor(mac),factor(angle)),function(x) x))
  
  # @para n int.
  regressionPlots = function(n){
    plot(signal~dist,data=distData[[n]],pch=19,cex=0.3,xlab="distance")
    mod = lm(signal~dist,data=distData[[n]])
    abline(mod)
  }
  cur=par(mfrow=c(4,4))
  sapply(seq(1:16),regressionPlots)
  #summary(mod)
  par(cur)  
   
  ####### Test data prep
  
  macs = unique(dfSummary$mac)
  online = readData("Data/online.final.trace.txt")
  online = subset(online,mac %in% macs)
  # create X-Y coord factor
  online$posXY = paste(online$posx,online$posy,sep="-")
  
  tabonlineXYA = table(online$posXY,online$angle)
  tabonlineXYA[1:6,]
  
  dfOnlineLocData = with(online, by(online, posXY,function(x) x))
                         
  onlinePlotCoord = sapply(dfOnlineLocData, function(x) c(x[1,c("posx","posy")],count=nrow(x)))
  onlinePlotCoord = t(onlinePlotCoord)
  par(mfrow=c(1,1))
  ##plot the locations and show the number of observations. 
  plot(onlinePlotCoord,type='n')
  text(onlinePlotCoord,labels = onlinePlotCoord[,3])
  
  ##Prepare the test data for use in the prediction model to be created later.
  # for each location collect the average reading at each Access Point. This will be the R^6 feature vector used 
  # to predict the location based on the training set. 
  
  keepVars = c("posXY","posx","posy","orientation","angle")
  byLoc = with(online,
               by(online,list(posXY),
                  function(x){
                    ans=x[1,keepVars]
                    avgSS = tapply(x$signal,x$mac,mean)
                    y = matrix(avgSS,nrow=1,ncol=6,
                               dimnames = list(ans$posXY,names(avgSS)))
                    cbind(ans,y)
                  }))
  
  onlineSummary = do.call("rbind",byLoc)
  
  ## Create function to extract the training subset from the full training set.
  # @angleNewObs = angle of the new observation
  # @m = number of closest 45 degree angles to include
  # @signals - the full dataset to subset
  # @varSignals = name of the variable in the signals data set to average.
  # returns the subsetted and aggregated and formatted dataframe.
  
  getTrainingSubset = function(angleNewObs,m,signals, varSignal = "avSignal"){
    refs = seq(0, by=45, length=8)
    nearestAngle = roundOrientation(angleNewObs)
    
    if (m%%2==1){
      angles = seq(-45*(m-1)/2,45*(m-1)/2,length=m)
    } else {
      m=m+1
      angles = seq(-45*(m-1)/2,45*(m-1)/2,length=m)
      if (sign(angleNewObs - nearestAngle)>0)
        angles = angles[-1]
      else
        angles = angles[-m]
    }
    
    # get the subset of the training set
    angles=angles+nearestAngle  # get the sequence of angles to include in the search for nearest neighbours
    angles[angles<0] = angles[angles<0]+360    
    angles[angles>360] = angles[angles>360]-360
    
    offlineSubset = signals[signals$angle %in% angles, ]
    keepVars = c("posXY","posx","posy")
    
    #calculate the means
    byLocation = 
      with(offlineSubset, by(offlineSubset, list(posXY),
                             function(x){
                               ans=x[1,keepVars]
                               avgSS= tapply(x[,varSignal ],x$mac, mean)
                               y=matrix(avgSS,nrow=1,ncol=6,dimnames=list(ans$posXY,names(avgSS)))
                               cbind(ans,y)
                             }))
    
    newDataSS = do.call("rbind",byLocation)
    return(newDataSS)
  }
  
  ##Sample of how to return the subsetted training set.
  ##trainnSS = getTrainingSubset(130,3,dfSummary)
  
  
  ##Find the single nearest neighbour to the passed in R^6 vector of readings.
  findNN = function(newSignal, trainSubset){
    diffs = apply(trainSubset[,4:9],1,function(x)x-newSignal)
    dists = apply(diffs,2,function(x)sqrt(sum(x^2)))
    closest = order(dists)
    return (trainSubset[closest,1:3])
  }
  
  # @k = number of nearest neighbours to base prediction on.
  predXY = function(newSignals, newAngles, trainData, numAngles=1,k=3){
    # init a list. One item for each test signal passed in.
    closeXY = list(length = nrow(newSignals))
    
    #get the NN for each test based on 3 close angles.
    for (i in 1:nrow(newSignals)){
      trainSS = getTrainingSubset(newAngles[i],numAngles,trainData)
      closeXY[[i]]=findNN(newSignal=as.numeric(newSignals[i, ]),trainSS)
    }
    # take the averages over each coordinate.
    estXY=lapply(closeXY,function(x) sapply(x[,2:3],function(x) mean (x[1:k])))
    estXY = do.call("rbind",estXY)
    return (estXY)
  }
  
  estXYK3 = predXY(newSignals = onlineSummary[,6:11],
                   newAngles = onlineSummary[,4],
                   dfSummary,numAngles=1,k=3)
  
  #Visualise the errors
  par(new=FALSE) # cause clearing the screen before plotting the next chart
  plot(dfSummary$posx,dfSummary$posy,'p',cex=0.3,col='blue',xlim=c(0,30),ylim=c(0,15),main="Plot of Test Readings vs NN Estimates")
  par(new=TRUE)
  errorData = cbind(onlineSummary$posx,onlineSummary$posy,estXYK3[,1],estXYK3[,2])
  for (i in 1:nrow(errorData)){
    m=t(matrix(errorData[i,],ncol=2))
    plot(m[1,1],m[1,2],'p',cex=0.8,xlim=c(0,30),ylim=c(0,15),xlab='',ylab='')
    par(new=TRUE)
    plot(m[2,1],m[2,2],'p',cex=0.3,xlim=c(0,30),ylim=c(0,15),xlab='',ylab='')
    par(new=TRUE)
    plot(m[,1],m[,2],'l',xlim=c(0,30),ylim=c(0,15),xlab='',ylab='')
    par(new=TRUE)
  }
  
  
  
  
  