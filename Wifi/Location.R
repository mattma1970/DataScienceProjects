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


## Read data and do some basic cleaning up and class conversion and angle quantisation

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

#### Start Data Preparation ###############################################

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
  
  
######  Signal and distance exploration. 
  subMac = names(sort(table(offline$mac),decreasing=TRUE)[1:7])

  ## create a thin plate spline model using Tps from 'fields' library.
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
  # @blSampleAngle = flag indicating that instead of subsetting based on a collection of near angles we will randomly select one angle only.
  #                   This feature used in cross validation to select the CV subset.
  # returns the subsetted and aggregated and formatted dataframe.
  
  getTrainingSubset = function(angleNewObs=0,m=1,signals, varSignal = "avSignal", blSampleAngle =FALSE){
    refs = seq(0, by=45, length=8)
    
    if (!blSampleAngle){
      # get the list of angles in 45 increments that flank the angleNewObs value passed in.
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
    
    # modulo 360 degrees
    angles=angles+nearestAngle  # get the sequence of angles to include in the search for nearest neighbours
    angles[angles<0] = angles[angles<0]+360    
    angles[angles>360] = angles[angles>360]-360
    
    # get the subset of the training set based on the angles list.
    offlineSubset = signals[signals$angle %in% angles, ]
    keepVars = c("posXY","posx","posy")
    }
    else
    { # cross validation branch. Select the single angle from the signals (mock of online data derived during CV)
      offlineSubset = signals[signals$angle == sample(refs,size=1),]
      keepVars = c("posXY","posx","posy","orientation","angle")
    }
    
    #calculate the means and constitute the reshaped data frame with jsut the fields of interest.
    byLocation = 
      with(offlineSubset, by(offlineSubset, list(posXY),
                             function(x){
                               ans=x[1,keepVars]
                               avgSS= tapply(x[,varSignal ],x$mac, mean)
                               y=matrix(avgSS,nrow=1,ncol=6,dimnames=list(ans$posXY,names(avgSS)))
                               cbind(ans,y)
                             }))
    
    newDataSS = do.call("rbind",byLocation)  # return the data.frame (note - not matrix because matrices can only have a single data type)
    return(newDataSS)
  }
  
  ##Find the single nearest neighbour to the passed in R^6 vector of readings.
  findNN = function(newSignal, trainSubset){
    diffs = apply(trainSubset[,4:9],1,function(x)x-newSignal)  # apply iterates over margin set by second para (1=rows,2=cols)
    dists = apply(diffs,2,function(x)sqrt(sum(x^2))) # calculate the sum of squared differences.
    closest = order(dists) #sort in ascending order.
    return (trainSubset[closest,1:3])
  }
  
  ## NN prediction of each row based on the k neighbour average and numAngle subet.
  ## Not optimised for Cross validation.
  # @k = number of nearest neighbours to base prediction on.
  # @ret = data.frame containing the NN estimate
  predXY = function(newSignals, newAngles, trainData, numAngles=1,k=3){
    # init a list. One item for each test signal passed in.
    closeXY = list(length = nrow(newSignals))
    
    #get the NN for each test based on 3 close angles.
    for (i in 1:nrow(newSignals)){
      trainSS = getTrainingSubset(newAngles[i],numAngles,trainData)
      closeXY[[i]]=findNN(newSignal=as.numeric(newSignals[i, ]),trainSS)  ## get the k nearest neighbours.
    }
    # take the averages over each coordinate of the neighbours.
    estXY=lapply(closeXY,function(x) sapply(x[,2:3],function(x) mean (x[1:k])))
    estXY = do.call("rbind",estXY)
    return (estXY)
  }
  
  ## Optimised verison of the predXY function used cumsum /(1:k) to calculate all 1:k NN averages in one pass
  # @k is the maximum number of nearest neighbour averages to calculate.
  # @ ret = list of matrices whose length is the length of newSignals and each matrix has k rows, one for each value of K.
  predXY_Optimised = function(newSignals, newAngles, trainData, numAngles=1,k=3){
    # init a list. One item for each test signal passed in.
    closeXY = list(length = nrow(newSignals))
    #get the NN for each test based on 3 close angles.
    for (i in 1:nrow(newSignals)){
      trainSS = getTrainingSubset(newAngles[i],numAngles,trainData)
      closeXY[[i]]=findNN(newSignal=as.numeric(newSignals[i, ]),trainSS)
    }
    # take the averages over each coordinate of the neighbours for each of the NN counts 1:k
    # returns a list of matrices where each row of the matrix corresponds to one value of k.
    list_estXY=lapply(closeXY,function(x) sapply(x[,2:3],function(x) t(cumsum(x[1:k])/(1:k))))
    return (list_estXY)
  }
  
  # Test: predict and check error for k=3
  estXYK3 = predXY(newSignals = onlineSummary[,6:11],
                   newAngles = onlineSummary[,4],
                   dfSummary,numAngles=3,k=5)
  
  #Visualise the errors for k=3 prediction. Plot endpoints and lines showing the actual vs predicted.
  par(new=FALSE) # cause clearing the screen before plotting the next chart
  plot(dfSummary$posx,dfSummary$posy,'p',cex=0.3,col='blue',xlim=c(0,30),ylim=c(0,15),main="Plot of Test Readings vs NN Estimates")
  par(new=TRUE)
  errorData = cbind(onlineSummary$posx,onlineSummary$posy,estXYK3[,1],estXYK3[,2])
  #loop through the estimates and plot
  for (i in 1:nrow(errorData)){
    m=t(matrix(errorData[i,],ncol=2)) # reshape the 4 list into a 2x 2 matrix suitable for using the generic plot.
    plot(m[1,1],m[1,2],'p',cex=0.8,xlim=c(0,30),ylim=c(0,15),xlab='',ylab='') # supress labels to avoid overwritting. Require ranges as plot will change scale to suit plot.
    par(new=TRUE)
    plot(m[2,1],m[2,2],'p',cex=0.3,xlim=c(0,30),ylim=c(0,15),xlab='',ylab='')
    par(new=TRUE)
    plot(m[,1],m[,2],'l',xlim=c(0,30),ylim=c(0,15),xlab='',ylab='')
    par(new=TRUE)
  }
  
  ## Calculate the sum of square errors for the estimate
  #@ret value of SSE.
  getEstimateError = function(est,actual){
    sum(rowSums((est-actual)^2))
  }
  
 print("NN Prediction Error ")
 sapply(list(errorData[,1:2]),getEstimateError,errorData[,3:4]) #apply error function with errorData[,1:2] as first para and 3:4 as second.
 
 ## Try using linear regressions on reading features to predict location. 
 ## Predict each coordiate independantly (ignores the correlation between a x and y)
 ## INCOMPLETE.
 
 ## NN prediction of each row based on the k neighbour average and numAngle subet.
 # @k = number of nearest neighbours to base prediction on.
 # @ret = data.frame containing the NN estimate
 #predRegressionXY = function(newSignals, newAngles, trainData, numAngles=1){
   # init a list. One item for each test signal passed in.
#   estXY = list(length = nrow(newSignals))
   
#   for (i in 1:nrow(newSignals)){
 #    trainSS = getTrainingSubset(newAngles[i],numAngles,trainData)
     
     ## build regression model on x and y independantly
#     form = as.formula(paste("posx~",paste(names(trainSS[,4:9]),collapse="+")))
#     mod.x = lm(form,data=trainSS)
#     mod.y = lm(form,data=trainSS)

#     pred.x = predict(mod.x,newdata=newSignals[i,6:11])
#     pred.y = predict(mod.y,newdata=newSignals[i,6:11])
     
#     closeXY[[i]]=c(pred.x,pred.y)
#   }
#   estXY = do.call("rbind",estXY)
#   return (estXY)
# }

#pred = predRegressionXY(onlineSummary[,6:11],newAngles = dfSummary[,4],dfSummary,numAngles=3)
 
### Cross validation to determine the optimal k.
 
v=11 # the number of folds
permuteLocs = sample(unique(dfSummary$posXY)) # randow reordering ( sample size equal total size)
permuteLocs = matrix(permuteLocs, ncol=v,nrow=floor(length(permuteLocs)/v)) # reshape into matrix where each column is a fold.

onlineCVSummary = getTrainingSubset(signals=dfSummary,blSampleAngle = TRUE)

blOptimised=TRUE

K = 20
err = rep(0,K)

if (blOptimised==FALSE){
for (j in 1:v){
  onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j])   # use test set based on the posXY in the j column.
  offlineFold = subset(dfSummary, posXY %in% permuteLocs[,-j]) # use the training set based on the posXY in everything but the jth column
  actualFold = onlineFold[,c("posx","posy")]  # get just the actual co-ords.
  
    for (myk in 1:K)  # rerun the estimation for each k seperately.
    {
      estFold = predXY(newSignals = onlineFold[,6:11],
                       newAngles = onlineFold[,4],
                       offlineFold, numAngles = 3, k=myk)
      err[myk] = err[myk] + getEstimateError(estFold,actualFold)
    }
  print(paste("iteration ",j))
  print(err[1])
}
} else {
  for (j in 1:v){
    
    ## optimised
    onlineFold = subset(onlineCVSummary, posXY %in% permuteLocs[,j])
    offlineFold = subset(dfSummary, posXY %in% permuteLocs[,-j])
    actualFold = onlineFold[,c("posx","posy")]
    
    list_estFold = predXY_Optimised(newSignals = onlineFold[,6:11],
                       newAngles = onlineFold[,4],
                       offlineFold, numAngles = 3, k=K)
    
    # loop over each value of k (being the number of nearest neighbours)
    for (i in 1:K){ 
      for (x in 1:length(list_estFold)){  # from the list of matrices returned (each list entry corresponds to a posXY from the onlineFold) select the row corresponding to i nearest neighbours
        if (x==1) {
          estFold = list_estFold[[x]][i,] # intialise the estimate of the fold
      } else {
        estFold = rbind(estFold,list_estFold[[x]][i,]) # pull the appropriate row.
      }
      }
      err[i] = err[i] + getEstimateError(estFold,actualFold) # add the error for the kth NN estimates on this the jth fold.
    }
    
    print(paste("iteration ",j, "Error of k=1 (should be increasing)"))
    print(err[1])
  }
}
## Plot the CV error curve
par(new==FALSE)
par(mfrow=c(1,1))
plot(seq(1:K),err,"b",main="Cross validation error vs number of nearest neighbours.",xlab="Number of nearest neighbours",ylab=paste("Cummulative error over all folds(",v,")"))

##############################################################################################333
# Additional exercises: Load data into a dataframe with one row per line in the input file.

txtData = readLines("Data/offline.final.trace.txt")

getAPMacs = function(x){
  tokens = strsplit(x,"[;=,]")[[1]]
  # split the text line and wrap the list into a x4 matrix.
  tmp = matrix(tokens[-(1:10)],ncol=4,byrow=TRUE)[,c(1,4)] ## at this stage all elements are char so can use a matrix.
  # tmp only returns a matrix if there is more than one row otherwise it coerces it into a vector.
  if (is.matrix(tmp))
    tmp = matrix(matrix(tmp[tmp[,2]==3,1],ncol=1),dimnames=list(NULL,c("mac")))
  else
  {
    # deal with items with just a single row.
    if (tmp[2]==3) 
      tmp =matrix(tmp[1],ncol=1,dimnames=list(NULL,c("mac")))
    else 
      tmp=NULL
  }
  return(tmp)
}

a=lapply(txtData[substr(txtData,1,1)!="#"],getAPMacs)
a= unique(do.call('rbind',a))
#create the empty data frame that will be populated on the second pass.
df = data.frame(matrix(vector(),0,length(a),dimnames=list(NULL,t(a))))



  
 