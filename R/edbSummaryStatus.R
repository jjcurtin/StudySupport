edbSummaryStatus = function(d)
{
  
  d = d[d$Status !='',]  #Select only first records for each participant
  
  tS=table(d$Sex, d$Status)
  mS = t(as.matrix(tS))
  
  dS = data.frame(matrix(NA, nrow=nrow(mS), ncol=ncol(mS)))
  rownames(dS) = rownames(mS)
  names(dS) = colnames(mS)
  for(i in 1:ncol(mS)){
    dS[,i]=mS[,i]
  }
  
  dS$Total = rowSums(dS)
  
  #order dS in meaningful way
  #Reorder with Not Assigned last
  rCompleted = which(rownames(dS) =="COMPLETED")
  dCompleted = dS[rCompleted,]
  if(nrow(dCompleted))dS = dS[-rCompleted,]
  
  rConsentNo = which(rownames(dS) =="Consent: NO")
  dConsentNo = dS[rConsentNo,]
  if(nrow(dConsentNo)) dS = dS[-rConsentNo,]
  
  rConsentNoShow = which(rownames(dS) =="Consent: NO-SHOW")
  dConsentNoShow = dS[rConsentNoShow,]
  if(nrow(dConsentNoShow)) dS = dS[-rConsentNoShow,]
  
  rDiscontinue = which(rownames(dS) =="DISCONTINUED")
  dDiscontinue = dS[rDiscontinue,]
  if(nrow(dDiscontinue)) dS = dS[-rDiscontinue,]
  
  rWithdrawn = which(rownames(dS) =="WITHDRAWN")
  dWithdrawn = dS[rWithdrawn,]
  if(nrow(dWithdrawn)) dS = dS[-rWithdrawn,]
  
  rScreenIneligble = which(rownames(dS) =="Screen: INELIGBLE")
  dScreenIneligble = dS[rScreenIneligble,]
  if(nrow(dScreenIneligble)) dS = dS[-rScreenIneligble,]
  
  rLost = which(rownames(dS) =="Follow-up: LOST")
  dLost = dS[rLost,]
  if(nrow(dLost)) dS = dS[-rLost,]
  
  rExcluded = which(rownames(dS) =="EXCLUDED")
  dExcluded = dS[rExcluded,]
  if(nrow(dExcluded)) dS = dS[-rExcluded,]
  
  dS = dS[order(rownames(dS)),]
  dS = rbind(dS,dCompleted)
  dS = rbind(dS,dConsentNo)
  dS = rbind(dS,dConsentNoShow)
  dS = rbind(dS,dScreenIneligble)
  dS = rbind(dS,dDiscontinue)
  dS = rbind(dS,dWithdrawn)
  dS = rbind(dS,dLost)
  dS = rbind(dS,dExcluded)
  
  #Add totals
  dS = rbind(dS, colSums(dS))
  rownames(dS)[nrow(dS)] ="TOTAL"
  return(dS)
}