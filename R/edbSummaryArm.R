edbSummaryArm = function(d, Status='COMPLETED')
{
  d = d[d$Status !='',]  #Select only first records for each participant
  
  Status = toupper(Status)
  if(Status =='COMPLETED'){
    d = d[d$Status=='COMPLETED',]
  }else{
    d = d[d$Status=='COMPLETED' | str_detect(d$Status,'(Active)'),]
  }
  
  tS=table(d$Sex, d$Arm)
  mS = t(as.matrix(tS))
  
  dS = data.frame(matrix(NA, nrow=nrow(mS), ncol=ncol(mS)))
  rownames(dS) = rownames(mS)
  names(dS) = colnames(mS)
  for(i in 1:ncol(mS)){
    dS[,i]=mS[,i]
  }
  
  dS$Total = rowSums(dS)
  
  #Reorder with Not Assigned last
  rNA = which(rownames(dS) =="Not Assigned")
  dNA = dS[rNA,]
  dS = dS[-rNA,]
  
  dS = dS[order(rownames(dS)),]
  dS = rbind(dS,dNA)
  
  return(dS)
}

