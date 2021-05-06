edbGetSubjectRecords= function(d, ASubID=NULL, Type='ALL')
  #Type = ALL or SCHEDULED
{
  if (is.null(ASubID)) stop('No SubID provided')
  
  Type = toupper(Type)
  if(!(Type=='ALL' || Type=='SCHEDULED')) stop('Type must be ALL or SCHEDULED')
  
  if (Type=='ALL'){
    dS = subset(d, d$SubID==ASubID)
  }else{
    dS = subset(d, d$SubID==ASubID & (d$Visit.Outcome =='SCHEDULED' | d$Call.Outcome =='SCHEDULED'))
    
    #Get their first record
    dF = subset(d, d$SubID==ASubID & d$Last !='')
    dS = rbind(dF, dS)  #add in first row from d for participant info
  }
  
  return(dS)
}