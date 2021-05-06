edbGetCalls= function(d, Start, End, CallType='ALL', CallOutcome = 'SCHEDULED')
{
  
  Start =as.Date(Start, format= '%Y/%m/%d')
  End = as.Date(End, format= '%Y/%m/%d')
  
  CallType = toupper(CallType)
  if(CallType=='ALL'){
    dV = subset(d,d$Call.Date>=Start & d$Call.Date <=End)
  }else{
    dV = subset(d,d$Call.Date>=Start & d$Call.Date <=End & toupper(d$Call.Type) == CallType)
  }
  
  CallOutcome = toupper(CallOutcome)
  if(CallOutcome =='SCHEDULED'){
    dV = subset(dV,dV$Call.Outcome=='SCHEDULED')
  }
  
  #Fill in Subject info
  for (i in 1:nrow(dV)){
    id = which(d$SubID ==dV$SubID[i] & d$Last !='')
    dV$Last[i] = d$Last[id]
    dV$First[i] = d$First[id]
    dV$Arm[i] = d$Arm[id]
    dV$Status[i] = d$Status[id]
    dV$Phone1[i] = d$Phone1[id]
    dV$Phone2[i] = d$Phone2[id]
    dV$Email[i] = d$Email[id]
  }
  
  #Remove unneeded columns
  dV$Visit.Date=NULL
  dV$Visit.Outcome=NULL
  dV$Sex=NULL
  dV$Race=NULL
  dV$Ethnicity=NULL
  dV$Address=NULL
  dV$Notes=NULL
  dV$Entry.Date=NULL
  dV$StaffID=NULL
  
  
  #order by Call.Date
  dV = dV[order(dV$Call.Date),]
  
  return(dV)
}