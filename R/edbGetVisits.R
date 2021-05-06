edbGetVisits= function(d, Start, End, VisitType='ALL', VisitOutcome = 'SCHEDULED')
{
  
    Start =as.Date(Start, format= '%Y/%m/%d', tz='America/Chicago')
    End = as.Date(End, format= '%Y/%m/%d', tz='America/Chicago')
    
    VisitType = toupper(VisitType)
    
    if(VisitType=='ALL'){
      dV = subset(d,d$Visit.Date>=Start & d$Visit.Date <=End)
    }else{
      dV = subset(d,d$Visit.Date>=Start & d$Visit.Date <=End & toupper(d$Visit.Type) == VisitType)
    }
    
    VisitOutcome = toupper(VisitOutcome)
    if(VisitOutcome =='SCHEDULED'){
      dV = subset(dV,dV$Visit.Outcome=='SCHEDULED')
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
    dV$Call.Type=NULL
    dV$Call.Date=NULL
    dV$Call.Outcome=NULL
    dV$Sex=NULL
    dV$Race=NULL
    dV$Ethnicity=NULL
    dV$Address=NULL
    dV$Notes=NULL
    dV$Entry.Date=NULL
    dV$StaffID=NULL
    
    
    #order by Visit.date
    dV = dV[order(dV$Visit.Date),]
    
    return(dV)
}