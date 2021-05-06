edbGetActiveParticipants = function(d)
{
  
  dA = d[d$Status !='',]  #Select only first records for each participant
  dA = dA[str_detect(dA$Status,"(Active)"),] #Select active only
  
  #Delete unnecessary variables
  dA$Visit.Type = NULL
  dA$Visit.Date = NULL
  dA$Visit.Outcome = NULL
  dA$Call.Type = NULL
  dA$Call.Date = NULL
  dA$Call.Outcome = NULL
  dA$Sex = NULL
  dA$Race=NULL
  dA$Ethnicity=NULL
  dA$Address=NULL
  dA$Entry.Date=NULL
  dA$StaffID=NULL
  
  #add in next.visit.type and next.visit.date
  dA$Next.Visit.Type = ""
  dA$Next.Visit.Date = as.Date(rep(NA,nrow(dA)))  
  for(i in 1:nrow(dA)){
    dSub = d[d$SubID==dA$SubID[i],]
    dSub = dSub[dSub$Visit.Outcome =="SCHEDULED",]
    dSub = dSub[order(dSub$Visit.Date),]
    dA$Next.Visit.Type[i]=dSub$Visit.Type[1]
    dA$Next.Visit.Date[i]=dSub$Visit.Date[1]
  }
  
  #Reorder columns
  dA = dA[,c("SubID", "Last", "First", "Arm", "Status", "Next.Visit.Type", "Next.Visit.Date", 
             "Phone1", "Phone2", "Email", "Notes")]
  
 return(dA)
}