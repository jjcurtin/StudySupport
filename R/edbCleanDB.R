edbCleanDB = function(d)
#Cleans and formats Enrollment DB.  Called internally by other functions
{
  if (nrow(d)>0) { #only clean if d contains data
    #Delete unnecessary variables -----------------
    d$ResponseID = NULL
    d$ResponseSet = NULL
    d$IPAddress = NULL
    d$EndDate = NULL
    d$RecipientLastName = NULL
    d$RecipientFirstName = NULL
    d$RecipientEmail = NULL
    d$ExternalDataReference = NULL
    d$Status = NULL
    d$LocationLatitude = NULL
    d$LocationLongitude = NULL
    d$LocationAccuracy = NULL

    d = d[d$Finished==1,]
    d$Finished = NULL

    #Rename variables for simpler use ------------------------
    #participant characteristics
    d = varRename(d, c('Q0',  'Q1',    'Q2',         'Q3',    'Q4',    'Q5',   'Q6',   'Q7',        'Q25', 'Q8',     'Q9',     'Q10',   'Q11'), 
                     c('Entry.Date', 'SubID', 'Entry.Type', 'First', 'Last',  'Sex',  'Race', 'Ethnicity', 'Arm', 'Phone1', 'Phone2', 'Email', 'Address'))

    #raw visit variables to be "cleaned"
    d = varRename(d, c('Q12',         'Q13',         'Q14',              'Q15'),
                     c('rVisit.Type', 'rVisit.Date', 'rOrig.Visit.Date', 'rNew.Visit.Date'))
 
    #raw call variables to be "cleaned"
    d = varRename(d, c('Q16',           'Q17',        'Q18',        'Q18_TEXT',        'Q19',           'Q19_TEXT'),
                     c('rReminderCall', 'rCall.Type', 'rCall.Date', 'rCall.Date.Text', 'rCall.Outcome', 'rCall.Outcome.Text'))
    
    #disposition and notes
    d = varRename(d, c('Q20', 'Q21'), c('rDisposition', 'Notes'))
    
    #Format Entry.Date (previously Q0) as GMT
    d$Entry.Date = as.POSIXct(d$Entry.Date, format= '%m/%d/%Y %H:%M:%S',tz='GMT')
    #attributes(d$Entry.Date)$tzone = 'America/Chicago'
    
    #Strip time and retain date
    #d$Entry.Date = format(d$Entry.Date, format = '%m/%d/%Y')
    #d$Entry.Date = as.POSIXct(d$Entry.Date, format= '%m/%d/%Y',tz='America/Chicago')
    
    #Add variables to be calculated from raw variables above
    d$Visit.Type = NA
    d$Visit.Date = as.Date(NA, format='%m/%d/%Y',tz='America/Chicago')
    d$Visit.Outcome = NA
    d$Call.Type = NA
    d$Call.Date = as.Date(NA, format='%m/%d/%Y',tz='America/Chicago')
    d$Call.Outcome = NA
    d$Status = NA
    
    #Reorder variables to put temp/raw variables at end
    d = d[,c('SubID', 'Entry.Type', 'Last', 'First', 'Arm', 'Status',
             'Visit.Type', 'Visit.Date', 'Visit.Outcome', 'Call.Type', 'Call.Date', 'Call.Outcome',
             'rVisit.Type', 'rVisit.Date', 'rOrig.Visit.Date',  'rNew.Visit.Date',  'rReminderCall',    
             'rCall.Type',  'rCall.Date',  'rCall.Date.Text', 'rCall.Outcome', 'rCall.Outcome.Text', 
             'rDisposition',    
             'Sex', 'Race', 'Ethnicity', 'Phone1', 'Phone2', 'Email', 'Address', 'Notes', 'Entry.Date', 'StaffID')]
    
    #order records by entry.date and SubID
    d = d[order(d$SubID,d$Entry.Date),]
    
    #Change visit types to upper
    d$Visit.Type = toupper(d$Visit.Type)
    d$rVisit.Type = toupper(d$rVisit.Type)

    #Loop though each participant for validity checks and prelim cleaning----------
    SubIDs = unique(d$SubID)
    for(SubID in SubIDs){
      dSub = d[d$SubID == SubID,]
      RN =  rownames(dSub)[which(dSub$Entry.Type == 'Participant: ENROLLED')]
      
      #handle enrollment entry
      if(length(RN) != 1) {  #check for 1 enrollment record
        stop(str_c(sum(dSub$Entry.Type == 'Participant: ENROLLED'), ' enrollment records detected for SubID ',SubID))
      }
      if(dSub$Entry.Type[1] != 'Participant: ENROLLED') {  #check first row is Enroll
        stop(str_c('First participant record != ENROLLED for SubID ',SubID))
      }
      d$Status[rownames(d)==RN] = 'ENROLLED'
    } #end For SubID loop    
    
    
    
    #Handle participant updates----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Participant: INFO UPDATED')
      {
        SubID = d$SubID[i]
        if (d$Arm[i] !='' && !is.na(d$Arm[i]))  {
          d$Arm[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Arm[i]
        }
        
        if (d$Phone1[i] !='' && !is.na(d$Phone1[i]))  {
          d$Phone1[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Phone1[i]
        }
        
        if (d$Phone2[i] !=''&& !is.na(d$Phone2[i]))  {
          d$Phone2[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Phone2[i]
        }
        
        if (d$Email[i] !=''&& !is.na(d$Email[i])) {
          d$Email[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Email[i]
        }
        
        if (d$Address[i] !=''&& !is.na(d$Address[i]))  {
          d$Address[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Address[i]
        }        

        if (d$Sex[i] !='' && !is.na(d$Sex[i]))  {
          d$Sex[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Sex[i]
        }
        
        if (d$Race[i] !=''&& !is.na(d$Race[i]))  {
          d$Race[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Race[i]
        } 
        
        if (d$Ethnicity[i] !=''&& !is.na(d$Ethnicity[i]))  {
          d$Ethnicity[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Ethnicity[i]
        }        
        
        
        
        d = d[-i,] #remove the update participant name Event after making correction
      } else i=i+1
    }
    
    #Format Visit: Schedule entries-------------
    d$Visit.Type[d$Entry.Type=='Visit: SCHEDULED'] = d$rVisit.Type[d$Entry.Type=='Visit: SCHEDULED']
    d$Visit.Date[d$Entry.Type=='Visit: SCHEDULED'] = as.Date(d$rVisit.Date[d$Entry.Type=='Visit: SCHEDULED'], format='%m/%d/%Y',tz='America/Chicago')
    d$Visit.Outcome[d$Entry.Type=='Visit: SCHEDULED'] = 'SCHEDULED'
    d$Call.Type[d$Entry.Type=='Visit: SCHEDULED' & d$rReminderCall=='Yes'] = 'Reminder'
    d$Call.Date[d$Entry.Type=='Visit: SCHEDULED' & d$rReminderCall=='Yes'] = d$Visit.Date[d$Entry.Type=='Visit: SCHEDULED'& d$rReminderCall=='Yes'] - days(1)
    d$Call.Outcome[d$Entry.Type=='Visit: SCHEDULED' & d$rReminderCall=='Yes'] = 'SCHEDULED'
    
    #Handle Visit:RESCHEDULED----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Visit: RESCHEDULED'){
        #Key info from reschedule entry
        SubID = d$SubID[i]
        Date =  as.Date(d$rOrig.Visit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        VisitType = d$rVisit.Type[i]
        
        #Format reschedule entry as visit
        d$Entry.Type[i] = 'Visit: SCHEDULED'
        d$Visit.Type[i] = d$rVisit.Type[i]
        d$Visit.Date[i] =as.Date(d$rNew.Visit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        d$Visit.Outcome[i] = 'SCHEDULED'
        if (d$rReminderCall[i] =='Yes'){
          d$Call.Type[i] = 'Reminder'
          d$Call.Date[i] = d$Visit.Date[i]-days(1)
          d$Call.Outcome[i] = 'SCHEDULED'
        }
        
        #update previous scheduled visit
        RI = which(d$SubID==SubID & d$Entry.Type == 'Visit: SCHEDULED' &  d$Visit.Type==VisitType & d$Visit.Date==Date & d$Visit.Outcome=='SCHEDULED')
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and rescheduled visits do not match for SubID ',SubID))
        }else{
          d$Visit.Outcome[RI] = 'RESCHEDULED'
          
          #mark call as rescheduled if still scheduled
          if (!is.na(d$Call.Outcome[RI]) && d$Call.Outcome[RI] == 'SCHEDULED') {
            d$Call.Outcome[RI] = 'RESCHEDULED'
          }
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
      }
      i=i+1
    }
    
    
    #Handle Visit:CANCELLED----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Visit: CANCELLED'){
        SubID = d$SubID[i]
        Date =  as.Date(d$rVisit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        VisitType = d$rVisit.Type[i]
        
        RI = which(d$SubID==SubID & d$Entry.Type == 'Visit: SCHEDULED' & d$Visit.Type==VisitType & d$Visit.Date==Date & d$Visit.Outcome=='SCHEDULED')
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and cancelled visits do not match for SubID ',SubID))
        }else{
          d$Visit.Outcome[RI] = 'CANCELLED' 
          
          #cancel if still scheduled
          if(!is.na(d$Call.Outcome[RI]) && d$Call.Outcome[RI] == 'SCHEDULED') {
            d$Call.Outcome[RI] = 'CANCELLED'
          }
          
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
        d = d[-i,] #remove the  Visit: Cancelled entry after cancelled
      } else i=i+1
    }
    
    #Handle Visit: NO-SHOW----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Visit: NO-SHOW'){
        SubID = d$SubID[i]
        Date =  as.Date(d$rVisit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        VisitType = d$rVisit.Type[i]
        
        RI = which(d$SubID==SubID & d$Entry.Type == 'Visit: SCHEDULED' & d$Visit.Type==VisitType & d$Visit.Date==Date & d$Visit.Outcome=='SCHEDULED')
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and no-show visits do not match for SubID ',SubID))
        }else{
          d$Visit.Outcome[RI] = 'NO-SHOW' 
          
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
        d = d[-i,] #remove the  Visit: No-Show entry after no-show indicated
      } else i=i+1
    }
    
    #Handle Visit:COMPLETED----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Visit: COMPLETED') {
        SubID = d$SubID[i]
        Date =  as.Date(d$rVisit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        VisitType = d$rVisit.Type[i]
        
        RI = which(d$SubID==SubID & d$Entry.Type == 'Visit: SCHEDULED' & d$Visit.Type==VisitType & d$Visit.Date==Date & d$Visit.Outcome=='SCHEDULED')
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and completed visits do not match for SubID ',SubID, ' for completion date: ',Date))
        } else{
          d$Visit.Outcome[RI] = 'COMPLETED'
          
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
          
          #update status
          d$Status[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$Visit.Type[RI]
          
        }
        d = d[-i,] #remove the  Visit: COMPLETED entry after completed updated
      } else  i=i+1
    }
    

    #Handle Staff Call: Scheduled
    d$Call.Type[d$Entry.Type=='Staff Call: SCHEDULED'] = 'Staff Call'
    d$Call.Date[d$Entry.Type=='Staff Call: SCHEDULED' & d$rCall.Date == 'ASAP'] =  as.Date(d$Entry.Date[d$Entry.Type=='Staff Call: SCHEDULED' & d$rCall.Date == 'ASAP'],tz='America/Chicago')
    d$Call.Date[d$Entry.Type=='Staff Call: SCHEDULED' & d$rCall.Date == 'Future Date (mm/dd/yyyy)'] =  as.Date(d$rCall.Date.Text[d$Entry.Type=='Staff Call: SCHEDULED' & d$rCall.Date == 'Future Date (mm/dd/yyyy)'], format='%m/%d/%Y',tz='America/Chicago')
    d$Call.Outcome[d$Entry.Type=='Staff Call: SCHEDULED'] = 'SCHEDULED'
    
  
    #Handle Staff Call: Completed - reminder
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Staff Call: COMPLETED' && d$rCall.Type[i]=='Reminder'){
        SubID = d$SubID[i]
        #Date =  min(d$Call.Date[d$SubID==SubID & d$Call.Type=='Reminder' & !is.na(d$Call.Date) & d$Call.Outcome=='SCHEDULED'])
        Date = as.Date(d$Entry.Date[i],tz='America/Chicago')
        
        RI = which(d$Entry.Type == 'Visit: SCHEDULED' & d$SubID==SubID & d$Call.Date==Date)
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and completed visits do not match for reminder call for SubID ',SubID))
        }else{
          d$Call.Outcome[RI] = 'COMPLETED'
          
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
        d = d[-i,] #remove the  Staff Call: COMPLETED entry after completed updated
      } else i=i+1
    }
    
    
    
    
    #Handle Staff Call: Completed - Other w/Call Again ASAP or Specific Date
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Staff Call: COMPLETED' && d$rCall.Type[i]=='Other' && d$rCall.Outcome[i]!='Completed'){
        SubID = d$SubID[i]
        Date = as.Date(d$Entry.Date[i],tz='America/Chicago')
        #Date =  d$Call.Date[d$SubID==SubID & d$Call.Type=='Staff Call' & !is.na(d$Call.Date) & d$Call.Outcome=='SCHEDULED']
        
        #Format completed entry as next staff call request
        d$Entry.Type[i] = 'Staff Call: SCHEDULED'
        d$Call.Type[i] = 'Staff Call'
        
        #COMMENTED OUT.  DONT THINK NEEDED?
        #$Visit.Date[i] =as.Date(d$rNew.Visit.Date[i], format='%m/%d/%Y',tz='America/Chicago')
        
        if(d$rCall.Outcome[i]=='Call Again ASAP'){
          d$Call.Date[i] =  as.Date(d$Entry.Date[i],tz='America/Chicago')
        }else{
          d$Call.Date[i] =  as.Date(d$rCall.Outcome.Text[i], format='%m/%d/%Y',tz='America/Chicago')
        }
        d$Call.Outcome[i] = 'SCHEDULED'
        
        #update previous scheduled staff call
        RI = which(d$Entry.Type == 'Staff Call: SCHEDULED' & d$Call.Type == 'Staff Call' & d$Call.Outcome == 'SCHEDULED' & d$SubID==SubID & d$Call.Date<=Date)
        
        if(length(RI)>1){
          #warning(str_c('Multiple staff calls not completed for SubID '), SubID, '. Updating last scheduled staff call. Record: ', i)
          #warning not needed.  Will match to most recent previous.  Could be other earlier scheduled calls that will get picked up by next loop of completed calls
          RI = RI[length(RI)]
        }
        if(length(RI)<1) {
          stop(str_c('Scheduled and completed staff calls do not match for SubID ',SubID))
        }else{
          d$Call.Outcome[RI] = 'RESCHEDULED'
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
      }
      i=i+1
    }    

    #Handle Staff Call: Completed - Other completed 
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Staff Call: COMPLETED' && d$rCall.Type[i]=='Other' && d$rCall.Outcome[i]=='Completed'){
        SubID = d$SubID[i]
        #Date =  d$Call.Date[d$SubID==SubID & d$Call.Type=='Staff Call' & !is.na(d$Call.Date) & d$Call.Outcome=='SCHEDULED']
        Date = as.Date(d$Entry.Date[i],tz='America/Chicago')
        
        RI = which(d$Entry.Type == 'Staff Call: SCHEDULED' & d$Call.Type == 'Staff Call' & d$Call.Outcome == 'SCHEDULED' 
                   & d$SubID==SubID & d$Call.Date<=Date)
        
        if(length(RI)!=1) {
          stop(str_c('Scheduled and completed calls do not match for SubID ',SubID))
        }else{
          d$Call.Outcome[RI] = 'COMPLETED'
          if(is.na(d$Notes[RI]) || d$Notes[RI]==''){
            d$Notes[RI] = d$Notes[i]  #add notes
          }else{
            d$Notes[RI] = str_c(d$Notes[RI], '; ', d$Notes[i])  #add notes
          }
        }
        d = d[-i,] #remove the  Staff Call: COMPLETED entry after completed updated
      } else  i=i+1
    }
    
    
    #Handle participant disposed----------
    i=1
    while (i <= nrow(d)){
      if (d$Entry.Type[i] =='Participant: DISPOSED'){
        SubID = d$SubID[i]
        d$Status[d$SubID==SubID & d$Entry.Type=='Participant: ENROLLED'] =  d$rDisposition[i]
        d = d[-i,] #remove the participant disposed Event after making update
      } else i=i+1
    }
    
    #Simplify some status labels
    d$Status[d$Status=='On-Study: WITHDRAWN'] = 'WITHDRAWN'
    d$Status[d$Status=='On-Study: DISCONTINUE'] = 'DISCONTINUED'
    
  }
   
  #sort by SubID and event order
  Dates = d$Visit.Date
  Dates[is.na(Dates)] = d$Call.Date[is.na(Dates)]
  d = d[order(d$SubID, Dates,na.last=FALSE),]
  
  #Remove unnecessary variables
  d$Entry.Type = NULL
  d$rVisit.Type = NULL
  d$rVisit.Date = NULL
  d$rOrig.Visit.Date = NULL
  d$rNew.Visit.Date = NULL
  d$rReminderCall = NULL
  d$rCall.Type = NULL
  d$rCall.Date = NULL
  d$rCall.Date.Text = NULL
  d$rCall.Outcome = NULL
  d$rCall.Outcome.Text = NULL
  d$rDisposition = NULL

  #Convert Arm to string if needed
  if(is.integer(d$Arm))  d$Arm = as.character(d$Arm)

  #Remove NA from string variables
  d$Status[is.na(d$Status)] = ''
  d$Arm[is.na(d$Arm)] = ''
  d$Visit.Type[is.na(d$Visit.Type)] = ''
  d$Visit.Outcome[is.na(d$Visit.Outcome)] = ''
  d$Call.Type[is.na(d$Call.Type)] = ''
  d$Call.Outcome[is.na(d$Call.Outcome)] = ''
  d$Last[is.na(d$Last)] = ''
  d$First[is.na(d$First)] = ''
  d$Sex[is.na(d$Sex)] = ''
  d$Race[is.na(d$Race)] = ''
  d$Ethnicity[is.na(d$Ethnicity)] = ''
  d$Phone1[is.na(d$Phone1)] = ''
  d$Phone2[is.na(d$Phone2)] = ''
  d$Email[is.na(d$Email)] = ''
  d$Address[is.na(d$Address)] = ''
  d$Notes[is.na(d$Notes)] = ''
  d$StaffID[is.na(d$StaffID)] = ''
  
  #Set blank arm to unassigned
  d$Arm[d$Status!='' & d$Arm==''] = 'Not Assigned'
  
  #Add (Active) to all Active statuses
  AddActive = !(d$Status=='' | d$Status== 'COMPLETED' | d$Status== 'DISCONTINUED'
                | d$Status== 'WITHDRAWN'| d$Status== 'Screen: INELIGBLE' | d$Status== 'Followup: LOST'
                | d$Status== 'EXCLUDED' | d$Status== 'Consent: NO-SHOW' | d$Status== 'Consent: NO')
  
  d$Status[AddActive] = str_c(d$Status[AddActive], '(Active)')
  
  #order final records by entry.date and SubID
  d = d[order(d$SubID,d$Entry.Date),]
  
  if (nrow(d)==0) d=NULL  #set to NULL if d is empty
  return(d)
}
