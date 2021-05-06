rdbCleanDB = function(d, DaysInactive)
#Cleans and formats RecruitDB.  Called internally by other functions
{
  if (nrow(d)>0)  #only clean if d contains data
  {
    #delete unnecessary variables -----------
    d$ResponseID = NULL
    d$ResponseSet = NULL
    d$IPAddress = NULL
    d$EndDate = NULL
    d$RecipientLastName = NULL
    d$RecipientFirstName = NULL
    d$RecipientEmail = NULL
    d$ExternalDataReference = NULL
    d$Status = NULL
    d$Q5_1 = NULL
    d$Q5_2 = NULL
    d$Q5_3 = NULL
    d$LocationLatitude = NULL
    d$LocationLongitude = NULL
    d$LocationAccuracy = NULL
    d$Q9 = NULL
  
    d = d[d$Finished==1,]
    d$Finished = NULL
    
    
    #rename other variables for simpler use -----------------
    d = varRename(d, c('StartDate',  'Q1',         'Q2',    'Q3',    'Q6',    'Q7',   'Q8',  'Q4',     'Q13',        'Q14'), 
                     c('Entry.Date', 'Event.Date', 'Event', 'Phone', 'First', 'Last', 'Sex', 'Status', 'Call.Notes', 'Other.Notes'))
    d = varRename(d,'Q11', 'Preferred.Times')
    d = varRename(d, c( "Q12_1_1", "Q12_1_2", "Q12_1_3", "Q12_1_4", "Q12_1_5", "Q12_1_6",   "Q12_1_7",   
                        "Q12_2_1", "Q12_2_2", "Q12_2_3", "Q12_2_4", "Q12_2_5", "Q12_2_6",   "Q12_2_7",   
                        "Q12_3_1", "Q12_3_2", "Q12_3_3", "Q12_3_4", "Q12_3_5", "Q12_3_6",   "Q12_3_7",
                        "Q12_4_1", "Q12_4_2", "Q12_4_3", "Q12_4_4", "Q12_4_5", "Q12_4_6",   "Q12_4_7"),
                  c("MO.M", "TU.M", "WE.M", "TH.M", "FR.M", "SA.M", "SU.M",   
                    "MO.A", "TU.A", "WE.A", "TH.A", "FR.A", "SA.A", "SU.A", 
                    "MO.E", "TU.E", "WE.E", "TH.E", "FR.E", "SA.E", "SU.E",   
                    "MO.All", "TU.All", "WE.All",  "TH.All",  "FR.All",  "SA.All",  "SU.All" ))    
    
    
    #Clean up  names and sex
    d$First[is.na(d$First)] = ''
    d$Last[is.na(d$Last)] = ''
    d$First[toupper(d$First)=='UNKNOWN'] = ''
    d$Last[toupper(d$Last)=='UNKNOWN'] = ''
    d$First[toupper(d$First)=='N/A'] = ''
    d$Last[toupper(d$Last)=='N/A'] = ''
    d$Sex[is.na(d$Sex)] = ''
    
    #Format and clean up date 'variables' -----------
    
    #Entry.Date (previously StartDate) as GMT
    d$Entry.Date = as.POSIXct(d$Entry.Date, format= '%Y-%m-%d %H:%M:%S',tz='GMT')
    attributes(d$Entry.Date)$tzone = 'America/Chicago'
    d$Entry.Date = format(d$Entry.Date, format = '%m/%d/%Y')
    
    #Event.Date (in local time zone; kinda) is set to Entry date string if it was blank (older surveys)
    d$Event.Date[(d$Event.Date=='')]= d$Entry.Date[(d$Event.Date=='')] 
    d$Event.Date = as.POSIXct(d$Event.Date, format='%m/%d/%Y', tz='America/Chicago') #convert to posixct
    
    
    #Clean up NAs in strings.  Set to '' for simpler boolean testing
    d$Q10[is.na(d$Q10)] = ''
    d$Preferred.Times[is.na(d$Preferred.Times)] = ''
    
    
    d$Call.Notes[is.na(d$Call.Notes)] = ''
    d$Other.Notes[is.na(d$Other.Notes)] = ''
    
    #Handle Q15
    #if(str_detect(names(d), 'Q15')) d$Q15[is.na(d$Q15)] = ''
    #if(str_detect(names(d), 'Q15')) d$Q15_TEXT[is.na(d$Q15_TEXT)] = ''
    d$Q15 = NULL
    d$Q15_TEXT = NULL
    
    #Handle next call.   "Earliest next call" saved as Q10 in survey
    d$Next.Call = as.POSIXct(NA, tz='America/Chicago')
    d$Next.Call[d$Q10=='Today'] = d$Event.Date[d$Q10=='Today']
    d$Next.Call[d$Q10=='Tomorrow'] = d$Event.Date[!is.na(d$Q10) & d$Q10=='Tomorrow'] + days(1)
    d$Next.Call[d$Q10=='Future Date (mm/dd/yyyy)'] = as.POSIXct(d$Q10_TEXT[d$Q10=='Future Date (mm/dd/yyyy)'], format = '%m/%d/%Y', tz='America/Chicago')
    d$Q10 = NULL
    d$Q10_TEXT = NULL 
    
    #Convert Event.date and Next Call to string.  All dates are now string in local time
    d$Event.Date = format(d$Event.Date, format = '%m/%d/%Y')
    d$Next.Call = format(d$Next.Call, format = '%m/%d/%Y')
    
    # #Clean preferred times
    # #handle columns with no text data that get imported as logical and NA
    # #set to ''
    # PTColF = which(colnames(d)=='MO.M')
    # PTColL = which(colnames(d)=='SU.E')
    # for (c in PTColF:PTColL)
    # {
    #   if (all(is.na(d[,c])==TRUE)) 
    #   {
    #     d[,c]=''
    #   }
    # }
    
    
    #Recode all selected times to Y and all NA to ''
    d$MO.M = ifelse(is.na(d$MO.M),'','Y')
    d$MO.A = ifelse(is.na(d$MO.A),'','Y')
    d$MO.E = ifelse(is.na(d$MO.E),'','Y')
    d$MO.All = ifelse(is.na(d$MO.All),'','Y')
    d$TU.M = ifelse(is.na(d$TU.M),'','Y')
    d$TU.A = ifelse(is.na(d$TU.A),'','Y')
    d$TU.E = ifelse(is.na(d$TU.E),'','Y')
    d$TU.All = ifelse(is.na(d$TU.All),'','Y')
    d$WE.M = ifelse(is.na(d$WE.M),'','Y')
    d$WE.A = ifelse(is.na(d$WE.A),'','Y')
    d$WE.E = ifelse(is.na(d$WE.E),'','Y')
    d$WE.All = ifelse(is.na(d$WE.All),'','Y')
    d$TH.M = ifelse(is.na(d$TH.M),'','Y')
    d$TH.A = ifelse(is.na(d$TH.A),'','Y')
    d$TH.E = ifelse(is.na(d$TH.E),'','Y')
    d$TH.All = ifelse(is.na(d$TH.All),'','Y')
    d$FR.M = ifelse(is.na(d$FR.M),'','Y')
    d$FR.A = ifelse(is.na(d$FR.A),'','Y')
    d$FR.E = ifelse(is.na(d$FR.E),'','Y')
    d$FR.All = ifelse(is.na(d$FR.All),'','Y')
    d$SA.M = ifelse(is.na(d$SA.M),'','Y')
    d$SA.A = ifelse(is.na(d$SA.A),'','Y')
    d$SA.E = ifelse(is.na(d$SA.E),'','Y')
    d$SA.All = ifelse(is.na(d$SA.All),'','Y')
    d$SU.M = ifelse(is.na(d$SU.M),'','Y')
    d$SU.A = ifelse(is.na(d$SU.A),'','Y')
    d$SU.E = ifelse(is.na(d$SU.E),'','Y')
    d$SU.All = ifelse(is.na(d$SU.All),'','Y')
    
    #Handle .All
    d$MO.M[d$MO.All=='Y'] = 'Y'
    d$MO.A[d$MO.All=='Y'] = 'Y'
    d$MO.E[d$MO.All=='Y'] = 'Y'
    d$TU.M[d$TU.All=='Y'] = 'Y'
    d$TU.A[d$TU.All=='Y'] = 'Y'
    d$TU.E[d$TU.All=='Y'] = 'Y'
    d$WE.M[d$WE.All=='Y'] = 'Y'
    d$WE.A[d$WE.All=='Y'] = 'Y'
    d$WE.E[d$WE.All=='Y'] = 'Y'
    d$TH.M[d$TH.All=='Y'] = 'Y'
    d$TH.A[d$TH.All=='Y'] = 'Y'
    d$TH.E[d$TH.All=='Y'] = 'Y'
    d$FR.M[d$FR.All=='Y'] = 'Y'
    d$FR.A[d$FR.All=='Y'] = 'Y'
    d$FR.E[d$FR.All=='Y'] = 'Y'
    d$SA.M[d$SA.All=='Y'] = 'Y'
    d$SA.A[d$SA.All=='Y'] = 'Y'
    d$SA.E[d$SA.All=='Y'] = 'Y'
    d$SU.M[d$SU.All=='Y'] = 'Y'
    d$SU.A[d$SU.All=='Y'] = 'Y'
    d$SU.E[d$SU.All=='Y'] = 'Y'
    d$MO.All = NULL
    d$TU.All = NULL
    d$WE.All = NULL
    d$TH.All = NULL
    d$FR.All = NULL
    d$SA.All = NULL
    d$SU.All = NULL
    
    #Update first contact preferred times to ALL days/times if not provided at first contact    
    PTColF = which(colnames(d)=='MO.M')
    PTColL = which(colnames(d)=='SU.E')
    d[d$Event=='First Participant Contact' & d$Preferred.Times=='No',PTColF:PTColL] = 'Y'

    
    #Add later preferred times updates to First Contact
    #Add to existing if not all.   Contrain to new times if previously all
    PTColF = which(colnames(d)=='MO.M')
    PTColL = which(colnames(d)=='SU.E')
    for (i in 1:nrow(d))
    {
      #search for a later preferred times
      if ((d$Event[i] == 'Later Participant Contact' | d$Event[i] == 'Lab Contact') && d$Preferred.Times[i] =='Yes')
      {
        ID = d$Phone[i]  #ID for the row that has new preferred times
        FCIndex = which(d$Phone==ID & d$Event=='First Participant Contact' ) #Index of First Contact for this ID
        
        if(d$Preferred.Times[FCIndex]=='No')
        {
          #Set to the new times
          for (c in (PTColF:PTColL) )
          {
            d[FCIndex,c] = d[i,c]
          }
          d$Preferred.Times[FCIndex]='Yes' #so these dont get over-ridden by later updates
        }else
        {
          for (c in (PTColF:PTColL) )
          {
            #Only update if not already set to Y
            if(d[FCIndex,c] !='Y')
            {
              d[FCIndex,c] = d[i,c]
            }
          }        
        }
      }
    }
    
    
    #CREATE SUMMARY TEXT FOR PREFERRED TIMES
    d$Preferred.Times = ''
    for(i in 1:nrow(d))
    {
      if(d$Event[i]=='First Participant Contact')
      {
        d$Preferred.Times[i] = 'MO:'
        if(d$MO.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$MO.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$MO.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-TU:')
        if(d$TU.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$TU.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$TU.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-WE:')
        if(d$WE.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$WE.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$WE.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-TH:')
        if(d$TH.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$TH.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$TH.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-FR:')
        if(d$FR.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$FR.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$FR.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-SA:')
        if(d$SA.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$SA.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$SA.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'-SU:')
        if(d$SU.M[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'m')
        if(d$SU.A[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'a')
        if(d$SU.E[i]=='Y') d$Preferred.Times[i] = paste0(d$Preferred.Times[i],'e')
        
        #Some abbreviations
        if(grepl('MO:mae-TU:mae-WE:mae-TH:mae-FR:mae-SA:mae-SU:mae', d$Preferred.Times[i])) d$Preferred.Times[i] = ''
      }
    }
    
    #Handle participant updates
    i=1
    while (i <= nrow(d))
    {
      if (d$Event[i] =='Update Participant Details')
      {
        ID = d$Phone[i]
        
        if (d$First[i] !='')
        {d$First[d$Phone==ID & d$Event=='First Participant Contact'] =  d$First[i]}
        
        if (d$Last[i] !='')
        {d$Last[d$Phone==ID & d$Event=='First Participant Contact'] =  d$Last[i]}
        if (d$Sex[i] !='')
        {d$Sex[d$Phone==ID & d$Event=='First Participant Contact'] =  d$Sex[i]}
        
        d = d[-i,] #remove the update participant name Event after making correction
      }
      i=i+1
    }
    
    # #Set Call.Notes and Other.Notes to character if no entries (and starts as logical)
    # if(class(d$Call.Notes)=='logical')
    # {
    #   d$Call.Notes = ''
    # }
    # 
    # if(class(d$Other.Notes)=='logical')
    # {
    #   d$Other.Notes = ''
    # }
    
    #Added EXPIRED events
    UniqueIDs = unique(d$Phone)
    
    for(ID in UniqueIDs){
      dID = rdbGetEntries(d,ID)
      
      #Add expired record for participants who havent called us in a while
      #Only check active or Screen Pass/Not Scheduled participants
      if(dID$Status[nrow(dID)]=='Active' | dID$Status[nrow(dID)]=='Screen Pass/Not Scheduled'){
        NextCall = as.Date(dID$Next.Call[nrow(dID)], format='%m/%d/%Y')
        
        dContact = dID[dID$Event=='First Participant Contact'|dID$Event=='Later Participant Contact',]
        LastCall = as.Date(dContact$Event.Date[nrow(dContact)], format='%m/%d/%Y')
        
        LastContact = max(LastCall,NextCall, na.rm=TRUE) #date of last actual or scheduled contact
        if(as.numeric(difftime(Sys.time(),LastContact, units='days'))>DaysInactive){
          d = rbind(d,rep(NA,ncol(d)))
          d$Entry.Date[nrow(d)] = format(Sys.time(), format='%m/%d/%Y')
          d$StaffID[nrow(d)] = 'Auto'
          d$Event.Date[nrow(d)] = format(Sys.time(), format='%m/%d/%Y')
          d$Event[nrow(d)] = 'Expired'
          d$Phone[nrow(d)] = ID
          d$Status[nrow(d)] = 'Inactive'
        }
      }
    }
    
    
    
    #Select and REORDER VARIABLE FOR SENSIBLE DISPLAY
    IncVars = c('Event.Date', 'Entry.Date', 'Event', 'Phone', 'First', 'Last', 'Sex', 
                'Status', 'Next.Call', 'Preferred.Times', 'Call.Notes', 'Other.Notes',
                'MO.M', 'TU.M', 'WE.M', 'TH.M', 'FR.M', 'SA.M', 'SU.M', 
                'MO.A', 'TU.A', 'WE.A', 'TH.A', 'FR.A', 'SA.A', 'SU.A',
                'MO.E', 'TU.E', 'WE.E', 'TH.E', 'FR.E', 'SA.E', 'SU.E', 
                'StaffID')
    d = d[,IncVars]
  }
  
  if (nrow(d) ==0) d=NULL  #set to NULL if d is empty
  return(d)
}