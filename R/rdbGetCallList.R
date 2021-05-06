rdbGetCallList = function(d, Filter = 'DateDayTime')
{
  if (!is.null(d) && nrow(d >0))
  {
    UniqueIDs = unique(d$Phone)
    CallIDs = NULL
    #Loop through all IDs to create CallIDs
    for (ID in UniqueIDs)
    {
      dID = rdbGetEntries(d, Phone=ID)
      
      #delete entries that are not active or screen pass/not scheduled
      if(!(dID$Status[nrow(dID)] == 'Active' | dID$Status[nrow(dID)] == 'Screen Pass/Not Scheduled'))
      {
        d = d[d$Phone != ID,]
        next  #skip to next ID
      }
      
      #delete entries with Next.Call for later date unless Filter = None
      if(Filter !='None')
        {
        Today = format(Sys.time(), format = '%m/%d/%Y')
        if (difftime(as.POSIXct(Today, format='%m/%d/%Y', tz='America/Chicago'), as.POSIXct(dID$Next.Call[nrow(dID)], format='%m/%d/%Y', tz='America/Chicago'))<0)
        {
          d = d[d$Phone != ID,]
          next  #skip to next ID
        }
      }
      
      #Delete entries that dont have a preferred day match with Sys.time() if Filter = DateDay
      if(Filter =='DateDay')
      {
        
        dPT = dID[dID$Event=='First Participant Contact',]
        if (!dPT$Preferred.Times=='')  #Only check to delete if have preferred times
        {
          #check here if Monday
          if(weekdays(Sys.time())=='Monday')
          {
            #No monday times preferred.
            if(dPT$MO.M=='' && dPT$MO.A=='' && dPT$MO.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }
          
          #check here if Tuesday
          if(weekdays(Sys.time())=='Tuesday')
          {
            #No tuesday times preferred.
            if(dPT$TU.M=='' && dPT$TU.A=='' && dPT$TU.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }
          
          #check here if Wednesday
          if(weekdays(Sys.time())=='Wednesday')
          {
            #No Wednesday times preferred
            if(dPT$WE.M=='' && dPT$WE.A=='' && dPT$WE.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }
          
          #check here if Thursday
          if(weekdays(Sys.time())=='Thursday')
          {
            #No Thursday times preferred.
            if(dPT$TH.M=='' && dPT$TH.A=='' && dPT$TH.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }
          
          #check here if Friday
          if(weekdays(Sys.time())=='Friday')
          {
            #No friday times preferred.
            if(dPT$FR.M=='' && dPT$FR.A=='' && dPT$FR.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }   
          
          #check here if Saturday
          if(weekdays(Sys.time())=='Monday')
          {
            #No saturday times preferred.
            if(dPT$SA.M=='' && dPT$SA.A=='' && dPT$SA.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }   
          
          #check here if Sunday
          if(weekdays(Sys.time())=='Sunday')
          {
            #No sunday times preferred.
            if(dPT$SU.M=='' && dPT$SU.A=='' && dPT$SU.E=='')
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
          }          
        }        
      }
      
      #Delete entries that dont have a preferred day and time match with Sys.time() Filter =DateDayTime
      if (Filter =='DateDayTime')
      {
        dPT = dID[dID$Event=='First Participant Contact',]
        if (!dPT$Preferred.Times=='')  #Only check to delete if have preferred times
        {
          #check here if Monday
          if(weekdays(Sys.time())=='Monday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$MO.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
  
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$MO.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$MO.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }
          
          #check here if Tuesday
          if(weekdays(Sys.time())=='Tuesday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$TU.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$TU.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$TU.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }
          
          #check here if Wednesday
          if(weekdays(Sys.time())=='Wednesday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$WE.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$WE.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$WE.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }
  
          #check here if Thursday
          if(weekdays(Sys.time())=='Thursday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$TH.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$TH.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$TH.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }        
          
          #check here if Friday
          if(weekdays(Sys.time())=='Friday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$FR.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$FR.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$FR.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }       
          
          #check here if Saturday
          if(weekdays(Sys.time())=='Saturday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$SA.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$SA.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$SA.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }        
          
          #check here if Sunday
          if(weekdays(Sys.time())=='Sunday')
          {
            #morning not preferred and systime < 12:00
            if(dPT$SU.M=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }
            
            #midday not preferred and systime > 12:00 & < 17:00
            if(dPT$SU.A=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('12:00', format='%H:%M'))>0 &&
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))<0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
            
            #evening not preferred and systime > 17:00
            if(dPT$SU.E=='' && 
               difftime(as.POSIXct(format(Sys.time(),format='%H:%M'), format='%H:%M'),as.POSIXct('17:00', format='%H:%M'))>0)
            {
              d = d[d$Phone != ID,]
              next  #skip to next ID
            }          
          }        
        }
      }
  
      CallIDs = c(CallIDs,ID)  #Survived so this is an ID to call
    }
    
    if(length(CallIDs)>0) #to create dC
    {
      dC = data.frame(Phone = rep('',length(CallIDs)), First = rep('',length(CallIDs)), Last = rep('',length(CallIDs)), 
                      Sex =rep('',length(CallIDs)), Status = rep('',length(CallIDs)), Next.Call = rep('',length(CallIDs)), 
                      Days.Entered = rep(NA,length(CallIDs)), Days.Participant.Call = rep(NA,length(CallIDs)), Days.Lab.Call = rep(NA,length(CallIDs)), 
                      Num.Participant.Calls = rep(NA,length(CallIDs)), Num.Lab.Calls = rep(NA,length(CallIDs)),
                      Preferred.Times = rep('',length(CallIDs)), Call.Notes = rep('',length(CallIDs)),
                      stringsAsFactors=FALSE)
      
      for(i in 1:length(CallIDs))
      {
        ID = CallIDs[i]
        dID = rdbGetEntries(d, Phone=ID)
        dC$Phone[i]= dID$Phone[dID$Event=='First Participant Contact']
        dC$First[i]= dID$First[dID$Event=='First Participant Contact']
        dC$Last[i]= dID$Last[dID$Event=='First Participant Contact']
        dC$Sex[i]= dID$Sex[dID$Event=='First Participant Contact']
        dC$Status[i]= dID$Status[nrow(dID)]
        dC$Next.Call[i]= dID$Next.Call[nrow(dID)]
        
        Today = format(Sys.time(), format = '%m/%d/%Y')
        dC$Days.Entered[i] = round(difftime(as.POSIXct(Today, format='%m/%d/%Y', tz='America/Chicago'), as.POSIXct(dID$Entry.Date[dID$Event=='First Participant Contact'], format='%m/%d/%Y', tz='America/Chicago'), units='days'))
        dPC = dID[(dID$Event =='First Participant Contact'| dID$Event=='Later Participant Contact'),]
        dC$Days.Participant.Call[i] = round(difftime(as.POSIXct(Today, format='%m/%d/%Y', tz='America/Chicago'), as.POSIXct(dPC$Entry.Date[nrow(dPC)], format='%m/%d/%Y', tz='America/Chicago'), units='days'))
        dC$Num.Participant.Calls[i] = nrow(dPC)
        
        dLC = dID[dID$Event =='Lab Contact',]
        if(nrow(dLC)>0)
        {
          dC$Days.Lab.Call[i] = round(difftime(as.POSIXct(Today, format='%m/%d/%Y', tz='America/Chicago'), as.POSIXct(dLC$Entry.Date[nrow(dLC)], format='%m/%d/%Y', tz='America/Chicago'), units='days'))
        }else
        {
          dC$Days.Lab.Call[i] = NA
        }
        dC$Num.Lab.Calls[i] = nrow(dLC)
        
        dC$Preferred.Times[i] = dID$Preferred.Times[dID$Event=='First Participant Contact']
        
        #Add most recent call.note by looping from first participant entry forward
        dC$Call.Notes[i] = ''
        for(j in 1:nrow(dID))
        {
          if(dID$Call.Notes[j] !='')
          {
            dC$Call.Notes[i] = dID$Call.Notes[j]
          }
          
        }
      }
    } else
      {
        dC = data.frame(Phone = '', First = '', Last = '', 
                        Sex ='', Status = '', Next.Call = '', 
                        Days.Entered = '', Days.Participant.Call = '', Days.Lab.Call = '', 
                        Num.Participant.Calls = '', Num.Lab.Calls = '', stringsAsFactors=FALSE)
      }
  } else
  {
    dC = data.frame(Phone = '', First = '', Last = '', 
                    Sex ='', Status = '', Next.Call = '', 
                    Days.Entered = '', Days.Participant.Call = '', Days.Lab.Call = '', 
                    Num.Participant.Calls = '', Num.Lab.Calls = '', stringsAsFactors=FALSE)
  }
  
  return(dC)
}