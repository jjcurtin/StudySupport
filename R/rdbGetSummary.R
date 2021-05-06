rdbGetSummary = function(d, FullFileName)
{

  if (!is.null(d) && nrow(d >0))
  {
    figNewDevice(Width=11,Height=8.5, Type='pdf', File=FullFileName)   #default is for a window/mac window.  can also use tiff, or pdf as Type
    
    ####################################################
    #Summary stats -----------------------------------------
    #Calculate contact summary stats
    FirstContact = nrow(d[d$Event=="First Participant Contact",])
    LabContact = nrow(d[d$Event=="Lab Contact",])
    LaterContact = nrow(d[d$Event=="Later Participant Contact",])
    
    #Calculate status summary stats
    UniqueIDs = unique(d$Phone)
    TotalSubs = length(UniqueIDs)
    TotalActive = 0
    TotalPassNotScheduled = 0
    TotalPassScheduled = 0
    TotalScreenFail = 0
    TotalInactive = 0
    
    #Loop through all IDs
    for (ID in UniqueIDs){
      dID = rdbGetEntries(d, Phone=ID)
      Status = dID$Status[nrow(dID)]
      if(Status == 'Active') TotalActive=TotalActive+1
      if(Status == 'Screen Pass/Not Scheduled') TotalPassNotScheduled=TotalPassNotScheduled+1
      if(Status == 'Screen Pass/Scheduled') TotalPassScheduled=TotalPassScheduled+1
      if(Status == 'Screen Fail') TotalScreenFail=TotalScreenFail+1
      if(Status == 'Inactive') TotalInactive=TotalInactive+1
    }      
  
    
    figPlotRegion(x=c(1,10),y=c(1,10))
    figText(5.5,10, 'Summary Statistics for Recruiting Database', text.font = 2,  text.adj = c(.5,.5))

    figText(1,8.5, 'Status Categories', text.font = 2, text.adj = c(0,.5))

    figText(1,7.5, sprintf('  Total Participants:'), text.font = 1, text.adj = c(0,.5))
    figText(4,7.5, sprintf('%i', TotalSubs), text.font = 1, text.adj = c(1,.5))

    figText(1,7, sprintf('  Active:'), text.font = 1, text.adj = c(0,.5))
    figText(4,7, sprintf('%i', TotalActive), text.font = 1, text.adj = c(1,.5))

    figText(1,6.5, sprintf('  Inactive:'), text.font = 1, text.adj = c(0,.5))
    figText(4,6.5, sprintf('%i', TotalInactive), text.font = 1, text.adj = c(1,.5))

    figText(1,6, sprintf('  Screen Fail:'), text.font = 1, text.adj = c(0,.5))
    figText(4,6, sprintf('%i', TotalScreenFail), text.font = 1, text.adj = c(1,.5))


    figText(1,5.5, sprintf('  Screen Pass'),  text.font = 1,text.adj = c(0,.5))
    figText(1,5, sprintf('     NOT Scheduled:'),  text.font = 1,text.adj = c(0,.5))
    figText(4,5, sprintf('%i', TotalPassNotScheduled),  text.font = 1,text.adj = c(1,.5))

    figText(1,4.5, sprintf('     Scheduled:'), text.font = 1, text.adj = c(0,.5))
    figText(4,4.5, sprintf('%i', TotalPassScheduled), text.font = 1, text.adj = c(1,.5))


    figText(1,3, 'Total Calls', text.font = 2, text.adj = c(0,.5))

    figText(1,2, sprintf('  First Contact:'), text.font = 1, text.adj = c(0,.5))
    figText(4,2, sprintf('%i', FirstContact), text.font = 1, text.adj = c(1,.5))

    figText(1,1.5, sprintf('  Lab Contacts:'), text.font = 1, text.adj = c(0,.5))
    figText(4,1.5, sprintf('%i', LabContact), text.font = 1, text.adj = c(1,.5))

    figText(1,1, sprintf('  Later Participant Contacts:'), text.font = 1, text.adj = c(0,.5))
    figText(4,1, sprintf('%i', LaterContact), text.font = 1, text.adj = c(1,.5))


    #################################################
    # Setup data frame for all weekly graphs
    d$Event.Date = as.POSIXct(d$Event.Date, format='%m/%d/%Y')
    FirstWeek = min(d$Event.Date)+days(6) #end of first week in database
    LastWeek = max(d$Event.Date)  #last day in database
    nWeeks = ceiling(as.numeric(LastWeek-FirstWeek)/7)
    dW = data.frame(Date = seq(as.Date(FirstWeek), as.Date(LastWeek)+6, by='weeks'), 
                         nActive=0, nInactive=0, nScreenFail=0, nScreenPassNotSch=0, nScreenPassSch=0, nFirst=0, nLater=0, nLab=0, totFirst=0, totPassScheduled=0)
    
    
    #################################################
    #Active cases by week graph ---------------------------
    
    #Loop through all IDs
    UniqueIDs = unique(d$Phone)
    for (ID in UniqueIDs){
      dID = rdbGetEntries(d, Phone=ID)
      #print(ID)
      for (i in 1:nrow(dW))
      {
        #consider all entries to date b/c active participants might not have an entry that week
        dIDWeek = dID[dID$Event.Date<=dW$Date[i],]
        
        if(nrow(dIDWeek)>0)
        {
          Status = dIDWeek$Status[nrow(dIDWeek)]
          if(Status=='Active') dW[i,'nActive'] = dW[i,'nActive']+1
        }
      }
    }  
    
    #The plot
    YMax = max(dW[,'nActive'])+1
    XMax = nrow(dW)
    figPlotRegion(x=c(1,nrow(dW)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, '# Active cases by Week', text.font = 2,  text.adj = c(.5,.5))
    
    figLines(1:nrow(dW),dW$nActive)
    figPoints(1:nrow(dW),dW$nActive, points.pch=1, points.cex=1.5 )
    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dW),scale.text=dW$Date)
    figAxis(side=2,lab.text='N')
    figLegend('topleft', legend=c('Active'),
              pch=c(1), border='white')
    
    
    #################################################
    #Dispositions (i.e. cases closed) by week graph ---------------------------
    
    #Loop through all IDs
    UniqueIDs = unique(d$Phone)
    for (ID in UniqueIDs)
    {
      dID = rdbGetEntries(d, Phone=ID)
      #print(ID)
      for (i in 1:nrow(dW))
      {

        #consider entries only in this week
        dIDWeek = dID[dID$Event.Date<=dW$Date[i],]
        if (i>1){ #starting with second entry check that date is greater than end of previous week
          dIDWeek = dIDWeek[dIDWeek$Event.Date>dW$Date[i-1],]  
        }  
        
        if(nrow(dIDWeek)>0)
        {
          Status = dIDWeek$Status[nrow(dIDWeek)]
          if(Status=='Inactive') dW[i,'nInactive'] = dW[i,'nInactive']+1
          if(Status=='Screen Fail') dW[i,'nScreenFail'] = dW[i,'nScreenFail']+1
          if(Status=='Screen Pass/Not Scheduled') dW[i,'nScreenPassNotSch'] = dW[i,'nScreenPassNotSch']+1
          if(Status=='Screen Pass/Scheduled') dW[i,'nScreenPassSch'] = dW[i,'nScreenPassSch']+1
        }
      }
    }  
    
    #The plot
    YMax = max(dW[,c('nInactive', 'nScreenFail', 'nScreenPassNotSch', 'nScreenPassSch')])+1
    XMax = nrow(dW)
    figPlotRegion(x=c(1,nrow(dW)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, 'Participant Dispositions (i.e., closed) by Week', text.font = 2,  text.adj = c(.5,.5))
    
    figLines(1:nrow(dW),dW$nInactive)
    figPoints(1:nrow(dW),dW$nInactive, points.pch=4, points.cex=1.5 )
    figLines(1:nrow(dW),dW$nScreenFail)
    figPoints(1:nrow(dW),dW$nScreenFail, points.pch=3, points.cex=1.5 )
    figLines(1:nrow(dW),dW$nScreenPassNotSch)
    figPoints(1:nrow(dW),dW$nScreenPassNotSch, points.pch=5, points.cex=1.5 )
    figLines(1:nrow(dW),dW$nScreenPassSch)
    figPoints(1:nrow(dW),dW$nScreenPassSch, points.pch=2, points.cex=1.5 )

    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dW),scale.text=dW$Date)
    figAxis(side=2,lab.text='N')
    figLegend('topleft', legend=c('Inactive', 'Screen Fail', 'Screen Pass/Not Scheduled', 'Screen Pass/Scheduled'),
              pch=c(4,3,5,2), border='white')

    #################################################
    #Cumulative First contacts vs. Scheduled by week graph ---------------------------
    
    #Loop through all IDs
    UniqueIDs = unique(d$Phone)
    for (ID in UniqueIDs){
      dID = rdbGetEntries(d, Phone=ID)
      #print(ID)
      for (i in 1:nrow(dW)) {
        #consider all entries to date 
        dIDWeek = dID[dID$Event.Date<=dW$Date[i],]
        
        if(nrow(dIDWeek)>0){
          dW$totFirst[i] = dW$totFirst[i] + 1  #if there are any entries, count a first contact
          Status = dIDWeek$Status[nrow(dIDWeek)]
          if(Status=='Screen Pass/Scheduled') dW$totPassScheduled[i] = dW$totPassScheduled[i] + 1
        }
      }
    }  
    
    #The plot
    YMax = max(dW[,c('totFirst', 'totPassScheduled')])+1
    XMax = nrow(dW)
    figPlotRegion(x=c(1,nrow(dW)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, 'Cumulative First Contacts vs. Scheduled by Week', text.font = 2,  text.adj = c(.5,.5))
    
    figLines(1:nrow(dW),dW$totFirst)
    figPoints(1:nrow(dW),dW$totFirst, points.pch=1, points.cex=1.5 )
    figLines(1:nrow(dW),dW$totPassScheduled)
    figPoints(1:nrow(dW),dW$totPassScheduled, points.pch=2, points.cex=1.5 )
    
    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dW),scale.text=dW$Date)
    figAxis(side=2,lab.text='N')
    figLegend('topleft', legend=c('First Participant Contacts', 'Screen Pass/Scheduled'),
              pch=c(1,2), border='white')
    
    

    
    #################################################
    #Calls by week graph ---------------------------
    
    #Loop through all IDs
    UniqueIDs = unique(d$Phone)
    for (ID in UniqueIDs){
      dID = rdbGetEntries(d, Phone=ID)

      for (i in 1:nrow(dW))
      {

        dIDWeek = dID[dID$Event.Date<=dW$Date[i],]
        if (i>1){ #starting with second entry check that date is greater than end of previous week
          dIDWeek = dIDWeek[dIDWeek$Event.Date>dW$Date[i-1],]  
        }
        if(nrow(dIDWeek)>0){
          dW$nFirst[i] = dW$nFirst[i] + sum(dIDWeek$Event=='First Participant Contact')
          dW$nLater[i] = dW$nLater[i] + sum(dIDWeek$Event=='Later Participant Contact')
          dW$nLab[i] = dW$nLab[i] + sum(dIDWeek$Event=='Lab Contact')
        }
      }
    }  
    
    #The plot
    YMax = max(dW[,7:9])+1
    XMax = nrow(dW)
    figPlotRegion(x=c(1,nrow(dW)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, 'Contacts by Week', text.font = 2,  text.adj = c(.5,.5))

    figLines(1:nrow(dW),dW$nFirst)
    figPoints(1:nrow(dW),dW$nFirst, points.pch=1, points.cex=1.5 )
    figLines(1:nrow(dW),dW$nLater)
    figPoints(1:nrow(dW),dW$nLater, points.pch=2, points.cex=1.5 )
    figLines(1:nrow(dW),dW$nLab)
    figPoints(1:nrow(dW),dW$nLab, points.pch=5, points.cex=1.5 )
    
    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dW),scale.text=dW$Date)
    figAxis(side=2,lab.text='N')
    figLegend('topleft', legend=c('First Participant Contact', 'Later Participant Contact', 'Lab Contact'),
              pch=c(1,2,5), border='white')
    
    #################################################
    #StaffID by week graph ---------------------------
    d$StaffID[d$StaffID == '' | is.na(d$StaffID)] = 'Unknown'
    UniqueStaff = unique(d$StaffID)
    UniqueStaff = UniqueStaff[UniqueStaff !='Auto']  #remove Auto entries for timed out
    dW[,UniqueStaff] = 0
    
    
    #Loop through all StaffID
    for (Staff in UniqueStaff){
      dStaff = d[d$StaffID==Staff,]
      #print(ID)
      for (i in 1:nrow(dW)){
        #print(dW$Date[i])
        dStaffWeek = dStaff[dStaff$Event.Date<=dW$Date[i],]
        if (i>1){ #starting with second entry check that date is greater than end of previous week
          dStaffWeek = dStaffWeek[dStaffWeek$Event.Date>dW$Date[i-1],]  
        }
        if(nrow(dStaffWeek)>0){
          dW[i,Staff] = dW[i,Staff] + nrow(dStaffWeek)
        }
      }
    }  
    
    #The plot
    YMax = max(dW[,UniqueStaff])+1
    XMax = nrow(dW)
    figPlotRegion(x=c(1,nrow(dW)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, 'Staff Activity by Week', text.font = 2,  text.adj = c(.5,.5))
  

        
    for(Staff in UniqueStaff){
      figLines(1:nrow(dW),dW[,Staff])
      figPoints(1:nrow(dW),dW[,Staff], points.pch=which(Staff==UniqueStaff), points.cex=1.5 )
    }
    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dW),scale.text=dW$Date)
    figAxis(side=2,lab.text='N')
    figLegend('topleft', legend=UniqueStaff,
              pch=1:length(UniqueStaff), border='white')
    
    
    
###########################################    
    #Mean response delay to first contact ---------------------------
    
    #Loop through all IDs
    dDelay = data.frame(Date = dW$Date, Delay1=0,N1=0, DelayAll=0, NAll=0, MeanDelay1=0, MeanDelayAll=0)
    UniqueIDs = unique(d$Phone)
    for (ID in UniqueIDs){
      dID = rdbGetEntries(d, Phone=ID)
      #message(ID)
      for (i in 1:nrow(dW)){
        dContactsWeek = dID[dID$Event.Date<=dW$Date[i] & 
                           (dID$Event== 'First Participant Contact' | dID$Event== 'Later Participant Contact'),]
 
        if(nrow(dContactsWeek)>0){
          
          #Count first contacts
          DateFirstParticipant = dContactsWeek$Event.Date[dContactsWeek$Event == 'First Participant Contact']
          DateFirstLab = dID$Event.Date[dID$Event == 'Lab Contact'][1]

          if(!(length(DateFirstParticipant)<1 || length(DateFirstLab)<1 || is.na(DateFirstParticipant) || is.na(DateFirstLab))){  
            dDelay$N1[i] = dDelay$N1[i]+1
            dDelay$Delay1[i] = dDelay$Delay1[i]+as.integer(difftime(DateFirstLab,DateFirstParticipant, units='days'))
          }
          
          #count all contacts
          for (j in 1:nrow(dContactsWeek)){
            DateParticipant = dContactsWeek$Event.Date[j]
            DatesLab = dID$Event.Date[dID$Event == 'Lab Contact']
            FutureDatesLab = DatesLab[DatesLab>=DateParticipant]
            if(length(FutureDatesLab)>0){
              dDelay$NAll[i] = dDelay$NAll[i]+1
              dDelay$DelayAll[i] = dDelay$DelayAll[i]+as.integer(difftime(FutureDatesLab[1],DateParticipant, units='days'))
            }
          }
        }
      }
    }  
    
    #The plot
    dDelay$MeanDelay1 = dDelay$Delay1/dDelay$N1
    dDelay$MeanDelayAll = dDelay$DelayAll/dDelay$NAll
    YMax = max(dDelay[,c('MeanDelay1', 'MeanDelayAll')])+1
    XMax = nrow(dDelay)
    figPlotRegion(x=c(1,nrow(dDelay)),y=c(0,YMax))
    figText((1+XMax)/2,YMax, 'Mean Callback Delay for Participant Contacts', text.font = 2,  text.adj = c(.5,.5))
    
    figLines(1:nrow(dDelay),dDelay$MeanDelay1)
    figPoints(1:nrow(dDelay),dDelay$MeanDelay1, points.pch=1, points.cex=1.5 )
    
    figLines(1:nrow(dDelay),dDelay$MeanDelayAll)
    figPoints(1:nrow(dDelay),dDelay$MeanDelayAll, points.pch=2, points.cex=1.5 )
    
    figAxis(side=1,lab.text='Week Ending', scale.at=1:nrow(dDelay),scale.text=dDelay$Date)
    figAxis(side=2,lab.text='Mean Days to Callback')
    figLegend('topleft', legend=c('First Participant Contact', 'All Participant Contacts'),
              pch=c(1,2), border='white')
    
    
##################################################################
# Clean up --------------------------------------------------------
    
    dev.off()
  }
  else stop('Recruiting database does not have any entries')
  
}