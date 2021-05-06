rdbVerifyDB= function(d)
  #Checks the integrity of the raw DB for known/common errors
  #Issues warnings and removes participants with bad records
{
  #loop through each phone ID and do various checks
  
  IDs = unique(d$Q3)  #Phone.Number
  
  for(ID in IDs){
    dID = d[d$Q3 == ID,]
    
    #confirm that first entry is 'First Participant Contact'
    if (dID$Q2[1] !='First Participant Contact'){
      warning(sprintf('ID: %s is malformed.  FIRST PARTICIPANT CONTACT entry is misplaced or missing.  
                      Entries for this ID are displayed and have been temporarily removed from database', dID$Q3[1]))
      View(dID)
      d = d[d$Q3!=dID$Q3[1],]
      next
    }
    
    #confirm that only ONE entry is 'First Participant Contact'
    dFirst = dID[dID$Q2=='First Participant Contact',]
    if (nrow(dFirst) > 1)
    {
      warning(sprintf('ID: %s is malformed. Multiple FIRST PARTICIPANT CONTACT entries detected.  
                      Entries for this ID are displayed and have been temporarily removed', dID$Q3[1]))
      View(dID)
      d = d[d$Q3!=dID$Q3[1],]
      next
    }
    
    #Confirm that not reset to active following screen pass/fail
    if (any(grepl('Screen', dID$Q4)) & dID$Q4[nrow(dID)]=='Active')
    {
      warning(sprintf('ID: %s is malformed. Status reset to ACTIVE following SCREEN.  
                      Entries for this ID are displayed and have been temporarily removed', dID$Q3[1]))      
      View(dID)
      d = d[d$Q3!=dID$Q3[1],]
      next
    }
  }
  return(d)
}