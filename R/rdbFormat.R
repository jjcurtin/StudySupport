rdbFormat = function(d, Verbose=FALSE)
{
  
  #if events database
  if(any(names(d) == 'Event.Date')){

    #select out columns
    if(!Verbose){
      d = d[,c("Event.Date", "Event", "Phone", "First", "Last", "Status", "Next.Call", "Preferred.Times", "Call.Notes", "Other.Notes")]
    }
  }

  #if call list
  if(any(names(d)=='Days.Entered')){

    #select out columns
    if(!Verbose){
      d$Sex = NULL
    }
  }
  return(d)
}