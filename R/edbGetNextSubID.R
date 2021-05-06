edbGetNextSubID= function(SurveyID)
  #Gets updated copy of clean database, prints next ID and then returns updated database
{
    d = edbGetCleanDB(SurveyID)
    
    if(nrow(d)>0){
      NextID = max(d$SubID)+1
    }else{
      NextID = 1
    }
    
    print(str_c('Next Available SubID: ', NextID))
    return(d)
}