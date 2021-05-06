rdbGetID= function(d, Last)
{
  dFirst = d[d$Event=='First Participant Contact',]

  d=dFirst[grep(Last, dFirst$Last),c('Phone', 'First', 'Last', 'Sex' )]
  
  return(d)
}