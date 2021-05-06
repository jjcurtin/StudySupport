rdbGetEntries= function(d = NULL, Phone = NULL)
{

  #return by phone
  if(!is.null(Phone)) {
    dE = d[d$Phone == Phone,]
  }else {
    dE = data.frame(NULL)
  }
  
  return(dE)
}