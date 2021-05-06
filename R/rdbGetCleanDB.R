rdbGetCleanDB= function(SurveyID, DaysInactive=30)
#Downloads qualtrics survey and cleans/formats it for subsequent use
{
    d = rdbGetRawDB(SurveyID = SurveyID)
 #   d = rdbVerifyDB(d)
    d = rdbCleanDB(d, DaysInactive=DaysInactive)
    return(d)
}