edbGetCleanDB= function(SurveyID)
{
    d = edbGetRawDB(SurveyID = SurveyID)
    d = edbCleanDB(d)
    return(d)
}