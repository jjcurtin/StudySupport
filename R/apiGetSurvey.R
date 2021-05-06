#apiGetSurvey()------------------------------
#Export a survey and download into R
#
#Export a qualtrics survey you own and import the survey directly into R.
#NOTE:  This function is a simplified version of code 
#from Jasper Ginn's qualtRics package
#because we had some problems with the the implementation some of the more complex
#aspects of his code that were unnecessary for our usage.
#
#This function uses utilites that are contained in QualtricsUtils.R

apiGetSurvey = function(SurveyID,UseLabels = TRUE,
                        APIToken='et9mgCBpiPT402404Rs0IvRbjG9goUiR0klGcACX', 
                        RootURL='https://uwmadison.co1.qualtrics.com') {
  
  # add endpoint to RootURL
  RootURL = str_c(RootURL,
                  ifelse(substr(RootURL, nchar(RootURL),
                                nchar(RootURL)) == '/',
                         'API/v3/responseexports/',
                         '/API/v3/responseexports/'))
  
  
  # Create raw JSON payload
  RawPayLoad = createRawPayload(surveyID = SurveyID, useLabels=UseLabels)
  
  # POST request for download
  res = qualtricsAPIRequest(APIToken, "POST", URL=RootURL, body = RawPayLoad)
  
  # Get ID
  if(is.null(res$result$id)) {
    if(is.null(res$content[[1]]$id)) {
      stop("Something went wrong. Please re-run your query.")
    } else{
      ID = res$content[[1]]$id
    }
  } else{
    ID = res$result$id
  } # NOTE This is not fail safe because ID can still be NULL
  
  # This is the url to use when checking the ID
  CheckURL = str_c(RootURL, ID)
  
  # Download, unzip and return file path
  SurveyPath = downloadQualtricsExport(CheckURL, APIToken)
  
  # Read data
  dSurvey = readSurvey(SurveyPath)
  p= file.remove(SurveyPath)
  return(dSurvey)
}
