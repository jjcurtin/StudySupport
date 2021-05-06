\name{apiGetSurvey}
\alias{apiGetSurvey}
\title{Download Qualtrics Survey}
\description{
Downloads a Quatrics Survey via their API.  This code was originally written by
Jasper Ginn and included in his qualtRics package.  It was simplfied for our
specific use case.
}
\usage{apiGetSurvey(SurveyID,UseLabels = TRUE,
                        APIToken='et9mgCBpiPT402404Rs0IvRbjG9goUiR0klGcACX', 
                        RootURL='https://uwmadison.co1.qualtrics.com')}
\arguments{
  \item{SurveyID}{Qualtrics survey ID as string}
  \item{UseLabels}{Use data response labels vs. data response values;  Default to TRUE}
  \item{APIToken}{Qualtrics API token;  Defaults to ours!}
  \item{RootURL}{Qualtrics Root URL for UW Madison}
}
\value{Returns a dataframe}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}