\name{rdbGetCleanDB}
\alias{rdbGetCleanDB}
\title{Download a clean Qualtrics recruiting database}
\description{
Downloads, cleans, and verifies a Qualtrics recruiting database
}
\usage{rdbGetCleanDB(SurveyID, DaysInactive=30)}
\arguments{
  \item{SurveyID}{Qualtrics SurveyID}
  \item{DaysInactive}{Status change to inactive if days 
  since last participant contact > DaysInactive}
}
\value{returns a cleaned dataframe including all events}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}