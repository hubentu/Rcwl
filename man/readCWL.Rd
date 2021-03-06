% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCWL.R
\name{readCWL}
\alias{readCWL}
\title{Read CWL
Function to read CWL command or workflow files.}
\usage{
readCWL(cwlfile)
}
\arguments{
\item{cwlfile}{The cwl file to read.}
}
\value{
A object of class `cwlProcess` or `cwlWorkflow`.
}
\description{
Read CWL
Function to read CWL command or workflow files.
}
\examples{
input1 <- InputParam(id = "sth")
echo <- cwlProcess(baseCommand = "echo",
                 inputs = InputParamList(input1))
tf <- writeCWL(echo)
readCWL(tf[1])
}
