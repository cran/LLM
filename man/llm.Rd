% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm.R
\name{llm}
\alias{llm}
\title{Create Logit Leaf Model}
\usage{
llm(X, Y, threshold_pruning = 0.25, nbr_obs_leaf = 100)
}
\arguments{
\item{X}{Dataframe containing numerical independent variables.}

\item{Y}{Numerical vector of dependent variable. Currently only binary classification is supported.}

\item{threshold_pruning}{Set confidence threshold for pruning. Default 0.25.}

\item{nbr_obs_leaf}{The minimum number of observations in a leaf node. Default 100.}
}
\value{
An object of class logitleafmodel, which is a list with the following components:
\item{Segment Rules}{The decision rules that define segments. Use \code{\link{table.llm.html}} to visualize.}
\item{Coefficients}{The segment specific logistic regression coefficients. Use \code{\link{table.llm.html}} to visualize.}
\item{Full decision tree for segmentation}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
\item{Observations per segment}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
\item{Incidence of dependent per segment}{The raw decision tree. Use \code{\link{table.llm.html}} to visualize.}
}
\description{
This function creates the logit leaf model. It takes a dataframe with numeric
values as input and a corresponding vector with dependent values.
Decision tree parameters threshold for pruning and number of observations per
leaf can be set.
}
\examples{
## Load PimaIndiansDiabetes dataset from mlbench package
if (requireNamespace("mlbench", quietly = TRUE)) {
  library("mlbench")
}
data("PimaIndiansDiabetes")
## Split in training and test (2/3 - 1/3)
idtrain <- c(sample(1:768,512))
PimaTrain <-PimaIndiansDiabetes[idtrain,]
Pimatest <-PimaIndiansDiabetes[-idtrain,]
## Create the LLM
Pima.llm <- llm(X = PimaTrain[,-c(9)],Y = PimaTrain$diabetes,
 threshold_pruning = 0.25,nbr_obs_leaf = 100)

}
\references{
Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
}
\seealso{
\code{\link{predict.llm}}, \code{\link{table.llm.html}}, \code{\link{llm.cv}}
}
\author{
Arno De Caigny, \email{a.de-caigny@ieseg.fr}, Kristof Coussement, \email{k.coussement@ieseg.fr} and Koen W. De Bock, \email{kdebock@audencia.com}
}
