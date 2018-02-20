#' Create Logit Leaf Model Prediction
#'
#' This function creates a prediction for an object of class logitleafmodel. It assumes a dataframe with numeric
#' values as input and an object of class logitleafmodel, which is the result of the \code{\link{llm}} function.
#' Currently only binary classification is supported.
#'
#' @param X Dataframe containing numerical independent variables.
#' @param object An object of class logitleafmodel, as that created by the function llm.
#' @param addrownumbers Boolean to add row numbers in output.
#' @param ... further arguments passed to or from other methods.
#' @return Returns a dataframe containing a probablity for every instance based on the LLM model. Optional rownumbers can be added.
#' @export
#' @importFrom stats predict
#' @references Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
#' @author Arno De Caigny, \email{a.de-caigny@@ieseg.fr}, Kristof Coussement, \email{k.coussement@@ieseg.fr} and Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{llm}}, \code{\link{table.llm.html}}, \code{\link{llm.cv}}
#' @examples
#' ## Load PimaIndiansDiabetes dataset from mlbench package
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   library("mlbench")
#' }
#' data("PimaIndiansDiabetes")
#' ## Split in training and test (2/3 - 1/3)
#' idtrain <- c(sample(1:768,512))
#' PimaTrain <-PimaIndiansDiabetes[idtrain,]
#' Pimatest <-PimaIndiansDiabetes[-idtrain,]
#' ## Create the LLM
#' Pima.llm <- llm(X = PimaTrain[,-c(9)],Y = PimaTrain$diabetes,
#'  threshold_pruning = 0.25,nbr_obs_leaf = 100)
#' ## Use the model on the test dataset to make a prediction
#' PimaPrediction <- predict.llm(object = Pima.llm, X = Pimatest[,-c(9)])
#' ## Optionally add the dependent to calculate performance statistics such as AUC
#' # PimaPrediction <- cbind(PimaPrediction, "diabetes" = Pimatest[,"diabetes"])
#'@export predict.llm
#'

predict.llm <- function(object, X, addrownumbers=TRUE, ...){
  # Custom fucntions that are used in the LLM function
  # _1_ trim.leading
  # _2_ ceiling.dec

  # _1_ trim.leading
  # Trim leading white spaces
  trim.leading <- function (x)  sub("^\\s+", "", x)

  # _2_ ceiling.dec
  # Round numerics at certain decimals ceiling
  ceiling.dec <- function(x,level=1) round(x+5*10^(-level-1),level)


  # Give the dataframe generic name (necessary to split the rules for both training and test set correctly)
  # TODO find more efficient solution to partition the data



  # Give the X a new name to be used to split
  basetabadc150392_2 <- X
  # Add a new row containing the row number (extra check to make sure we match always with corresponding Y value)
  basetabadc150392_2[,(ncol(basetabadc150392_2)+1)] <- c(1:nrow(basetabadc150392_2))
  # define the split expression from LM list that has to be changed
  tbc <- substr(object[[1]][1],1,(regexpr("\\[", object[[1]][1])[1]-1))

  # Create three vector where we can save the results
  predvector <- as.numeric()
  # classvector <- as.numeric()
  rowvector <- as.numeric()
  # Loop over all the different splits and use the corresponding LR to make predictions for each subset
  for (i in 1:length(object[[1]])) {

    object[[1]][i] <- gsub(pattern = tbc, replacement = "basetabadc150392_2", x = object[[1]][i])

    # Split the X according to the splits from the model
    ifelse(length(object[[1]]) > 1,
           # If nbr of split is >1, than we subset according to rules DT
           val_ss <- basetabadc150392_2[which(eval(parse(text= object[[1]][i]))), ],
           # otherewise we select the entire validation set
           val_ss <- basetabadc150392_2[,])

    rownbrs <- val_ss[,ncol(val_ss)]
    # y_sel <- id_vector[rownbrs]

    # Use the model to make a prediction on selection data.
    predLRstep <-  stats::predict(object[[2]][i][[1]], newdata=val_ss[,1:(ncol(val_ss)-1)], type="response", verbose = FALSE)
    vectlen <- length(rownbrs)

    # Update the prediction and class vectors
    predvector[(length(predvector)+1):(length(predvector)+vectlen)] <- predLRstep
    # classvector[(length(classvector)+1):(length(classvector)+vectlen)] <- y_sel
    rowvector[(length(rowvector)+1):(length(rowvector)+vectlen)] <- rownbrs
  }

  myreturn <- as.data.frame(cbind(predvector,rowvector))
  names(myreturn) <- c("probability", "RowNumber")
  myreturn <-myreturn[order(myreturn$RowNumber),]
  if (addrownumbers==F) {
    myreturn <- myreturn[,1]
    names(myreturn) <- c("probability")
  }

  return(myreturn)
}
