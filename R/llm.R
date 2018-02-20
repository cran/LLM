#' Create Logit Leaf Model
#'
#' This function creates the logit leaf model. It takes a dataframe with numeric
#' values as input and a corresponding vector with dependent values.
#' Decision tree parameters threshold for pruning and number of observations per
#' leaf can be set.
#'
#' @param X Dataframe containing numerical independent variables.
#' @param Y Numerical vector of dependent variable. Currently only binary classification is supported.
#' @param threshold_pruning Set confidence threshold for pruning. Default 0.25.
#' @param nbr_obs_leaf The minimum number of observations in a leaf node. Default 100.
#' @return An object of class logitleafmodel, which is a list with the following components:
#' \item{DecisionRules}{The raw decision rules that define segments. Use \code{\link{table.llm.html}} to visualize.}
#' \item{Coefficients}{The segment specific logistic regression coefficients. Use \code{\link{table.llm.html}} to visualize.}
#' @import partykit
#' @importFrom stats binomial step glm
#' @importFrom RWeka J48
#' @export
#' @references Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
#' @author Arno De Caigny, \email{a.de-caigny@@ieseg.fr}, Kristof Coussement, \email{k.coussement@@ieseg.fr} and Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{predict.llm}}, \code{\link{table.llm.html}}, \code{\link{llm.cv}}
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
#'

llm <- function(X,Y,threshold_pruning=0.25 , nbr_obs_leaf=100) {
  if (threshold_pruning < 0 | threshold_pruning> 1){
    stop("Enter a valid threshold for pruning value [0,1] (threshold_pruning)")
  }
  if (nbr_obs_leaf < 1){
    stop("Enter a valid mimimum number of observations per leaf (minimum =1) (nbr_obs_leaf)")
  }
  if (nrow(X) != length(Y)){
    stop("The number of instances in (X) and (Y) should be the same")
  }
  if (nrow(X) == 0 | length(Y)== 0){
    stop("There are no instances in (X) or (Y)")
  }
  if (length(which(sapply(X, is.numeric)==FALSE))>0){
    stop("All variables in the dataframe (X) should be numeric")
  }
  if (nlevels(Y) != 2){
    stop("Only binary classification is supported at the moment")
  }
  # Custom functions that are used in the LLM function
  # _1_ trim.leading
  # _2_ ceiling.dec
  # _3_ dt.splitter

  # _1_ trim.leading
  # Trim leading white spaces
  trim.leading <- function (x)  sub("^\\s+", "", x)

  # _2_ ceiling.dec
  # Round numerics at certain decimals ceiling
  ceiling.dec <- function(x,level=1) round(x+5*10^(-level-1),level)

  # _3_ dt.splitter
  # Splis space into subspaces according to decision tree rules
  # _3.1_ dt.splitter requires unepxorted .list.rules.party function from partykit. Therefore it is added below
  list.rules.party <- function(x, i = NULL, ...) {
    if (is.null(i)) i <- partykit::nodeids(x, terminal = TRUE)
    if (length(i) > 1) {
      ret <- sapply(i, list.rules.party, x = x)
      names(ret) <- if (is.character(i)) i else names(x)[i]
      return(ret)
    }
    if (is.character(i) && !is.null(names(x)))
      i <- which(names(x) %in% i)
    stopifnot(length(i) == 1 & is.numeric(i))
    stopifnot(i <= length(x) & i >= 1)
    i <- as.integer(i)
    dat <- partykit::data_party(x, i)
    if (!is.null(x$fitted)) {
      findx <- which("(fitted)" == names(dat))[1]
      fit <- dat[,findx:ncol(dat), drop = FALSE]
      dat <- dat[,-(findx:ncol(dat)), drop = FALSE]
      if (ncol(dat) == 0)
        dat <- x$data
    } else {
      fit <- NULL
      dat <- x$data
    }

    rule <- c()

    recFun <- function(node) {
      if (partykit::id_node(node) == i) return(NULL)
      kid <- sapply(partykit::kids_node(node), id_node)
      whichkid <- max(which(kid <= i))
      split <- partykit::split_node(node)
      ivar <- partykit::varid_split(split)
      svar <- names(dat)[ivar]
      index <- partykit::index_split(split)
      if (is.factor(dat[, svar])) {
        if (is.null(index))
          index <- ((1:nlevels(dat[, svar])) > breaks_split(split)) + 1
        slevels <- levels(dat[, svar])[index == whichkid]
        srule <- paste(svar, " %in% c(\"",
                       paste(slevels, collapse = "\", \"", sep = ""), "\")",
                       sep = "")
      } else {
        if (is.null(index)) index <- 1:length(kid)
        breaks <- cbind(c(-Inf, partykit::breaks_split(split)),
                        c(partykit::breaks_split(split), Inf))
        sbreak <- breaks[index == whichkid,]
        right <- partykit::right_split(split)
        srule <- c()
        if (is.finite(sbreak[1]))
          srule <- c(srule,
                     paste(svar, ifelse(right, ">", ">="), sbreak[1]))
        if (is.finite(sbreak[2]))
          srule <- c(srule,
                     paste(svar, ifelse(right, "<=", "<"), sbreak[2]))
        srule <- paste(srule, collapse = " & ")
      }
      rule <<- c(rule, srule)
      return(recFun(node[[whichkid]]))
    }
    node <- recFun(partykit::node_party(x))
    paste(rule, collapse = " & ")
  }

  dt.splitter <- function(DTmodel, nom =ifelse(length(as.character(DTmodel$call$data))==1,nom <- as.character(DTmodel$call$data), nom<- as.character(DTmodel$call$data)[2])){
    # This function has two inputs:
    #       1) Decision tree model (J48)
    #       2) nom, standard value is the data used in the DT model (your train model) but you might want to change it for test set

    m2 <- partykit::as.party.Weka_tree(DTmodel)
    # Retrieve the rules of the Decision tree

    listrules <- list.rules.party(m2)
    # listrules <- partykit:::.list.rules.party(m2)
    newvar2 <- ""

    # If the listrules does not contain a subtree, do nothing (a regular LR will be fitted)
    if(length(listrules) > 1) {
      for (l in 1:length(listrules)) {
        # Split them based on the & sign
        splittest <- strsplit(listrules[[l]],split = "&")
        new <- ""
        # Recode each condition in the splittest and save it in a newvar
        for (i in 1:length(splittest[[1]])) {
          splittest2 <- strsplit(trim.leading(splittest[[1]][i]),split = " ")
          new <- paste(paste0(nom,"[,'",splittest2[[1]][1], "']") , splittest2[[1]][2],"ceiling.dec(", splittest2[[1]][3],",4)", sep= " ")
          ifelse(i== 1, newvar <- new, newvar <- paste(newvar, "&" , new, sep = " "))
        }
        newvar2[l] <- newvar
      }
    }

    return(newvar2)
  }

  # Give the dataframe generic name (necessary to split the rules for both training and test set correctly)
  # TODO find more efficient solution to partition the data
  basetabadc150392 <- X
  basetabadc150392[,(ncol(basetabadc150392)+1)] <- c(1:nrow(basetabadc150392))
  myreturn <- vector("list", 2)
  # Create DT model based on paramens
  m1 <- RWeka::J48(as.factor(as.character(Y)) ~ .,
                   data = basetabadc150392[,1:(ncol(basetabadc150392)-1)],
                   control = RWeka::Weka_control(M = nbr_obs_leaf, C= threshold_pruning))
  # Split the dataset based on the number of "cluster" from the decision tree
  newvar <- dt.splitter(m1, "basetabadc150392")
  listythelist <- vector("list",length(newvar))

  # If length newvar > 1 : there are splits, so we do a LR for every split
  for (l in 1:length(newvar)) {
    # Subset the train set based on the rule of the DT
    ifelse(length(newvar)>1,
           # If nbr of split is >1, than we subset according to rules DT
           train_ss <- basetabadc150392[which(eval(parse(text= newvar[l]))), ],
           # Otherwise we select the entire dataset
           train_ss <- basetabadc150392[,]
    )

    rownbrs <- train_ss[,ncol(train_ss)]
    y_sel <- Y[rownbrs]

    # Train a LR for each subset with forward variable selection
    # build a glm model on the training data
    LR <- stats::glm(y_sel ~ ., data=train_ss[,1:(ncol(basetabadc150392)-1)], family=stats::binomial("logit"))
    LR1 <- stats::glm(y_sel ~ 1, data=train_ss[,1:(ncol(basetabadc150392)-1)], family=stats::binomial("logit"))

    # stepwise variable selection
    listythelist[[l]] <- stats::step(LR1,direction="forward" ,scope = list(lower= LR1, upper = LR), trace = 0)

  }
  myreturn[[1]] <- newvar
  myreturn[[2]] <- listythelist
  class(myreturn) <- "logitleafmodel"
  names(myreturn)[[1]] <- "SegmentRulesNotClean"
  names(myreturn)[[2]] <- "Coefficients"
  return(myreturn)
}
