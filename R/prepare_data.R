#' Preparing Data
#'
#' @description Prepares the given data for rank aggregation functions. The function 
#' returns a matrix of input rankings and a vector indicating weights of the 
#' ranking for each judge. Useful when scores need to be converted to rankings. 
#' Also helpful in reducing the size of the problem for large \code{p}, especially 
#' when \code{p} > \code{n}!.
#'
#' @param df a \code{n} by \code{p} matrix or dataframe of scores of \code{n} 
#' objects given by \code{p} judges. Each column corresponds to a different judge. 
#'
#' @param HighertheBetter an integer with 1 indicating that the higher values in the
#' input correspond to the better rank. An optional parameter. Default value is 0, 
#' i.e., the lower the score the better the rank (e.g., score of 1 is the topmost rank).
#'
#' @return A list containing a matrix of input rankings (named \code{input_rkgs}) and a
#' weight vector corresponding to weights for each judge (named \code{wt}). These
#' two objects are used as inputs to \code{\link{subit_convergence}}, 
#' \code{\link{rap_greedy_alg}}, \code{\link{fur}}, and \code{\link{sigfur}}.
#'
#' 
#' @seealso \code{\link{subit_convergence}}, \code{\link{rap_greedy_alg}}, \code{\link{fur}}, \code{\link{sigfur}}
#'
#' @examples
#' ## Five input rankings with five objects 
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' out = prepare_data(input_rkgs) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' 
#' ## Five input rankings with five objects
#' ## testing the higher the better 
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' input_rkgs = input_rkgs*2+input_rkgs #artificially create a score matrix
#' # Testing the higher the better rank
#' out = prepare_data(input_rkgs, HighertheBetter = 1) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' 
#' @export

prepare_data <- function(df,HighertheBetter = 0){
  #Each column is an input ranking
  if(HighertheBetter == 1){
    df = apply(-df, 2, rank,ties.method = "average", na.last = "keep") 
  } else {
    df = apply(df, 2, rank,ties.method = "average", na.last = "keep")  }
  # transposing to count the number of rows  
  df = t(abs(df))    
  # prepare a weight vector and input_rkgs in the desired format    
  df <- data.table::data.table(as.data.frame(df))
  # counting frequency of duplicate rows
  newData = plyr::count(df) #df[, .(COUNT = .N), by = names(df)]
  #deleting rows with incomplete rankings
  idx = which(rowSums(is.na(newData))==0)
  newData = newData[idx,]
  
  #creating the new inputs
  wt = as.matrix(newData)[,ncol(df)+1]
  input_rkgs = t(as.matrix(newData[,1:ncol(df)]))
  return(list("input_rkgs"=input_rkgs, "wt"=wt))
}
