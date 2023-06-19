#' Mean Seed Ranking
#'
#' @description Determine the \emph{mean seed ranking} of the given input rankings.
#' The average rank of an object is the sum of its various rankings from each input
#' ranking divided by the total number of rankings. The mean seed ranking is formed
#' by ranking the objects based on their average ranks, and ties are broken by ranking
#' the first tied object with a higher rank.
#'
#' @param input_rkgs a \code{k} by \code{n} matrix of \code{k} rankings of \code{n} 
#' objects, where each row is a complete ranking. Note that this is a transpose of 
#' matrix used for functions like \code{fur}, \code{sigfur}, \code{rap_greedy_alg}, 
#' and \code{subit_convergence}.
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @return A vector containing the mean seed ranking of the input rankings.
#'
#' @seealso \code{\link[base]{rank}}, \code{\link{subit_convergence}}, \code{\link{fur}}, \code{\link{sigfur}}
#'
#' @examples
#' ## Four input rankings of five objects
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' mean_seed(t(input_rkgs)) # Found the mean seed ranking
#'
#' ## Five input rankings with five objects 
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' wt = c(1,1,0,1,1)
#' mean_seed(t(input_rkgs),wt=wt) # Found the mean seed ranking
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- t(as.matrix(data50x15[, -1]))
#' mean_seed(input_rkgs)
#'
#' @export

mean_seed <- function(input_rkgs,wt=c()) {
  # Find the average ranking down the columns (by object)
    # Assign equal weights if none are given 
  if (length(wt) == 0) {
    wt <- rep(1,dim(input_rkgs)[1])
  } 
  avg_rkg <- apply(input_rkgs, 2, stats::weighted.mean, w = wt)
  ties_method <- "first"
  avg_seed <- rank(avg_rkg, ties.method = ties_method)
  return(avg_seed)
}
