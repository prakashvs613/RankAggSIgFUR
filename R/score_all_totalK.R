#' Summary Statistics of Multiple given Rankings
#'
#' @description Calculates the sum, maximum, and variance of \code{k} individual Kemeny distances of
#' multiple given rankings from input rankings. 
#'
#' @param rkgs a matrix of rankings to be compared against the input rankings. Each row must be a
#' complete ranking, meaning that all of the objects have a rank.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param pairs a \code{2} by \code{n choose 2} matrix of all combinations of
#' object pairs of n objects, where each column contains a pair of object indices.
#'
#' @return A vector of the total Kemeny distance, the maximum Kemeny distance of
#' the individual Kemeny distances, and the variance of the individual Kemeny distances.
#'
#' @seealso \code{\link{mod_kemeny}}, \code{\link{subit_convergence}}, \code{\link{rap_greedy_alg}},
#'  \code{\link{seed_based_iteration}}
#'
#' @export

score_all_totalK <- function(rkgs, input_rkgs, pairs){
  retMat <- matrix(0, nrow=dim(rkgs)[1], ncol=3)

  # Get the total Kemeny distance, maximum Kemeny distance, and variance for
  # each of the given rankings and the input rankings
  for(i in 1:dim(rkgs)[1]) {
    retMat[i,] <- score_one_totalK(rkgs[i,], input_rkgs, pairs)
  }

  return((retMat))
}


