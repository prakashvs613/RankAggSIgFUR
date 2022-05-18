#' Summary Statistics of Kemeny Distances of One Ranking from Input Rankings
#'
#' @description Calculates the sum, maximum, and variance of \code{k} individual Kemeny distances of
#' the given ranking from input rankings. 
#'
#' @param rkg a vector to be compared against the input rankings. The ranking
#' should be complete which means that all of the objects have a rank.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param pairs a \code{2} by \code{n choose 2} matrix of all combinations of
#' object pairs of \code{n} objects, where each column contains a pair of object indices.
#'
#' @return A vector of the total Kemeny distance, the maximum Kemeny distance of
#' the individual Kemeny distances, and the variance of the individual Kemeny distances.
#'
#' @seealso \code{\link{score_all_totalK}}, \code{\link{subit_convergence}}, \code{\link{rap_greedy_alg}},
#'  \code{\link{fur}}
#'
#' @export

score_one_totalK <- function(rkg, input_rkgs, pairs) {
  k <- dim(input_rkgs)[1]
  score_vector <- rep(0, k)
  vec_rkg <- (rkg[pairs[1,]] - rkg[pairs[2,]])

  # Find the differences in rankings for each object pair in the input rankings
  mat_inprkgs <- ((input_rkgs[,pairs[1,]] - input_rkgs[,pairs[2,]]))
  #print(mat_inprkgs)
  if(ncol(pairs)==1){
    mat_inprkgs <- matrix(mat_inprkgs,ncol=1)
  }

  # Multiply the pairwise differences in the input rankings by the differences in the given ranking
  # A negative implies that the first object in the pair is ranked higher in the given ranking
  # but lower in an input ranking or vice versa
  indiv_score <- (mat_inprkgs*rep(vec_rkg, each = nrow(mat_inprkgs)))

  # Determine the total Kemeny distances for the ranking and each input ranking
  score_vector <- 2*Rfast::rowsums(indiv_score < 0) + Rfast::rowsums(indiv_score == 0)

  return(c(sum(score_vector), max(score_vector), stats::var(score_vector)))
}
