#' Summary Statistics of Multiple given Rankings
#'
#' @description Calculates the sum, maximum, and variance of \code{k} individual Kemeny distances of
#' multiple given rankings from input rankings. 
#'
#' @param rkgs a matrix of rankings to be compared against the input rankings. Each row must be a
#' complete ranking, meaning that all of the objects have a rank.
#'
#' @param input_rkgs a \code{k} by \code{n} matrix of \code{k} rankings of \code{n} 
#' objects, where each row is a complete ranking. Note that this is a transpose of 
#' matrix used for functions like \code{fur}, \code{sigfur}, \code{rap_greedy_alg}, 
#' and \code{subit_convergence}.
#'
#' @param pairs a \code{2} by \code{n choose 2} matrix of all combinations of
#' object pairs of n objects, where each column contains a pair of object indices.
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @return A vector of the total Kemeny distance, the maximum Kemeny distance of
#' the individual Kemeny distances, and the variance of the individual Kemeny distances.
#'
#' @seealso \code{\link{mod_kemeny}}, \code{\link{subit_convergence}}, \code{\link{rap_greedy_alg}},
#'  \code{\link{seed_based_iteration}}
#'
#' @keywords internal
#'
#' @export

totalKem_mult <- function(rkgs, input_rkgs, pairs, wt=wt){
  retMat <- matrix(0, nrow=dim(rkgs)[1], ncol=3)
  
  # Get the total Kemeny distance, maximum Kemeny distance, and variance for
  # each of the given rankings and the input rankings
  for(i in 1:dim(rkgs)[1]) {
    ## block to finding ranking of one ranking with other
	  rkg = rkgs[i,, drop=F]
	  k <- dim(input_rkgs)[1]
  score_vector <- rep(0, k)
 
  vec_rkg <- (rkg[pairs[1,,drop=F]] - rkg[pairs[2,,drop=F]])

  # Find the differences in rankings for each object pair in the input rankings
  mat_inprkgs <- ((input_rkgs[,pairs[1,,drop=F]] - input_rkgs[,pairs[2,,drop=F]]))
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
  # print(score_vector)
   xm <- stats::weighted.mean(score_vector, wt)
   v <- sum(wt * ((score_vector - xm)^2))/(sum(wt)-1)
 
  # retVec = c(sum(score_vector), max(score_vector), stats::var(score_vector))
   retVec <- c(sum(score_vector*wt), max(score_vector), v)
   
    ## end of block
    retMat[i,] <- retVec
  }

  return((retMat))
}


