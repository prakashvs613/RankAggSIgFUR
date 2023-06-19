#' Greedy Algorithm for Rank Aggregation
#'
#' @description \emph{Greedy Algorithm} is a heuristic method that hunts for improved rankings
#' by moving one object at a time (up or down). In case an object’s movement results in an
#' improved ranking, the next object is moved with respect to this improved ranking. The process
#' is repeated until all objects are considered once.
#'
#' @param seed_rkg an initial ranking to begin the algorithm. The algorithm is often used in
#' conjunction with \emph{Subiterative Convergence}.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param search_radius a positive integer for the maximum change in the rank of each
#' object. The default value of \code{0} considers all possible rank changes for each object.
#' Recommended value of search radius is less than or equal to \eqn{\min(30, \lfloor \mbox{n}/2 \rfloor)}{min(30, floor(n/2))}.
#' 
#' @param objNames a \code{n}-length vector containing object names. An optional 
#'  parameter. 
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @return A list containing the consensus ranking (expressed as ordering), total Kemeny distance, and average
#' tau correlation coefficient corresponding to the consensus ranking.
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @seealso \code{\link{subit_convergence}}, \code{\link{fur}}, \code{\link{sigfur}}
#'
#' @examples
#' ## Four input rankings of five objects
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' mean_seed_rkg <- mean_seed(t(input_rkgs))
#' rap_greedy_alg(mean_seed_rkg, input_rkgs, search_radius = 0) # Determined the consensus ranking,
#'                                                              # total Kemeny distance, and average
#'                                                              # tau correlation coefficient
#'
#' ## Five input rankings with five objects 
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' wt = c(1,1,0,1,1)
#' mean_seed_rkg <- mean_seed(t(input_rkgs),wt=wt)
#' rap_greedy_alg(mean_seed_rkg, input_rkgs, search_radius = 0,wt=wt) # Determined the
#' #consensus ranking,  total Kemeny distance, and average tau correlation coefficient
#'
#'
#' ## Using five input rankings with five objects with prepare_data to 
#' ## automatically prepare the weight vector
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' out = prepare_data(input_rkgs) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' mean_seed_rkg <- mean_seed(t(input_rkgs),wt=wt)
#' rap_greedy_alg(mean_seed_rkg, input_rkgs, search_radius = 0,wt=wt) # Determined the
#' #consensus ranking,  total Kemeny distance, and average tau correlation coefficient
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- as.matrix(data50x15[, -1])
#' mean_seed_rkg <- mean_seed(t(input_rkgs)) # Use the mean seed ranking as the seed ranking
#' rap_greedy_alg(mean_seed_rkg, input_rkgs, search_radius = 1)
#'
#' @export

rap_greedy_alg <- function(seed_rkg, input_rkgs, search_radius = 0, objNames = c(),wt=c()) {


  
  input_rkgs <- t(input_rkgs)
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]

  if (length(wt) == 0) {
    wt <- rep(1,k)
  } 

  output_rkg <- list()
  pairs <- combinat::combn(1:length(seed_rkg), 2, simplify = T)

  # Iterate through each object of seed_rkg
  for (i in 1:n) {
    # Create the subset of rankings with the ith object moved
    sub_rkgs <- get_sub_rkgs(seed_rkg, i, search_radius)

    # Get the total kemeny distance of the seed ranking and consensus ranking
    # among the subset rankings
    seed_K <- totalKem_mult(matrix(seed_rkg,nrow=1), input_rkgs, pairs,wt=wt)[1]

    all_K <- totalKem_mult(sub_rkgs, input_rkgs, pairs,wt=wt)[,1]
    optimal_K <- min(all_K)

    # Update output ranking to the consensus (optimal) ranking
    if (optimal_K < seed_K) {
      ind_opt <- which(all_K %in% optimal_K)[1]
      seed_rkg <- sub_rkgs[ind_opt, , drop = F]
      output_rkg <- c(seed_rkg, optimal_K)

      # If consensus is no better than the seed ranking, then keep the seed ranking
    } else {
      output_rkg <- c(seed_rkg, seed_K)
    }
  }

  avg_tau <- compute_avg_tau(output_rkg[n+1], n, k, wt = wt)
  results <- matrix(c(output_rkg, avg_tau), nrow=1)
  
  out_rkg = c(output_rkg[1:n])
  # Naming the objects if the corresponding information is provided
  if (length(objNames) != 0) {
    out_rkg = objNames[out_rkg]
  }
  
  return(list(ConsensusRanking = out_rkg, KemenyDistance = output_rkg[n+1],
              tau = avg_tau))
}
