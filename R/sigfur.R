#' SIgFUR
#'
#' @description \emph{SIgFUR} applies \emph{Seed-Based Iteration}, \emph{Greedy Algorithm},
#' and \emph{FUR} in sequence for each element of \code{subit_len_list_sbi}. The
#' \emph{mean seed ranking} is used as the input to \emph{Seed-Based Iteration}.
#' The best of all output rankings from \emph{FUR} is considered as the consensus
#' ranking.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param subit_len_list_sbi a vector containing positive integer(s) for the subiteration
#' lengths to \emph{Seed-Based Iteration}. Recommended values are between 2 and 8.
#' Smaller subiteration lengths result in shorter run-time.
#'
#' @param omega_sbi a positive integer for the number of repetitions of perturbing
#' the seed ranking in \emph{Seed-Based Iteration}. An \code{omega_sbi} value of 1
#' corresponds to a single application of \emph{Subiterative Convergence}.
#'
#' @param subit_len_list_fur a vector containing positive integer(s) for the subiteration
#' lengths to \emph{FUR}.
#'
#' @param search_radius a positive integer for the maximum change in the rank of each
#' object in the \emph{Greedy Algorithm} and \emph{FUR}. The default value
#' of \code{0} considers all possible rank changes for each object. It is
#' recommended to use a search radius of less than or equal to \eqn{\min(30, \lfloor \mbox{n}/2 \rfloor)}{min(30, floor(n/2))}.
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
#' @seealso \code{\link{seed_based_iteration}}, \code{\link{rap_greedy_alg}}, \code{\link{fur}}, \code{\link{mean_seed}}
#'
#' @examples
#' ## Four input rankings of five objects
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' subit_len_list_sbi <- c(2:3)
#' omega_sbi <- 10
#' subit_len_list_fur <- c(2:3)
#' search_radius <- 1
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius)
#' # Determined the consensus ranking, total Kemeny distance, and average tau correlation coefficient
#'
#' ## Five input rankings with five objects
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' subit_len_list_sbi <- c(2:3)
#' omega_sbi <- 10
#' subit_len_list_fur <- c(2:3)
#' search_radius <- 1
#' wt = c(1,1,0,1,1)
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius, wt=wt)
#' # Determined the consensus ranking, total Kemeny distance, and average tau correlation coefficient
#'
#' ## Using five input rankings with five objects with prepare_data to 
#' ## automatically prepare the weight vector
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' out = prepare_data(input_rkgs) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' subit_len_list_sbi <- c(2:3)
#' omega_sbi <- 10
#' subit_len_list_fur <- c(2:3)
#' search_radius <- 1
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius, wt=wt)
#' # Determined the consensus ranking, total Kemeny distance, and average tau correlation coefficient
#' 
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- as.matrix(data50x15[, -1])
#' subit_len_list_sbi <- c(3)
#' omega_sbi <- 5
#' subit_len_list_fur <- c(2:3)
#' search_radius <- 1
#' sigfur(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius)
#'
#' @export

sigfur <- function(input_rkgs, subit_len_list_sbi, omega_sbi, subit_len_list_fur, search_radius, objNames = c(),wt=c()) {


  input_rkgs <- t(input_rkgs)
  I <- length(subit_len_list_sbi)
  i <- 1
  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]
  
  if (length(wt) == 0) {
    wt <- rep(1,k)
  } 
  
  results_fur <- matrix(0, nrow = (I), ncol = n+1)

  # For each subiteration length in subit_len_list_sbi
  for (i in 1:I){
    # Do seed based iteration
	out_sbi <- seed_based_iteration(subit_len_list_sbi[i], omega_sbi, input_rkgs,wt=wt)
    out_sbi_rkg <- out_sbi$ConsensusRanking
    out_sbitotK <- out_sbi$KemenyDistance

	# Apply greedy algorithm using the output from seed based iteration
    out_ga <- rap_greedy_alg(out_sbi_rkg, t(input_rkgs), search_radius,wt=wt)
    out_rkg <- out_ga$ConsensusRanking
    out_totK <- out_ga$KemenyDistance


	# Do FUR with the initial seed ranking of the output from greedy
	out_fur <- fur(t(input_rkgs), subit_len_list = subit_len_list_fur, search_radius, seed_rkg=out_rkg,wt=wt)
    results_fur[i, 1:n] <- out_fur$ConsensusRanking
    results_fur[i, n + 1] <- out_fur$KemenyDistance
	}

  # Return the optimal ranking from the results from FUR
  min_totK <- as.integer(min(results_fur[, n+1]))
  out_rkg <- results_fur[which(results_fur[, n+1] == min_totK)[1], 1:n]

  # Find avg tau and elapsed time
  avg_tau <- compute_avg_tau(min_totK, n, k, wt = wt)
  results <- matrix(c(out_rkg, min_totK, avg_tau), nrow = 1)
  
  # Naming the objects if the corresponding information is provided
  if (length(objNames) != 0) {
    out_rkg = objNames[out_rkg]
  }
  
  return(list(ConsensusRanking = out_rkg, KemenyDistance = min_totK,
              tau = avg_tau))
}
