#' FUR
#'
#' @description \emph{FUR} is a heuristic algorithm to obtain a consensus ranking.
#' It contains three branches -- Fixed, Update, and Range -- that use
#' \emph{Subiterative Convergence} and \emph{Greedy Algorithm} iteratively.
#' See `Details' for more information on each branch.
#'
#' @param input_rkgs a \code{n} by \code{k} matrix of \code{k} rankings of \code{n}
#' objects, where each column is a complete ranking.
#'
#' @param subit_len_list a vector containing positive integer(s) for the subiteration
#' lengths to \emph{Subiterative Convergence}. Recommended values are between 2 and 8.
#' Smaller subiteration lengths result in shorter run-time.
#'
#' @param search_radius a positive integer for the maximum change in the rank of each
#' object in the \emph{Greedy Algorithm}. The default value
#' of \code{0} considers all possible rank changes for each object. It is
#' recommended to use a search radius of less than or equal to \eqn{\min(30, \lfloor \mbox{n}/2 \rfloor)}{min(30, floor(n/2))}.
#'
#' @param seed_rkg a vector of length \code{n} with an initial ranking to begin FUR. If
#' the default value of an empty vector is used, then the \code{mean seed ranking} is adopted
#' as the initial ranking to FUR.
#' 
#' @param objNames a \code{n}-length vector containing object names. An optional 
#'  parameter. 
#'
#' @param wt a \code{k}-length vector containing weights for each
#' judge or attribute. An optional parameter. 
#'
#' @details The Fixed branch applies \emph{Subiterative Convergence} using one subiteration
#' length from \code{subit_len_list} at a time.
#'
#' The Update branch executes \emph{Subiterative Convergence} using the first
#' subiteration length in \code{subit_len_list}, and then uses its output in the
#' next call to \emph{Subiterative Convergence} with the next subiteration length in the list.
#' This process repeats until \code{subit_len_list} is exhausted.
#'
#' The Range branch calls \emph{Subiterative Convergence} on all subiteration lengths in
#' \code{subit_len_list} and only retains the best ranking among these separate calls.
#'
#' The output from the \emph{Subiterative Convergence} calls are fed into the \emph{Greedy Algorithm}
#' as its seed ranking, and the FUR algorithm is terminated when the input to the
#' \emph{Greedy Algorithm} converges to the output and all branches have been executed at
#' least once.
#'
#' @return A list containing the consensus ranking (expressed as ordering), total Kemeny distance, and average
#' tau correlation coefficient corresponding to the consensus ranking.
#'
#' @references Badal, P. S., & Das, A. (2018). Efficient algorithms using subiterative
#' convergence for Kemeny ranking problem. Computers & Operations Research, 98, 198-210.
#' \doi{10.1016/j.cor.2018.06.007}
#'
#' @seealso \code{\link{mean_seed}}, \code{\link{subit_convergence}}, \code{\link{rap_greedy_alg}}, \code{\link{sigfur}}
#'
#' @examples
#' ## One subiteration length
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' subit_len_list <- 2
#' search_radius <- 1
#' fur(input_rkgs, subit_len_list, search_radius) # Determined the consensus ranking, total Kemeny
#'                                               # distance, and average tau correlation coefficient
#'
#' ## Multiple subiteration lengths
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 2, 4, 5, 3),
#'     byrow = FALSE, ncol = 4)
#' subit_len_list <- c(2,3)
#' search_radius <- 1
#' fur(input_rkgs, subit_len_list, search_radius)
#'
#' ## Five input rankings with five objects 
#' ## 2nd ranking == 3rd ranking, so if a third object is weighted as zero,
#' ## we should get the same answer as the first examples
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' ## Multiple subiteration lengths
#' wt = c(1,1,0,1,1)
#' subit_len_list <- c(2,3)
#' search_radius <- 1
#' fur(input_rkgs, subit_len_list, search_radius,wt=wt)
#'
#' ## Using five input rankings with five objects with prepare_data to 
#' ## automatically prepare the weight vector
#' input_rkgs <- matrix(c(3, 2, 5, 4, 1, 2, 3, 1, 5, 4, 2, 3, 1, 5, 4, 5, 1, 3, 4, 2, 1, 
#'                        2, 4, 5, 3),byrow = FALSE, ncol = 5)
#' out = prepare_data(input_rkgs) 
#' input_rkgs = out$input_rkgs
#' wt = out$wt
#' subit_len_list <- c(2,3)
#' search_radius <- 1
#' fur(input_rkgs, subit_len_list, search_radius,wt=wt)
#'
#' ## Included dataset of 15 input rankings of 50 objects
#' data(data50x15)
#' input_rkgs <- as.matrix(data50x15[, -1])
#' subit_len_list <- c(2, 3)
#' search_radius <- 1
#' fur(input_rkgs, subit_len_list, search_radius)
#'
#' @export

fur <- function(input_rkgs, subit_len_list, search_radius, seed_rkg = c(), objNames = c(),wt=c()) {
  input_rkgs <- t(input_rkgs)
  I <- length(subit_len_list)
  BranchID <- 1
  i0 <- 1

  n <- dim(input_rkgs)[2]
  k <- dim(input_rkgs)[1]
  
  if (length(wt) == 0) {
    wt <- rep(1,k)
  } 
  
  kem_mat <- matrix(nrow = 100, ncol = n+1)
  pairs <- combinat::combn(1:n, 2, simplify = T)

  # Determine whether Update and Range branch are necessary
  if (I > 1) {
    endBranch <- 3
  }else{
    endBranch <- 1
  }

  for (BranchID in 1:endBranch) {

    # If a starting ranking is not provided, use the mean seed ranking
    if(length(seed_rkg) == 0) {
      starting_rkg <- mean_seed(input_rkgs,wt=wt)
    } else {
      starting_rkg <- seed_rkg
    }
   
    seed_totK <- totalKem_mult(matrix(starting_rkg,nrow=1), input_rkgs, pairs,wt=wt)[1]
   
    out_rkg <- c()
    out_totK <- 0

    for (i in i0:I) {
      out_rkg <- c()
      out_totK <- 0

      # stopping criteria based on Kemeny distance
      while(out_totK != seed_totK){

        
         # Fixed Branch
        if (BranchID == 1) {
          
          # Apply one iteration of Osc with ith subiteration length
          out <- subit_convergence(subit_len_list[i], starting_rkg, t(input_rkgs),wt=wt)
          
          out_rkg <- out$ConsensusRanking
          out_totK <- out$KemenyDistance

          # Update Branch
        } else if (BranchID == 2 & I > 1) {

          # Apply Osc with the first subiteration length on seed ranking
          out <- subit_convergence(subit_len_list[1], starting_rkg, t(input_rkgs),wt=wt)
          out_rkg <- out$ConsensusRanking

          # Apply Osc on next subiteration length with previous output ranking
          # as the seed ranking
          if (i > 1){
            for (j in 2:i) {
              out <- subit_convergence(subit_len_list[j], out_rkg, t(input_rkgs),wt=wt)
              out_rkg <- out$ConsensusRanking
            }
          }
          out_totK <- out$KemenyDistance

          # Range Branch
        } else if (BranchID == 3 & I > 1) {

          if (i>1) {
            
            # Apply Osc on all subiteration lengthins (1-i) and keep best ranking as output
            all_out <- lapply(subit_len_list[1:i], subit_convergence, seed_rkg=starting_rkg, input_rkgs = t(input_rkgs), wt=wt)

            # Find best ranking
            # Convert all output to a matrix to extract Kemeny distances and rankings
            all_mat <- matrix(unlist(all_out), nrow = i, byrow = T)
            # Find the smallest total Kemeny distance
            min_totK <- min(all_mat[, n+1])

            # Return the first ranking with the smallest total Kemeny distance
            out_rkg <- all_mat[which(all_mat[, n+1] == min_totK)[1], 1:n]
            out_totK <- min_totK
          }
        }

        # Run Oga on output from Osc; update seed as resulting ranking
        seed <- rap_greedy_alg(out_rkg, t(input_rkgs), search_radius,wt=wt)
        starting_rkg <- seed$ConsensusRanking
        seed_totK <- seed$KemenyDistance
      }

      # Update kem_mat
      kem_ind <- which(is.na(kem_mat[, 1]))[1]
     # kem_mat[kem_ind, ] <- c(seed$ConsensusRanking, seed$KemenyDistance)
      kem_mat[kem_ind, ] <- c(starting_rkg, seed_totK)
      
    }

    # Update i0 if more than 1 subiteration length to complete Update and Range branches
    if (I >= 2){
      i0 <- 2
    }
  }

  # Remove NA's from kem_mat
  res_mat <- kem_mat[which(!is.na(kem_mat[, 1])), , drop = F]

  # Return the optimal ranking from kem_mat
  min_totK <- as.integer(min(res_mat[, n+1]))
  out_rkg <- res_mat[which(res_mat[, n+1] == min_totK)[1], 1:n]

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
