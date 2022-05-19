#' Determine a Subset of Rankings for Greedy Algorithm
#'
#' @description This function creates the new rankings for \emph{Greedy Algorithm}
#' after moving an object to a higher and lower rank within the search radius.
#' Cyclical moving is permitted in the event of overflow.
#'
#' @param seed_rkg a vector containing the initial ranking on which subsequent moves will be based.
#'
#' @param move_ind an integer representing the index of the object to be moved.
#'
#' @param search_radius a positive integer of the maximum shift allowed to increase or decrease
#' the rank of the object.
#'
#' @return A matrix of the subset of rankings, with each row as a new ranking with the moved object.
#'
#' @seealso \code{\link{rap_greedy_alg}}
#'
#' @keywords internal
#'
#' @export

get_sub_rkgs <- function(seed_rkg, move_ind, search_radius) {
  n <- length(seed_rkg)

  # Gets the object index of the top eta ranked objects
  obj_ind <- which(seed_rkg %in% move_ind)
  rank1 <- move_ind

  # Make a copy of original seed_rkg
  rkg <- seed_rkg
  reverse <- F
  cycle <- F

  # Find all movements (search radius=floor(n/2))
  if (search_radius == 0 || search_radius == floor(n / 2)) {
    sub_rkgs <- matrix(rep(0, n * (n - 1)), ncol = n)

    for (i in 1:(n - 1)) {

      # If rank is the at the lowest rank, n, then reverse the rankings to move cyclically
      if (rank1 == n) {
        reverse <- T
        rkg <- seed_rkg
        rank1 <- move_ind
      }

      if (!reverse) {
        rank2 <- rank1
        rank1 <- rank1 + 1
      } else {
        rank2 <- rank1
        rank1 <- rank1 - 1
      }

      # Swap objects/ ranks from previous ranking
      ind1 <- which(rkg %in% rank1)
      ind2 <- which(rkg %in% rank2)
      rkg[ind2] <- rank1
      rkg[ind1] <- rank2

      # Add updated ranking to subset rankings
      sub_rkgs[i, ] <- rkg
    }

    # search radius is less than the defaul value of floor(n/2)
  } else {
    sub_rkgs <- matrix(rep(0, n * (2 * search_radius)), ncol = n)

    for (i in 1:(2 * search_radius)) {

      # Now move to the left for the second half of the moves
      if (i == (search_radius + 1)) {
        reverse <- T
        rkg <- seed_rkg
        rank1 <- move_ind
      }

      if (!reverse) {
        if (rkg[obj_ind] == n) {
          cycle <- T

          # Reset cycle variable
        } else {
          cycle <- F
        }
      } else {
        if (rkg[obj_ind] == 1) {
          cycle <- T

          # Reset cycle variable
        } else {
          cycle <- F
        }
      }

      if (!cycle) {
        if (!reverse) {
          rank2 <- rank1
          rank1 <- rank1 + 1
        } else {
          rank2 <- rank1
          rank1 <- rank1 - 1
        }

        # Swap objects/ ranks from previous ranking
        ind1 <- which(rkg %in% rank1)
        ind2 <- which(rkg %in% rank2)
        rkg[ind2] <- rank1
        rkg[ind1] <- rank2
      } else {
        if (!reverse) {
          rkg <- rkg + 1
          rank1 <- 1
        } else {
          rkg <- rkg - 1
          rank1 <- n
        }
        rkg[obj_ind] <- rank1
      }

      sub_rkgs[i, ] <- rkg
    }
  }
  return(sub_rkgs)
}
