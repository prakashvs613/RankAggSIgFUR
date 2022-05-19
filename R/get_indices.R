#' Determine Indices of Ranked Objects
#'
#' @description Used in \emph{Subiterative Convergence} to determine the index of
#' the subset of objects from the output from \emph{Modified Kemeny}. These indices
#' are then used to find the object indices for the next step in \emph{Subiterative Convergence}.
#'
#' @param x a vector of integers, typically a series from 1 to \eqn{\eta}{eta} from \emph{Subiterative Convergence}.
#'
#' @param y a vector containing the ranking, typically the output from \emph{Modified Kemeny}.
#'
#' @return The index or indices of the ranked objects.
#'
#' @seealso \code{\link{subit_convergence}}
#'
#' @keywords internal
#'
#' @export

get_indices <- function(x, y) {
  which(y == x)
}


