#' @title Separate baseflow with the Eckhardt digital filter
#'
#' @description
#' Splits daily streamflow into baseflow and quickflow with the
#' two-parameter recursive digital filter of Eckhardt (2005):
#'
#' `b[t] = ((1 - BFImax) * alpha * b[t-1] + (1 - alpha) * BFImax * q[t]) / (1 - alpha * BFImax)`
#'
#' Baseflow is kept between 0 and the streamflow of the same day.
#'
#' Typical values:
#' * `alpha` (the recession constant): 0.95 to 0.99; closer to 0.99 when
#'   recession is slow, as in groundwater-fed springs.
#' * `BFImax` (the largest baseflow index the catchment can reach): about
#'   0.80 for perennial streams with porous aquifers, 0.50 for ephemeral
#'   streams with intense runoff, 0.25 for flashy hard-rock catchments.
#'
#' @param q Numeric vector. Daily streamflow, in time order and without
#'   gaps, at least 2 values. Required.
#' @param alpha Numeric from 0 to 1. The recession constant. Default:
#'   `0.98`.
#' @param BFImax Numeric from 0 to 1. The maximum baseflow index. Default:
#'   `0.8`.
#' @param bf_0 Numeric or `NULL`. The baseflow on the first day.
#'   * A number: used as the first baseflow value. Stops with an error unless
#'     it is less than `q[1]`.
#'   * `NULL`: uses `q[1] * BFImax`.
#'
#'   Default: `NULL`.
#'
#' @return A data frame with one row per day and columns `q` (streamflow),
#'   `bf` (baseflow) and `quickflow` (`q - bf`).
#' @references
#' Eckhardt, K. (2005). How to construct recursive digital filters for
#'   baseflow separation. Hydrological Processes, 19(2), 507-515.
#' @export
#' @seealso [listNWIS()] to find streamflow gages.
#' @examples
#' # A storm on top of slowly falling flow
#' q <- c(10, 9.5, 9, 30, 22, 15, 12, 10.5, 9.8, 9.2)
#' baseflow_eckhardt(q)
#'
#' # Slower recession and a set starting baseflow
#' baseflow_eckhardt(q, alpha = 0.99, BFImax = 0.5, bf_0 = 5)

baseflow_eckhardt <- function(q, alpha = 0.98, BFImax = 0.8, bf_0 = NULL) {
  
  
  # If bf_0 is provided, ensure it is numeric and less than the first discharge value
  if (!is.null(bf_0)) {
    if (!is.numeric(bf_0) || bf_0 >= q[1]) {
      stop(paste0("Invalid bf_0: Must be numeric and less than q[1] (", q[1], "). ",
                  "Provided value: ", bf_0))
    }
  }
  
  n <- length(q)
  bf <- numeric(n)
  
  
  # Initialize: Assume first day baseflow is 50% of discharge 
  # or use a more specific starting point if available.
  if (is.null(bf_0)) {
    # Defaulting to q[1] * BFImax is a standard Eckhardt initialization
    bf[1] <- q[1] * BFImax
  } else {
    bf[1] <- bf_0
  }
  
  # Calculate filter constants to save computation in the loop
  c1 <- (1 - BFImax) * alpha
  c2 <- (1 - alpha) * BFImax
  denom <- 1 - (alpha * BFImax)
  
  for (t in 2:n) {
    # Eckhardt recursive formula
    bt_calc <- (c1 * bf[t-1] + c2 * q[t]) / denom
    
    # Constraint: Baseflow cannot exceed total discharge
    bf[t] <- min(bt_calc, q[t])
    
    # Prevent negative baseflow
    if (bf[t] < 0) bf[t] <- 0
  }
  
  quickflow <- q - bf
  
  return(data.frame(
    q = q,
    bf = bf, quickflow = quickflow
  ))
}
