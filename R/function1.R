#' Title
#'
#' @param x
#' @param n
#'
#' @return
#' @export
#'
#' @examples
myseq_n <- function(x, n) {
  if (n <= 0) {
    stop("n should large than 0")
  } else if ((n %% 1) != 0) {
    stop("n should be an integer")
  } else if (length(x) != 3) {
    stop("the length of vector should be 3")
  } else if(typeof(x) != "double") {
    stop("the type of vector should be double")
  }
  nums <- vector(mode = "double", length = n)
  for (i in seq_along(nums)){
    if(i <= 3){
      nums[i] <- x[i]
    }
    else {
      nums[i] <- nums[i-1] + (nums[i-3] - nums[i-2])/i
    }
  }
  return(nums[n])
}
