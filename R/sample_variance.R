#'@title insstats
#'
#'@description calculeaza varianta unui esantion
#'
#'@param vector
#'
#'@return numeric
#'
#'@examples
#'
#'d <- rnorm(100, 15, 0.2)
#'v <- sample_variance(d)
#'v
#'
#'@export

sample_variance <- function(x) {
  suma <- function(x) {
    sum <- 0
    for (i in x) {
      sum <- sum + i
    }
    sum
  }

  var2 <- function(x, media) {
    sum <- 0
    for (i in x) {
      sum <- sum + (i - media) ^ 2
    }
    sum
  }

  m <- suma(x) / length(x)
  s2 <- var2(x, m)
  return (s2 / (length(x) - 1))
}
