#'@title insstats
#'
#'@description calculeaza media unui esantion
#'
#'@param vector
#'
#'@return numeric
#'
#'@examples
#'
#'d <- rnorm(100, 15, 0.2)
#'x <- sample_mean(d)
#'x
#'
#'@export

sample_mean <- function(x) {
  suma <- function(x) {
    sum <- 0
    for (i in x) {
      sum <- sum + i
    }
    sum
  }
  m <- suma(x) / length(x)

  return (m)
}
