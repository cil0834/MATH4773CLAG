#' matrix_rsquared
#'
#' @param x1 the interval for x knot 1
#' @param x2 the interval for x knot 2
#' @param h the size of the interval of each grid
#' @param data the data of the linear model
#' The function that finds the max r^2 withing the confidence interval using grid approximation

#'
#' @return returns x knot 1, x knot 2, and r^2
#' @export
#' @examples
#' matrix_rsquared(x1, x2, 0.001, spruce.df)
matrix_rsquared = function(x1, x2, h, data)
{
  max_r = 0
  xk1 = 0
  xk2 = 0
  values = c()
  for(x_1 in seq(from=x1[1], to=x1[2], by=h)){
    for(x_2 in seq(from=x2[1], to=x2[2], by=h)){
      r_2 = rsq_xk1_xk2(x_1, x_2, data)

      if(r_2 > max_r)
      {
        max_r = r_2
        xk1 = x_1
        xk2 = x_2
      }
    }
  }
  a = c(xk1, xk2, max_r)
  values = append(values, a)
  values
}
