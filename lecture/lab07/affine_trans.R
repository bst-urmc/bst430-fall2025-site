unit_circle = function(t){
  theta = 2 * pi * t
  tibble(t = t, x = cos(theta), y = sin(theta))
}

#' Apply an affine transformation to a set of points
#'
#' The points will be 
#' 1.  Scaled by the matrix `scale`, 
#' 2.  Rotated through an angle `r_theta_rad` about the origin, 
#' 3.  Translated by `center`.
#' 
#' `scale` doesn't have to be a diagonal matrix, in which case it
#' is just applied as a linear transformation.
#' @param data data frame containing (at least) columns `x` and `y`
#' @param center length-2 vector containing the (x, y) offset
#' @param scale 2x2 matrix, possibly diagonal.
#' @param r_theta_rad rot
#'
#' @return data frame, same number of rows as `data` with modified `x` and `y`.
#' @export
#'
#' @examples
#' cir = unit_circle(ppoints(20)) %>% select(-t)
#' plot(cir)
#' ell = trans_affine(cir, scale  = diag(c(2, 1)), r_theta_rad = pi/4, center = c(1, 1))
#' plot(ell, col = 'red', add = TRUE)
trans_affine = function(data, center = c(0, 0), scale = diag(1, 1), r_theta_rad = 0){
  if(length(center) != 2 || !is.numeric(center)) stop("`center` must be length-2 numeric")
  if(!is.matrix(scale) || dim(scale) != c(2, 2)) stop("`scale` must be 2x2 matrix.")
  if(length(r_theta_rad) != 1 || !is.numeric(r_theta_rad)) stop("`r_theta_rad must be length 1 numeric")
  if(length(setdiff(c('x', 'y'), names(data))) > 0 || !inherits(data, 'data.frame')){
    stop("`data` must be data frame containing at least columns `x` and `y`.")
  }
  
  r_mat = matrix(c(cos(r_theta_rad), sin(r_theta_rad),
                   -sin(r_theta_rad), cos(r_theta_rad)), nrow = 2)
  stopifnot(near( abs(eigen(r_mat, only.values = )$values), 1))
  xy = as.matrix(data[c('x', 'y')])
  sxy = scale %*% t(xy)
  rxy = r_mat %*% sxy
  cxy = t(rxy + center)
  
  data = data %>% mutate(x = cxy[,1],
                         y = cxy[,2])
  return(data)
}
