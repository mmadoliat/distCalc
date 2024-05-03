sn_density <- function(x, p1, p2, p3) {
  return(sn::dsn(x, p1, p2, p3))
}

sn_cdf <- function(x, p1, p2, p3) {
  return(sn::psn(x, p1, p2, p3))
}

sn_hazard <- function(x, p1, p2, p3) {
  return(sn_density(x, p1, p2, p3) / (1 - sn_cdf(x, p1, p2, p3)))
}

