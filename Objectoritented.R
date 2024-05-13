norm_density <- function(x, p1, p2) {
  dnorm(x, input$p1, input$p2)
}
norm_cdf <- function(x, p1, p2){
  pnorm(x, input$p1, input$p2)
}
norm_hazard <- function(x, p1, p2){
  norm_density(x, input$p1, input$p2) / (1-norm_cdf(x, input$p1, input$p2))
}


t_density <- function(x, p1) {
  dt(x, input$p1)
}
t_cdf <- function(x, p1) {
  pt(x, input$p1)
}
t_hazard <- function(x, p1){
  t_density(x, input$p1) / (1- t_cdf(x, input$p1))
}

chisq_density <- function(x, p1) {
  dchisq(x, input$p1)
}
chisq_cdf <- function(x, p1) {
  pchisq(x, input$p1)
}
chisq_hazard <- function(x, p1) {
  chisq_density(x, input$p1) / (1- chisq_cdf(x, input$p1))
}

f_density <- function(x, p1, p2){
  df(x, input$p1, input$p2)
}
f_cdf <- function(x, p1, p2){
  pf(x, input$p1, input$p2)
}
f_hazard <- function(x, p1, p2){
  f_density(x, input$p1, input$p2) / (1-f_cdf(x, input$p1, input$p2))
}

unif_density <- function(x, p1, p2) {
  dunif(x, input$p1, input$p2)
}
unif_cdf <- function(x, p1, p2){
  punif(x, input$p1, input$p2)
}
unif_hazard <- function(x, p1, p2){
  unif_density(x, input$p1, input$p2) / (1-unif_cdf(x, input$p1, input$p2))
}

beta_density <- function(x, p1, p2) {
  dbeta(x, input$p1, input$p2)
}
beta_cdf <- function(x, p1, p2) {
  pbeta(x, input$p1, input$p2)
}
beta_hazard <- function(x, p1, p2) {
  beta_density(x, input$p1, input$p2) / (1-beta_cdf(x, input$p1, input$p2))
}

exp_density <- function(x, p1) {
  dexp(x, input$p1)
}
exp_cdf <- function(x, p1) {
  pexp(x, input$p1)
}
exp_hazard <- function(x, p1) {
  exp_density(x, input$p1) / (1-exp_cdf(x, input$p1))
}

gamma_density <- function(x, p1, p2) {
  dgamma(x, input$p1, input$p2)
}
gamma_cdf <- function(x, p1, p2){
  pgamma(x, input$p1, input$p2)
}
gamma_hazard <- function(x, p1, p2){
  gamma_density(x, input$p1, input$p2) /(1 - gamma_cdf(x, input$p1, input$p2))
}

weibull_density <- function(x, p1, p2) {
  dweibull(x, input$p1, input$p2)
}
weibull_cdf <- function(x, p1, p2){
  pweibull(x, input$p1, input$p2)
}
weibull_hazard <- function(x, p1, p2){
  weibull_density(x, input$p1, input$p2) / (1-weibull_cdf(x, input$p1, input$p2))
}

sn_density <- function(x, p1, p2, p3) {
  sn::dsn(x, input$p1, input$p2, input$p3)
}
sn_cdf <- function(x, p1, p2, p3) {
  sn:psn(x, input$p1, input$p2, input$p3)
}
sn_hazaed <- function(x, p1, p2, p3) {
  sn_density(x, input$p1, input$p2, input$p3) / (1-sn_cdf(x, input$p1, input$p2, input$p3))
}

binom_density <-function (x, p1, p2){
  dbinom(x, input$p1, inpupt$p2)
}
binom_cdf <- function(x, p1, p2) {
  pbinom(x, input$p1, input$p2)
}
binom_hazard <- function(x, p1, p2) {
  binom_density(x, input$p1, input$p2) / (1-binom_cdf(x, input$p1, input$p2))
}

pois_density <- function(x, p1){
  dpois(x, input$p1)
} 
pois_cdf <- function(x, p1) {
  ppois(x, input$p1)
}
pois_hazard <- function(x, p1) {
  pois_density(x, input$p1) / (1-pos_cdf(x, input$p1))
}

geom_density <- function(x, p1) {
  dgeom(x, input$p1)
}
geom_cd <- function(x, p1) {
  pgeom(x, input$p1)
}
geom_hazard <- function(x, p1) {
  geom_density(x, input$p1) / (1-geom_cdf(x,input$p1))
}

dbinom_density <- function( x, p1, p2) {
  dnbinom(x, input$p1, input$p2)
}
dbinom_cdf <- function(x, p1, p2){
  pbinom(x, input$p1, input$p2)
}
dbinom_hazard <- function(x, p1, p2) {
  dbinom_density(x, input$p1, input$p2) / (1-dbinom_cdf(x, p2, p3))
}

nbinom_density <- function(x, p1, p2){
  dnbinom(x, input$p1, input$p2)
}
nbinom_cdf <- function(x, p1, p2) {
  pnbinom(x, input$p1, input$p2)
}
nbinom_hazard <- function(x, p1, p2) {
  nbinom_density(x, input$p1, input$p2) / (1-nbinom_cdf(x, input$p1, input$p2))
}

hyper_density <- function(x, p1, p2, p3){
  dhyper(x, input$p1, input$p2, input$p3)
}
hyper_cdf <- function(x, p1, p2, p3){
  phyper(x, input$p1, input$p2, input$p3)
}
hyper_hazard <- function(x1, p1, p2, p3){
  hyper_density(x, input$p1, input$p2, input$p3) / (1-hyper_cdf(x, input$p1, input$p2, input$p3))
}

createDistribution <- function(name, density_func, cdf_func, hazard_func) {
  list(
    name = name,
    density = density_func,
    cdf = cdf_func,
    hazard = hazard_func
  )
}

normal_distribution <- createDistribution(
  name = "Normal",
  density_func = norm_density,
  cdf_func = norm_cdf,
  hazard_func = norm_hazard
)

