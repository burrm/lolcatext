#' Fit Distribution - Johnson SU  
#' 
#' Fit distribution, cumulative distribution, quantile, and random generation functions for a distribution. 
#' Warning: This function overwrites d.fn, p.fn, q.fn, and r.fn in global environment.
#'
#' @param x Vector - data to be fit
#' @param probs Vector - quantiles to use for fitdistrplus (method=qme)
#'
#' @return A list with distribution cumulative distribution, quantile, and random generation functions and fit details. 
dist.fit.johnson.su <- function(
    x,
    probs = c(0.25, 0.5, 0.75, 0.95)
) {
  
  parms.init <- JohnsonFit(t = x)
  dist.init  <- dist.create.johnson.su(parms = parms.init)

  #troubleshooting path 1 - no go
  #d.fn <- dist.init$d.fn
  #p.fn <- dist.init$p.fn
  #q.fn <- dist.init$q.fn
  #r.fn <- dist.init$r.fn

  #class(d.fn) <- "function"
  #class(p.fn) <- "function"
  #class(q.fn) <- "function"
  #class(r.fn) <- "function"

  #if (!exists("d.fn", mode="function")) {
  #    stop("d.fn does not exist in a way that fitdist will find it")
  #}
  
  #print(str(d.fn))
  #print(str(p.fn))
  #print(str(q.fn))
  #print(str(r.fn))

  #troubleshooting path 2  - no go
  #list2env(x = dist.init, envir = environment())

  #troubleshooting path 3 - not optimal, but survivable
  min.lst <- list(
      d.fn = dist.init$d.fn,
      p.fn = dist.init$p.fn,
      q.fn = dist.init$q.fn,
      r.fn = dist.init$r.fn
  )
  list2env(x = min.lst, envir = globalenv())

  gof.out <- fitdist(
    data    = x, 
    distr   = ".fn", 
    start   = list(
      gamma  = parms.init$gamma, 
      delta  = parms.init$delta, 
      xi     = parms.init$xi, 
      lambda = parms.init$lambda 
    )
    ,method = "qme"
    ,probs  = probs
  )

  dist.final <- dist.create.johnson.su(
    gamma   = utility.extract.named.value(gof.out$estimate,"gamma")
    ,delta  = utility.extract.named.value(gof.out$estimate,"delta")
    ,xi     = utility.extract.named.value(gof.out$estimate,"xi")
    ,lambda = utility.extract.named.value(gof.out$estimate,"lambda")
  )

  dist.final$goodness.of.fit = gof.out

  dist.final   
}