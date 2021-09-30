#' Create Distribution - Johnson SU  
#' 
#' Create distribution, cumulative distribution, quantile, and random generation functions for a distribution. 
#' Use parameters from JohnsonFit function in SuppDists, recommend.johnson in lolcat, or specify parameters directly. 
#'
#' @param parms parameters, likely from SuppDists JohnsonFit function
#' @param gamma gamma parameter 
#' @param delta delta parameter
#' @param xi xi parameter
#' @param lambda lambda parameter. 
#'
#' @return A data frame with functions, distribution name, and parameters. 

dist.create.johnson.su <- function(
  parms   = list(
              gamma  = NA
              ,delta  = NA
              ,xi     = NA
              ,lambda = NA
              ,type   = "SU"
            )
  ,gamma  = NA
  ,delta  = NA
  ,xi     = NA
  ,lambda = NA
  
) {
  
  if (is.finite(gamma) & is.finite(delta) & is.finite(xi) & is.finite(lambda)) {

  } else {

    if (is.data.frame(parms)) {
      #parms from lolcat
      idx <- which(parms$transform == "su")[1]

      if (length(idx) == 0) {
        idx <- 1
      }

      gamma  <- parms$gamma[idx]
      delta  <- parms$eta[idx]
      xi     <- parms$epsilon[idx]
      lambda <- parms$lambda[idx]
      

    } else if (is.list(parms)) {
      #parms from JohnsonFit
      gamma  <- parms$gamma
      delta  <- parms$delta
      xi     <- parms$xi
      lambda <- parms$lambda
    }
  }
  #if (is.null(gamma) | is.na(gamma) | is.nan(gamma) | is.infinite(gamma)) {
  #  stop("gamma not provided")
  #}

  #if (is.null(delta) | is.na(delta) | is.nan(delta) | is.infinite(delta)) {
  #  stop("delta not provided")
  #}

  #if (is.null(xi) | is.na(xi) | is.nan(xi) | is.infinite(xi)) {
  #  stop("xi not provided")
  #}

  #if (is.null(lambda) | is.na(lambda) | is.nan(lambda) | is.infinite(lambda)) {
  #    stop("lambda not provided")
  #}

  gamma.out  <- gamma
  delta.out  <- delta
  xi.out     <- xi
  lambda.out <- lambda
  

  d.fn <- function (
    x
    ,gamma  = gamma.out
    ,delta  = delta.out
    ,xi     = xi.out
    ,lambda = lambda.out
    ,log = F
  ) {
      ret <- NA
      if (length(x) == 0) {
        ret <- numeric(0)
      } else {
        parms <- list(
          gamma
          ,delta
          ,xi
          ,lambda
          ,type = "SU"
        )

        ret <- sapply(
          x, 
          FUN = function(v) {
            if (is.na(v)) {
              NA
            } else if (is.infinite(v) | is.nan(v)) {
              NaN
            } else {
              dJohnson(
                x = v 
                ,parms = parms
                ,log = log
              )
            }
          }
        )
      }

      ret
  }
  
  p.fn <- function (
    q
    ,gamma  = gamma.out
    ,delta  = delta.out
    ,xi     = xi.out
    ,lambda = lambda.out
    ,lower.tail = T
    ,log.p = F
  ) {
      ret <- NA

      if (length(q) == 0) {
          ret <- numeric(0)
      } else {
        parms <- list(
          gamma
          ,delta
          ,xi
          ,lambda
          ,type = "SU"
        )
          
        ret <- sapply(
          q, 
          FUN = function(v) {
            if (is.na(v)) {
                NA
            } else if (is.infinite(v) | is.nan(v)) {
                NaN
            } else {
              pJohnson(q           = v 
                       ,parms      = parms
                       ,lower.tail = lower.tail
                       ,log.p      = log.p
              )
            }
          }
        )
      }

      ret
  }

  q.fn <- function (
    p
    ,gamma  = gamma.out
    ,delta  = delta.out
    ,xi     = xi.out
    ,lambda = lambda.out 
    ,lower.tail = T 
    ,log.p = F
  ) {
    ret <- NA

    if (length(p) == 0) {
      ret <- numeric(0)
    } else {
      parms <- list(
          gamma
          ,delta
          ,xi
          ,lambda
          ,type = "SU"
      )

      ret <- sapply(
        p, 
        FUN = function(v) {
          if (is.na(v)) {
            NA
          } else if (is.infinite(v) | is.nan(v)) {
            NaN
          } else {
            qJohnson(
              p           = v 
              ,parms      = parms
              ,lower.tail = lower.tail
              ,log.p      = log.p
            )
          }
        }
      )
    }

    ret
  }

  r.fn <- function (
    n
    ,gamma  = gamma.out
    ,delta  = delta.out
    ,xi     = xi.out
    ,lambda = lambda.out 
  ) {
    parms <- list(
      gamma
      ,delta
      ,xi
      ,lambda
      ,type = "SU"
    )

    ret <- rJohnson(
      n           = n 
      ,parms      = parms
    )

    ret
  }

  ret <- list(
      dist.name = "Johnson SU"
      ,d.fn = d.fn
      ,p.fn = p.fn
      ,q.fn = q.fn
      ,r.fn = r.fn
      ,parameters = list(
        parms = list(
          gamma   = gamma.out
          ,delta  = delta.out
          ,xi     = xi.out
          ,lambda = lambda.out
          ,type   = "SU"
        )
      )
      ,goodness.of.fit = list()
  )

  ret
}