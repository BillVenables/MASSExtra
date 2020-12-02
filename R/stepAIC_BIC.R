### step AIC and BIC
###

#' Stepwise model construction and inspection
#'
#' Front-ends to \code{\link[MASS]{stepAIC}} and \code{\link[MASS]{dropterm}} with changed defaults.
#' \code{step_BIC} implements a stepwise selection with BIC as the criterion and
#' \code{step_GIC} uses an experimental criterion with a penalty midway between AIC and BIC: the
#' "Goldilocks" criterion.
#'
#' @param object as for \code{\link[MASS]{stepAIC}}
#' @param ... additional arguments passed on to main function in \code{MASS}
#' @param trace,k as for \code{\link[MASS]{stepAIC}}
#' @param sorted,test as for \code{\link[MASS]{dropterm}}
#'
#' @return A fitted model object after stepwise refinement, or a data frame.
#' @export
#'
#' @examples
#' fm <- glm.nb(Days ~ .^3, quine)
#' dropterm(fm_aic <- step_AIC(fm))
#' dropterm(fm_bic <- step_BIC(fm))
step_AIC <- function(object, ..., trace = 0, k = 2) {
  if(isTRUE(object$call$trace)) {
    warning("Trace detected. Turning it off.", immediate. = TRUE)
    call <- substitute(update(OBJ, trace = FALSE),
                       list(OBJ = substitute(object)))
    object <- eval.parent(call)
  }
  out <- stepAIC(object, ..., trace = trace, k = k)
  attr(out, "penalty") <- k
  out
}

#' @rdname step_AIC
#' @export
step_BIC <- function(object, ..., trace = 0,
                    k = max(2, log(nobs(object)))) {
  if(isTRUE(object$call$trace)) {
    warning("Trace detected. Turning it off.", immediate. = TRUE)
    call <- substitute(update(OBJ, trace = FALSE),
                       list(OBJ = substitute(object)))
    object <- eval.parent(call)
  }
  out <- stepAIC(object, ..., trace = trace, k = k)
  attr(out, "penalty") <- k
  out
}

#' @rdname step_AIC
#' @export
step_GIC <- function(object, ..., trace = 0,
                     k = (2 + log(nobs(object)))/2) {
  if(isTRUE(object$call$trace)) {
    warning("Trace detected. Turning it off.", immediate. = TRUE)
    call <- substitute(update(OBJ, trace = FALSE),
                       list(OBJ = substitute(object)))
    object <- eval.parent(call)
  }
  out <- stepAIC(object, ..., trace = trace, k = k)
  attr(out, "penalty") <- k
  out
}

#' @rdname step_AIC
#' @export
dropterm <- function(object, ..., sorted = TRUE, test = default_test(object), k) {
  if(!isS4(object) && isTRUE(object$call$trace)) {
    warning("Trace detected. Turning it off.", immediate. = TRUE)
    call <- substitute(update(OBJ, trace = FALSE),
                      list(OBJ = substitute(object)))
    object <- eval.parent(call)
  }
  k <- if(missing(k)) {
    p <- attr(object, "penalty")
    ifelse(is.null(p), 2, p)
  } else {
    if(is.character(k)) {
      switch(tolower(k),
             bic = max(2, log(nobs(object))),
             gic = (2 + log(nobs(object)))/2,
             aic =, 2)
    } else k
  }
  out <- MASS::dropterm(object, ..., sorted = sorted, test = test, k = k)
  if(k != 2) {
    isBIC <- isTRUE(all.equal(k, log(nobs(object))))
    isGIC <- isTRUE(all.equal(k, (2 + log(nobs(object)))/2))
    names(out) <- sub("AIC", ifelse(isBIC, "BIC",
                                    ifelse(isGIC, "GIC",
                                           paste0("IC(", round(k, 2), ")"))),
                      names(out))
  }
  structure(out, pName = grep("IC", names(out), value = TRUE)[1],
            class = c("dropterm", class(out)))
}

#' Dropterm plot method
#'
#' @param x An object of class \code{"dropterm"}
#' @param ...,horiz arguments past on to \code{graphics::barplot}
#' @param fill,colour \code{barplot} fill and border colour(s)
#' @param las graphics parameter
#' @param show.model logical: should the model itself be displayed?
#'
#' @return \code{x} invisibly
#' @export
#' @examples
#' fm <- lm(medv ~ . + (rm + tax + lstat)^2 +
#'            I((rm - 6)^2) + I((tax - 400)^2) + I((lstat - 12)^2), Boston)
#' d <- dropterm(fm, k = "bic")
#' plot(d)
#' plot(d, horiz = FALSE)
plot.dropterm <- function(x, ..., horiz = TRUE,
                          las = ifelse(horiz, 1, 2),
                          fill = c("red", "steel blue"),
                          colour = c("red", "steel blue"),
                          show.model = TRUE) {
  pName <- attr(x, "pName")
  AIC <- x[[pName]]
  names(AIC) <- rownames(x)
  AIC <- sort(AIC - AIC["<none>"])
  if(is.character(fill) && length(fill) == 2) {
    fill <- ifelse(AIC < 0, fill[1], fill[2])
  }
  if(is.character(colour) && length(colour) == 2) {
    colour <- ifelse(AIC < 0, colour[1], colour[2])
  }
  pmar <- pmax(par("mar"), (if(horiz) c(4,6,1,1) else c(6,2,1,1)) + 0.1)
  oldPar <- par(mar = pmar, cex.axis = 0.8)
  on.exit(par(oldPar))
  if(show.model) {
    h <- attr(x, "heading")
    h <- format(gsub("\n", "", h[!grepl("^Single ", h)]), justify = "left")
  } else {
    h <- ""
  }
  if(horiz) {
    barplot(AIC, xlab = bquote(Delta*' '*.(pName)), horiz = TRUE,
            las = las, fill = fill, colour = colour, ...)
    text("bottom right", h, cex = 0.8, family = "mono", font = 2)
  } else {
    barplot(AIC, ylab = bquote(Delta*' '*.(pName)), horiz = FALSE,
            las = las, fill = fill, colour = colour, ...)
    text("top left", h, cex = 0.8, family = "mono", font = 2)
  }
  invisible(x)
}

#' Guess the default test
#'
#' Find an appropriate test to use in \code{\link[MASS]{dropterm}} if not specified
#'
#' @param object a fitted model object accommodated by \code{\link[MASS]{dropterm}}
#'
#' @return A character string, one of \code{"F", "Chisq", or "none"}
#' @export
#'
#' @examples
#' fm <- glm.nb(Days ~ .^3, quine)
#' default_test(fm)
default_test <- function(object) {
  UseMethod("default_test")
}

#' @rdname default_test
#' @export
default_test.default <- function(object) {
  "none"
}

#' @rdname default_test
#' @export
default_test.negbin <- function(object) {
  "Chisq"
}

#' @rdname default_test
#' @export
default_test.lmerMod <- function(object) {
  "Chisq"
}

#' @rdname default_test
#' @export
default_test.glmerMod <- function(object) {
  "Chisq"
}

#' @rdname default_test
#' @export
default_test.multinom <- function(object) {
  ## same as 'negbin'
  "Chisq"
}

#' @rdname default_test
#' @export
default_test.polr <- function(object) {
  ## same as 'negbin'
  "Chisq"
}

#' @rdname default_test
#' @export
default_test.glm <- function(object) {
  switch(object$family$family,
         binomial =, poisson = "Chisq",
         gaussian =, Gamma =, quasi =, quasibinomial =,
         quasipoisson =, inverse.gaussian = "F",
         "none")
}

#' @rdname default_test
#' @export
default_test.lm <- function(object) {
  "F"
}


############################################################################
############################################################################
###                                                                      ###
###             A CRUDE BACKWARD ELIMINATION TOOL FOR MODELS             ###
###                (GOOD ENOUGH FOR GOVERNMENT PURPOSES)                 ###
###                                                                      ###
############################################################################
############################################################################


.eliminate <- function(object, ..., trace) {
  d <- dropterm(object, sorted = TRUE, ...)
  if(trace)
    print(d)
  if(rownames(d)[1] %in% c("<none>", "1")) { ## hit the limit, do nothing
    return(invisible(object))
  }
  var <- parse(text = rownames(d)[1])[[1]]
  if(trace)
    cat("\nEliminating: ", rownames(d)[1], "\n-----------\n")
  upd <- substitute(update(OBJ, . ~ . - VAR), list(OBJ = substitute(object),
                                                   VAR = var))
  return(invisible(eval.parent(upd)))
}

#' Naive backeward elimination
#'
#' A simple facility to refine models by backward elimination.
#' Covers cases where \code{\link{dropterm}} works but \code{\link{step_AIC}}
#' does not
#'
#' @param object A fitted model object
#' @param ... additional arguments passed to \code{\link{dropterm}} such as \code{k}
#' @param trace logical: do you want a trace of the process printed?
#' @param k penalty (default 2, as for AIC)
#'
#' @return A refined fitted model object
#' @export
#'
#' @examples
#' fm <- lm(medv ~ . + (rm + tax + lstat)^2 +
#'            I((rm - 6)^2) + I((tax - 400)^2) + I((lstat - 12)^2), Boston)
#' sfm <- step_down(fm, trace = TRUE, k = "bic")
step_down <- function(object, ..., trace = FALSE, k) {
  if(!isS4(object) && isTRUE(object$call$trace)) {
    warning("Trace detected. Turning it off.", immediate. = TRUE)
    call <- substitute(update(OBJ, trace = FALSE),
                       list(OBJ = substitute(object)))
    object <- eval.parent(call)
  }
  k <- if(missing(k)) {
    p <- attr(object, "penalty")
    ifelse(is.null(p), 2, p)
  } else {
    if(is.character(k)) {
      switch(tolower(k),
             bic = max(2, log(nobs(object))),
             gic = (2 + log(nobs(object)))/2,
             aic =, 2)
    } else k
  }
  oldOpt <- options(warn = -1)
  on.exit(options(oldOpt))

  obj <- object
  repeat {
    tmp <- .eliminate(obj, ..., trace = trace, k = k)
    if(identical(tmp, obj)) break
    obj <- tmp
  }
  attr(obj, "penalty") <- k
  obj
}


#' Intermediate Information Criterion
#'
#' An AIC-variant criterion that weights complexity with a penalty
#' mid-way between 2 (as for AIC) and log(n) (as for BIC).  I.e.
#' "not too soft" and "not too hard", just "Glodilocks".
#'
#' @param object a fitted model object for which the criterion is desired
#'
#' @return The GIC criterion value
#' @export
#'
#' @examples
#' gm <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), quine)
#' c(AIC = AIC(gm), GIC = GIC(gm), BIC = BIC(gm))
GIC <- function(object) {
  stats::AIC(object, k = (2 + log(stats::nobs(object)))/2)
}
