library(test_that)
testthat::local_edition(3)

# Create some dummy data
hillEqn <- function(x, Emin, Emax, EC50, lambda) {
    (Emin + Emax * (x / EC50)^lambda) / (1 + (x / EC50)^lambda)
}
# Set parameters for function testing
doses <- rev(1000 / (2^(1:20)))
lambda <- 1
Emin <- 1
Emax <- 0.1
EC50 <- median(doses)
# Helper to combine
fx <- if (is_optim_compatible(hillEqn)) hillEqn else
    make_optim_function(hillEqn, lambda=lambda, Emin=Emin)
response <- hillEqn(doses, Emin=Emin, lambda=lambda, Emax=Emax, EC50=EC50)

# -- Loss function tests
lmsg <- c("LOSS FUNCTION: ")
testthat::test_that(paste0(lmsg, ".residual and .normal_loss produce equal results."), {
    trunc_vals <- c(FALSE, TRUE, FALSE, TRUE)
    nvals <- c(1, 1, 3, 3)
    for (i in seq_along(trunc_vals)) {
        n1 <- .residual(par=c(Emax=0.2, EC50=10), x=doses, y=response, f=fx,
            family="normal", n=nvals[i], trunc=trunc_vals[i], scale=0.07)
        n2 <- .normal_loss(par=c(Emax=0.2, EC50=10), x=doses, y=response, fn=fx,
            n=nvals[i], trunc=trunc_vals[i], scale=0.07)
        testthat::expect_equal(n1, n2,
            info=paste0("trunc: ", trunc_vals[i], ", n: ", nvals[i])
        )
    }
})

testthat::test_that(paste0(lmsg, ".residual and .cauchy_loss produce equal results."), {
    trunc_vals <- c(FALSE, TRUE, FALSE, TRUE)
    nvals <- c(1, 1, 3, 3)
    for (i in seq_along(trunc_vals)) {
        c1 <- .residual(par=c(Emax=0.2, EC50=10), x=doses, y=response, f=fx,
            family="Cauchy", n=nvals[i], trunc=trunc_vals[i], scale=0.07)
        c2 <- .cauchy_loss(par=c(Emax=0.2, EC50=10), x=doses, y=response, fn=fx,
            n=nvals[i], trunc=trunc_vals[i], scale=0.07)
        testthat::expect_equal(c1, c2,
            info=paste0("trunc: ", trunc_vals[i], ", n: ", nvals[i])
        )
    }
})

# -- Curve fitting
cmsg <- "CURVE FITTING: "
testthat::test_that(paste0(cmsg, ".fitCurve and .fitCurve2 produce equal results."), {
    par_list <- list(c(Emax=0.1, EC50=0.1), c(Emax=0.2, EC5O=10), c(Emax=0.2, EC50=100))
    for (i in seq_along(par_list))
    norm_par1 <- .fitCurve()
    norm_par2 <- .fitCurve2()
    cauch_par1
    cauch_par2
    logcosh_par1
    logcosh_par2
})