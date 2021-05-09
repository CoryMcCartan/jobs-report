# shared helper
make_model_d = function(d) {
    d %>%
        mutate(nfp_init_chg = coalesce(nfp_init_chg, nfp_chg),
               lclaims = 100*(log(claims) - 5),
               llclaims = lag(lclaims),
               lnfp_init_chg = lag(nfp_init_chg)) %>%
        drop_na()
}

#' Fit the jobs model
#'
#' @param path the path to the jobs data, from [update_data()]
#'
#' @return a fitted `stanreg` object
fit_model = function(path = here("data/jobs.csv")) {
    d = read_csv(path) %>%
        suppressMessages()
    model_d = make_model_d(d)
    fit_d = filter(model_d, finalized)

    m = stan_glm(nfp_chg ~ adp_chg + nfp_init_chg +
                     lnfp_init_chg + I(nfp_init_chg>0)*lclaims,
                 data=fit_d, prior=student_t(df=3, scale=2, autoscale=TRUE),
                 prior_intercept=normal(scale=20),
                 chains=1, iter=4000, QR=TRUE)
    m
}

#' Plot posterior predictive checks
#'
#' @param m the fitted model, from [fit_model()]
post_check = function(m, pred_d=NULL) {
    qqnorm(resid(m)) # fine
    pp_check(m, plotfun="hist", nreps=8) # fine
    pp_check(m, plotfun="stat_2d", stat=c("mean", "sd")) # fine
    pp_check(m, plotfun="stat_2d", stat=c("median", "mad")) # MAD a bit bit
    pp_check(m, plotfun="stat_2d", stat=c("min", "max")) # range too narrow
    pp_check(m, plotfun = "intervals", fatten=0) +
        coord_cartesian(ylim=1e3*c(-1, 1)) +
        labs(x="Date") # fine
    pp_check(m, plotfun = "intervals", x=m$y, fatten=0) +
        coord_cartesian(xlim=1e3*c(-1, 1), ylim=1e3*c(-1, 1)) +
        geom_abline(slope=1, lty="dashed") +
        labs(x="Actual change") # fine

    if (!is.null(pred_d)) {
        ppred = posterior_predict(m, pred_d)
        bayesplot::ppc_intervals(pred_d$nfp_chg, ppred) +
            coord_cartesian(ylim=30e3*c(-1, 1))
        bayesplot::ppc_intervals(pred_d$nfp_chg, ppred) +
            coord_cartesian(ylim=2e3*c(-1, 1))

        rel_error = sweep(ppred, 2, pred_d$nfp_chg, "/")-1
        bayesplot::ppc_intervals(rep(0, nrow(pred_d)), rel_error,
                                 x=rank(abs(pred_d$nfp_chg), ties.method="random")) +
            coord_cartesian(ylim=c(-5,5)) +
            labs(x="NFP change ranked by abs. value", y="Relative error")
        bayesplot::ppc_intervals(rep(0, nrow(pred_d)), rel_error,
                                 x=rank(pred_d$nfp_chg, ties.method="random")) +
            coord_cartesian(ylim=c(-2,2)) +
            labs(x="NFP change ranked by value", y="Relative error")
    }
}

#' Calculate how much to inflate the prediction intervals using the approximate
#' conformal jackknife (LOO)
#'
#' @param m the fitted model, from [fit_model()]
#'
#' @return A double
loo_se_inflate = function(m) {
    loo_fit = loo_predict(m)
    loo_resid = loo_fit$value - m$y
    sd(loo_resid)/sd(resid(m))
}

