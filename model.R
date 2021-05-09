library(here)
library(tidyverse)
library(rstanarm)

d = read_rds(here("data/jobs.rds"))
model_d = transmute(d, date=date, year=year,
                    nfp_chg = nfp_chg,
                    adp_lchg_pm = 1000*adp_lchg,
                    nfp_init = initial,
                    lclaims = lclaims,
                    l2claims = lag(lclaims),
                    lnfp_init = lag(nfp_init)) %>%
    drop_na()
fit_d = filter(model_d, year <= 2019)
pred_d = filter(model_d, year > 2019)

plot(m, pars=c("adp_lchg_pm"


m = stan_glm(nfp_chg ~ adp_lchg_pm + nfp_init*lclaims + lnfp_init*l2claims,
             data=model_d, prior=student_t(df=3, autoscale=TRUE),
             prior_intercept=normal(scale=20))
