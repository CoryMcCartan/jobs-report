if (F) {
library(here)
library(tidyverse)
library(rstanarm)

d = read_rds(here("data/jobs.rds"))
model_d = mutate(d, lclaims = 100*(log(claims) - 5),
                 llclaims = lag(lclaims),
                 lnfp_init_chg = lag(nfp_init_chg)) %>%
    drop_na()
fit_d = filter(model_d, year <= 2019)
pred_d = filter(model_d, year > 2019)

m = stan_glm(nfp_chg ~ adp_chg + nfp_init_chg*lclaims + lnfp_init_chg*llclaims,
             data=fit_d, prior=student_t(df=3, autoscale=TRUE),
             prior_intercept=normal(scale=20),
             chains=1, QR=TRUE)
}
