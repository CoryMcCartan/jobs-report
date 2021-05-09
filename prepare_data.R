library(here)
library(tidyverse)
library(lubridate)
library(fredr)
library(rvest)


# FRED series -------------------------------------------------------------

fredr_set_key(Sys.getenv("FRED_KEY"))

obs_start = ymd("2003-05-01")
nfp_d = fredr(series_id="PAYEMS", observation_start=obs_start)
claims_d = fredr(series_id="ICSA", observation_start=obs_start)
adp_d = fredr(series_id="NPPTTL", observation_start=obs_start)

proc_series = function(d, name) {
    name_sym = rlang::sym(name)
    d %>%
        mutate(year = year(date),
               month = month(date)) %>%
        group_by(year, month) %>%
        summarize("{name}" := mean(value)) %>%
        ungroup() %>%
        mutate("{name}_lchg" := log(!!name_sym) - log(lag(!!name_sym)))
}

d = inner_join(proc_series(nfp_d, "nfp"),
           proc_series(claims_d, "claims"),
           by=c("year", "month")) %>%
    inner_join(proc_series(adp_d, "adp"), by=c("year", "month")) %>%
    mutate(date = make_date(year=year, month=month),
           nfp_chg = nfp - lag(nfp),
           across(starts_with("claims"), ~ . / 1000),
           .before="nfp")


# Revision data -----------------------------------------------------------

page = read_html("https://www.bls.gov/web/empsit/cesnaicsrev.htm")
get_rev_year = function(year, page) {
    html_element(page, paste0("table#", year, ">tbody")) %>%
        html_table(header=FALSE) %>%
        head(12) %>%
        select(year=2, initial=3, final=5) %>%
        mutate(month = 1:12, .after="year")
}

ces_rev_d = map_dfr(2003:2020, get_rev_year, page) %>%
    mutate(date = make_date(year=year, month=month), .before="year")
# limit only to months where 'final' CES est. = 'prev' estimate for QCEW benchmark
bench_rev_d = read_csv(here("data/benchmark_revision.csv")) %>%
    inner_join(ces_rev_d, by=c("date", "previous"="final"))

if (interactive()) {
inner_join(d, bench_rev_d, by=c("date", "year", "month")) %>%
    select(date, nfp, nfp_chg, revised, previous, initial) %>%
    View()
}

rev_d = inner_join(d, bench_rev_d, by=c("date", "year", "month")) %>%
    select(date, nfp, nfp_chg, revised, previous, initial) %>%
    mutate(error = initial - nfp_chg,
           rel_error = error / nfp_chg)
if (interactive()) {
# eyeball
qqplot(qt(ppoints(21), df=3), rev_d$error)
}

d = left_join(d, ces_rev_d, by=c("date", "year", "month")) %>%
    mutate(time = seq_len(n()),
           lclaims = log(claims))

write_rds(d, here("data/jobs.rds"), compress="gz")

if (interactive()) {
filter(d, nfp_chg >= -2000, nfp_chg <= 2000) %>%
ggplot(aes(lclaims, nfp_chg, color=initial)) +
    geom_point() +
    geom_smooth(method=lm)
}

