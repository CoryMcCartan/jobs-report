#' Download the BLS schedule
#'
#' @param month current month; for targets purposes; ignored
#' @param url the URL for the release page
#'
#' @return A vector of release times
get_data_sched = function(year, url="https://www.bls.gov/schedule/news_release/empsit.htm") {
    page = read_html(url)
    html_element(page, "table.release-list") %>%
        html_table(header=TRUE) %>%
        mutate(time = mdy_hm(paste(`Release Date`, `Release Time`),
                             tz="America/New_York")) %>%
        pull(time)
}


#' Load FRED jobs-related data
#'
#' @param obs_start When to start the time series. New methodology as of May 2003.
#'
#' @returns A data frame with CES and ADP nonfarm payroll, and initial jobless
#' claims, plus diff & log diff versions.
get_fred_data = function(obs_start = ymd("2003-05-01")) {
    fredr_set_key(Sys.getenv("FRED_KEY"))

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
            ungroup()
    }

    inner_join(proc_series(nfp_d, "nfp"),
               proc_series(claims_d, "claims"),
               by=c("year", "month")) %>%
        inner_join(proc_series(adp_d, "adp"), by=c("year", "month")) %>%
        mutate(date = make_date(year=year, month=month),
               nfp_chg = nfp - lag(nfp),
               adp_chg = adp - lag(adp),
               across(starts_with("claims"), ~ . / 1000),
               .before="nfp")
}

#' Get CES revision data
#'
#' @param years the years to download
#' @param url the URL for the CES revisions
#' @param bench_path the path to the local benchmark data
#'
#' @return A list: `ces` has the CES revision data, `bench` has the near-final
#'   benchmark (QCEW) revision data.
get_rev_data = function(years=2003:2020,
                        url="https://www.bls.gov/web/empsit/cesnaicsrev.htm",
                        bench_path=here("data/benchmark_revision.csv")) {
    page = read_html(url)
    get_rev_year = function(year, page) {
        html_element(page, paste0("table#", year, ">tbody")) %>%
            html_table(header=FALSE) %>%
            head(12) %>%
            select(year=2, initial=3, final=5) %>%
            mutate(month = 1:12, .after="year")
    }

    ces_rev_d = map_dfr(years, get_rev_year, page) %>%
        mutate(date = make_date(year=year, month=month), .before="year")
    # limit only to months where 'final' CES est. = 'prev' estimate for QCEW benchmark
    bench_rev_d = read_csv(bench_path) %>%
        suppressMessages() %>%
        inner_join(ces_rev_d, by=c("date", "previous"="final"))

    list(ces = ces_rev_d,
         bench = bench_rev_d)
}


#' Load and merge jobs data
#'
#' @param ... passed on to [get_fred_data()] and [get_rev_data()]
#'
#' @returns A data frame
update_data = function(path=here("data/jobs.csv"), ...) {
    fred_d = get_fred_data()

    finalized = filter(fred_d, month == 2) %>%
        pull(year) %>%
        head(-1) %>%
        c(2003)
    revs = get_rev_data(years=finalized)

    d = left_join(fred_d, revs$ces, by=c("date", "year", "month")) %>%
        select(-final) %>%
        rename(nfp_init_chg = initial) %>%
        mutate(finalized = year %in% finalized & (n() - row_number()) > 3) %>%
        select(date, nfp, nfp_chg, nfp_init_chg, adp, adp_chg, claims, finalized)

    write_csv(d, path)
    invisible(path)
}
