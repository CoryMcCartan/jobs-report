library(here)
library(targets)

invisible(lapply(Sys.glob(here("R/*.R")), source))

# base packages
tar_option_set(packages=c("here", "readr", "dplyr", "tidyr", "lubridate", "purrr"))
options(tidyverse.quiet=T, dplyr.summarise.inform=F)

# TARGETS
list(
    tar_target(release_schedule, get_data_sched(format(Sys.Date(), "%b %Y")),
               packages=c(tar_option_get("packages"), "rvest")),
    tar_target(release, release_schedule[which.max(Sys.Date() < release_schedule)]),
    tar_target(benchmark_file, here("data/benchmark_revision.csv"), format="file"),
    tar_target(jobs_data, get_data(release=release, bench_path=benchmark_file),
               packages=c(tar_option_get("packages"), "fredr", "rvest")),
    tar_target(jobs_data_output, save_data(jobs_data), format="file")
)
