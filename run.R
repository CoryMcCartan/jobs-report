#!/usr/bin/env Rscript
installed = rownames(installed.packages())
if (!("here" %in% installed)) install.packages("here")
if (!("targets" %in% installed)) install.packages("targets")
if (!("remotes" %in% installed)) install.packages("remotes")
if (!("tarchetypes" %in% installed)) remotes::install_github("ropensci/tarchetypes")
if (!("stringr" %in% installed)) install.packages("stringr")
if (!("rmarkdown" %in% installed)) install.packages("rmarkdown")

# For rendering to website
render_site_proj = function(input, ..., site_path="~/Documents/Code/website/content/projects/") {
    fm = rmarkdown::yaml_front_matter(input)
    slug = if (!is.null(fm$slug)) fm$slug else xfun::sans_ext(basename(input))
    out_path = paste0(site_path, slug, "/")
    if (!dir.exists(out_path)) dir.create(out_path, recursive=T)
    if (!dir.exists("figures/")) dir.create("figures/")
    out_file = paste0(out_path, "index.md")
    setwd(dirname(input))
    rmarkdown::render(input, output_file="index.md",
                      rmarkdown::md_document(preserve_yaml=TRUE))
    file.copy("index.md", paste0(out_path, "index.md"), overwrite=T)
    file.copy("figures/", paste0(out_path), recursive=T)
    file.remove("index.md")
}

args = commandArgs(trailingOnly=TRUE)
print_usage = function() cat("Usage: run.R [deps|install|make|render|help]\n")
dep_file = here::here(".github/depends.txt")
if (length(args) == 0 || args[1] == "help") {
    print_usage()
} else if (args[1] == "deps") {
    library(stringr)
    tmp = tempfile()
    targets::tar_renv(extras=character(0), path=tmp)
    packages = na.omit(str_extract(readLines(tmp), "(?<=library\\()[a-zA-Z-]+"))
    packages = c(packages, "distill", "ragg")
    if (dir.exists(here::here(".github"))) {
        writeLines(packages, dep_file)
        writeLines(sprintf("R-%i.%i", getRversion()$major, getRversion()$minor),
                   here::here(".github/R-version"))
    }
} else if (args[1] == "install") {
    cat("INSTALLING REQUIRED PACKAGES\n")
    library(stringr)
    if (!file.exists(dep_file)) {
        tmp = tempfile()
        targets::tar_renv(extras=character(0), path=tmp)
        packages = na.omit(str_extract(readLines(tmp), "(?<=library\\()[a-zA-Z-]+"))
        packages = c(packages, "distill", "ragg")
    } else {
        packages = readLines(dep_file)
    }
    to_install = setdiff(packages, installed)
    if (length(to_install) > 0)
        install.packages(to_install)
    else
        cat("All packages already installed.\n")
} else if (args[1] == "make") {
    cat("MAKING TARGETS\n")
    targets::tar_make()
} else if (args[1] == "render") {
    cat("RENDERING REPORT FOR SITE\n")
    if (length(args) > 1)
        render_site_proj(here::here("report/jobs_report.Rmd"), args[2])
    else 
        render_site_proj(here::here("report/jobs_report.Rmd"))
} else {
    print_usage()
}
