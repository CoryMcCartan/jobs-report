# For rendering to website
render_site_proj = function(input, ..., site_path="~/Documents/Code/website/content/projects/") {
    fm = rmarkdown::yaml_front_matter(input)
    slug = if (!is.null(fm$slug)) fm$slug else xfun::sans_ext(basename(input))
    out_path = paste0(site_path, format(Sys.Date(), "%Y-%m-"), slug, "/")
    if (!dir.exists(out_path)) dir.create(out_path, recursive=T)
    if (!dir.exists("figures/")) dir.create("figures/")
    out_file = paste0(out_path, "index.md")
    rmarkdown::render(input, output_file="index.md",
                      rmarkdown::md_document(preserve_yaml=TRUE))
    file.copy("index.md", paste0(out_path, "index.md"), overwrite=T)
    file.copy("figures/", paste0(out_path), recursive=T)
    file.remove("index.md")
}
