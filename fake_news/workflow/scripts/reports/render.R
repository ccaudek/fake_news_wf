library("here")
library("rmarkdown")

if (exists("snakemake")) {
    rmd <- snakemake@input[["rmd"]]
} else {
    rmd <- here::here("scripts", "reports", "my_report.Rmd")
}
rmarkdown::render(rmd, output_dir=here::here("scripts", "reports"), output_format="all")
