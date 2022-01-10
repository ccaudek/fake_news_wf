log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

library("rio")
library("tidyverse")
library("tidyr")
library("viridis")
library("here")

# load data
# For debugging:
# d <- rio::import(here::here("playground", "workflow", "scripts", "data", "penguins.csv"))
d <- rio::import(snakemake@input$penguins_data)

my_title <- snakemake@params$title_label
my_x_label <- snakemake@params$x_label

p1 <- d %>%
  ggplot(aes(bill_length_mm, fill = species)) +
  geom_histogram() +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  labs(
    title = my_title,
    x = my_x_label
    )

ggsave(snakemake@output$hist1, width=8, height = 5)
# ggsave(here::here("results", "plots", "hist_1.pdf"), width=8, height = 5)

# dev.off()

# Check this out:
# https://snakemake.readthedocs.io/en/stable/snakefiles/rules.html#non-file-parameters-for-rules
