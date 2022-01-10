rule make_report:
    input:
        data=config["raw_data"],
        subset_data="../results/data/processed/penguin_subset.rds",
        table_data=rules.save_table.output.tab1,
        fig_pdf=rules.save_figures.output.fig2
    output:
        "../results/reports/report.html"
    script:
        "../scripts/reports/report.Rmd"
