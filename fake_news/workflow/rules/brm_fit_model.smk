
rule fit_model:
    input:
        brm_data = rules.save_figures.output.data_subset,
    output:
        fitted_model=protected("../results/brms/fit_01.rds")
    log:
        "../results/logs/fit_model.log"
    benchmark:
        "../results/benchmarks/fit_model.txt"
    script:
        "../scripts/analysis/brm_fit.R"


rule summary_fit:
    input:
        fitted_model=rules.fit_model.output.fitted_model,
    output:
        table_1="../results/brms/table_1.csv"
    log:
        "../results/logs/summary_fit.log"
    script:
        "../scripts/analysis/get_summary_fit.R"
