rule fit_ref_model:
    input:
        pol_data="../results/processed_data/pol_dat.rds"
    output:
        saved_model_fit="../results/models/td_ref_pol_fit.rds",
        pp_check="../results/models/td_ref_pol_pp_check.pdf"
    # message: "Rule {rule} is using this input: {input}",
    log:
        "../results/logs/fit_ref_model.log"
    script:
        "../scripts/analysis/01_fit_ref_model_pol.R"
