rule remove_outliers_and_tidy:
    input:
        raw_data="scripts/data/fake_news_raw_data.rds" #config["raw_data"]
    output:
        tidy_data="../results/processed_data/fake_news_tidy_data.rds"
    # message: "Rule {rule} is using this input: {input}",
    log:
        "../results/logs/remove_outliers_and_tidy.log"
    script:
        "../scripts/data_management/01_tidy_data.R"
