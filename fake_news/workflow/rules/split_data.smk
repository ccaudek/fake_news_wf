rule save_train_test_data:
    input:
        tidy_data="../results/processed_data/fake_news_tidy_data.rds"
    output:
        pol_split_data="../results/processed_data/pol_dat.rds",
        covid_split_data="../results/processed_data/covid_dat.rds"
    log:
        "../results/logs/save_train_test_data.log"
    script:
        "../scripts/data_management/02_td_split_data.R"
