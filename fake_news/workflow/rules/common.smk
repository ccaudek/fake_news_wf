## clean                : removes all content from results/ directory
rule clean:
    # input:
    #     expand("../results/{dir}", dir=config["RES_DIRS"])
    log:
        "../results/logs/clean.log"
    shell:
        "rm -rf ../results/*"
        # '''
        # rm -rf {input}
        # '''

## -----------------------------------------------------------------------------
## Success and failure messages
## -----------------------------------------------------------------------------

onsuccess:
    print("\n 🎉 workflow complete!\n")

onerror:
    print("\n ⛔️ something went wrong...\n")


# --- Help Rules --- #

## help_main            : prints help comments for Snakefile in ROOT directory.
##                        Help for rules in other parts of the workflows
# rule help_log:
#     input:
#         "Snakefile"
#     shell:
#         "sed -n 's/^##//p' {input}"
