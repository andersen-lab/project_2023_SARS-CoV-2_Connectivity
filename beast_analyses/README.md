### BEAST analyses
Directory contains XMLs and log files for all BEAST analyses performed. Briefly:
 * 2022-09-08_exponential_fixed.xml:
    - XML for main phylogenetic analysis. Two chains run of 200 million states run.
    - Combined log file with burnin removed: 2022-09-08_exponential_fixed.combined.log
 * 2022-09-08_exponential_fixed_discrete.xml:
    - XML for discrete state analysis using Baja California, Los Angeles, San Diego, United States, and Rest of World as discrete state. One chain of 1 million states run.
    - Log file without burnin removed: 2022-09-08_exponential_fixed_discrete.log
 * 2022-09-08_exponential_fixed_discrete_HHSA.xml:
    - XML for discrete state analysis in which the location state of San Diego genomes is assigned their HHSA region. One chain of 1 million states run.
    - Log file without burnin removed: 2022-09-08_exponential_fixed_discrete_HHSA.log
