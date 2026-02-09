# Experiments & Real Data Applications



## Simulations

This folder contains code for implementing the simulation studies discussed in the paper. The correspondence between folders and figures/tables is summarized as follows:

```text
├── DGPs
├── sim1-mean ← Figure 3
│   ├── G1sub
│   ├── G3-ext
│   ├── mar
│   ├── permutation
│   └── trash
├── sim2 ← Table 1
│   ├── G1sub
│   ├── G3-ext
│   ├── mar
│   └── permutation
├── sim3-causal ← Figure 4
│   ├── G1sub
│   ├── G3-ext
│   ├── mar
│   ├── permutation
```



A summary of the functionality of commonly used files is provided below.

- joblist*.txt: This is the job file for simulation. Each line corresponding to one simulation. It is recommended to execute the job lists using parallel computing.
- write_job.R: This is the R script for producing the joblist*.txt files.
- main.R: The major script for running the analysis. This script calls missing data methods coded in `est_mice.R` `est_em.R` `est_complete.R` and `est_flexmissing.R`
- organize.R Organize results in the output folder
- plot.R: This is used for generating plots.
- table.R: This is the R script used for generating tables in the paper.



## Real data application

We analyzed data from the Student Feedback Survey for
Bachelor Graduates 2016. Due to data sharing restrictions, we cannot provide direct access to the raw datasets used in this study. The dataset is available through the [Finnish Social Science Data Archive](https://services.fsd.tuni.fi/catalogue/FSD3185?study_language=en) and can be downloaded upon approval of an application.



