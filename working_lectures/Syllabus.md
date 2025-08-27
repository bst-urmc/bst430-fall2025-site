# Syllabus

- R for data science, statistics, biostatistics, bioinformatics, and computational biology.
- Take home exam: 30%
- In class quizzes/participation: 10%
- Assignments/Labs: 60%
- References:
    - https://r4ds.had.co.nz/
    - https://www.huber.embl.de/msmb/
    - https://r-pkgs.org/
    - https://adv-r.hadley.nz/

| Class | **Topics** | **Readings** | **Hands-on / Lab Focus** |
| --- | --- | --- | --- |
| **Aug 27** | Introduction to R and RStudio. Reproducibility.  | MSMB Ch. 1, R4DS Ch. 1–3, Posit blogs | RStudio orientation, git, quarto. |
| **Aug 29** | Exploratory data analysis. Intro to data types and structures. Intro to Tidyverse.  |  |  |
| **Sep 3** | Data wrangling I: `readr` `dplyr` and `tidyr`. Pipe operator `%>%`. | R4DS Ch. 5–6, MSMB Ch. 2 | Filtering, grouping, joining, reshaping data |
| **Sep 5** | Data wrangling II: `tibble` merging data. Memory efficient extension of `data.frame`: `data.table`. |  |  |
| **Sep 10** | Data visualization I. Base R graphics. Grammar of graphics: `ggplot2`. | R4DS Ch. 3, 7, MSMB Ch. 3 | Scatterplots, histograms, boxplots, `facet_wrap` |
| **Sep 12** | Class cancelled: David Oakes Seminar. |  |  |
| **Sep 17** | Data visualization II. `shiny` `plotly`  | R4DS Ch. 4, 7, MSMB Ch. 3–4 | Data summary, `skimr`, `janitor`, custom EDA reports |
| **Sep 19** | Strings, factors, and date times (`stringr`, `forcats`, `lubridate`)  |  |  |
| **Sep 24** | Functions, iteration, and `purrr`. | R4DS Ch. 14–15, MSMB Ch. 2 | Cleaning names, reordering factors, extracting info from strings |
| **Sep 26** | Object oriented programming (classes and generics) | MSMB Ch. 11, Bioconductor intro | DNA string ops, GC content, coverage histograms |
| **Oct 1** | Bioinformatics in R: Bioconductor,  `Biostrings`, `GenomicRanges`. | R4DS Ch. 19–21, MSMB Ch. 6 | Writing reusable functions, mapping across columns |
| **Oct 3** | Simulation and sampling, permutation. | MSMB Ch. 1-2. |  |
| **Oct 8** | Mixture modeling and clustering | MSMB Ch. 4-5 |  |
| **Oct 10** | Multivariate analysis. Dimension reduction. | MSMB Ch. 7 |  |
| **Oct 15** | Modeling I: Linear models, assumptions, and diagnostics. | MSMB Ch. 6, 8 | `lm`, `broom::tidy()`, residual plots, model comparison |
| **Oct 17** | Modeling II: Generalized linear models (GLMs), logistic regression. | MSMB Ch. 6, 8 | `glm`, confusion matrix, ROC, interpretation of odds ratios, Tidymodels |
| **Oct 22** | Tree-like data: `data.tree`, nested lists, parsing JSON. Graphs igraph (network analysis) | MSMB Ch. 10 (as relevant), vignette | Hierarchical data manipulation, visualizing nested structures |
| **Oct 24** | Special topics: `ellmer`  `optim` `torch` `Rcpp` |  |  |
| **Oct 29** | Developing R packages |  |  |
| **Oct 31** | Profiling, debugging and testing |  |  |
| **Nov 5** | Class cancelled: Final Exam |  |  |