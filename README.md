# REMIND: power analysis for drug repurposing signals

This repository contains a simulation framework developed to evaluate the performance of signal detection strategies in high-dimensional drug repurposing studies using real-world data (RWD).

The framework mimics the dimensionality of large healthcare databases by simulating hundreds of drug exposure variables and covariates used for risk adjustment. Signal detection relies on repeated sample splitting, LASSO-based variable selection, and logistic regression to identify drug exposures consistently associated with the outcome of interest.

The repository allows evaluation of key performance metrics, including:
- **Sensitivity (SE):** the probability of correctly identifying a true drug signal;
- **Mean number of False Signal (MFS):** the number or proportion of falsely selected drug exposures not truly associated with the outcome.

Different modeling choices and study design parameters can be explored, including:
- sample size;
- matching ratio;
- number of covariates;
- prevalence of drug exposures;
- effect sizes;
- LASSO penalization strategies (e.g. lambda.min vs lambda.1se).

The framework was developed within the REMIND project and is fully parameterized, allowing adaptation to different pharmacoepidemiologic and RWD settings. It can be used to assess the expected ability of a signal detection strategy to detect true associations while controlling false discoveries before accessing or analysing real-world databases.

