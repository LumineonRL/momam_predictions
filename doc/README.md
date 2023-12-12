## Doc

`Run_MOMAM.Rmd` is a full run that collects all necessary data from the igdb API, stores it in a database file, `MOMAM.sqlite`, and trains and tunes several hundred models from 10 different model families and 10 different preprocessing techiques. This will take well over an hour to run.

`Run_MOMAM_abridged.Rmd` removes the API calls, does not build `MOMAM.sqlite` from scratch, and trains only a small subset of models. This should take approximately ~5 minutes to run.

# Models and Preprocessing Techniques

Below are the models trained and preprocessing techniques used in `Run_MOMAM.Rmd`. Entries with a 〇 are also included in `Run_MOMAM_abridged.Rmd`. All of these models had all of their hyperparameters tuned if applicable.

- Decision Tree 〇
- Logistic Regression
- Linear Discriminant Analysis
- Multiple Discriminant Analysis
- Random Forest 〇
- Naive Bayes
- XGB Boost 〇
- C5 Rules
- Nearest Neighbor
- Multi-Layer Perceptron

- "Base" Preprocessor (Remove Zero-Variance, Remove Near-Zero Variance, Log Transform Play Time columns, Remove Multicollinearity, Remove Linear Combinations, Normalize) 〇
- Base + PCA with 20% Variance threshold
- Base + PCA with 80% Variance threshold 〇
- Base + PCA with 95% Variance threshold
- Base + KPCA with 10 components
- Base + "Grouped" PCA (PCA of each unique metadata field i.e. PCA on all `genre` columns, then a PCA on all `franchise` columns)
- Grouped + PCA ("Hierarchical PCA") 〇
- "Modified Base" (The "Base" processor, with modified threshold for NZV and collinearity and a slightly modified order)

# Disclaimers

- Both of these notebooks utilize parallel processing to train models more efficiently. They are hard-coded to use 10 CPU cores. If attempting to reproduce on a machine does not meet that requirement, either modify the `train` cell where `doParallel::registerDoParallel(cores = 10)` is set, or try again on a machine with more CPU cores.
- When attempting to `knit` `doParallel` that uses multiple cores, a Windows Firewall popup can occur on Windows. If one does not feel safe with this, removing the parallel processing code would be sufficient, although training time would increase significantly.
- A `Latex` installation is required in order to reproduce as a pdf.