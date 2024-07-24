# Pattern Causality

## How to install

This developing package could be installed by `devtool`.

```R
devtools::install_github("skstavroglou/pattern-causality")
```


## Basic Tutorial

We can import the existing data.

```R
data(climate)
```

This dataset contains 4 time series of climate index, we could use the patterncausality in this dataset. 

Then we need to determine the `E` and `tao`.

```R
dataset <- climate[, -1] # remove the date column
parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)
```

After the parameters are confirmed, we could calculate the pattern causality.

```R
X <- climate$AO
Y <- climate$AAO
pc <- PC.Mk.II.Lightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
```

Then the percentage of each status will be showed below.

If we wonder the status in each time point, we can run the code.

```R
X <- climate$AO
Y <- climate$AAO
detail <- PC.Mk.II.Full.Details(X, Y, E = 2, tau = 1, metric = "euclidean", h = 3, weighted = TRUE)
predict_status <- result$spectrumOfCausalityPredicted
real_stattus <- result$spectrumOfCausalityReal
```

Then the status series will be saved in `predict_status` and `real_stattus`.

## References

A package based on the following **references**.

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M. (2019). Hidden interactions in financial markets. *Proceedings of the National Academy of Sciences, 116(22)*, 10646-10651.

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M. (2020). Unveiling causal interactions in complex systems. *Proceedings of the National Academy of Sciences, 117(14)*, 7599-7605.

- Stavroglou, S. K., Ayyub, B. M., Kallinterakis, V., Pantelous, A. A., & Stanley, H. E. (2021). A novel causal risk‐based decision‐making methodology: The case of coronavirus. *Risk Analysis, 41(5)*, 814-830.

- Stavroglou, S. K., Pantelous, A. A., Ayyub, B. M., Lambert, J. H., Hall, J. W., & Stanley, H. E. (2023). Prometheus Framework for Public Policy Decisions on Environmental Health Risks. *Available at SSRN 4526256*.
