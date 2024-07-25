
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pattern Causality

<!-- badges: start -->
<!-- badges: end -->

The goal of patterncausality is to measure the causality in the complex
system and financial market,

## Installation

You can install the development version of patterncausality from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("skstavroglou/pattern-causality")
```

## Example

### Application in climate

We can import the existing data.

``` r
library(patterncausality)
#> Warning in fun(libname, pkgname): couldn't connect to display ":0"
```

``` r
data(climate)
```

This dataset contains 4 time series of climate index, we could use the
patterncausality in this dataset.

Then we need to determine the `E` and `tao`.

``` r
dataset <- climate[, -1] # remove the date column
parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)
```

After the parameters are confirmed, we could calculate the pattern
causality.

``` r
X <- climate$AO
Y <- climate$AAO
pc <- PC.Mk.II.Lightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
print(pc)
#>       total  positive  negative      dark
#> 1 0.3140187 0.2371795 0.3846154 0.3782051
```

Then the percentage of each status will be showed below.

If we wonder the status in each time point, we can run the code.

``` r
X <- climate$AO
Y <- climate$AAO
detail <- PC.Mk.II.Full.Details(X, Y, E = 2, tau = 1, metric = "euclidean", h = 3, weighted = TRUE)
predict_status <- detail$spectrumOfCausalityPredicted
real_stattus <- detail$spectrumOfCausalityReal
```

Then the status series will be saved in `predict_status` and
`real_status`.

### Application in financial market

First of all, we can import the data of AAPL and MSFT.

``` r
data(stock)
```

We can visualize this stock price.

    #> Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    #> 3.5.0.
    #> ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    #> This warning is displayed once every 8 hours.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Then search the best parameters for the PC.

``` r
dataset <- stock
parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)
```

After that, calculate the causality of each status.

``` r
X <- stock$AAPL.Close
Y <- stock$MSFT.Close
pc <- PC.Mk.II.Lightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
print(pc)
#>       total  positive  negative      dark
#> 1 0.2445206 0.2443325 0.2382872 0.5173804
```

Lastly we can also visualize this result.

``` r
library(ggplot2)
df = data.frame(
  name=stringr::str_to_title(c(colnames(pc))),
  val=as.vector(unlist(pc))
)

ggplot(df, aes(x=name, y=val, fill=name)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  scale_fill_grey(start=0, end=0.8) +  # start and end define the range of grays
  labs(x='Status',y='Strength')+
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position="none", axis.text   = element_text(size = rel(0.8)), 
                strip.text  = element_text(size = rel(0.8)))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

## References

Based on the following **references**.

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M.
  (2019). Hidden interactions in financial markets. *Proceedings of the
  National Academy of Sciences, 116(22)*, 10646-10651.

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M.
  (2020). Unveiling causal interactions in complex systems. *Proceedings
  of the National Academy of Sciences, 117(14)*, 7599-7605.

- Stavroglou, S. K., Ayyub, B. M., Kallinterakis, V., Pantelous, A. A.,
  & Stanley, H. E. (2021). A novel causal risk‐based decision‐making
  methodology: The case of coronavirus. *Risk Analysis, 41(5)*, 814-830.

- Stavroglou, S. K., Pantelous, A. A., Ayyub, B. M., Lambert, J. H.,
  Hall, J. W., & Stanley, H. E. (2023). Prometheus Framework for Public
  Policy Decisions on Environmental Health Risks. *Available at SSRN
  4526256*.
