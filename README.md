
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pattern Causality <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/patterncausality)](https://cran.r-project.org/package=patterncausality)
[![R-CMD-check](https://github.com/skstavroglou/pattern_causality/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/skstavroglou/pattern_causality/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/patterncausality)](https://cran.r-project.org/package=patterncausality)

<!-- badges: end -->

## Overview

The patterncausality package implements a novel approach for detecting
and analyzing causal relationships in complex systems. Key features
include:

### Core Capabilities

- Pattern-based causality detection
- State space reconstruction
- Multi-dimensional causality analysis
- Robust cross-validation methods

### Applications

- Financial market analysis
- Climate system interactions
- Medical diagnosis
- Ecological system dynamics

### Key Advantages

- Detects nonlinear causal relationships
- Quantifies causality strength
- Identifies hidden patterns
- Handles noisy data effectively

This algorithm has a lot of advantages.

- You can find the hidden pattern in the complex system.
- You can measure the causality in different fields.
- You can search for the best parameters for the complex system.

## Installation

You can install the development version of patterncausality from
[GitHub](https://github.com/skstavroglou/pattern_causality) with:

``` r
# install.packages("devtools")
devtools::install_github("skstavroglou/pattern_causality")
```

You can also install the package from
[CRAN](https://CRAN.R-project.org/package=patterncausality) with:

``` r
install.packages("patterncausality")
```

## Example

### Application in climate

We can import the existing data.

``` r
library(patterncausality)
data(climate_indices)
head(climate_indices)
#>         Date      AO    AAO   NAO   PNA
#> 1 1979-01-01 -2.2328 0.2088 -1.38 -0.69
#> 2 1979-02-01 -0.6967 0.3563 -0.67 -1.82
#> 3 1979-03-01 -0.8141 0.8992  0.78  0.38
#> 4 1979-04-01 -1.1568 0.6776 -1.71  0.09
#> 5 1979-05-01 -0.2501 0.7237 -1.03  1.35
#> 6 1979-06-01  0.9332 1.7000  1.60 -1.64
```

This dataset contains 4 famous time series of climate index, we can find
the introduction of this dataset in the CRAN and R documment, we could
use the `patterncausality` in this dataset to detect the hidden
causality in this climate system.

The climate system is a typical complex system like lorenz system, which
are both originating from the climate system, it’s a good example to
show how to find the hidden causality in the complex system.

First of all, we need to determine the `E` and `tao`, it could be easy
to complete by `optimalParametersSearch` function like this:

``` r
dataset <- climate_indices[, -1] # remove the date column
parameter <- optimalParametersSearch(Emax = 5, tauMax = 5, metric = "euclidean", dataset = dataset)
```

| E   | tau | Total     | Positive  | Negative   | Dark         |
|-----|-----|-----------|-----------|------------|--------------|
| 2   | 1   | 0.5503802 | 0.5529091 | 0.44647239 | 0.0006185057 |
| 2   | 2   | 0.5672403 | 0.5722529 | 0.42461112 | 0.0031359329 |
| 2   | 3   | 0.5647436 | 0.5471488 | 0.45106762 | 0.0017836150 |
| 2   | 4   | 0.5538362 | 0.5485637 | 0.44961187 | 0.0018243903 |
| 2   | 5   | 0.5616083 | 0.5433907 | 0.45513014 | 0.0014791531 |
| 3   | 1   | 0.3203775 | 0.3460809 | 0.24690959 | 0.4070094904 |
| 3   | 2   | 0.3362460 | 0.4010403 | 0.25410446 | 0.3448552507 |
| 3   | 3   | 0.3388998 | 0.3657369 | 0.26857083 | 0.3656922393 |

Of course, we can also change the distance style to calculate the
distance matrix or even custom distance function, we can find more
inforation on our website. Then according the combo that produces the
highest percentages collectively, we can choose the best parameters
here.

After the parameters are confirmed, we could calculate the pattern
causality.

``` r
X <- climate_indices$AO
Y <- climate_indices$AAO
pc <- pcLightweight(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE)
print(pc)
#> Pattern Causality Analysis Results:
#> Total: 0.2336
#> Positive: 0.4471
#> Negative: 0.1380
#> Dark: 0.4150
```

The percentages of each causality status will be displayed below.

To examine the causality status at each time point, we can run the
following code and find the causality strength at each time point by
function `pcFullDetails`, the `causality_predict` is the predicted
causality status at each point, the parameter `weighted = TRUE` is used
to for erf function and if it’s FALSE, then it will just use the 1 or 0
to present the causality strength, however, whatever which one is used,
the total causality points will be the same.

``` r
X <- climate_indices$AO
Y <- climate_indices$AAO
detail <- pcFullDetails(X, Y, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE)
predict_status <- detail$causality_predict
real_status <- detail$causality_real
```

Then the causality strength series will be saved in `predict_status` and
`real_status`, if we want to plot the causality strength series, we can
use the `plot_causality` function for the `pc_full_details` class, and
it will show the continuous causality strength series in the whole time
period, we can find the dynamic pattern causality strength by this way.

### Conclusion

After calculating the causality, we can get the result here.

| Pairs         | total     | positive  | negative  | dark      | Dataset |
|---------------|-----------|-----------|-----------|-----------|---------|
| AAPL –\> MSFT | 0.2698665 | 0.3881279 | 0.1369863 | 0.4748858 | stock   |
| MSFT –\> AAPL | 0.2759887 | 0.4075893 | 0.1388393 | 0.4535714 | stock   |
| AO –\> AAO    | 0.2841121 | 0.326087  | 0.2318841 | 0.442029  | climate |
| AAO –\> AO    | 0.2803738 | 0.3602941 | 0.2647059 | 0.375     | climate |
| AO –\> P      | 0.3084112 | 0.1192053 | 0.4503311 | 0.4304636 | AUCO    |
| P –\> AO      | 0.3308411 | 0.3374233 | 0.2515337 | 0.4110429 | AUCO    |

## About the authors

**Stavros** is lecturer in credit risk and fin-tech at the University of
Edinburgh Business School and is the main creator for the algorithm of
the pattern causality.

<a href="mailto:stavros.k.stavroglou@gmail.com">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/envelope.svg" width="40" height="40"/>
</a> <a href="https://github.com/skstavroglou">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/github.svg" width="40" height="40"/>
</a> <a href="https://orcid.org/0000-0003-3931-0391">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/orcid.svg" width="40" height="40"/>
</a>
<a href="https://www.linkedin.com/in/stavros-k-stavroglou-5b1995324/">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/linkedin.svg" width="40" height="40"/>
</a>
<a href="https://scholar.google.co.uk/citations?user=jpSj6xgAAAAJ&hl=en">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/google.svg" width="40" height="40"/>
</a>

**Athanasios** is professor in econometrics and business statistics of
Monash Business School and is the main author of the pattern causality.

<a href="mailto:Athanasios.Pantelous@monash.edu">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/envelope.svg" width="40" height="40"/>
</a> <a href="https://orcid.org/0000-0001-5738-1471">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/orcid.svg" width="40" height="40"/>
</a>
<a href="https://www.linkedin.com/in/athanasios-pantelous-6129513b/">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/linkedin.svg" width="40" height="40"/>
</a>
<a href="https://scholar.google.gr/citations?user=ZMaiiQwAAAAJ&hl=en">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/google.svg" width="40" height="40"/>
</a>

**Hui** is MPhil student in econometrics and business statistics of
Monash Business School and is the author and maintainer of the
`patterncausality` package.

<a href="mailto:huiw1128@gmail.com">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/envelope.svg" width="40" height="40"/>
</a> <a href="https://github.com/wanghui5801">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/github.svg" width="40" height="40"/>
</a> <a href="https://orcid.org/0009-0006-0095-0243">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/orcid.svg" width="40" height="40"/>
</a> <a href="https://www.linkedin.com/in/hui-wang-29b30029b/">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/linkedin.svg" width="40" height="40"/>
</a>
<a href="https://scholar.google.com/citations?view_op=list_works&hl=en&user=_FRaLycAAAAJ">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/google.svg" width="40" height="40"/>
</a>

## References

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M.
  (2019). Hidden interactions in financial markets. *Proceedings of the
  National Academy of Sciences, 116(22)*, 10646-10651.

- Stavroglou, S. K., Pantelous, A. A., Stanley, H. E., & Zuev, K. M.
  (2020). Unveiling causal interactions in complex systems. *Proceedings
  of the National Academy of Sciences, 117(14)*, 7599-7605.

- Stavroglou, S. K., Ayyub, B. M., Kallinterakis, V., Pantelous, A. A.,
  & Stanley, H. E. (2021). A novel causal risk‐based decision‐making
  methodology: The case of coronavirus. *Risk Analysis, 41(5)*, 814-830.

## Test environments

- local R installation, R 4.1.0
- ubuntu 20.04 (on GitHub Actions), R 4.1.0
- win-builder (devel and release)

## R CMD check results

0 errors \| 0 warnings \| 0 notes

## Downstream dependencies

There are currently no downstream dependencies for this package
