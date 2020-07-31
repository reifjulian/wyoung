# WYOUNG: control the family-wise error rate when performing multiple hypothesis tests

- Current version: `1.1 24jul2020`
- Jump to: [`overview`](#overview) [`installation`](#Installation) [`update history`](#Update-history) [`citation`](#citation) 

-----------

## Overview: 

`wyoung` is a [Stata](http://www.stata.com) command that calculates adjusted *p*-values using the free step-down resampling methodology of Westfall and Young (1993). It also computes the Bonferroni-Holm and Sidak-Holm adjusted *p*-values.

For more details, see the Stata help file included in this package. Additional documentation is available online at www.nber.org/workplacewellness/s/wyoung.pdf.

## Installation:

Type `which wyoung` at the Stata prompt to determine your `wyoung` version. To install the most recent version, copy and paste the following line of code:

```
net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace
```

To install the version that was uploaded to SSC, copy/paste the following line of code:
```
ssc install wyoung, replace
```

These two versions are typically synced, but occasionally the SSC version may be slightly out of date.

## Examples
1. Test whether the outcome variables `mpg`, `headroom`, or `turn` are significantly associated with `displacement` (conditional on `length`) after controlling for multiple inference.
```stata
sysuse auto.dta, clear
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) bootstraps(100) seed(20)
```
The output reports both unadjusted and adjusted *p*-values for the null hypothesis that the coefficient on the variable `displacement` is equal to 0. For example, in the regression `regress turn displacment length`, the unadjusted *p*-value is 0.09 and the Westfall-Young adjusted $p$-value is 0.14.
![Example 1](images/example1.PNG)
## Update History:
* **1.1**
  - `familyp()` option now supports the testing of linear and nonlinear combinations of parameters

* **1.0.5**
  - `familyp()` option now supports factor variables and time-series operators

* **1.0.4**
  - Added support for commands that don't store p-values in `r(table)` (eg `ivreg2`)

* **1.0.3**
  - Better error handling for missing observations
  
* **1.0.2**
  - Cluster bootstrap now required when clustered standard errors are present; force option added

* **1.0.1**
  - Cluster bootstrap option added

## Citation: 

`wyoung` is not an official Stata command. It is a free contribution to the research community. You may cite it as:

Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *The Quarterly Journal of Economics*, November 2019, 134(4): 1747-1791.

