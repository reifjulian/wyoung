# WYOUNG: control the family-wise error rate when performing multiple hypothesis tests

- Current version: `1.0.5 07jan2020`
- Jump to: [`updates`](#recent-updates) [`install`](#install) [`description`](#description) [`citation`](#citation)

-----------

## Updates:

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

## Install:

Type `which wyoung` at the Stata prompt to determine which version you have installed. To install the most recent version of `wyoung`, copy/paste the following line of code:

```
net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace
```

To install the version that was uploaded to SSC, copy/paste the following line of code:
```
ssc install wyoung, replace
```

These two versions are typically synced, but occasionally the SSC version may be slightly out of date.

## Description: 

`wyoung` is a [Stata](http://www.stata.com) command that calculates adjusted *p*-values using the free step-down resampling methodology of Westfall and Young (1993). It also computes the Bonferroni-Holm and Sidak-Holm adjusted *p*-values.

For more details, see the Stata help file included in this package. Additional documentation is available online at www.nber.org/workplacewellness/s/wyoung.pdf.


## Citation: 

`wyoung` is not an official Stata command. It is a free contribution to the research community. You may cite it as:

Jones, D., D. Molitor, and J. Reif. 2018. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *National Bureau of Economic Research Working Paper No. 24229*.

