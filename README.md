# WYOUNG: control the family-wise error rate when performing multiple hypothesis tests

- Current version: `2.0 21nov2024`
- Jump to: [`overview`](#overview) [`installation`](#installation) [`examples`](#examples) [`update history`](#update-history) [`citation`](#citation) 

-----------

## Overview: 

`wyoung` is a Stata command designed to calculate adjusted *p*-values using the free step-down resampling method developed by Westfall and Young (1993). In addition, it computes Bonferroni-Holm and Sidak-Holm adjusted *p*-values. Algorithm details and simulation test results are provided [here](/documentation/wyoung.pdf). Syntax and usage instructions can be accessed directly in Stata by typing `help wyoung` at the command prompt.


This command was developed as part of the [Illinois Workplace Wellness Study](https://www.nber.org/workplacewellness/).

## Installation:

Type `which wyoung` at the Stata prompt to determine your current version number. To install the most recent version, copy and paste the following line of code:

```stata
net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace
```

To install the version that was uploaded to SSC, copy/paste the following line of code:
```stata
ssc install wyoung, replace
```

After installing, type `help wyoung` to learn the syntax.

## Syntax

Syntax 1: multiple hypothesis testing -- one model with multiple outcomes

wyoung varlist, `cmd(model)` `familyp(varlist)`
[`subgroup(varname)` `controls("varlist1" ["varlist2" ...])`]

Syntax 2: multiple hypothesis testing -- more general but lengthier syntax for specifying different models with multiple outcomes

wyoung, `cmd("model1" ["model2" ...])` `familyp("varname1" ["varname2" ...])`

Options:

Syntax 1: one model with multiple outcomes
- `cmd(model)`: Specifies a single model with multiple outcomes. Replace "OUTCOMEVAR" with each variable from varlist.
- `familyp(varlist)`: Calculate adjusted p-values for null hypotheses of coefficients being zero.
- `subgroup(varname)`: Estimate models separately for each subgroup.
- `controls("varlist1" ["varlist2" ...])`: Specify different controls for each outcome, replacing "CONTROLVARS".
- `controlsinteract()`: Estimate models for all pairwise combinations of outcomes and controls.

Syntax 2: different models with multiple outcomes
- `cmd("model1" ["model2" ...])`: Specify a list of models.
- `familyp("varname1" ["varname2" ...])`: Calculate adjusted p-values for specific coefficients in each model.

Additional Options:
- `reps(#)` perform # bootstraps/permutations for resampling; default is `reps(100)`.

- `seed(#)` sets the random-number seed. Specifying this option is equivalent to typing the following command prior to calling `wyoung`:

  `. set seed #`

- `strata(varlist)` specifies variables that identify identify strata. If `strata()` is specified, bootstrap/permutation samples are selected within each stratum.

- `cluster(varlist)` specifies variables that identify clusters.  
If `cluster()` is specified, the bootsrap/permutation samples are selected treating each cluster, as defined by varlist, as one unit of assignment.
This option is required if model includes clustered standard errors, unless `force` is specified.
See example 3 below.

- `permute(varlist)` instructs `wyoung` to permute (rerandomize) varlist instead of drawing a bootstrap sample.
When varlist includes more than one variable, those variables are permuted jointly, preserving their relations to each other.
varlist is not permitted to include missing values, unless `force` is specified.
If `strata()` is specified, varlist is permuted within strata.
If `cluster()` is specified, permutations are performed treating each cluster as one unit.

- `permuteprogram(pgmname [, options])` instructs `wyoung` to perform permutations by calling pgmname, 
with the varlist contents of `permute(varlist)` passed as the first argument and options passed as options. 
By default, `strata()` and `cluster()` are also passed as options to pgmname.

- `force` allows the user to include a model with clustered standard errors without also specifying the `cluster()` bootstrap option,
and to permute variables with missing values.

- `singlestep` computes the single-step adjusted p-value in addition to the step-down value. Resampling-based single-step methods often control type III (sign) error rates. Whether their
step-down counterparts also control the type III error rate is unknown (Westfall and Young 1993, p. 51).

- `detail` produces sample size statistics for the bootstrap/permutation samples.

- `noresampling` computes only the Bonferroni-Holm and Sidak-Holm adjusted p-values (very fast).

- `familypexp` indicates that you are providing `familyp(exp)` instead of `familyp(varlist)` when employing Syntax 1, where exp specifies a coefficient or combination of coefficients.
exp follows the syntax of lincom and nlcom and must not contain an equal sign.
If employing Syntax 2, then `familypexp` indicates that 
you are providing `familyp("exp1" ["exp2" ...])` instead of `familyp("varname1" ["varname2" ...])`.
Specifying `familypexp` increases the set of possible hypothesis tests, but may cause `wyoung` to produce less helpful error messages when you make a syntax mistake.

- `replace` replaces data in memory with `wyoung` results.

## Examples
*Example 1.* Estimate a model separately for three outcomes (`mpg`, `headroom`, and `turn`) and calculated adjusted *p*-value for `displacement` (3 hypotheses).
```stata
sysuse auto.dta, clear
set seed 20
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) reps(100)
```
![Example 1](images/example1.PNG)
For each regression, the output provides both unadjusted and adjusted *p*-values for testing the null hypothesis that the coefficient on the variable `displacement` equals 0. For example, in the regression `regress turn displacment length`, the unadjusted *p*-value is 0.09, while the Westfall-Young adjusted *p*-value is 0.14. The `reps(100)` option specifies 100 bootstrap replications, which is the default setting and is omitted in the examples below for simplicity.

*Example 2.* Estimate a model separately for three outcomes and for two subgroups defined by `foreign` (3 X 2 = 6 hypotheses).
```stata
sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(displacement) subgroup(foreign)
```
![Example 2](images/example_subgroup.PNG)

*Example 3.* Estimate a model for three outcomes, for two subgroups defined by `foreign`, and calculate adjusted *p*-values for both `displacement` and `length` (3 X 2 X 2 = 12 hypotheses).
```stata
sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(displacement length) subgroup(foreign)
```
![Example 3](images/example_subgroup_manytreat.PNG)

*Example 4.* Estimate a model for three outcomes and test the linear restriction `_b[length] + 50*_b[displacement] = 0` (3 hypotheses).

```stata
sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(length+50*displacement) familypexp
```
![Example 4](images/example_lincom.PNG)

By default, `wyoung` uses bootstrapping. Alternatively, users can specify the `permute()` option to shuffle a `varlist`. As with bootstrapping, permutation can be combined with the `strata()` and `cluster()` options to account for stratified and/or clustered assignments (see Example 5 below). For more complex treatment assignments, users can define a custom program to handle the assignment process using the `permuteprogram()` option (see Example 6).

*Example 5.* Perform the Westfall-Young adjustment using randomization inference (3 hypotheses, stratified random assignment).

```stata
sysuse auto.dta, clear
set seed 20
gen stratum = floor(mpg/11)
gen treat = foreign
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR treat) familyp(treat) permute(treat) strata(stratum)
```

*Example 6.* Perform the Westfall-Young adjustment using randomization inference (3 hypotheses, customized assignment program).

```stata
program define myshuffle

	* By default, strata() and cluster() are passed as arguments
	syntax varname [, *]
	tempvar randsort shuffled n_init

	gen long `n_init' = _n
	gen double `randsort' = uniform()
	sort `randsort', stable
	gen `shuffled' = `varlist'[`n_init']

	drop `varlist'
	ren `shuffled' `varlist'
end

sysuse auto, clear
set seed 20 
gen treat = foreign
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR treat) familyp(treat) permute(treat) permuteprogram(myshuffle)
```

## Update History:
* **2.0**
  - added `permute()` option (thanks to Adam Sacarny). Fixed factor variables bug that caused `wyoung` to break.

* **1.3.3**
  - fixed bug where unadjusted p-val was reported assuming normality (affected Stata versions 14 and lower only)
  
* **1.3.2**
  - better error handling added for `detail` option

* **1.3.1**
  - `controls()` option edited; previous functionality moved to `controlsinteract()`

* **1.3**
  - `controls()` option added

* **1.2**
  - `familyp()` option now supports multiple variables. `subgroup()` option added

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

Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." *Quarterly Journal of Economics*, November 2019, 134(4): 1747-1791.

