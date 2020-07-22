*********************************************************************************************************************
*********************************************************************************************************************
* This script provides companion code for the -wyoung- Stata command. See www.nber.org/workplacewellness/s/wyoung.pdf for additional details.

* Citation:
* Jones, D., D. Molitor, and J. Reif. "What Do Workplace Wellness Programs Do? Evidence from the Illinois Workplace Wellness Study." The Quarterly Journal of Economics, November 2019, 134(4): 1747-1791.


*********************************************************************************************************************
*********************************************************************************************************************
*********************************************************************************************************************
* This script runs simulations to test the family-wise error rate (and power) of the Westfall-Young resampling method
* NSIM specifies how many simulations to run for each particular example
* NBOOT specifies the number of bootstraps employed by the Westfall-Young command for each particular simulation
* NOBS specifies the sample size
* Ideally, both NSIM and NBOOT are "large". However, if forced to choose for computational reasons, Westfall and Young (1993) recommend erring on the side of using NSIM > NBOOT. 

* Note: this script requires the -regsave- and -texsave- modules to run. Type -ssc install regsave, replace- and -ssc install texsave, replace- at the Stata prompt to install them.
*********************************************************************************************************************
*********************************************************************************************************************
*********************************************************************************************************************
clear
adopath ++"../../src"
version 16
set more off
program drop _all
tempfile results t

* Simulation intermediate results are outputted here
local outdir "output"
cap mkdir "`outdir'"

* Table is outputted here
local tbldir "tables"
cap mkdir "`tbldir'"

local NSIM  = 2000
local NBOOT = 1000

local NOBS  = 100

***********************************
* Run simulations for each scenario
***********************************

qui foreach scen in "normal" "subgroup" "lognormal" "correlated" "cluster" "multiple" {

	set seed 20

	forval s = 1/`NSIM' {
	
		noi di "`scen' `s'"
		drop _all
		set obs `NOBS'
		
		***
		* Normal data, no correlations: Standard inference over-rejects (>5% rejection rate). Sidak-Holm correction should be accurate (5% error rate), and Westfall-Young should be similar
		***
		if "`scen'"=="normal" {
			gen x = rnormal(0,1)
			qui forval y = 1/10 {		
				gen e_`y' = rnormal(0,1)
				gen y_`y' = e_`y'
			}
			wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x") familyp(x) singlestep replace
		}
		
		***
		* Subgroups
		***
		if "`scen'"=="subgroup" {
			expand 10
			gen subgroup = mod(_n,10)+1
			gen x = rnormal(0,1)
			gen y = rnormal(0,1)
			
			wyoung, bootstraps(`NBOOT') cmd("_regress y x if subgroup==1" "_regress y x if subgroup==2" "_regress y x if subgroup==3" "_regress y x if subgroup==4" "_regress y x if subgroup==5" "_regress y x if subgroup==6" "_regress y x if subgroup==7" "_regress y x if subgroup==8" "_regress y x if subgroup==9" "_regress y x if subgroup==10") strata(subgroup) familyp(x) singlestep replace
		}
				
		***
		* log-normal data, no correlations: Not only does standard inference over-reject, but also Bonferroni-Holm and Sidak-Holm over-reject despite being "more conservative"
		***
		if "`scen'"=="lognormal" {
			* Mean of lognormal distribution is sqrt(e)=sqrt(exp(1))
			forval y = 1/10 {
				gen y_`y' = exp(rnormal(0,1)) - sqrt(exp(1))
			}
			gen byte dummy=1
			wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR dummy, nocons") familyp(dummy) singlestep replace
		}
		
		***
		* Correlated outcomes: null hypothesis (\beta=0) here is in fact false. Westfall-young rejects more often (i.e., has more power) than Sidak-Holm.
		***
		if "`scen'"=="correlated" {
			* Set all pairwise correlations equal to 0.9
			local elist "e_1"
			matrix c = I(10)
			forval r = 2/10 {
				forval c = 1/`=`r'-1' {
					matrix c[`r',`c']=0.9
				}
				local elist "`elist' e_`r'"
			}
			mata: st_replacematrix("c", makesymmetric(st_matrix("c")))
			corr2data `elist', cov(c)
			
			local beta = 0.2
			gen x = rnormal(0,1)
			forval y = 1/10 {
				gen y_`y' = `beta'*x + e_`y'
			}
			wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x") familyp(x) singlestep replace
		}
		
		***
		* Clustered errors: treatment assignment is at the panel level, 10 observations per panel
		***
		if "`scen'"=="cluster" {
		
			gen panel = _n
			
			qui forval y = 1/10 {		
				gen base_`y' = rnormal(0,1)
			}
			gen x_start = rpoisson(5)
			
			* treatment (x) is binary
			expand 10
			bysort panel: gen t = _n
			gen x = t>x_start			
	
			qui forval y = 1/10 {		
				gen e_`y' = rnormal(0,1)			
				gen y_`y' = base_`y' + e_`y'
			}
			
			* Estimate three specifications: (1) clustered standard errors + cluster bootstrap; (2) clustered standard errors + normal bootstrap; (3) iid errors + normal bootstrap
			local current_seed "`c(seed)'"
			preserve
				wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x, cluster(panel)") familyp(x) singlestep replace cluster(panel)
				gen vce       = "cluster"
				gen bootstrap = "cluster"
				save "`t'", replace
			restore, preserve 
				set seed `current_seed'
				wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x, cluster(panel)") familyp(x) singlestep replace force
				gen vce       = "cluster"
				gen bootstrap = "standard"
				append using "`t'"
				save "`t'", replace
			restore
				set seed `current_seed'
				wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x") familyp(x) singlestep replace
				gen vce       = "iid"
				gen bootstrap = "standard"
				append using "`t'"				
		}
		
		***
		* Multiple restrictions
		***
		if "`scen'"=="multiple" {
			gen x1 = rnormal(0,1)
			gen x2 = rnormal(0,1)
			qui forval y = 1/10 {		
				gen e_`y' = rnormal(0,1)
				gen y_`y' = 2*x1 + 0.5*x2 + e_`y'
			}
			
			* Estimate two sets of hypotheses: (1) linear: _b[x1] - 4*_b[x2] = 2 - 4*0.5 = 0 (2) nonlinear: _b[x1]*_b[x2]-1 = 2*0.5 - 1 = 0
			local current_seed "`c(seed)'"
			preserve
				wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x1 x2") familypalt(_b[x1] - 4*_b[x2]) singlestep replace
				gen scenario = "linear"
				save "`t'", replace
			restore
				set seed `current_seed'
				wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x1 x2") familypalt( _b[x1]*_b[x2] - 1) singlestep replace
				gen scenario = "nonlinear"
				append using "`t'"
		}

		***
		* Save results for each simulation
		***
		drop model outcome regressor
		gen sim = `s'
		compress
		if `s'>1 append using "`results'"
		save "`results'", replace
	}
	
	* Output results scenario-by-scenario
	gen NOBS  = `NOBS'
	gen NSIM = `NSIM'
	gen NBOOT = `NBOOT'
	compress
	save "`outdir'/simulation_`scen'.dta", replace
}


***********************************
* Calculate family-wise error rates for Table 1
***********************************


use "`outdir'/simulation_normal.dta", clear
gen scenario = "normal"

append using "`outdir'/simulation_lognormal.dta"
replace scenario = "lognormal" if mi(scenario)

append using "`outdir'/simulation_correlated.dta"
replace scenario = "simulation_correlated" if mi(scenario)

append using "`outdir'/simulation_subgroup.dta"
replace scenario = "subgroup" if mi(scenario)

* Flag hypotheses that are rejected at alpha = 0.05
foreach v in p pwyoung psidak pbonf {
	assert inrange(`v',0,1)
	gen `v'_05 = `v'<.05
}

* Family-wise error rate: probability of rejecting 1 (or more) hypotheses out of this family of 10 hypotheses
collapse (max) *_*, by(sim scenario) fast

* Calculate what proportion of the time this happens
collapse (mean) *_*, by(scenario) fast
list

***
* Format and output LaTeX
***
gen dummy=1
preserve
	foreach v in pwyoung_05 psidak_05 pbonf_05 p_05 {
		keep `v' dummy scenario
		reshape wide `v', i(dummy) j(scenario) str
		rename `v'* *
		gen var = "`v'"
		if "`v'"!="pwyoung_05" append using "`t'"
		save "`t'", replace
		restore, preserve
	}
restore, not

use "`t'", clear
drop dummy
order var normal subgroup simulation_correlated lognormal 

replace var = "Unadjusted"      if var=="p_05"
replace var = "Bonferroni"      if var=="pbonf1_05"
replace var = "Bonferroni-Holm" if var=="pbonf_05"
replace var = "Sidak-Holm"      if var=="psidak_05"
replace var = "Westfall-Young"  if var=="pwyoung_05"

set obs `=_N+3'
replace var = "Num. observations"   if _n==_N-2
replace var = "Num. hypotheses"     if _n==_N-1
replace var = "Hypotheses are true" if _n==_N

foreach v of varlist normal-lognormal {
	replace `v' = 10  if var=="Num. hypotheses"
	replace `v' = 100 if var=="Num. observations"
	
	tostring `v', replace force format(%12.3fc)
	replace `v' = subinstr(`v',".000","",1)
	
	if "`v'"!="simulation_correlated" replace `v' = "Y" if var=="Hypotheses are true"
	else                              replace `v' = "N" if var=="Hypotheses are true"
}

label var var "Adjustment method"
label var normal "Normal errors"
label var subgroup "Multiple subgroups"
label var lognormal "Lognormal errors"
label var simulation_correlated "Correlated errors"

local fn "Notes: Table reports the fraction of 2,000 simulations where at least one null hypothesis in a family of 10 hypotheses was rejected. All hypotheses are true for the simulations reported in columns (1), (2), and (4), i.e., lower rejection rates are better. All hypotheses are false for the simulation reported in column (3), i.e., higher rejection rates are better. The Westfall-Young correction is performed using 1,000 bootstraps."
texsave using "`tbldir'/wyoung1.tex", hlines(-3) autonumber nofix marker("tab:wyoung1") title("Family-wise rejection proportions at \(\alpha = 0.05\)") footnote("`fn'") varlabels replace


***********************************
* Calculate family-wise error rates for Table 2 (clustering)
***********************************

use "`outdir'/simulation_cluster.dta", clear
gen scenario = "v_" + vce + "_b_" + bootstrap

* Flag hypotheses that are rejected at alpha = 0.05
foreach v in p pwyoung psidak pbonf {
	assert inrange(`v',0,1)
	gen `v'_05 = `v'<.05
}

* Family-wise error rate: probability of rejecting 1 (or more) hypotheses out of this family of 10 hypotheses
collapse (max) *_*, by(sim scenario) fast

* Calculate what proportion of the time this happens
collapse (mean) *_*, by(scenario) fast
list

***
* Format and output Table 2
***
gen dummy=1
preserve
	foreach v in pwyoung_05 psidak_05 pbonf_05 p_05 {
		keep `v' dummy scenario
		reshape wide `v', i(dummy) j(scenario) str
		rename `v'* *
		gen var = "`v'"
		if "`v'"!="pwyoung_05" append using "`t'"
		save "`t'", replace
		restore, preserve
	}
restore, not

use "`t'", clear
drop dummy
order var v_iid_b_standard v_cluster_b_standard v_cluster_b_cluster 

replace var = "Unadjusted"      if var=="p_05"
replace var = "Bonferroni"      if var=="pbonf1_05"
replace var = "Bonferroni-Holm" if var=="pbonf_05"
replace var = "Sidak-Holm"      if var=="psidak_05"
replace var = "Westfall-Young"  if var=="pwyoung_05"

set obs `=_N+4'
replace var = "Num. observations"  if _n==_N-3
replace var = "Num. hypotheses"    if _n==_N-2
replace var = "Model std. errors"  if _n==_N-1
replace var = "Cluster bootstrap"  if _n==_N

foreach v of varlist v_* {
	replace `v' = 10   if var=="Num. hypotheses"
	replace `v' = 1000 if var=="Num. observations"
	
	tostring `v', replace force format(%12.3fc)
	replace `v' = subinstr(`v',".000","",1)
	
	if inlist("`v'","v_iid_b_standard")                           replace `v' = "Homoskedastic" if var=="Model std. errors"
	if inlist("`v'","v_cluster_b_standard","v_cluster_b_cluster") replace `v' = "Clustered" if var=="Model std. errors"
	
	if "`v'"=="v_cluster_b_cluster" replace `v' = "Y" if var=="Cluster bootstrap"
	else                            replace `v' = "N" if var=="Cluster bootstrap"
}

label var var "Adjustment method"
label var v_iid_b_standard ""
label var v_cluster_b_cluster ""

local fn "Notes: Table reports the fraction of 2,000 simulations where at least one null hypothesis in a family of 10 hypotheses was rejected. The difference between columns (1) and (2) is the assumption about the standard errors (homoskedastic or clustered). The difference between columns (2) and (3) is the method of bootstrapping (resampling over individual observations versus clusters), which matters only for the Westfall-Young correction. All null hypotheses are true, i.e., lower rejection rates are better. Each simulation generated 100 panels (clusters) with 10 time periods. The Westfall-Young correction is performed using 1,000 bootstraps."
texsave using "`tbldir'/wyoung2.tex", hlines(-4) autonumber nofix marker("tab:wyoung2") title("Family-wise rejection proportions at \(\alpha = 0.05\), when the data generating process is serially correlated") footnote("`fn'") nonames replace


***********************************
* Calculate family-wise error rates for Table 3 (multiple linear and nonlinear restrictions)
***********************************

use "`outdir'/simulation_multiple.dta", clear

* Flag hypotheses that are rejected at alpha = 0.05
foreach v in p pwyoung psidak pbonf {
	assert inrange(`v',0,1)
	gen `v'_05 = `v'<.05
}

* Family-wise error rate: probability of rejecting 1 (or more) hypotheses out of this family of 10 hypotheses
collapse (max) *_*, by(sim scenario) fast

* Calculate what proportion of the time this happens
collapse (mean) *_*, by(scenario) fast
list

***
* Format and output LaTeX
***
gen dummy=1
preserve
	foreach v in pwyoung_05 psidak_05 pbonf_05 p_05 {
		keep `v' dummy scenario
		reshape wide `v', i(dummy) j(scenario) str
		rename `v'* *
		gen var = "`v'"
		if "`v'"!="pwyoung_05" append using "`t'"
		save "`t'", replace
		restore, preserve
	}
restore, not

use "`t'", clear
drop dummy
order var linear nonlinear

replace var = "Unadjusted"      if var=="p_05"
replace var = "Bonferroni"      if var=="pbonf1_05"
replace var = "Bonferroni-Holm" if var=="pbonf_05"
replace var = "Sidak-Holm"      if var=="psidak_05"
replace var = "Westfall-Young"  if var=="pwyoung_05"

set obs `=_N+2'
replace var = "Num. observations"   if _n==_N-1
replace var = "Num. hypotheses"     if _n==_N

foreach v of varlist linear nonlinear {
	replace `v' = 10  if var=="Num. hypotheses"
	replace `v' = 100 if var=="Num. observations"
	
	tostring `v', replace force format(%12.3fc)
	replace `v' = subinstr(`v',".000","",1)
}

label var var "Adjustment method"
label var linear "Linear restriction"
label var nonlinear "Nonlinear restriction"

local fn "Notes: Table reports the fraction of 2,000 simulations where at least one null hypothesis in a family of 10 hypotheses was rejected. All null hypotheses are true, i.e., lower rejection rates are better. The Westfall-Young correction is performed using 1,000 bootstraps."
texsave using "`tbldir'/wyoung3.tex", hlines(-2) autonumber nofix marker("tab:wyoung3") title("Family-wise rejection proportions at \(\alpha = 0.05\), when testing hypotheses with multiple restrictions") footnote("`fn'") varlabels replace

** EOF
