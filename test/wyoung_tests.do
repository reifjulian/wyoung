local linesize = c(linesize)
cscript wyoung adofile wyoung
set linesize `linesize'

clear
adopath ++ "../src"
version 15
set more off
set tracedepth 1
* set trace on


*********************************************
* Example 1
*********************************************
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace
cf _all using "compare/examp1.dta"

* Quotes are unnecessary in -cmd-
sysuse auto, clear
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) bootstraps(100) seed(20) replace
cf _all using "compare/examp1.dta"

* Resampling option
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace noresampling
cf _all using "compare/examp1.dta"

* Compound double quotes
sysuse auto, clear
wyoung, cmd(`" `""regress mpg displacement length""' `""regress headroom displacement length""' `""regress turn displacement length""' "') familyp(displacement) bootstraps(100) seed(20) replace
cf _all using "compare/examp1.dta"

* Compound double quotes with clustering
sysuse auto, clear
wyoung, cmd(`" `""regress mpg displacement length, cluster(foreign)""' `""regress headroom displacement length, cluster(foreign)""' `""regress turn displacement length, cluster(foreign)""' "') cluster(foreign) familyp(displacement) bootstraps(10) seed(20) replace
rename (p*) (new_p*)
merge 1:1 k model outcome familyp using "compare/examp1_cluster.dta", assert(match) nogenerate
foreach v of varlist p* {
	assert abs(`v' - new_`v')<0.0000001
}

* Alternative syntax
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(_b[displacement]) bootstraps(100) seed(20) replace familypexp
cf k-outcome coef-psidak using "compare/examp1.dta"

sysuse auto, clear
rcof noi wyoung, cmd(`" `""regress mpg displacement length""' `""regress headroom displacement length""' `""regress turn displacement length""' "') familyp(_b[displacement) bootstraps(100) seed(20) replace
assert _rc==111
wyoung, cmd(`" `""regress mpg displacement length""' `""regress headroom displacement length""' `""regress turn displacement length""' "') familyp(_b[displacement]) bootstraps(100) seed(20) replace familypexp
cf k-outcome coef-psidak using "compare/examp1.dta"

sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(_b[displacement]-0) bootstraps(100) seed(20) replace familypexp
cf k-outcome coef-psidak using "compare/examp1.dta"

*********************************************
* Example 2 (alternative syntax)
*********************************************
sysuse auto, clear
wyoung, cmd("regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length") familyp(displacement) bootstraps(100) seed(20) replace
cf _all using "compare/examp1.dta"

*********************************************
* Example 3 (clustered standard errors)
*********************************************
sysuse auto, clear
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length, cluster(rep78)) cluster(rep78) familyp(displacement) bootstraps(100) seed(20)
cf _all using "compare/examp3.dta"

* Failing to specify bootwstrap cluster should generate an error when clustering, unless force option is specified
rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length, cluster(rep78)) familyp(displacement) bootstraps(10) seed(20)
assert _rc==198
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length, cluster(rep78)) familyp(displacement) bootstraps(10) seed(20) force

*********************************************
* Subgroup example (two outcomes X two subgroups)
*********************************************

sysuse auto, clear
wyoung, cmd("regress mpg displacement length if foreign==0" "regress headroom displacement length if foreign==0" "regress mpg displacement length if foreign==1" "regress headroom displacement length if foreign==1" ) familyp(displacement) bootstraps(100) seed(20) detail replace
cf _all using "compare/examp_subgroups.dta"

* Same, but with compound quoted strings
sysuse auto, clear
wyoung, cmd(`" `""regress mpg displacement length if foreign=="Domestic":origin""' `""regress headroom displacement length if foreign=="Domestic":origin""' `""regress mpg displacement length if foreign=="Foreign":origin""' `""regress headroom displacement length if foreign=="Foreign":origin""' "' ) familyp(displacement) bootstraps(100) seed(20) detail replace
cf outcome-psidak using "compare/examp_subgroups.dta"

* Stratified random sample example
sysuse auto, clear
wyoung, cmd("regress mpg displacement length if foreign==0" "regress headroom displacement length if foreign==0" "regress mpg displacement length if foreign==1" "regress headroom displacement length if foreign==1" ) familyp(displacement) bootstraps(100) seed(20) strata(foreign) detail replace
cf _all using "compare/examp_subgroups_strata.dta"

* reg/areg/reghdfe example
sysuse auto, clear
wyoung mpg headroom turn, cmd("areg OUTCOMEVAR displacement length, absorb(foreign)") familyp(displacement) bootstraps(50) seed(20) replace
cf _all using "compare/examp_areg.dta"

sysuse auto, clear
wyoung mpg headroom turn, cmd("reg OUTCOMEVAR displacement length i.foreign") familyp(displacement) bootstraps(50) seed(20) replace
cf outcome familyp pwyoung using "compare/examp_areg.dta"

sysuse auto, clear
wyoung mpg headroom turn, cmd("reghdfe OUTCOMEVAR displacement length, absorb(foreign)") familyp(displacement) bootstraps(50) seed(20) replace
cf outcome familyp pwyoung using "compare/examp_areg.dta"


* Invalid seeds should generate a syntax error
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVAR weight length) familyp(weight) bootstraps(10) seed(x)
assert _rc==198

* For main syntax, "OUTCOMEVAR" must be present
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVA weight length) familyp(weight) bootstraps(10)
assert _rc==198

rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVARS weight length) familyp(weight) bootstraps(10)
assert _rc==198

rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVARweight length) familyp(weight) bootstraps(10)
assert _rc==198


* familyp var must be listed in all regressions
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(weight) bootstraps(100) seed(20) replace
assert _rc==111

*nbootstraps must be greater than 0
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(weight) bootstraps(0) seed(20) replace
assert _rc==198

* clustering example
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length, cluster(turn)") familyp(displacement) bootstraps(100) seed(20) replace cluster(turn)
cf outcome familyp pwyoung using "compare/examp_cluster.dta"

* Control var examples
sysuse auto, clear
wyoung mpg headroom, cmd("regress OUTCOMEVAR displacement CONTROLVARS") familyp(displacement) controls("length weight" "gear_ratio") bootstraps(50) seed(20) replace
cf _all using "compare/controlvars.dta"

sysuse auto, clear
wyoung, cmd(`" `""regress mpg displacement length weight""' `""regress headroom displacement length weight""' `""regress mpg displacement gear_ratio""' `""regress headroom displacement gear_ratio""' "') familyp(displacement) bootstraps(50) seed(20) replace
cf _all using "compare/controlvars.dta"

* Undocumented weights() option 
use "compare/wellness.dta", clear
wyoung spend_0715_0716 spendOff_0715_0716 spendHosp_0715_0716 spendRx_0715_0716 nonzero_spend_0715_0716, familyp(hra_c_yr1) bootstraps(100) seed(11) cmd("_regress OUTCOMEVAR hra_c_yr1 [aw=WEIGHTVAR], robust") weights(covg_0715_0716 covg_0715_0716 covg_0715_0716 covg_0715_0716 constant) replace
cf _all using "compare/wellness_wy.dta"

***
* Missing data examples
***
* 1) All data missing for one regression
sysuse auto, clear
keep in 1/10
replace mpg=. in 1/10
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace
assert _rc==2000

* 2) Insufficient data to calculate p-val in initial regression
sysuse auto, clear
keep in 1/10
replace mpg=. in 1/7
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace
assert _rc==504

* 3) Missing data in bootstrapped regression
sysuse auto, clear
keep in 1/10
replace mpg=. in 1/6
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace
assert _rc==2001

* 4) Insufficient variation
sysuse auto, clear
replace mpg = 100
cap wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) bootstraps(100) seed(20) replace

*****
* ivreg2 example (requires ivreg2 to be installed)
*****

sysuse auto, clear
wyoung trunk foreign rep78, cmd("ivreg2 OUTCOMEVAR (length=price)") familyp(length) bootstraps(100) seed(20) replace
cf _all using "compare/iv1.dta"

* Generate error if user incorrectly asks for familyp value for the instrument
sysuse auto, clear
rcof noi wyoung trunk foreign rep78, cmd("ivreg2 OUTCOMEVAR (length=price)") familyp(price) bootstraps(100) seed(20) replace
assert _rc == 111

*******************************
* Support for factor variables
*******************************

sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement i.foreign") familyp("1.foreign") bootstraps(5) seed(20) replace
cf _all using "compare/examp_fv.dta"

sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement i.foreign") familyp("i1.foreign") bootstraps(5) seed(20) replace
cf _all using "compare/examp_fv.dta"

* Generate error if user inputs factor variable that is not estimated
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement i.foreign") familyp("i0.foreign") bootstraps(5) seed(20) replace
assert _rc==504

* Generate error if user inputs nonsense factor variables
sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement i.foreign") familyp("i.weight#i.weight") bootstraps(5) seed(20) replace
assert _rc==198

rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement i.foreign") familyp("i.weight2#i.weight") bootstraps(5) seed(20) replace
assert _rc==111


*********************************************
* Multiple restrictions
*********************************************
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(length+50*displacement) bootstraps(100) seed(20) replace familypexp
cf _all using "compare/multiple1.dta"

sysuse auto, clear
cap wyoung length headroom price, cmd("regress OUTCOMEVAR mpg weight  turn displacement") familyp(_b[weight]*_b[displacement]=1) bootstraps(100) seed(20) replace familypexp
assert _rc==198
replace length = length*100
replace headroom = headroom*1000
wyoung length headroom price, cmd("regress OUTCOMEVAR mpg weight  turn displacement") familyp(_b[weight]*_b[displacement]-1) bootstraps(100) seed(20) replace familypexp
cf _all using "compare/multiple2.dta"

*********************************************
* Multiple treatments
*********************************************
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement length) bootstraps(100) seed(20) replace
cf _all using "compare/multiple3.dta"

sysuse auto, clear
wyoung, cmd("regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length" "regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length" ) familyp(displacement displacement displacement length length length) bootstraps(100) seed(20) replace
cf _all using "compare/multiple3.dta"

sysuse auto, clear
wyoung, cmd("regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length" "regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length" ) familyp(`" "displacement" "displacement" "displacement" "length" "length" "length" "') bootstraps(100) seed(20) replace
cf _all using "compare/multiple3.dta"


*********************************************
* subgroup examples
*********************************************

* subgroup() option
sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) subgroup(foreign) bootstraps(100) seed(20) replace
cf _all using "compare/subgroup1.dta"

* Alternative way to replicate the subgroup() option
sysuse auto, clear
wyoung, cmd("regress mpg displacement length if foreign==0" "regress headroom displacement length if foreign==0" "regress turn displacement length if foreign==0" "regress mpg displacement length if foreign==1" "regress headroom displacement length if foreign==1" "regress turn displacement length if foreign==1") familyp(displacement) bootstraps(100) strata(foreign) seed(20) replace
cf _all using "compare/subgroup1.dta"

sysuse auto, clear
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) subgroup(gear_ratio) bootstraps(100) seed(20) replace
assert _rc==109
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length if price<100") familyp(displacement) subgroup(foreign) bootstraps(100) seed(20) replace
assert _rc==198
rcof noi wyoung, cmd(`" `""regress mpg displacement length, cluster(foreign)""' `""regress headroom displacement length, cluster(foreign)""' `""regress turn displacement length, cluster(foreign)""' "') cluster(foreign) familyp(displacement) subgroup(foreign) bootstraps(10) seed(20) replace
assert _rc==198
rcof noi wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length") familyp(displacement) subgroup(price) bootstraps(100) seed(20) replace
assert _rc==2001

sysuse auto, clear
wyoung mpg headroom turn, cmd("regress OUTCOMEVAR displacement length, robust") familyp(displacement length) subgroup(foreign) bootstraps(100) seed(20) replace
cf _all using "compare/subgroup2.dta"


*********************************************
* GitHub examples
*********************************************
sysuse auto.dta, clear
set seed 20
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) bootstraps(100)
cf _all using "compare/gh_example1.dta"

sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(displacement) subgroup(foreign) boot(100)
cf _all using "compare/gh_example2.dta"

sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(displacement length) subgroup(foreign) boot(100)
cf _all using "compare/gh_example3.dta"

sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR length CONTROLVARS) controls("trunk" "weight" "trunk weight") familyp(length) boot(100)
cf _all using "compare/gh_example4.dta"

sysuse auto.dta, clear
set seed 20
local yvars "mpg headroom turn"
wyoung `yvars', cmd(reg OUTCOMEVAR displacement length) familyp(length+50*displacement) familypexp boot(100)
cf _all using "compare/gh_example5.dta"

*********************************************
* help file examples
*********************************************
sysuse auto.dta, clear
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) bootstraps(100) seed(20)
cf _all using "compare/hf_example1.dta"

wyoung, cmd("regress mpg displacement length" "regress headroom displacement length" "regress turn displacement length") familyp(displacement) bootstraps(100) seed(20)
cf _all using "compare/hf_example2.dta"
 
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length, cluster(rep78)) cluster(rep78) familyp(displacement) bootstraps(100) seed(20)
cf _all using "compare/hf_example3.dta"
  
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement) subgroup(foreign) bootstraps(100) seed(20)
cf _all using "compare/hf_example4.dta"
  
wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(displacement length) subgroup(foreign) bootstraps(100) seed(20)
cf _all using "compare/hf_example5.dta"

wyoung mpg headroom turn, cmd(regress OUTCOMEVAR displacement length) familyp(length+50*displacement) familypexp bootstraps(100) seed(20)
cf _all using "compare/hf_example6.dta"
  
wyoung mpg, cmd(regress OUTCOMEVAR displacement CONTROLVARS) familyp(displacement) controls("headroom" "turn" "headroom turn") bootstraps(100) seed(20)
cf _all using "compare/hf_example7.dta"

******************************************************************************************************************************************
* Simulations (NSIM=1):
******************************************************************************************************************************************

local NSIM    = 1
local NBOOT   = 100

* Normal data
set seed 20
local num_obs = 100

qui forval s = 1/`NSIM' {

	drop _all
	set obs `num_obs'

	gen x = rnormal(0,5)
	
	qui forval x = 1/10 {
		
		gen e_`x' = rnormal(0,5)
		gen y_`x' = e_`x'
	}

	wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x") familyp(x) singlestep replace
	compress	
}
keep k model outcome familyp p*
rename (p*) (new_p*)
merge 1:1 k model outcome familyp using "compare/example_normal.dta", assert(match) nogenerate
foreach v of varlist p* {
	assert abs(`v' - new_`v')<0.0000001
}

* Non-normal data
set seed 20
local num_obs = 10

qui forval s = 1/`NSIM' {

	drop _all
	set obs `num_obs'
	
	* Mean of lognormal distribution is sqrt(e)=sqrt(exp(1))
	forval y = 1/10 {
		gen y`y' = exp(rnormal(0,1)) - sqrt(exp(1))
	}
	
	gen byte dummy=1
	wyoung y*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR dummy, nocons") singlestep familyp(dummy) replace
	compress
}
keep k model outcome familyp p*
rename (p*) (new_p*)
merge 1:1 k model outcome familyp using "compare/example_nonnormal.dta", assert(match) nogenerate
foreach v of varlist p* {
	assert abs(`v' - new_`v')<0.0000001
}

* Correlated outcomes example
set seed 20
local num_obs = 100
local num_outcomes = 20
local beta = 0.05

qui forval s = 1/`NSIM' {

	drop _all
	set obs `num_obs'

	* Generate correlated variables
	local elist "e_1"
	matrix c = I(`num_outcomes')
	forval r = 2/`num_outcomes' {
		forval c = 1/`=`r'-1' {
			matrix c[`r',`c']=0.75
		}
		local elist "`elist' e_`r'"
	}
	mata: st_replacematrix("c", makesymmetric(st_matrix("c")))
	corr2data `elist', corr(c)
	
	gen x = rnormal(0,5)
	forval x = 1/`num_outcomes' {
		gen y_`x' = `beta'*x + e_`x'
	}

	wyoung y_*, bootstraps(`NBOOT') cmd("_regress OUTCOMEVAR x") familyp(x) singlestep replace
	compress

}
keep k model outcome familyp p*
rename (p*) (new_p*)
merge 1:1 k model outcome familyp using "compare/example_correlated.dta", assert(match) nogenerate
foreach v of varlist p* {
	assert abs(`v' - new_`v')<0.0000001
}

* Subgroup example
set seed 20
local num_obs = 1000

qui forval s = 1/`NSIM' {

	drop _all
	set obs `num_obs'

	gen subgroup = mod(_n,10)+1
	
	gen x = rnormal(0,1)
	gen y = rnormal(0,1)
	
	wyoung , bootstraps(`NBOOT') cmd("_regress y x if subgroup==1" "_regress y x if subgroup==2" "_regress y x if subgroup==3" "_regress y x if subgroup==4" "_regress y x if subgroup==5" "_regress y x if subgroup==6" "_regress y x if subgroup==7" "_regress y x if subgroup==8" "_regress y x if subgroup==9" "_regress y x if subgroup==10") familyp(x) singlestep replace
	
	compress
}
keep k model outcome familyp p*
rename (p*) (new_p*)
merge 1:1 k model outcome familyp using "compare/example_subgroup.dta", assert(match) nogenerate
foreach v of varlist p* {
	assert abs(`v' - new_`v')<0.0000001
}


** EOF
