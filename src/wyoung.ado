*! wyoung 2.0.1 30jan2026 by Julian Reif
* 2.0.1: fixed bug affecting permute() when performing nonlinear tests
* 2.0: added permute option (thanks to Adam Sacarny). renamed bootstraps option to reps and set default to 100. fixed factor variables bug
* 1.3.3: fixed bug where unadjusted p-val was reported assuming normality (affected Stata versions 14 and lower only)
* 1.3.2: error handling code added for case where user specifies both detail and noresampling
* 1.3.1: new controls option functionality. old functionality moved to controlsinteract
* 1.3: controls option added
* 1.2: familyp option now supports multiple variables. subgroup option added
* 1.1: familyp option now supports the testing of linear and nonlinear combinations of parameters
* 1.0.5: familyp option now supports factor variables and time-series operators
* 1.0.4: add support for commands that don't store p-values in r(table) (eg ivreg2)
* 1.0.3: better error handling for missing observations
* 1.0.2: cluster bootstrap now required when clustered standard errors are present; force option added
* 1.0.1: cluster bootstrap option added

***
* Notation
***

* K = number of hypotheses = num subgroups X num familyp X num outcomes X num controls
* N = number of bootstraps/permutations

program define wyoung, rclass

	version 13

	* Syntax 1: one model with multiple outcomes (and possibly multiple controls and subgroups)
	syntax [varlist(default=none)], cmd(string) familyp(string) [Reps(numlist int max=1 >0) BOOTstraps(numlist int max=1 >0) weights(varlist) noRESAMPling seed(numlist max=1) strata(varlist) cluster(varlist) subgroup(varname numeric) controls(string asis) controlsinteract(string asis) force detail SINGLEstep familypexp permute(varlist) PERMUTEProgram(string) replace]

	local outcome_vars "`varlist'"
	
	* Syntax 2: different models
	if "`outcome_vars'"=="" {
	
		* Perform a full trim to remove leading and trailing spaces from the cmd() option
		mata: st_local("cmd",strtrim(st_local("cmd")))
	
		* If user did NOT use compound double quotes in cmd(), pass through the string asis. This ensures the -tokenize- command below works properly.
		mata: if( substr(st_local("cmd"),1,1)!=char(96) ) stata("syntax, cmd(string asis) *");;
	}
		
	local user_cmd "wyoung `0'"
	
	tempfile bs
	tempname mat nlcom_b nlcom_V

	******
	* Error check syntax options
	******	

	* N = number of bootstraps/permutations (default=100). Provide legacy support for bootstraps() option.
	if "`reps'"!="" & "`bootstraps'"!="" {
		di as error "cannot specify both reps() and bootstraps()"
		exit 198
	}
	if "`reps'"=="" local reps `bootstraps'
	if "`reps'`bootstraps'"=="" local reps=100	
	local N = `reps'
	
	* Seed option
	if !mi("`seed'") {
		cap set seed `seed'
		if _rc {
			di as error "invalid syntax for option seed()"
			set seed `seed'
		}
	}
	
	* Strata and cluster options
	if !mi("`strata'") local strata_option "strata(`strata')"
	
	if !mi("`cluster'") {
		local cluster_option "cluster(`cluster')"
		
		* Bootstrapping will require a cluster ID variable
		if "`permute'"=="" {
			tempname id_cluster
			local idcluster_option "idcluster(`id_cluster')"
		}
		
		* Permutation var must be constant within strata/cluster groups (within-group stdev should be 0)
		if "`permute'"!="" {
			tempvar group
			egen long `group' = group(`strata' `cluster')
			foreach v of varlist `permute' {
				qui loneway `permute' `group'
				if r(sd_w) != 0 & !mi(r(sd_w)) {
					di as err "permutation variable `permute' is not constant within clusters"
					exit 9
				}
			}
			drop `group'
		}
	}
	
	* Permute option (default is bootstrapping)
	if "`permute'"!="" {
		foreach v of varlist `permute' {
			cap assert !mi(`v')
			if _rc & "`force'"=="" {
				di as error "permutation variable " as result "`v'" as error " has missing values; specify {bf:force} to override"
				exit 416
			}
			if _rc & "`force'"!="" {
				di as error "Warning: permutation variable " as result "`v'" as error " has missing values" _n
			}
		}
	}
	
	* Permute program option (default is _wyoung_shuffle)
	if `"`permuteprogram'"'!="" {

		if "`permute'"=="" {
			di as error "permute() required when specifying permuteprogram()"
			exit 198
		}
	
		* Split out cmd name and cmd options
		gettoken permutecmd 0: permuteprogram, parse(" ,")
		syntax [, *]
		local permutecmd_options `"`options'"'
		
		* Confirm valid program was passed as argument
		cap which `permute_cmd'
		if !_rc {
			qui program list `program_name'
		}
	}
	else local permutecmd _wyoung_shuffle

	* Detail options
	if "`detail'"!="" & "`resampling'"=="noresampling" {
		di as error "cannot specify both the detail and noresampling options"
		exit 198
	}

	* Subgroup option
	local num_subgroups = 1
	qui if "`subgroup'"!="" {
		
		if mi("`outcome_vars'") {
			di as error "subgroup() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		* Ensure subgroups are all integers
		local subgroup_err_msg = 0
		cap confirm float var `subgroup'
		if _rc==0 local subgroup_err_msg = 1
		cap confirm double var `subgroup'
		if _rc==0 local subgroup_err_msg = 1
		if `subgroup_err_msg'==1 {
			di as error "float or double variables not allowed in option subgroup()"
			exit 109
		}

		* Subgroup option triggers stratified resampling, unless user overrides
		if mi("`strata'") local strata_option "strata(`subgroup')"
		
		* Subgroup option not allowed with a command that contains an "if" 
		if strpos(`"`cmd'"', " if ") {
			di as error "if expressions not allowed when specifying subgroup()"
			exit 198
		}
		
		levelsof `subgroup'
		local subgroup_vals `"`r(levels)'"'
		local num_subgroups : word count `subgroup_vals'
	}
	
	* CONTROLVARS needs to be specified with either controls() or controlsinteract()
	if strpos(`"`cmd'"'," CONTROLVARS") & (`"`controlsinteract'"'=="" & `"`controls'"'=="") {
		di as error "cannot have {it:CONTROLVARS} without specifying option {cmd:controls()} or {cmd:controlsinteract()}"
		exit 198		
	}	
	
	* Cannot specify both CONTROLS and CONTROLVARS
	if (`"`controlsinteract'"'!="" & `"`controls'"'!="") {
		di as error "cannot specify both {cmd:controls()} and {cmd:controlsinteract()}"
		exit 198
	}
	
	* Controlsinteract option (multiplies number of hypotheses being tested)
	local num_sets_controls = 1
	qui if `"`controlsinteract'"'!="" {
		
		if mi("`outcome_vars'") {
			di as error "controlsinteract() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		if !strpos("`cmd'"," CONTROLVARS") {
			di as error "did not specify {it:CONTROLVARS} in option {cmd:cmd()}"
			exit 198
		}		
		
		* Split out the different control sets
		tokenize `"`controlsinteract'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))
			
			confirm variable `1'

			local controlvars_`k' `"`1'"'
			macro shift
		}
		
		local num_sets_controls = `k'		
	}

	* Controls option (simple 1:1 substitution, no increase in number of hypotheses)	
	qui if `"`controls'"'!="" {
		
		if mi("`outcome_vars'") {
			di as error "controls() option not allowed when employing Syntax 2 of wyoung"
			exit 198
		}
		
		if !strpos("`cmd'"," CONTROLVARS") {
			di as error "did not specify {it:CONTROLVARS} in option {cmd:cmd()}"
			exit 198
		}		
		
		* Split out the different control sets
		tokenize `"`controls'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))
			
			confirm variable `1'

			local controlvars_`k' `"`1'"'
			macro shift
		}
		local tmp : word count `outcome_vars'
		if `k'!=`tmp' {
			di as error "Number of varlists in controls() option does not equal the number of outcomes"
			exit 198
		}
	}		
	
	******
	* Syntax 1: user specifies varlist that will replace "OUTCOMEVAR"
	******
	if "`outcome_vars'"!="" {

		local num_outcomes : word count `outcome_vars'
		
		* If user specified familypexp (rare), then the input is a single lincom/nlcom expression, not a varlist
		local num_familypvars = 1
		if "`familypexp'"=="" {
			_wyoung_fvexpandnobase `familyp'
			local familyp `r(varlist)'
			local num_familypvars : word count `familyp'
		}

		* Ensure that "OUTCOMEVAR" is present in the command
		if !strpos("`cmd'"," OUTCOMEVAR ") {
			di as error "did not specify {it:OUTCOMEVAR} in option {cmd:cmd()}"
			exit 198
		}
		
		* If weights are specified, ensure there is exactly one weight variable for each outcome
		* Note: this option is undocumented
		if "`weights'"!="" {
			
			local num_weightvars : word count `weights'
			if "`num_weightvars'"!="`num_outcomes'" {
				di as error "number of weight vars = `num_weightvars' != `num_outcomes' = number of outcomes"
				exit 198
			}
			
			forval w = 1/`num_weightvars' {
				tokenize `weights'
				local weightvar_`w' ``w''
			}
		}

		* K = number of hypotheses = subgroups X outcomes X family pvars X control sets
		local K = `num_subgroups' * `num_familypvars' * `num_outcomes' * `num_sets_controls'
		
		* For each hypothesis k=1...K, define the outcome, familyp, and regression command
		local k = 1
		forval s = 1/`num_subgroups' {
			
			if "`subgroup'"!="" {
				tokenize `subgroup_vals'
				local subgroup_touse "``s''"
			}
					
			forval f = 1/`num_familypvars' {
				
				if `num_familypvars'==1 local familyp_touse "`familyp'"
				else {
					tokenize `familyp'
					local familyp_touse "``f''"
				}
				
				forval c = 1/`num_sets_controls' {
					
					* Default is blank; otherwise controlsinteract() option operates here; overwritten below if controls() was specified instead
					local controls_touse "`controlvars_`c''"
					
					forval i = 1/`num_outcomes' {
						
						if `"`controls'"'!="" local controls_touse "`controlvars_`i''"

						tokenize `outcome_vars'
						local outcomevar_`k' "``i''"
						local familyp_`k' "`familyp_touse'"
						local subgroup_`k' "`subgroup_touse'"
						local controls_`k' "`controls_touse'"
						
						* Baseline regression (note: WEIGHTVAR substitution here is an undocumented feature; WEIGHTVAR is numbered from i=1...num_outcomes)
						local cmdline_`k': subinstr local cmd         "OUTCOMEVAR" "`outcomevar_`k''", word
						local cmdline_`k': subinstr local cmdline_`k' "WEIGHTVAR"  "`weightvar_`i''"
						local cmdline_`k': subinstr local cmdline_`k' "CONTROLVARS"  "`controls_`k''"
						
						* Subgroup option: insert an if clause in front of the comma (if present)
						if "`subgroup'"!="" {
							local tmp `"`cmdline_`k''"'
							local index_comma = strpos(`"`tmp'"', ",")
							if `index_comma'==0 {
								local cmd_part1 `"`tmp'"'
							}
							else {
								mata: st_local("cmd_part1", substr(st_local("tmp"), 1, strpos(st_local("tmp"), ",")-1))
								mata: st_local("cmd_part2", substr(st_local("tmp"), strpos(st_local("tmp"), ","), .))
							}

							local cmdline_`k' `"`cmd_part1' if `subgroup'==`subgroup_touse'`cmd_part2'"'
						}

						local k = `k'+1
					}
				}
			}
		}
	}
	
	******
	* Syntax 2: user specifies each individual model
	******
	else {

		* Split out the different models
		tokenize `"`cmd'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))

			* Strip leading and trailing quotes, if present (occurs when user specifies compound double quotes)
			mata: if( substr(st_local("1"),1,1)==char(34) & substr(st_local("1"),-1,1)==char(34) ) st_local(  "1", substr(st_local("1"), 2, strlen(st_local("1"))-2)  );;

			local cmdline_`k' `"`1'"'
			macro shift
		}
		
		* K = number of hypotheses = number of models
		local K = `k'
		
		* Split out the familyp variable/exp that corresponds to each model
		tokenize `"`familyp'"'

		local k = 0
		while `"`1'"' != "" {
			local k = `k'+1

			* Perform a full trim to remove leading and trailing spaces
			mata: st_local("1",strtrim(st_local("1")))

			* Strip leading and trailing quotes, if present (occurs when user specifies compound double quotes)
			mata: if( substr(st_local("1"),1,1)==char(34) & substr(st_local("1"),-1,1)==char(34) ) st_local(  "1", substr(st_local("1"), 2, strlen(st_local("1"))-2)  );;

			local familyp_`k' `"`1'"'
			if "`familypexp'"=="" fvunab familyp_`k' : `familyp_`k''
			
			macro shift
		}		
		local num_familypvars = `k'
		
		* If user specifiy only a single family p command, assume it applies to all models
		if `num_familypvars'==1 {
			forval k = 2/`K' {
				local familyp_`k' `familyp_1'
			}				
		}
		else if `num_familypvars'!= `K' {
			di as error "Number of familyp commands, `num_familypvars', does not match number of models, `K'"
			exit 198
		}
		
		* num_outcomes = 0 indicates syntax 2 was called
		local num_outcomes = 0
	}
	
	******
	* Step 1: Estimate the initial, unadjusted models
	******
	di as text "Estimating family-wise adjusted {it:p}-values for " as result `K' as text " hypothesis tests"

	qui forval k = 1/`K' {

		if "`subgroup'"!="" & mod(`k',`num_outcomes')==1                         noi di as text _n "subgroup: " as result `"`subgroup_`k''"'
		else if (`num_outcomes'==0 | mod(`k',`num_outcomes')==1)                 noi di as text ""
		if `num_outcomes'==0 | mod(`k',`num_outcomes')==1                        noi di as text "familyp: " as result `"`familyp_`k''"'
		if `"`controlsinteract'"'!="" & (`num_outcomes'==0 | mod(`k',`num_outcomes')==1) noi di as text "controls: " as result `"`controls_`k''"'
		
		noi di in yellow _skip(4) `"`cmdline_`k''"'
	
		tempname p_`k' ystar_`k'

		* Run regression k
		`cmdline_`k''
		
		local N_`k' = e(N)
		if "`e(vce)'"=="cluster" local vce_cluster 1
		if !mi("`e(depvar)'") local outcomevar_`k' "`e(depvar)'"
		
		* Calculate _b, _se, and the associated unadjusted p-val using lincom (or nlcom if _rc==131, which indicates nonlinearity)
		cap lincom `familyp_`k''
		if !_rc {
			local beta_`k' = r(estimate)
			local stderr_`k' = r(se)
			scalar `p_`k'' = r(p)
		}
		else if _rc==131 {
			cap nlcom `familyp_`k''
			if !_rc {
				
				matrix `nlcom_b' = r(b)
				matrix `nlcom_V' = r(V)
				
				local beta_`k' = `nlcom_b'[1,1]
				local stderr_`k' = sqrt(`nlcom_V'[1,1])
				scalar `p_`k'' = 2*normal(-abs(`beta_`k''/`stderr_`k''))					
				matrix drop `nlcom_b' `nlcom_V'
			}
			else {
				noi di as error _n `"`familyp_`k'' is invalid syntax for {cmd:lincom} and {cmd:nlcom}"'
				error _rc
			}
		}
		else {
			noi di as error "The following error occurred when running the command " as result `"lincom `familyp_`k'':"'
			lincom `familyp_`k''
		}
		
		* Ensure that beta and pval are recovered
		if `beta_`k''==. {
			noi di as error "coefficient estimate for " as result "`familyp_`k''" as error " not available after running the command " as result `"`cmdline_`k''"'
			exit 504			
		}		
		
		if `p_`k''==. {

			local tstat = abs(`beta_`k'' / `stderr_`k'')
			
			* Calculate p-val using degrees of freedom from e(df_r), if it exists. Else assume normality
			cap local df = `e(df_r)'
			cap confirm number `df'
			if !_rc scalar `p_`k'' = tprob(`df', abs(`tstat'))
			else    scalar `p_`k'' = 2*(1-normprob(abs(`tstat')))

			if `p_`k''==. {
				noi di as error "p-value not available and could not be calculated when running the command " as result `"`cmdline_`k''"'
				exit 504
			}
		}
	}

	* Issue error if user is estimating a model with clustered standard errors AND did not specify a resampling cluster (unless force option specified)
	if "`vce_cluster'"=="1" & mi("`cluster'") {
		if mi("`force'") {
			di as error "estimating model with clustered standard errors, but {bf:cluster()} option was not specified; specify {bf:force} to override"
			exit 198
		}
		else {
			di as error "Warning: estimating model with clustered standard errors, but {bf:cluster()} option was not specified" _n
		}
	}

	preserve
	
	******
	* Resample the data and calculated Westfall-Young adjusted p-vals
	******	
	if "`resampling'"!="noresampling" {
		
		if "`permute'"=="" noi di as text _n "Performing " as result "`reps' " as text "bootstrap replications..."
		else               noi di as text _n "Performing " as result "`reps' " as text "permutations..."		

		***
		* Step 2(a). Loop over each sample i and calculate pstar's
		***
		qui forval i = 1/`N' {

			* Do a permutation, OR draw a random sample with replacement
			if "`permute'"!="" {
				
				* Default permute program is _wyoung_shuffle
				`permutecmd' `permute', `strata_option' `cluster_option' `permutecmd_options' 
            }
            else {
                bsample, `strata_option' `cluster_option' `idcluster_option'
				
				if "`cluster'"!="" {
					drop `cluster'
					ren `id_cluster' `cluster'
				}				
            }

			* Calculate pstar for each model, using test or testnl
			qui forval k = 1/`K' {

				cap `cmdline_`k''
				if _rc {
					noi di as error _n "The following error occurred when running the command " as result `"`cmdline_`k''"' as error " on a bootstrap/permutation sample:"
					error _rc
				}
				local Ni_`k' = e(N)

				* Under permutation (randomization inference), which breaks link between X and Y, we test the null coef = 0. Under bootstrapping, which preserves link between X and Y, we test coef = original beta
				if ("`permute'"!="") local complete_null = 0
				else                 local complete_null = `beta_`k''

				cap test `familyp_`k'' == `complete_null'
				if _rc==131 {
					cap testnl `familyp_`k'' == `complete_null'
					if _rc {
						noi di as error _n `"`familyp_`k'' is invalid syntax for {cmd:test} and {cmd:testnl}"'
						error _rc
					}
				}
				else if _rc {
					noi di as error _n "The following error occurred when running the command " as result `"test `familyp_`k'' == `complete_null'"' as error " on a bootstrap/permutation sample:"
					test `familyp_`k'' == `complete_null'
				}
				local pstar_`k' = r(p)
			}
			
			* Store results from each model k
			drop _all
			set obs `K'
			gen i = `i'
			gen k = _n
			gen pstar = .
			gen p = .
			if !mi("`detail'") gen N = .
			qui forval k = 1/`K' {
				replace pstar = `pstar_`k'' in `k'
				replace p  = `p_`k'' in `k'
				if !mi("`detail'") replace N = `Ni_`k'' in `k'
			}
			
			if `i'>1 append using "`bs'"
			save "`bs'", replace
			
			restore, preserve
		}

		***
		* Calculate Westfall-Young adjusted p-vals
		***
		use "`bs'", clear

		* Step 2(b). Enforce monotonicity with successive minima to produce step-down p-values. Include k in the sort to break ties.
		gsort i -p k	
		qui by i: gen qstar = pstar if _n==1
		qui by i: replace qstar = min(qstar[_n-1],pstar) if _n>1
		
		* Define minimum for single-step p-value
		qui by i: egen qstar1 = min(pstar)
		
		if !mi("`detail'") local Nstats "(mean) Navg=N (min) Nmin=N (max) Nmax=N"

		* Steps 3 and 4. Calculate step-down and single-step Westfall-Young adjusted p-value
		qui gen counter  = qstar <=p
		qui gen counter1 = qstar1<=p
		collapse (sum) counter* `Nstats', by(k) fast
		qui gen pwyoung  = counter /`N'
		qui gen pwyoung1 = counter1/`N'
		drop counter*
		
		assert pwyoung<=pwyoung1
	}
	qui else {
		drop *
		set obs `K'
		gen k = _n
	}
	
	******
	* Enforce monotonicity, format results, and calculate some additional adjustments
	******	
	
	* Fill in descriptive information
	qui gen model=""
	qui gen outcome=""
	qui gen double coef = .
	qui gen double stderr = .
	qui gen double p = .
	qui gen familyp = ""
	if !mi("`subgroup'") qui gen subgroup = .
	if !mi(`"`controlsinteract'"') qui gen controlspec = ""
	if !mi("`detail'") qui gen N = .

	qui forval k = 1/`K' {
		replace model = `"`cmdline_`k''"'      if k==`k'
		replace outcome = "`outcomevar_`k''"   if k==`k'
		replace coef = `beta_`k''              if k==`k'
		replace stderr = `stderr_`k''          if k==`k'
		replace p  = `p_`k''                   if k==`k'
		replace familyp = "`familyp_`k''"                                          if k==`k'
		if !mi("`subgroup'")           replace subgroup = `subgroup_`k''           if k==`k'
		if !mi(`"`controlsinteract'"') replace controlspec = "`controls_`k''"      if k==`k'
		if !mi("`detail'")             replace N        = `N_`k''                  if k==`k'
	}	
	
	* Step 5. Enforce monotonicity using successive maximization.  Include k in the sort to break ties.
	sort p k
	qui cap replace pwyoung  = max(pwyoung[_n-1]  ,pwyoung)   if _n>1
	qui cap replace pwyoung1 = max(pwyoung1[_n-1] ,pwyoung1)  if _n>1

	* Calculate Holm-Bonferroni and Holm-Sidak step-down corrections
	tempname j
	qui gen `j' = _N-_n+1
	
	qui gen double pbonf = min(p*`j',1) if _n==1
	qui replace    pbonf = min(max(p*`j',pbonf[_n-1]),1) if _n>1

	qui gen double psidak = min((1-(1-p)^(`j')),1) if _n==1
	qui replace    psidak = min(max((1-(1-p)^(`j')),psidak[_n-1]),1) if _n>1
	
	if !mi("`detail'") local Ns "N Navg Nmin Nmax"
	if !mi("`subgroup'") local subgroup subgroup
	if !mi(`"`controlsinteract'"') local controlspec controlspec
	
	label var pbonf "Bonferroni-Holm p-value"
	label var psidak "Sidak-Holm p-value"
	label var stderr "Unadjusted standard error"
	label var p  "Unadjusted p-value"
	cap label var pwyoung   "Westfall-Young adjusted p-value"
	cap label var pwyoung1 "Westfall-Young adjusted p-value (single-step)"
	cap label var N "Number of obs"
	cap label var Navg "Average number of obs (reps)"
	cap label var Nmin "Min number of obs (reps)"
	cap label var Nmax "Max number of obs (reps)"
	
	assert psidak<=pbonf+0.00000000001
	foreach v of varlist p* {
		assert `v'<=1
	}
	
	* Single-step values
	if !mi("`singlestep'")  {
		gen double pbonf1 = min(`K'*p,1)
		gen double psidak1 = 1-((1-p)^`K')
		label var pbonf1 "Bonferroni p-value (single-step)"
		label var psidak1 "Sidak p-value (single-step)"
	}
	else cap drop pwyoung1
	
	sort k
	drop `j'
	order k model outcome `controlspec' familyp `subgroup' coef stderr p `Ns'
	list
	
	mkmat coef stderr p*, matrix(`mat')
	
	if "`replace'"=="replace" restore, not
	
	******
	* Return values
	******
	return matrix table `mat'
	
	return local cmdline `user_cmd'
	return local cmd wyoung
end

* Default permutation program
program define _wyoung_shuffle

	syntax varlist(min=1) [, strata(varname) cluster(varname)]

	tempvar randsort clfirst n_init

	* If no cluster var is specified, each individual observation will be a "cluster"
	if "`cluster'"=="" {
		tempvar cluster
		gen long `cluster' = _n
	}

	* Identify first observation in each cluster
	sort `strata' `cluster', stable
	by `strata' `cluster': gen byte `clfirst' = 1 if _n==1		
	
	* Within strata, for all first observations within clusters, save their position in the data set
	sort `strata' `clfirst', stable
	by `strata' `clfirst': gen long `n_init' = _n if `clfirst'!=.
	
	* Reshuffle these first observations within strata, and take the treatment status from the observation which was at this position before
	gen double `randsort' = runiform()
	sort `strata' `clfirst' `randsort', stable
	foreach var in `varlist' {		
		tempvar new`var'
		local type : type `var'
		by `strata' `clfirst': gen `type' `new`var'' = `var'[`n_init']
	}
	
	* Copy treatment status to all observations in the same cluster
	sort `strata' `cluster' `clfirst', stable
	foreach var in `varlist' {
		by `strata' `cluster': replace `new`var'' = `new`var''[_n-1] if mi(`new`var'')
		drop `var'
		ren `new`var'' `var'
	}
end

/* EXAMPLE SHUFFLES (QC code)

* Simple shuffle
set seed 21
qui forval x = 1/100 {
	noi di "`x'"
	set seed `x'
sysuse auto, clear
label drop origin
gen cluster = floor(mpg/5)
gen strata = floor(mod(_n,7))
drop price-gear_ratio
gen foreign0 = foreign

_wyoung_shuffle foreign
egen sum1 = sum(foreign0)
egen sum2 = sum(foreign)
assert sum1==sum2
drop sum* foreign
gen foreign = foreign0


* Stratified shuffle
_wyoung_shuffle foreign, strata(strata)
sort strata
by strata: egen sum1 = sum(foreign0)
by strata: egen sum2 = sum(foreign)
assert sum1==sum2
drop sum* strata foreign foreign0

* Cluster shuffle
bysort cluster: gen byte t = round(uniform()) if _n==1
by cluster: egen foreigncl = mean(t)
gen foreigncl0 = foreigncl
drop t
preserve

_wyoung_shuffle foreigncl, cluster(cluster)
bysort cluster: keep if _n==1
egen sum1 = sum(foreigncl0)
egen sum2 = sum(foreigncl)
assert sum1==sum2
drop sum*
restore

* Stratified clustered shuffle
bysort cluster: gen byte t = round(uniform()) if _n==1
by cluster: egen stratacl = mean(t)
drop t
_wyoung_shuffle foreigncl, cluster(cluster) strata(stratacl)
bysort cluster: keep if _n==1
bysort stratacl: egen sum1 = sum(foreigncl0)
bysort stratacl: egen sum2 = sum(foreigncl)
assert sum1==sum2
	}
*/


program _wyoung_fvexpandnobase
    
	* Record original setting
	local fvbase = c(fvbase)
	set fvbase off

	cap noi fvexpand `0'

	* Restore original setting
	set fvbase `fvbase'

	if _rc exit _rc
end

** EOF
