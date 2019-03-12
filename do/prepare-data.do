* load the scenario file  1
#d ;
infile id age value_prog1 cons_prog1 wealth_prog1 income_prog1 
	oop_prog1 rate_prog1 inv_prog1 fin_prog1 educ share_prog1 prog1 elig1 using data/simulations/simknow_prog1.dat , clear;
	#d cr

forvalues j = 2/12 {
* load j and save
preserve
#d ;
infile id age value_prog`j' cons_prog`j' wealth_prog`j' income_prog`j' 
	oop_prog`j' rate_prog`j' inv_prog`j' fin_prog`j' educ share_prog`j' prog`j' elig`j' using data/simulations/simknow_prog`j'.dat , clear;
	#d cr
save 	data/simulations/simknow_prog`j'.dta, replace
restore

* merge progj
merge 1:1 id age using data/simulations/simknow_prog`j'.dta
drop _merge
erase data/simulations/simknow_prog`j'.dta
}


foreach n in "delta_min" "delta_max" "pi0_min" "pi0_max" {
preserve
#d ;
infile id age value_base_`n' cons_base_`n' wealth_base_`n' income_base_`n' 
	oop_base_`n' rate_base_`n' inv_base_`n' fin_base_`n' educ 
	share_base_`n' base_`n' elig_base_`n' using data/simulations/simknow_baseline_`n'.dat , clear;
	#d cr
save 	data/simulations/simknow_baseline_`n'.dta, replace
restore
* merge progj
merge 1:1 id age using data/simulations/simknow_baseline_`n'.dta
drop _merge
erase data/simulations/simknow_baseline_`n'.dta
preserve
#d ;
infile id age value_prog12_`n' cons_prog12_`n' wealth_prog12_`n' income_prog12_`n' 
	oop_prog12_`n' rate_prog12_`n' inv_prog12_`n' fin_prog12_`n' educ 
	share_prog12_`n' prog12_`n' elig_prog12_`n' using data/simulations/simknow_prog12_`n'.dat , clear;
	#d cr
save 	data/simulations/simknow_prog12_`n'.dta, replace
restore
* merge progj
merge 1:1 id age using data/simulations/simknow_prog12_`n'.dta
drop _merge
erase data/simulations/simknow_prog12_`n'.dta
}


* load baseline and save
preserve
#d ;
infile id age value_base cons_base wealth_base income_base 
	oop_base rate_base inv_base fin_base educ_base share_base prog_base elig_base using data/simulations/simknow_baseline.dat , clear;
	#d cr
save data/simulations/simknow_baseline.dta, replace
restore

* merge baseline
merge 1:1 id age using data/simulations/simknow_baseline.dta
drop _merge

replace age = age + 1

* save policy dataset
save data/simulations/simknow_policy.dta, replace









