* load the scenario file  1
#d ;
infile id age value_prog1 cons_prog1 wealth_prog1 income_prog1 
	oop_prog1 rate_prog1 inv_prog1 fin_prog1 educ share_prog1 prog1 elig1 using data/simulations/simknow_prog1.dat , clear;
	#d cr

forvalues j = 2/11 {
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









