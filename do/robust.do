*  robustness

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(5,5,.)

egen everelig12 = total(elig12), by(id)
egen everprog12 = total(prog12), by(id)

local var "wealth"

qui sum `var'_prog12 if age==65&everelig12==1&everprog12==1
matrix result[1,1] = r(mean)
qui sum `var'_base   if age==65&everelig12==1&everprog12==1
matrix result[1,2] = r(mean)
qui sum `var'_prog12 if age==65&everelig12==1&everprog12==0
matrix result[1,3] = r(mean)
qui sum `var'_prog12 if age==65&everelig12==0
matrix result[1,4] = r(mean)
qui sum everprog12 if age==65&everelig12==1
matrix result[1,5] = r(mean)


local j = 2
foreach n in "delta_min" "delta_max" "pi0_min" "pi0_max" {
egen everelig12_`n' = total(elig_prog12_`n'), by(id)
egen everprog12_`n' = total(prog12_`n'), by(id)
qui sum `var'_prog12_`n' if age==65&everelig12_`n'==1&everprog12_`n'==1
matrix result[`j',1] = r(mean)
qui sum `var'_base_`n'   if age==65&everelig12_`n'==1&everprog12_`n'==1
matrix result[`j',2] = r(mean)
qui sum `var'_prog12_`n' if age==65&everelig12_`n'==1&everprog12_`n'==0
matrix result[`j',3] = r(mean)
qui sum `var'_prog12_`n' if age==65&everelig12_`n'==0
matrix result[`j',4] = r(mean)
qui sum everprog12_`n' if age==65&everelig12_`n'==1
matrix result[`j',5] = r(mean)
local j = `j'+1
}

matrix rownames result = baseline low_delta high_delta low_pi0 high_pi0
matrix colnames result = participants counterfactual nonparticipant noneligible rate


matrix list result

global fmt f(%9.4g)
outtable using tables/table_robust, mat(result) replace center nobox $fmt 


exit


