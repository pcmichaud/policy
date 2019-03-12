*  heterogeneity in effects

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(4,3,.)

local s = 12

egen everelig`s' = total(elig`s'), by(id)
egen everprog`s' = total(prog`s'), by(id)

egen minc = mean(income_base), by(id)
xtile qinc = minc, nq(4)
local stat "p50"
local var "wealth"

forvalues j = 1/3 {
    qui sum `var'_prog`s' if age==65&everelig`s'==1&everprog`s'==1&educ==`j', d
    matrix result[`j',1] = r(`stat')
    qui sum `var'_base   if age==65&everelig`s'==1&everprog`s'==1&educ==`j', d
    matrix result[`j',2] = r(`stat')
    matrix result[`j',3] = (result[`j',1] - result[`j',2])/result[`j',2]
}
qui sum `var'_prog`s' if age==65&everelig`s'==1&everprog`s'==1, d
matrix result[4,1] = r(`stat')
qui sum `var'_base   if age==65&everelig`s'==1&everprog`s'==1, d
matrix result[4,2] = r(`stat')
matrix result[4,3] = (result[4,1] - result[4,2])/result[4,2]

matrix rownames result = lessHS HS college total
matrix colnames result = participants counterfactual effect

matrix list result


global fmt f(%9.4g)
outtable using tables/table_hetero_educ, mat(result) replace center nobox $fmt 


matrix result = J(5,3,.)

forvalues j = 1/4 {
    qui sum `var'_prog`s' if age==65&everelig`s'==1&everprog`s'==1&qinc==`j', d
    matrix result[`j',1] = r(`stat')
    qui sum `var'_base   if age==65&everelig`s'==1&everprog`s'==1&qinc==`j', d
    matrix result[`j',2] = r(`stat')
    matrix result[`j',3] = (result[`j',1] - result[`j',2])/result[`j',2]
}
qui sum `var'_prog`s' if age==65&everelig`s'==1&everprog`s'==1, d
matrix result[5,1] = r(`stat')
qui sum `var'_base   if age==65&everelig`s'==1&everprog`s'==1, d
matrix result[5,2] = r(`stat')
matrix result[5,3] = (result[5,1] - result[5,2])/result[5,2]

matrix rownames result = q1 q2 q3 q4 total
matrix colnames result = participants counterfactual effect

matrix list result


global fmt f(%9.4g)
outtable using tables/table_hetero_income, mat(result) replace center nobox $fmt 


exit


