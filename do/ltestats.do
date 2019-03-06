* long-term effects

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(11,4,.)
gen logw_base = log(wealth_base)
egen avgcons_base = mean(cons_base), by(id)
forvalues j = 1/11 {
	egen everelig`j' = total(elig`j'), by(id)
	egen everprog`j' = total(prog`j'), by(id)
	qui gen logw`j' = log(wealth_prog`j')
	qui egen avgcons_prog`j' = mean(cons_prog`j'), by(id)
	qui sum wealth_prog`j' if age==65&everelig`j'==1&everprog`j'==1
	matrix result[`j',1] = r(mean)
	qui sum wealth_base if age==65&everelig`j'==1&everprog`j'==1
	matrix result[`j',2] = r(mean)
	qui sum wealth_prog`j' if age==65&everelig`j'==1&everprog`j'==0
	matrix result[`j',3] = r(mean)
	qui sum wealth_prog`j' if age==65&everelig`j'==0
	matrix result[`j',4] = r(mean)	
}
matrix pars = (age,pi0,cost) 
matrix tabprog = (pars,result)
matrix colnames tabprog = age pi0 cost participants counterfactual nonparticipant noneligible
matrix list tabprog

global fmt f(%9.4g)
outtable using tables/table3, mat(tabprog) norow replace center nobox $fmt 


forvalues j = 1/11 {
	di "`j'"
	sum everelig`j' if age==65
	sum everprog`j' if age==65&everelig`j'==1	
}
