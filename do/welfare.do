* welfare stats (not reported in paper)

matrix age = (30,35,40,45,50,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25)'
matrix cost = (500,500,500,500,500,500,500,250,750,250)'

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(10,4,.)

forvalues j = 1/10 {
	egen everelig`j' = total(elig`j'), by(id)
	egen everprog`j' = total(prog`j'), by(id)
	qui sum value_prog`j' if age==age[`j',1]&everelig`j'==1&everprog`j'==1
	matrix result[`j',1] = r(mean)
	qui sum value_base if age==age[`j',1]&everelig`j'==1&everprog`j'==1
	matrix result[`j',2] = r(mean)
	qui sum value_prog`j' if age==age[`j',1]&everelig`j'==1&everprog`j'==0
	matrix result[`j',3] = r(mean)
	qui sum value_prog`j' if age==age[`j',1]&everelig`j'==0
	matrix result[`j',4] = r(mean)	
}
matrix pars = (age,pi0,cost) 
matrix tabprog = (pars,result)
matrix colnames tabprog = age pi0 cost participants counterfactual nonparticipant noneligible
matrix list tabprog

global fmt f(%9.4g)
outtable using tables/table8, mat(tabprog) norow replace center nobox $fmt 

