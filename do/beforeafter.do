* before and after comparisons

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(11,6,.)
forvalues j = 1/11 {
	qui egen everelig`j' = total(elig`j'), by(id)
	qui egen everprog`j' = total(prog`j'), by(id)
	
	gen logw`j' = log(1+f.wealth_prog`j')
	gen fin`j' = f.fin_prog`j'
	
	qui xtreg inv_prog`j' prog`j' income_prog`j' ib26.age,fe	
	matrix b = e(b)
	matrix result[`j',1] = b[1,1]
	matrix V = e(V)
	matrix result[`j',2] = sqrt(V[1,1])
	qui xtreg fin`j' prog`j' income_prog`j' ib26.age, fe  
	matrix b = e(b)
	matrix result[`j',3] = b[1,1]
	matrix V = e(V)
	matrix result[`j',4] = sqrt(V[1,1])
	qui xtreg logw`j' prog`j' income_prog`j' ib26.age, fe  
	matrix b = e(b)
	matrix result[`j',5] = b[1,1]
	matrix V = e(V)
	matrix result[`j',6] = sqrt(V[1,1])


}


matrix tabdd = (age,pi0,cost,result)
matrix colnames tabdd = age pi0 cost invest norow se1 fin se2 logwealth se3
matrix list tabdd

global fmt f(%9.4g)
outtable using tables/table8, mat(tabdd) replace center norow nobox $fmt 

