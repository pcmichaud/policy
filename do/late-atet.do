* late and atet estimates

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

* late atet
use data/simulations/simknow_policy.dta, clear
tsset id age
global ageeval = 65
egen perminc = mean(income_base), by(id)
sort id age
by id: gen initwealth = wealth_base[1]

matrix result = J(11,2,.)
forvalues j = 1/11 {
	qui egen everelig`j' = total(elig`j'), by(id)
	qui egen everprog`j' = total(prog`j'), by(id)
	gen logw`j' = log(1+wealth_prog`j')
	qui ivregress 2sls logw`j' (everprog`j'=everelig`j') ib1.educ perminc if age==$ageeval
	matrix b = e(b)'
	matrix V = e(V)
	matrix result[`j',1] = b[1,1]
	matrix result[`j',2] = sqrt(V[1,1])
}
matrix tab = (age,pi0,cost,result)
matrix colnames tab = age pi0 cost late se
matrix list tab

global fmt f(%9.4g)
outtable using tables/table6, mat(tab) replace center norow nobox $fmt 

