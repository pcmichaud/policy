* to produce table of participation rates

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(11,4,.)
forvalues j = 1/11 {
	qui tabstat prog`j' if age==age[`j',1]&elig`j'==1, by(educ) save
	forvalues k = 1/3 {
		matrix result[`j',`k'] = r(Stat`k')
	}
	matrix result[`j',4] = r(StatTotal)
	

}

matrix pars = (age,pi0,cost) 
matrix tabprog = (pars,result)
matrix colnames tabprog = age pi0 cost lessHS HS college total
matrix list tabprog

global fmt f(%9.4g)
outtable using tables/table1, mat(tabprog) replace norow center nobox $fmt 

exit

