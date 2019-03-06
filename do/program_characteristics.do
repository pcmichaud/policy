* to produce stats on characteristics by program

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

* statistics on participation
use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(11,6,.)
forvalues j = 1/11 {
	qui tabstat income_prog`j' fin_prog`j' wealth_prog`j' if age==age[`j',1]&elig`j'==1, by(prog`j') save
	matrix result[`j',1] = r(Stat1)
	matrix result[`j',4] = r(Stat2)
	
}
matrix pars = (age,pi0,cost) 
matrix tabprog = (pars,result)
matrix colnames tabprog = age pi0 cost income_no fin_no wealth_no income_yes fin_yes wealth_yes
matrix list tabprog

global fmt f(%9.4g)
outtable using tables/table2, mat(tabprog) replace norow center nobox $fmt 
