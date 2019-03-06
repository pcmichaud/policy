* difference in difference estimates

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.25,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

use data/simulations/simknow_policy.dta, clear
tsset id age
matrix result = J(11,3,.)
label def gr 1 "not eligible" 2 "non-participant" 3 "participant"
label def cl -1 "before" 1 "after 5 years"
gen logw = log(1+wealth_base)
forvalues j = 1/11 {
	qui egen everelig`j' = total(elig`j'), by(id)
	qui egen everprog`j' = total(prog`j'), by(id)
	
	gen logw`j' = log(1+wealth_prog`j')

	gen clock = -1 if age==(age[`j',1]-1)
	replace clock = 1 if (age==age[`j',1]+5)
	label val clock cl
	label var clock "program"
	gen group = 1 if everelig`j'==0
	replace group = 2 if everelig`j'==1&everprog`j'==0
	replace group = 3 if everelig`j'==1&everprog`j'==1
	label val group gr

	qui sum logw`j' if group==1 & clock==-1
	local y10 = r(mean)
	qui sum logw`j' if group==1 & clock==1
	local y11 = r(mean)	
	
	qui sum logw`j' if group==2 & clock==-1
	local y20 = r(mean)
	qui sum logw`j' if group==2 & clock==1
	local y21 = r(mean)

	qui sum logw`j' if group==3 & clock==-1
	local y30 = r(mean)
	qui sum logw`j' if group==3 & clock==1
	local y31 = r(mean)

	qui sum logw if group==3 & clock==-1
	local y40 = r(mean)
	qui sum logw if group==3 & clock==1
	local y41 = r(mean)

	matrix result[`j',1] = (`y31' - `y30') - (`y41'-`y40')
	matrix result[`j',2] = (`y31' - `y30') - (`y21'-`y20')
	matrix result[`j',3] = (`y31' - `y30') - (`y11'-`y10')
	
	drop clock group
}



matrix tabdd = (age,pi0,cost,result)
matrix colnames tabdd = age pi0 cost counterfactual nonparticipant noneligible 
matrix list tabdd

global fmt f(%9.4g)
outtable using tables/table7, mat(tabdd) replace center norow nobox $fmt 

