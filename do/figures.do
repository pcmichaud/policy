* do a batch of graphs on different programs

matrix age = (30,35,40,45,50,40,40,40,40,40,40)'
matrix pi0 = (0.5,0.5,0.5,0.5,0.5,0.25,0.75,0.5,0.5,0.1,0.1)'
matrix cost = (500,500,500,500,500,500,500,250,750,250,100)'

forvalues j = 1/11 {
graph drop _all
use data/simulations/simknow_policy.dta, clear
tsset id age
global age = age[`j',1]
global prog = "prog`j'"
* construct ever on program
egen ever$prog = total($prog), by(id)
* construct a savings variable

* investment
preserve
	collapse inv_$prog inv_base, by(ever$prog age)
	drop if age>64
	#d ;
		twoway (line inv_$prog age if ever$prog==0) (line inv_$prog age if ever$prog==1)
			(line inv_base age if ever$prog==1) 
		
		, xline($age, lpattern(dash)) nodraw
		legend(label(1 "not enrolled") label(2 "enrolled") label(3 "enrolled - counterfactual") size(small))
		xtitle("age") xlabel(25(5)65, labsize(small)) ylabel(,labsize(small))
		ytitle("investment in knowledge", size(small)) name(inv);
	#d cr

restore
* stock of knowledge
preserve
	collapse fin_$prog fin_base , by(ever$prog age)
	drop if age>64
	#d ;
		twoway (line fin_$prog age if ever$prog==0) (line fin_$prog age if ever$prog==1)
			 (line fin_base age if ever$prog==1) 
		
		, xline($age, lpattern(dash))  nodraw
		legend(label(1 "not enrolled") label(2 "enrolled") label(3 "enrolled - counterfactual") rows(1) size(small))
		xtitle("age") xlabel(25(5)65, labsize(small)) ylabel(,labsize(small))
		ytitle("stock of financial knowledge", size(small)) name(fin);
	#d cr	
restore
* wealth change
preserve
	collapse wealth_$prog wealth_base , by(ever$prog age)
	drop if age>64
	tsset ever$prog age
	gen saverate_$prog = log(f.wealth_$prog/wealth_$prog)
	gen saverate_base = log(f.wealth_base/wealth_base)
	
	#d ;
		twoway (line saverate_$prog age if ever$prog==0) (line saverate_$prog age if ever$prog==1)
			 (line saverate_base age if ever$prog==1) 
		
		, xline($age, lpattern(dash))  nodraw
		legend(label(1 "not enrolled") label(2 "enrolled") label(3 "enrolled - counterfactual") size(small))
		xtitle("age") xlabel(25(5)65, labsize(small)) ylabel(,labsize(small))
		ytitle("chang in wealth (percent)", size(small)) name(wealth);
	#d cr	
restore
* share in sophisticated technology
preserve
	collapse share_$prog share_base , by(ever$prog age)
	drop if age>64
	#d ;
		twoway (line share_$prog age if ever$prog==0) (line share_$prog age if ever$prog==1)
			 (line share_base age if ever$prog==1) 
		
		, xline($age, lpattern(dash))  nodraw
		legend(label(1 "not enrolled") label(2 "enrolled") label(3 "enrolled - counterfactual") size(small))
		xtitle("age") xlabel(25(5)65, labsize(small))
		ylabel(,labsize(small))
		ytitle("share sophisticated", size(small)) name(share)
		;
	#d cr	
restore
* do figure
global cost = cost[`j',1]
global pi0 = pi0[`j',1]
grc1leg inv fin wealth share, rows(2) cols(2) legendfrom(fin) note("intervention at age $age, program cost of $cost, relative marginal cost is $pi0")

graph export "figures/effect_$prog.eps", as(eps) replace

}





