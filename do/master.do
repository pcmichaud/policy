* master code for EER paper
clear all
capture log close
set more off
set scheme s2mono, permanently 

capture cd /home/bepp/michaudp/policy
capture cd ~/cedia/Projets/evaluation/policy

net install grc1leg,from( http://www.stata.com/users/vwiggins/)
ssc install outtable

* to take fortran output for each scn and create dataset
do do/prepare-data.do

* produce table 1 (participation)
do do/program_participation.do

* produce table 2 (characteristics)
do do/program_characteristics.do

* produce figures 1 to 4 (counterfactuals)
do do/figures.do

* produde table 3 (stats compliers)
do do/ltestats.do

* produce table 4 (itt)
do do/itt.do

* produce table 5 (ols)
do do/ols.do

* produce table 6 (late-atet)
do do/late-atet.do

* produce table 7 (diff in diff)
do do/diffindiff.do

* produce robustness table (new in revision)
do do/robust.do

* produce table on heterogeneity by education and income
do do/hetero.do