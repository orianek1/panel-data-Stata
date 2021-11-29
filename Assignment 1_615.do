*question 2
*To open the data file
use "C:\Users\Oriane\Downloads\wagepan.dta"
*This is  the panel data on 545 men worked year from 1980-1987
*i)fixed effect estimator
xtset nr year
xtreg lwage exper year hours married i.year, fe cluster (nr)

*ii) fixed Effect regression

egen mean_lwage = mean(lwage)
egen mean_exper = mean(exper)
gen exper_demeaned = exper - mean_exper
gen lwage_demeaned = lwage - mean_lwage
xtreg lwage_demeaned exper_demeaned i.year, fe cluster(nr)

*iii)
*first difference*
xtset nr year
keep lwage exper nr year hours married
reshape wide lwage exper hours married, i( nr ) j(year)
gen diff_lwage= lwage1987 - lwage1986
gen diff_exper= exper1987 - exper1986
reg diff_lwage diff_exper, nocons
reg diff_lwage diff_exper, nocons cluster(nr)


*question 3
*To open the data file
use "C:\Users\Oriane\Downloads\jtrain1.dta"
*This is data on the scrap rate of 157 Michigan firms

*3A construction of difference in difference estimator
drop if year>=1989

*DID regress model*
bys fcode: egen treated = max(grant) 
gen group = .
replace group = 1 if d88 == 0 & treated == 1
replace group = 2 if d88 == 1 & treated == 1
replace group = 3 if d88 == 0 & treated == 0
replace group = 4 if d88 == 1 & treated == 0
bys group: egen avghrs = mean(hrsemp)  
*i) construction of the 4 means (control, treatment*before, after)
bys group: sum avghrs
*ii) runing the regression with dummy variable for treatmet
reg hrsemp grant d88 treated 
*iii) running fixed effect regression 
xtreg hrsemp grant d88, fe

*3B Including a firm specific time trend in the model in two different way
use "C:\Users\Oriane\Downloads\jtrain1.dta"
gen yr = year - 1986
*i) 
/*
xi: reg hrsemp grant i.fcode*yr
*/
//instead of using xi commands: 
egen fid = group(fcode) 
sum fid
local temp = r(max) 
forval n=1/`temp'{
gen fid`n'Xyr = yr*(fid == `n')
}
sum fid*X*
reg hrsemp grant yr i.fid fid*Xyr 

*ii) runing regresion and a time trend 
drop fid 
keep if hrsemp!=. & grant !=.
egen fid = group(fcode) 

gen res1 = .
sum fid 
local temp = r(max) 
forval n=1/`temp'{
capture qui reg grant yr if fid == `n'
capture qui predict double res1_`n', residuals
capture qui replace res1 = res1_`n' if fid == `n'
}

gen res2 = .
sum fid 
local temp = r(max) 
forval n=1/`temp'{
capture qui reg hrsemp yr if fid == `n'
capture qui predict double res2_`n', residuals
capture qui replace res2 = res2_`n' if fid == `n' & hrsemp !=. 
}
reg hrsemp res1 
reg res2 res1



*question 4


use "C:\Users\Oriane\Downloads\regm.dta" 

*a) standard  errors  3  ways,  robust,  cluster  by  state  year, cluster by state
regress coll merit male black asian, robust
egen state_year = group(state year)
regress coll merit male black asian,cluster (state_year)
regress coll merit male black asian, vce(cluster state)

*c) taking the mean of all variable by state × year and run the differences-in-differences regression
sort state year

by state year: egen merit1 = mean(merit)
by state year: egen coll1 = mean(coll)
by state year: egen male1 = mean(male)
by state year: egen black1 = mean(black)
by state year: egen asian1 = mean(asian)
regress coll1 merit1 male1 black1 asian1, robust
regress coll1 merit1 male1 black1 asian1, cluster(state)