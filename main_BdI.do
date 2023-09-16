qui { // Directories //
	global dir0 : pwd 
	global dird "$dir0/Data"
	local j = 1
	foreach d in Graphs Tables Do_files {
		cap mkdir "$dir0/`d'"
		global dir`j' "$dir0/`d'"
		local j = `j' + 1
	}
}
qui { // External Packages //
	foreach p in shp2dta spmap coefplot cleanplots ftools reghdfe estout {
		noi ssc install `p', replace
	}
}
////////////////////////////////////////////////////////////////////////////////
//  Please desynchronize Dropbox if you run this script in a Dropbox folder   // 
////////////////////////////////////////////////////////////////////////////////
qui { // Data loading //
	use "$dird/ASCI_redux.dta", clear

	gen attivo = log(voce9991)
	gen l_coll = log(voce2000)
	gen roe = (voce9993 + voce9994) / voce3000
	gen liquidity = voce0100 / voce9991
	gen equity = voce3000 / voce9991
	gen mortgages = voce0500 / voce9991
	gen beni_immob = voce0700 / voce9991
}
qui { // Generation of Figure 6 //
	shp2dta using "$dird/Reg01012023_g_WGS84.shp", data("$dird/Reg_shape.dta") coor("$dird/Reg.dta") replace genid(regid) gencentroids(stub)
	shp2dta using "$dird/ProvCM01012023_g_WGS84.shp", data("$dird/Prov_shape.dta") coor("$dird/Prov.dta") replace genid(regid) gencentroids(stub)

	preserve
	keep if Categoria == "SOC" & Anno == 1936
	gen tot = 1
	collapse (sum) tot, by(Provincia)
	replace Provincia = proper(Provincia)
	rename Provincia DEN_UTS
	replace DEN_UTS = "Massa Carrara" if DEN_UTS == "Massa"
	replace DEN_UTS = "Reggio di Calabria" if DEN_UTS == "Reggio Di Calabria"
	replace DEN_UTS = "Reggio nell'Emilia" if DEN_UTS == "Reggio Nell'Emilia"
	replace DEN_UTS = "Monza e della Brianza" if DEN_UTS == "Monza E Della Brianza"
	merge 1:1 DEN_UTS using "$dird/Prov_shape.dta"
	tostring tot, gen(tot_l)
	replace tot_l = "{bf:" + tot_l + "}"
	keep if _merge == 3
	drop _merge
	spmap tot using "$dird/Prov.dta", id(regid) fcolor(Blues) os(0.01 0.01 0.01 0.01) mos(0.01) nds(0.01) label(xcoord(x_stub) ycoord(y_stub) label(tot_l) size(*0.65)) ///
	leg(off) polygon(data("$dird/Reg.dta")) title("SOC - 1936")
	gr export "$dir1/SOC_map.pdf", replace
	restore
	preserve
	keep if Categoria == "MDP" & Anno == 1936
	gen tot = 1
	collapse (sum) tot, by(Regione)
	replace Regione = proper(Regione)
	rename Regione DEN_REG
	merge 1:1 DEN_REG using "$dird/Reg_shape.dta"
	drop if _merge == 1
	tostring tot, gen(tot_l)
	replace tot_l = "{bf:" + tot_l + "}"
	replace tot_l = "" if tot == .
	drop _merge
	spmap tot using "$dird/Reg.dta", id(regid) fcolor(Blues) os(0.2 0.2 0.2 0.2) mos(0.2) nds(0.2) clmethod(custom) clbreaks(0 1 3 6) label(xcoord(x_stub) ycoord(y_stub) label(tot_l) size(*0.65)) ///
	leg(off) title("MDP - 1936") polygon(data("$dird/Reg.dta")) 
	gr export "$dir1/MDP_map.pdf", replace
	restore
}
qui { // Generation of Figure 7 //
	global start = 1928
	global end = 1948
	local qt = char(34)
	cap file close boxout 
	file open boxout using "$dir3/boxout.do", write replace
	file write boxout "gr box attivo if Categoria == "
	file write boxout `"`qt'"'
	file write boxout "SOC"
	file write boxout `"`qt'"'
	file write boxout " & Anno >= $start & Anno <= $end, over(Anno, label(labsize(small) angle(45))) title("
	file write boxout `"`qt'"'
	file write boxout "SOC"
	file write boxout `"`qt'"'
	file write boxout ") ylabel(10(5)25, grid)  yline(10 15 20 25, lc(black) lp(dot) lw(0.1)) ytitle("
	file write boxout `"`qt'"'
	file write boxout "Total Assets"
	file write boxout `"`qt'"'
	file write boxout ") leg(off) asyvars showyvars "
	local j = 1
	forv y = $start/1936 {
		file write boxout "box(`j', color(blue)) m(`j', mc(blue) ms(o)) "
		local j = `j' + 1
	}
	forv y = 1937/$end {
		file write boxout "box(`j', color(red)) m(`j', mc(red) ms(o)) "
		local j = `j' + 1
	}
	file close boxout
	do "$dir3/boxout.do"
	gr export "$dir1/asset_box.pdf", replace
	local qt = char(34)
	cap file close boxout 
	file open boxout using "$dir3/boxout.do", write replace
	file write boxout "gr box attivo if Categoria == "
	file write boxout `"`qt'"'
	file write boxout "MDP"
	file write boxout `"`qt'"'
	file write boxout " & Anno >= $start & Anno <= $end, over(Anno, label(labsize(small) angle(45))) title("
	file write boxout `"`qt'"'
	file write boxout "MDP"
	file write boxout `"`qt'"'
	file write boxout ") ylabel(10(5)25, grid) yline(10 15 20 25, lc(black) lp(dot) lw(0.1)) ytitle("
	file write boxout `"`qt'"'
	file write boxout "Total Assets"
	file write boxout `"`qt'"'
	file write boxout ") leg(off) asyvars showyvars "
	local j = 1
	forv y = $start/1936 {
		file write boxout "box(`j', color(blue)) m(`j', mc(blue) ms(o)) "
		local j = `j' + 1
	}
	forv y = 1937/$end {
		file write boxout "box(`j', color(red)) m(`j', mc(red) ms(o)) "
		local j = `j' + 1
	}
	file close boxout
	do "$dir3/boxout.do"
	gr export "$dir1/asset_box_2.pdf", replace
}
qui { // Generation of DiD variables //
	bys Anno Categoria: egen m_tot = sum(voce9991)
	gen m_share = (voce9991/m_tot)^2
	bys Anno Categoria: egen herf = sum(m_share)
	drop m_share m_tot

	set scheme cleanplots
	label var l_coll "Avg. Collateral (Log)"
	label var herf "HHI"
	drop *voce*

	gen treated = Categoria == "SOC"
	gen post = Anno > 1936
	gen war = (Anno >= 1940 & Anno <= 1945)
}
qui { // Generation of Figure 8 //
	// Short Run 1927 - 1947 //
	global start = 1928
	global end = 1948
	global C "MDP"
	global dep l_coll

	// Descriptive Stats //
	preserve
	keep if inlist(Categoria, "SOC", "$C")
	collapse (mean) herf $dep, by(Anno Categoria)
	scatter herf Anno if Categoria == "SOC" & Anno >= $start & Anno <= $end, mlc(black) mfc(red) ms(o) msize(2) ///
	|| lfit herf Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936, lc(red) lw(0.05) lp(-) ///
	|| lfit herf Anno if Categoria == "SOC" & Anno > 1936 & Anno <= $end, lc(red) lw(0.05) lp(-) ///
	|| scatter herf Anno if Categoria == "$C" & Anno >= $start & Anno <= $end, mlc(black) mfc(gs6) ms(o) msize(2) ///
	|| lfit herf Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936, lc(gs6) lw(0.05) lp(-) ///
	|| lfit herf Anno if Categoria == "$C" & Anno > 1936 & Anno <= $end, lc(gs6) lw(0.05) lp(-) ///
	||, xline(1936, lc(black)) legend(order(1 "Commercial Banks" 4 "Pledge Banks") pos(6) rows(1)) xlabel($start(1)$end, labsize(vsmall) angle(45) nogrid) ///
	xtitle(" ") ytitle("HHI") saving("$dir1/hhi.gph", replace)
	// Slopes //
	reg herf Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936
	local slope = e(b)[1,1]
	reg herf Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936
	noi test Anno == `slope'
	reg herf Anno if Categoria == "$C" & Anno > 1936 & Anno <= $end
	local slope = e(b)[1,1]
	reg herf Anno if Categoria == "SOC" & Anno > 1936 & Anno <= $end
	noi test Anno == `slope'
	restore

	preserve
	keep if inlist(Categoria, "SOC", "$C")
	collapse (mean) herf $dep, by(Anno Categoria)
	scatter $dep Anno if Categoria == "SOC" & Anno >= $start & Anno <= $end, mlc(black) mfc(red) ms(o) msize(2) ///
	|| lfit $dep Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936, lc(red) lw(0.05) lp(-) ///
	|| lfit $dep Anno if Categoria == "SOC" & Anno > 1936 & Anno <= $end, lc(red) lw(0.05) lp(-) ///
	|| scatter $dep Anno if Categoria == "$C" & Anno >= $start & Anno <= $end, mlc(black) mfc(gs6) ms(o) msize(2) ///
	|| lfit $dep Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936, lc(gs6) lw(0.05) lp(-) ///
	|| lfit $dep Anno if Categoria == "$C" & Anno > 1936 & Anno <= $end, lc(gs6) lw(0.05) lp(-) ///
	||, xline(1936, lc(black)) legend(order(1 "Commercial Banks" 4 "Pledge Banks") pos(6) rows(1)) xlabel($start(1)$end, labsize(vsmall) angle(45) nogrid) ///
	xtitle(" ") ytitle("Collateral") saving("$dir1/collateral.gph", replace)
	// Slopes //
	reg $dep Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936
	local slope = e(b)[1,1]
	reg $dep Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936
	noi test Anno == `slope'
	restore

	gr combine "$dir1/hhi.gph" "$dir1/collateral.gph", xsize(100) ysize(50)
	gr export "$dir1/motivation.pdf", replace
}
qui { // Generation of Table 1 //
	label var attivo "Assets"
	label var l_coll "Collateral"
	label var herf "HHI"
	label var equity "Equity ratio"
	label var roe "ROE"
	label var liquidity "Liquidity ratio"
	label var beni_immob "Real estate ratio"
	label var mortgages "Mortgages ratio"
	label var war "WWII"

	label define g 0 "MDP" 1 "SOC"
	label values treated g
	label define t 0 "Before 1936" 1 "After 1936"
	label values post t

	global ratios equity liquidity mortgages beni_immob roe

	drop if l_coll == .
	local nl = char(10)
	cap file close tabout
	file open tabout using "$dir2/ttest_tab.tex", write replace
	file write tabout "\begin{tabular}{lcccc} `nl' \hline \hline `nl'"
	file write tabout " & SOC & MDP & $\Delta_{1928 - 1948}$ & $\Delta_{1937 - 1948}$ \\ `nl' \hline `nl'"
	foreach v of varlist attivo $ratios herf l_coll {
		local k : var label `v'
		ttest `v' if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, by(treated) reverse
		local stars = 0
		foreach l in 0.95 0.975 0.995 {
			if r(t) > invt(r(level), `l') | r(t) < - invt(r(level), `l') {
				local stars = `stars' + 1
			}
		}
		local s1 = `stars' * "*"
		file write tabout "`k' & `: di %9.2fc r(mu_1)' & `: di %9.2fc r(mu_2)' & `: di %9.2fc r(mu_1) - r(mu_2)'$^{`s1'}$"
		ttest `v' if inlist(Categoria, "SOC", "$C") & Anno > 1936 & Anno <= $end, by(treated) reverse
		local stars = 0
		foreach l in 0.95 0.975 0.995 {
			if r(t) > invt(r(level), `l') | r(t) < - invt(r(level), `l') {
				local stars = `stars' + 1
			}
		}
		local s2 = `stars' * "*"
		file write tabout " &  `: di %9.2fc r(mu_1) - r(mu_2)'$^{`s2'}$ \\ `nl'"
	}
	count if Categoria == "SOC" & Anno >= $start & Anno <= $end
	file write tabout " \hline `nl' N & `: di %9.0fc r(N)' & "
	count if Categoria == "MDP" & Anno >= $start & Anno <= $end
	file write tabout " `: di %9.0fc r(N)' & & \\ `nl' "
	file write tabout "\hline \hline `nl'"
	file write tabout "\end{tabular}"
	file close tabout
}
qui { // Generation of Table 2
	cap est drop model_*
	reghdfe herf treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, noabs vce(cluster Provincia)
	estadd local prov "No"
	est sto model_1
	reghdfe herf treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_2
	reghdfe herf treated##post war $ratios if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_3
	reghdfe $dep treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, noabs vce(cluster Provincia)
	estadd local prov "No"
	est sto model_4
	reghdfe $dep treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_5
	reghdfe $dep treated##post war $ratios if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_6
	esttab model_* using "$dir2/sr_reg.tex", replace booktabs label keep(1.treated 1.post 1.treated#1.post war $ratios) se star(* 0.10 ** 0.05 *** 0.01) s(N prov, label("Observations" "Province FE")) nonote
	est drop model_*

	reghdfe $dep treated##b1936.Anno war $ratios if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= 1936, abs(Provincia) vce(cluster Provincia)
	qui {
		cap file close ftest
		file open ftest using "$dir3/ftest.do", write replace
		file write ftest "test "
		forv t = $start/1935 {
			file write ftest "(1.treated#`t'.Anno == 0)"
		}
		file close ftest
	}
	noi do "$dir3/ftest.do"
}
qui { // Generation of Figure 9 //
	reghdfe $dep treated##b1936.Anno  war $ratios if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	est sto model_event
	local qt = char(34)
	cap file close coefout
	file open coefout using "$dir3/coefout.do", write replace
	file write coefout "coefplot model_event, keep(1.treated#*) vertical ci(95) color(blue) recast(scatter) ms(o) omitted baselevels ciopts(recast(rspike) lc(blue*.5)) rename("
	forv y = $start(1)$end {
		file write coefout "1.treated#`y'.Anno = "
		file write coefout `"`qt'"'
		file write coefout "`y'"
		file write coefout `"`qt'"'
		file write coefout " "
	}
	file write coefout ") xlabel(, nogrid labsize(small) angle(45)) yline(0, lc(black) lp(-)) xline(9, lc(red) lp(-))"
	file close coefout
	do "$dir3/coefout.do"
	gr export "$dir1/sr_coef.pdf", replace
	est drop model_*
}
qui { // Generation of Figure 10 //
	// Long Run //
	global start = 1928
	global end = 1973

	// Descriptive Stats //
	scatter herf Anno if Categoria == "SOC" & Anno >= $start & Anno <= $end, mlc(black) mfc(red) ms(o) msize(2) ///
	|| lfit herf Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936, lc(red) lw(0.05) lp(-) ///
	|| lfit herf Anno if Categoria == "SOC" & Anno > 1936 & Anno <= $end, lc(red) lw(0.05) lp(-) ///
	|| scatter herf Anno if Categoria == "$C" & Anno >= $start & Anno <= $end, mlc(black) mfc(gs6) ms(o) msize(2) ///
	|| lfit herf Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936, lc(gs6) lw(0.05) lp(-) ///
	|| lfit herf Anno if Categoria == "$C" & Anno > 1936 & Anno <= $end, lc(gs6) lw(0.05) lp(-) ///
	||, xline(1936, lc(black)) legend(order(1 "Commercial Banks" 4 "Pledge Banks") pos(6) rows(1)) xlabel($start(1)$end, labsize(tiny) angle(45) nogrid) ///
	xtitle(" ") ytitle("HHI") saving("$dir1/hhi.gph", replace)

	preserve
	keep if inlist(Categoria, "SOC", "$C")
	collapse (mean) herf $dep, by(Anno Categoria)
	scatter $dep Anno if Categoria == "SOC" & Anno >= $start & Anno <= $end, mlc(black) mfc(red) ms(o) msize(2) ///
	|| lfit $dep Anno if Categoria == "SOC" & Anno >= $start & Anno <= 1936, lc(red) lw(0.05) lp(-) ///
	|| lfit $dep Anno if Categoria == "SOC" & Anno > 1936 & Anno <= $end, lc(red) lw(0.05) lp(-) ///
	|| scatter $dep Anno if Categoria == "$C" & Anno >= $start & Anno <= $end, mlc(black) mfc(gs6) ms(o) msize(2) ///
	|| lfit $dep Anno if Categoria == "$C" & Anno >= $start & Anno <= 1936, lc(gs6) lw(0.05) lp(-) ///
	|| lfit $dep Anno if Categoria == "$C" & Anno > 1936 & Anno <= $end, lc(gs6) lw(0.05) lp(-) ///
	||, xline(1936, lc(black)) legend(order(1 "Commercial Banks" 4 "Pledge Banks") pos(6) rows(1)) xlabel($start(1)$end, labsize(tiny) angle(45) nogrid) ///
	xtitle(" ") ytitle("Collateral") saving("$dir1/collateral.gph", replace)
	restore

	gr combine "$dir1/hhi.gph" "$dir1/collateral.gph", xsize(100) ysize(50)
	gr export "$dir1/motivation_lr.pdf", replace
}
qui { // Generation of Table 3 //
	cap est drop model_*
	reghdfe herf treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, noabs vce(cluster Provincia)
	estadd local prov "No"
	est sto model_1
	reghdfe herf treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_2
	reghdfe herf treated##post war $ratios  if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_3
	reghdfe $dep treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, noabs vce(cluster Provincia)
	estadd local prov "No"
	est sto model_4
	reghdfe $dep treated##post if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_5
	reghdfe $dep treated##post war $ratios  if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local prov "Yes"
	est sto model_6
	esttab model_* using "$dir2/lr_reg.tex", replace booktabs label keep(1.treated 1.post 1.treated#1.post  war $ratios) se star(* 0.10 ** 0.05 *** 0.01) s(N prov, label("Observations" "Province FE")) nonote sfmt(9.2fc)
	est clear
}
qui { // Generation of Figure 11 //
	cap est drop model_*
	reghdfe $dep treated##b1936.Anno  war $ratios if inlist(Categoria, "SOC", "$C") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	est sto model_event
	local qt = char(34)
	cap file close coefout
	file open coefout using "$dir3/coefout.do", write replace
	file write coefout "coefplot model_event, keep(1.treated#*) vertical ci(95) color(blue) recast(scatter) ms(o) omitted baselevels ciopts(recast(rspike) lc(blue*.5)) rename("
	forv y = $start(1)$end {
		file write coefout "1.treated#`y'.Anno = "
		file write coefout `"`qt'"'
		file write coefout "`y'"
		file write coefout `"`qt'"'
		file write coefout " "
	}
	file write coefout ") xlabel(, nogrid labsize(vsmall) angle(45)) yline(0, lc(black) lp(-)) xline(9, lc(red) lp(-))"
	file close coefout
	do "$dir3/coefout.do"
	gr export "$dir1/lr_coef.pdf", replace
}
qui { // Generation of Table 4 //
	// Granger //
	global start = 1928
	global end = 1948
	tsset Id_Banca Anno
	cap drop *_lag*
	foreach v in l_coll herf {
		forv j = 1/5 {
			gen `v'_lag`j' = l`j'.`v'
		}
	}

	cap est drop model_*
	reghdfe l_coll l_coll_lag1 herf herf_lag1 war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local depl1 "Yes"
	estadd local indl1 "Yes"
	estadd local depl5 "No"
	estadd local indl5 "No"
	estadd local war "Yes"
	estadd local ratios "Yes"
	estadd local prov "Yes"
	est sto model_1
	reghdfe herf l_coll_lag1 l_coll herf_lag1 war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local depl1 "Yes"
	estadd local indl1 "Yes"
	estadd local depl5 "No"
	estadd local indl5 "No"
	estadd local war "Yes"
	estadd local ratios "Yes"
	estadd local prov "Yes"
	est sto model_2
	reghdfe l_coll l_coll_lag* herf herf_lag* war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local depl1 "Yes"
	estadd local indl1 "Yes"
	estadd local depl5 "Yes"
	estadd local indl5 "Yes"
	estadd local war "Yes"
	estadd local ratios "Yes"
	estadd local prov "Yes"
	est sto model_3
	reghdfe herf l_coll_lag* l_coll herf_lag* war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	estadd local depl1 "Yes"
	estadd local indl1 "Yes"
	estadd local depl5 "Yes"
	estadd local indl5 "Yes"
	estadd local war "Yes"
	estadd local ratios "Yes"
	estadd local prov "Yes"
	est sto model_4
	esttab model_* using "$dir2/granger.tex", replace booktabs label keep(herf l_coll) se star(* 0.10 ** 0.05 *** 0.01) s(N depl1 indl1 depl5 indl5 war ratios prov, label("Observations" "Dep. var. t -1" "Indep. var. t - 1"  "Dep. var. t - 2 to t - 5"  "Indep. var. t - 2 to t - 5" "WWII" "Financial ratios" "Province FE")) nonote

	reghdfe l_coll l_coll_lag* herf herf_lag* war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	noi test (herf == 0)(herf_lag1 == 0)(herf_lag2 == 0)(herf_lag3 == 0)(herf_lag4 == 0) (herf_lag5 == 0)
	reghdfe herf l_coll_lag* l_coll herf_lag* war $ratios if inlist(Categoria, "SOC") & Anno >= $start & Anno <= $end, abs(Provincia) vce(cluster Provincia)
	noi test (l_coll == 0)(l_coll_lag1 == 0)(l_coll_lag2 == 0)(l_coll_lag3 == 0)(l_coll_lag4 == 0) (l_coll_lag5 == 0)
}