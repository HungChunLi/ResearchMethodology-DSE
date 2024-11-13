
clear
set more off 

local user = "`c(username)'"

if "`user'" == "brianhjli" {
    cd "/Users/brianhjli/Dropbox/113-1/Research Methodology/DSE-5"
}
else if "`user'" == "QQ" {
    cd "C:/Users/QQ/Dropbox/DSE-5"
}
else if "`user'" == "hayashijikyou" {
    cd "/Users/hayashijikyou/Library/CloudStorage/Dropbox/DSE-5"
}
else {
    display as error "使用者名稱為空，請檢查檔案路徑！"
}

use "參考資料/dse_final/group7_data.dta"

// cd "/Users/elisa/Library/Mobile Documents/com~apple~CloudDocs/NTU/研究方法/AIDS report/"
//log using "tw_avghousehold.log", replace
// use "tw_avghousehold.dta", replace


**# The analysis result #1
aidsills bana_w belt_w guva_w pine_w, prices(bana_p belt_p guva_p pine_p) expenditure(exp) ivexpenditure(ln_house_income) intercept(h_people_1 h_people_2 h_people_3 h_people_4 h_people_5 h_child_ratio h_old_ratio urbanisation) symmetry

** generate good-looking table
eststo aids /* esttab aids using my.rtf, replace */

/* output table */
putdocx begin
putdocx table tablename = etable
putdocx save "result.docx", replace

** elasticity
aidsills_elas


**# Endogenous #2
aidsills bana_w belt_w guva_w pine_w, prices(bana_p belt_p guva_p pine_p) expenditure(exp) ivexpenditure(ln_house_income) intercept(h_people_1 h_people_2 h_people_3 h_people_4 h_people_5 h_child_ratio h_old_ratio urbanisation) symmetry
 
test rho_vexp
// reject hypothesis of endogeneity (內生) at the significant level 5% and 10%, which means expenditure is endogeneous. But is exogenous when the significant level is 1%.
/* expditure used here is the total value of the 4 fruits of each county in each year. */


**# Homogeneity Test #3
aidsills bana_w belt_w guva_w pine_w, prices(bana_p belt_p guva_p pine_p) expenditure(exp) ivexpenditure(ln_house_income) intercept(h_people_1 h_people_2 h_people_3 h_people_4 h_people_5 h_child_ratio h_old_ratio urbanisation) iteration(0)
// the result doesn't shows the rejection of the hypothesis of homogeneity at the significant level 5%, meaning it is homogenous

**# Bookmark #1
aidsills bana_w belt_w guva_w pine_w, prices(bana_p belt_p guva_p pine_p) expenditure(exp) ivexpenditure(ln_house_income) intercept(h_people_1 h_people_2 h_people_3 h_people_4 h_people_5 h_child_ratio h_old_ratio urbanisation) homogeneity
// the result DOES NOT show rejection of the hypothesis of symmetry, meaning the estimation of pi on wj is equal to the estimation of pj on wi.

// another test of symmetry
quietly test [bana_w]gamma_lnbelt_p =[belt_w]gamma_lnbana_p, notest
quietly test [bana_w]gamma_lnguva_p=[guva_w]gamma_lnbelt_p , notest accumulate
test [belt_w]gamma_lnguva_p=[guva_w]gamma_lnbelt_p, accumulate
// Do NOT Reject, Probably Symmetry
// 我們的誤差小到可以被認為是symmetry


//log close


