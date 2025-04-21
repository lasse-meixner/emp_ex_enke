use "Data_programs/Data/Survey.dta", clear


set matsize 4000

global controls "female i.inc_bracket i.educ i.yob i.ethnicity ln_pd employed i.occupation i.religious_denomination abs_values_relative altruism trust_general"
global controls_benchmark "female i.yob i.ethnicity employed i.occupation i.religious_denomination abs_values_relative altruism trust_general"
global controls_drop "*inc_bracket* *educ* *ethnicity* ln_pd employed *occupation* *religious_denomination* trust_general altruism job_prospects"
global controls_benchmark_drop "i i.ethnicity employed i.occupation abs_values_relative"
global benchmark "liberalism std_ln_inc std_educ std_ln_pd religiosity"
global benchmark_lifetime "liberalism std_ln_life_inc std_educ std_ln_pd religiosity"



* Table 4

fvset base 1 yob
fvset base 1 county

eststo clear
eststo: qui reg gop_2016 values_universal values_communal i.state, ro
eststo: qui reg gop_2016 values_relative i.state, ro
eststo: qui reg gop_2016 values_relative i.state $controls, ro
eststo: qui reg trump_election_ave values_universal values_communal i.state, ro
eststo: qui reg trump_election_ave values_relative i.state, ro
eststo: qui reg trump_election_ave values_relative i.state $controls, ro
eststo: qui reg trump_primaries values_universal values_communal i.state, ro
eststo: qui reg trump_primaries values_relative i.state, ro
eststo: qui reg trump_primaries values_relative i.state $controls, ro
esttab 	using $pathout/Survey_base.tex, booktabs nonotes replace compress  label nomtitles rename() ///
		indicate("State FE=*state*" "Year of birth FE=*yob*" "Race FE=*ethnicity*" "Income bracket and education FE=*inc_bracket" "Religious denomination FE=*denom*" "Occupation FE=*occup*" "Gender and employment status=female" "Local population density=ln_pd" "Altruism and generalized trust=altruism" "Abs. value of moral values index=abs_values_relative")  ///
		drop(_cons employed *educ* trust_general) se(2) b(a2) r2(2)  star(* 0.10 ** 0.05 *** 0.01) ///
		prehead("{\begin{tabular}{l*{9}{c}}\toprule\toprule&\multicolumn{9}{c}{\textit{Dependent variable:}}\\[.1cm] &\multicolumn{6}{c}{Votes in presidential election} &\multicolumn{3}{c}{Votes in GOP primaries}\\\cmidrule(lr){2-7}\cmidrule(lr){8-10} &\multicolumn{3}{c}{1 if voted for Trump} &\multicolumn{3}{c}{$\Delta$ [Trump -- Ave. GOP]} & \multicolumn{3}{c}{1 if voted for Trump}\\\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-10}")



* Table 5

eststo clear
eststo: qui reg gop_2016 values_relative $benchmark i.state, ro
eststo: qui reg gop_2016 values_relative $benchmark $controls_benchmark i.county, ro
eststo: qui pdslasso gop_2016 values_relative ($benchmark $controls_benchmark i.county), ro
eststo: qui reg trump_election_ave values_relative $benchmark i.state, ro
eststo: qui reg trump_election_ave values_relative $benchmark $controls_benchmark i.county, ro
eststo: qui pdslasso trump_election_ave values_relative ($benchmark $controls_benchmark i.county), ro
eststo: qui reg trump_primaries values_relative $benchmark i.state, ro
eststo: qui reg trump_primaries values_relative $benchmark $controls_benchmark i.county, ro
eststo: qui pdslasso trump_primaries values_relative ($benchmark $controls_benchmark i.county), ro

esttab 	using $pathout/Survey_controls.tex, booktabs nonotes replace compress  label nomtitles rename() ///
		indicate("State FE=*state*" "County FE=*county*" "Additional controls=*yob*")  ///
		drop(_cons *ethni* female *occup* *denom* employed abs_values_relative altruism trust_general) se(2) b(a2) r2(2)  star(* 0.10 ** 0.05 *** 0.01) ///
		prehead("{\begin{tabular}{l*{9}{c}}\toprule\toprule&\multicolumn{9}{c}{\textit{Dependent variable:}}\\[.1cm] &\multicolumn{6}{c}{Votes in presidential election} &\multicolumn{3}{c}{Votes in GOP primaries}\\\cmidrule(lr){2-7}\cmidrule(lr){8-10} &\multicolumn{3}{c}{1 if voted for Trump} &\multicolumn{3}{c}{$\Delta$ [Trump -- Ave. GOP]} & \multicolumn{3}{c}{1 if voted for Trump}\\\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-10} & OLS & OLS & PDS & OLS & OLS & PDS & OLS & OLS & PDS\\\cmidrule(lr){2-2}\cmidrule(lr){3-3}\cmidrule(lr){4-4}\cmidrule(lr){5-5}\cmidrule(lr){6-6}\cmidrule(lr){7-7}\cmidrule(lr){8-8}\cmidrule(lr){9-9}\cmidrule(lr){10-10}")



