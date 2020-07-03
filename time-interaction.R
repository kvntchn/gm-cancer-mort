# Testing covariate-time interactions to assess ####
# Proportional hazards assumption
library(here)

if (sum(grepl("cohort_analytic", ls())) == 0) {
	source(here::here('mortality modeling', 'modeling.R'))
	lapply(paste0(gsub(" ", "_", outcome), ".coxph"),
				 function(outcome.coxph) {
				 	load(to_drive_D(here::here(
				 		'mortality modeling/resources',
				 		paste0(outcome.coxph, '.rdata'))
				 	),
				 	envir = .GlobalEnv)
				 })
}

panderOptions('table.split.table', 160)

for (outcome_i in outcome.selected) {

tmp.cohort_prepped <- get(paste0(gsub(" ", "_", outcome_i), ".cohort_prepped"))

tmp_tt.coxph <-
	coxph(
		as.formula(
			paste(
				"Surv(age.year1, age.year2, event) ~",
				"Soluble.cat +",
				# "tt(as.numeric(Soluble.cat)) +",
				"Straight.cat +",
				# "tt(as.numeric(Straight.cat))+",
				"Synthetic +",
				"tt(as.numeric(Synthetic.cat))+",
				if (min(table(tmp.cohort_prepped[event == 1]$Year.cat)) > 1) {
					paste0("Year.cat +"
								 # "tt(as.numeric(Year.cat))+"
					)
				},
				if (min(table(tmp.cohort_prepped$`Year of hire.cat`)) > 1) {
					paste0("`Year of hire.cat` +"
								 # tt(as.numeric(`Year of entry.cat`))+"
					)
				},
				if (min(table(tmp.cohort_prepped$`Race`)) > 1) {
					paste0("Race +"
								 # "tt(as.numeric(Race))+"
					)
				},
				if (min(table(tmp.cohort_prepped[event == 1]$Sex)) > 1) {
					paste0("Sex +"
								 # "tt(as.numeric(Sex))+"
					)
				},
				paste0("strata(Plant)"
				)
			)
		)
		,
		tt = function(x, t, ...) {
			x * t
		},
		data = {
			if (min(table(tmp.cohort_prepped[event == 1]$Sex)) <= 10) {
				tmp.cohort_prepped[Sex == levels(Sex)[which.max(table(tmp.cohort_prepped[event == 1]$Sex))]]
			} else
				tmp.cohort_prepped
		},
		ties = 'efron'
	)

pander(
	if (sum(grepl("tt", names(coef(tmp_tt.coxph)))) > 1) {
		summary(tmp_tt.coxph)$coefficients[grepl("tt", names(coef(tmp_tt.coxph))),]
	} else {
		cbind(
			data.frame(covariate = rownames(summary(tmp_tt.coxph)$coefficients)[grepl("tt", names(coef(tmp_tt.coxph)))]),
			as.data.frame(t(apply(
				t(summary(tmp_tt.coxph)$coefficients[grepl("tt", names(coef(tmp_tt.coxph))),]),
				2, as.numeric
			)))
		)},
		digits = 3)

assign(paste0(gsub(" ", "_", outcome_i), "_tt.coxph"), tmp_tt.coxph, inherits = T)

}