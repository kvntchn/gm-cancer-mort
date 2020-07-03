# Effect measure modificaton by other fluids ####
# With loosend referent group
# Kevin Chen
# November 6, 2019

# setwd('gm')
# rm(list = ls())
# rm(list = ls()[-grep("outcome", ls())])
# rm(list = ls()[-grep('cohort$', ls())])

library(here)
library(survival)

exposure.lag <- 30

if (!('cohort_analytic' %in% ls())) {
	source(here::here('modeling', 'modeling.R'))
	names(outcome) <- outcome
}

# at 500 ug/m^3/year for 10 years ####
# For straight-outcome modeling, ###
# censor those with nonzero soluble or synthetic exposure. but no straight
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 5 & cum_synthetic == 0) |
																(cum_straight != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_straight.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "niosh"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(
				 	paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Straight'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Soluble (neds special treatment) ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 5 & cum_synthetic == 0) |
																(cum_soluble != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_soluble.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "niosh"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Soluble'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Synthetic ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 5 & cum_synthetic == 0) |
																(cum_synthetic != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "niosh"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Synthetic'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# at 10 ug/m^3/year for 10 years ####
# For straight-outcome modeling, ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.1 & cum_synthetic == 0) |
																(cum_straight != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_straight.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "ambient"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Straight'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Soluble (neds special treatment) ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.1 & cum_synthetic == 0) |
																(cum_soluble != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_soluble.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "ambient"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Soluble'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Synthetic ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.1 & cum_synthetic == 0) |
																(cum_synthetic != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "ambient"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Synthetic'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# at 5 ug/m^3/year for 10 years ####
# For straight-outcome modeling, ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.055 & cum_synthetic == 0) |
																(cum_straight != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_straight.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "first"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol5.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Straight'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Soluble (neds special treatment) ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.055 & cum_synthetic == 0) |
																(cum_soluble != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_soluble.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "first"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol5.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Soluble'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# Synthetic ###
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble <= 0.055 & cum_synthetic == 0) |
																(cum_synthetic != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	run_messy =  "Soluble",
	specific_messy = "first"
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), "_sol5.coxph"),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/clean_referent/Synthetic'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)


# As interaction: Straight ####
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Straight',
	run_messy = "Soluble"
)
lapply(
	c(paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol5.coxph")),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/Intxn/Straight'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# As interaction: Soluble ####
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Soluble',
	run_messy = "Soluble"
)
lapply(
	c(paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol5.coxph")),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/Intxn/Soluble'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)

# As interaction: Synthetic ####
rm(list = ls()[grepl('.coxph', ls())])
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Synthetic',
	run_messy = "Soluble"
)
lapply(
	c(paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
		paste0(gsub(" ", "_", outcome), "_sol5.coxph")),
	function(tmp.coxph) {
		save(list = tmp.coxph,
				 file = to_drive_D(here::here(paste0('modeling/resources/mortality/emm/lag ', exposure.lag, '/Intxn/Synthetic'),
				 						paste0(tmp.coxph, '.rdata')))
				 )
	}
)