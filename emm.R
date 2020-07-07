# Effect measure modificaton by other fluids ####
# Kevin Chen
# October 14, 2019

# setwd('gm')
# rm(list = ls())
# rm(list = ls()[-grep("outcome", ls())])
# rm(list = ls()[-grep('cohort$', ls())])

library(here)
library(survival)

exposure.lag <- 21

if (!('cohort_analytic' %in% ls())) {
	source(here::here('modeling.R'))
	names(outcome) <- outcome
}

# For straight-outcome modeling, ###
# censor those with nonzero soluble or synthetic exposure. but no straight
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble == 0 & cum_synthetic == 0) |
																(cum_straight != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_straight.cohort_prepped'
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/clean_referent/', 'Straight/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paset0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)

# Soluble ###
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble == 0 & cum_synthetic == 0) |
																(cum_soluble != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_soluble.cohort_prepped'
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/clean_referent/', 'Soluble/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paste0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)

# Synthetic ###
get.mod(
	cohort_py = cohort_analytic[(cum_straight == 0 &
															 	cum_soluble == 0 & cum_synthetic == 0) |
																(cum_synthetic != 0)],
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped'
)
rm(list = ls()[grepl('.cohort_prepped', ls())])
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/clean_referent/', 'Synthetic/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paste0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)


# As interaction: Straight ####
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Straight'
)
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/Intxn/Straight/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paste0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)

# As interaction: Soluble ####
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Soluble'
)
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/Intxn/Soluble/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paste0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)

# As interaction: Synthetic ####
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome,
	probs = probs,
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = T,
	stratify.baseline = T,
	cohort_prepped.suffix = '_synthetic.cohort_prepped',
	special.interaction = 'Synthetic'
)
lapply(
	paste0(gsub(" ", "_", outcome), ".coxph"),
	function(tmp.coxph) {
		dir.path <- to_drive_D(here::here(paste0('resources/emm/lag ', exposure.lag, '/Intxn/Synthetic/')))
		dir.create(directory.path, showWarnings = F, recursive = T)
		save(list = tmp.coxph,
				 file = paste0(dir.path, paste0(tmp.coxph, '.rdata'))
				 )
	}
)