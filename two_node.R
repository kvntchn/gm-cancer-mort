# LTMLE ####
# Kevin Chen
# October 24, 2019

# rm(list = ls())
# rm(list = ls()[-grep("outcome", ls())])
# rm(list = ls()[-grep('cohort$', ls())])

library(here)
library(survival)
library(multinom)

if (!('outcome.type' %in% ls())) {
	outcome.type <- 'mortality'
}
if (!('exposure.lag' %in% ls())) {
	exposure.lag <- 30
	}

# Get outcome/exposure data ####
if (!('cohort_analytic' %in% ls())) {
	source(here::here('../gm-wrangling/wrangling', '05-Get-Exposure-Outcome.R'))
	cohort_analytic <- get.cohort_analytic(
		outcome_type = outcome.type,
		exposure.lag = exposure.lag,
		deathage.max = NULL
		)
	setorder(cohort_analytic, studyno, year)
	cohort_analytic <- cohort_analytic[year >= 1941 & (
		year(yin) < 1938 | year >= year(yin + 365.25 * 3)
		)]
}

# Get analytic datasets ####
source(here::here('modeling', 'modeling.R'))
outcome <- c("Laryngeal cancer",	"Lung cancer",
										"Esophageal cancer", "Stomach cancer",
										"Colon cancer",
										"Rectal cancer", "Bladder cancer",
										"Liver cancer",
										"Pancreatic cancer", "Skin cancer",
										"Prostate cancer", "Brain and nervous system cancers",
										"Leukemia", "Breast cancer"
										)
get.mod(
	cohort_py = cohort_analytic,
	outcome = outcome[4],
	probs = seq(0, 1, 1/6),
	outcome_type = outcome.type,
	specific.race = NULL,
	run_coxph = F
)

Obs <- as.data.frame(Stomach_cancer.cohort_prepped[,.(
	t = age.year,
	studyno,
	W1 = Race,
	W2 = Sex,
	W3 = `Year of hire.cat`,
	W4 = Plant,
	W5 = Year.cat,
	A = Straight.cat,
	E = Soluble + Synthetic > 1,
	Y = event
)
])
setDT(Obs)

Obs[,.(N = n_distinct(studyno)), by = .(t)][order(t)]

# Treatment mechanism g(E) ####
get.g_E <- function(t.min = 20,
										t.max = 108) {

	X <- dcast(Obs[t >= t.min & t <= t.max],
							 studyno ~ t,
							 value.var = c("W4", "W5",
							 							"A", "E", "Y"))


	dcast(Obs[t >= t.min & t <=t.max],
				studyno ~ 1,
				value.var = c("W1", "W2", "W3"),
				fun.aggregate = function(x) {unique(x[!is.na(x)])})


	X[, .(
		W1
	),
		by = .(studyno)]

	lapply(t.min:t.max, function(t) {
		assign(paste0("formula_", t),
				 paste(
				 	paste0('Y_', t),
				 	'~',
				 	c('W1', 'W2', 'W3'),
				 	"+",
				 	paste0('W4_', t.min:t, collapse = " + "),
				 	"+",
				 	paste0('A_', t.min:t, collapse = " + "),
				 	"+",
				 	paste0('sum(c(', paste0('E_', t.min:t, collapse = ', '), ')) > 0')
				 ),
				 inherits = T)
	})

	for (t in t.min:t.max) {
		assign(paste0("g_E_", t),
					 glm(
					 	as.formula(get(paste0('formula_', t))),
					 		data = X,
					 		family = binomial),
					 envir = .GlobalEnv)
	}


}