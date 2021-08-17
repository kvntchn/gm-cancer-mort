# Survival modeling ####
# Kevin Chen
# August 7, 2019

# rm(list = ls())
# rm(list = ls()[-grep("outcome.selected", ls())])
# rm(list = ls()[-grep('cohort', ls())])

library(here)
library(survival)

outcome.type <- 'mortality'

if (!('probs' %in% ls())) {
	probs <- seq(0, 1, 1/3)
}

if (!('exposure.lag' %in% ls())) {
	exposure.lag <- 21
}

# Get outcome/exposure data ####
if (!('cohort_analytic' %in% ls())) {
	source(here::here('../gm-wrangling/wrangling', '00-Hello.R'))
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

# Remove people with unknown cause of death
cohort_analytic <- cohort_analytic[!(status15 == 6 & (is.na(icd)))]

# Model! ####

# Quantiles for specifying continuous variables as categorical
get.mod <- function(
	cohort_py = as.data.table(as.data.frame(cohort_analytic)),
	outcome = outcome,
	outcome_type = 'mortality',
	probs = seq(0, 1, 1 / 3),
	covariate.probs = seq(0, 1, 1/6),
	new_data = T,
	run_coxph = F,
	special.interaction = NULL,
	formula.append = NULL,
	stratify.baseline = T,
	cohort_prepped.suffix = ".cohort_prepped",
	run_messy = NULL,
	specific_messy = NULL,
	rm_stepwise = F,
	...) {

	cohort_py <- as.data.table(as.data.frame(cohort_py))
	if (!('probs' %in% ls(envir = .GlobalEnv))) {
		assign('probs', probs, envir = .GlobalEnv)}
	if (!('covariate.probs' %in% ls(envir = .GlobalEnv))) {
		assign('covariate.probs', probs, envir = .GlobalEnv)}

	if (!is.null(special.interaction)) {
		if (!(special.interaction %in% c("Straight", "Soluble", "Synthetic"))) {
			warning("special.interation must be 'Straight', 'Soluble', or 'Synthetic'")
		}
	}

	# if (sum(grepl('soluble', messy, ignore.case = T)) > 0) {
	# 	cohort_py[, `:=`(
	# 		cum_soluble500 = ifelse(cum_soluble < 5, 0, cum_soluble),
	# 		cum_soluble10 = ifelse(cum_soluble < 0.1, 0, cum_soluble),
	# 		cum_soluble5 = ifelse(cum_soluble < 0.05, 0, cum_soluble)
	# 		)]
	# }

	lapply(outcome,
				 function(outcome) {
				 	if (rm_stepwise) {
				 		Sys.sleep(0)
				 		coxph.list <- ls(envir = .GlobalEnv)[
				 			grepl('.coxph', ls(envir = .GlobalEnv))]
				 		rm(list = coxph.list[!grepl(
				 			paste0(substr(outcome, 1, 4), ".*coxph"),
				 			coxph.list,
				 			ignore.case = T
				 		)], envir = .GlobalEnv)

				 	}
				 	if (new_data) {
				 		# Define quantiles
				 		quantiles.exposure <- cohort_py[immortal == 0 &
				 																			get(outcome) == 1 &
				 																			nohist == 0 &
				 																			wh == 1 &
				 																			right.censored != 1 &
				 																			!is.na(race)
				 																		, .(
				 																			cum_soluble = cum_soluble[.N],
				 																			cum_straight = cum_straight[.N],
				 																			cum_synthetic = cum_synthetic[.N],
				 																			# cum_soluble500 = cum_soluble500[.N],
				 																			# cum_soluble10 = cum_soluble10[.N],
				 																			# cum_soluble5 = cum_soluble5[.N],
				 																			year = year[.N],
				 																			yearwork = yearwork[.N],
				 																			yin = unique(yin),
				 																			yob   = unique(yob)
				 																		),
				 																		by = .(studyno)]

				 		quantiles.exposure <- quantiles.exposure[, .(
				 			Outcome = c(
				 				paste('\\hline', outcome),
				 				paste0('$(n=',
				 							 prettyNum(sum(cohort_py[
				 							 	immortal == 0 &
				 							 		nohist == 0 &
				 							 		wh == 1 & right.censored != 1 &
				 							 		!is.na(race),
				 							 	get(outcome)]), big.mark = '\\,'),
				 							 ')$'),
				 				rep('',
				 						max(length(covariate.probs),
				 								length(probs)) - 2)
				 			),
				 			# Percentile = probs * 100,
				 			cum_straight  = if (sum(cum_straight != 0) > 1) {
				 				quantile.tmp <- c(quantile(cum_straight[cum_straight > 0], probs),
				 													rep(max(cum_straight[cum_straight > 0]),
				 															length(covariate.probs) - length(probs)))
				 				if (min(table(cut(cum_straight[cum_straight > 0],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_straight[cum_straight > 0], seq(0, 1, 1 / j)),
				 								rep(max(cum_straight), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_straight[cum_straight > 0],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_straight[cum_straight > 0],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_straight[cum_straight > 0], seq(0, 1, 1)),
				 						rep(max(cum_straight), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			cum_soluble   = if (sum(cum_soluble != 0) > 1) {
				 				quantile.tmp <- c(quantile(cum_soluble[cum_soluble > 0], probs),
				 													rep(max(cum_soluble[cum_soluble > 0]),
				 															length(covariate.probs) - length(probs)))
				 				if (min(table(cut(cum_soluble[cum_soluble > 0],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_soluble[cum_soluble > 0], seq(0, 1, 1 / j)),
				 								rep(max(cum_soluble), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_soluble[cum_soluble > 0],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_soluble[cum_soluble > 0],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_soluble[cum_soluble > 0], seq(0, 1, 1)),
				 						rep(max(cum_soluble), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			cum_soluble500   = if (sum(cum_soluble > 5) > 1) {
				 				quantile.tmp <-
				 					c(quantile(cum_soluble[cum_soluble > 5], probs),
				 						rep(
				 							max(cum_soluble[cum_soluble > 5]),
				 							length(covariate.probs) - length(probs)
				 						))
				 				if (min(table(cut(cum_soluble[cum_soluble > 5],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_soluble[cum_soluble > 5], seq(0, 1, 1 / j)),
				 								rep(max(cum_soluble), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_soluble[cum_soluble > 5],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_soluble[cum_soluble > 5],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_soluble[cum_soluble > 5], seq(0, 1, 1)),
				 						rep(max(cum_soluble), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			cum_soluble10   = if (sum(cum_soluble > 0.1) > 1) {
				 				quantile.tmp <-
				 					c(quantile(cum_soluble[cum_soluble > 0.1], probs),
				 						rep(
				 							max(cum_soluble[cum_soluble > 0.1]),
				 							length(covariate.probs) - length(probs)
				 						))
				 				if (min(table(cut(cum_soluble[cum_soluble > 0.1],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_soluble[cum_soluble > 0.1], seq(0, 1, 1 / j)),
				 								rep(max(cum_soluble), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_soluble[cum_soluble > 0.1],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_soluble[cum_soluble > 0.1],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_soluble[cum_soluble > 0.1], seq(0, 1, 1)),
				 						rep(max(cum_soluble), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			cum_soluble5   = if (sum(cum_soluble > 0.05) > 1) {
				 				quantile.tmp <-
				 					c(quantile(cum_soluble[cum_soluble > 0.05], probs),
				 						rep(
				 							max(cum_soluble[cum_soluble > 0.05]),
				 							length(covariate.probs) - length(probs)
				 						))
				 				if (min(table(cut(cum_soluble[cum_soluble > 0.05],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_soluble[cum_soluble > 0.05], seq(0, 1, 1 / j)),
				 								rep(max(cum_soluble), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_soluble[cum_soluble > 0.05],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_soluble[cum_soluble > 0.05],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_soluble[cum_soluble > 0.05], seq(0, 1, 1)),
				 						rep(max(cum_soluble), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			cum_synthetic = if (sum(cum_synthetic != 0) > 1) {
				 				quantile.tmp <- c(quantile(cum_synthetic[cum_synthetic > 0], probs),
				 													rep(
				 														max(cum_synthetic[cum_synthetic > 0]),
				 														length(covariate.probs) - length(probs)
				 													))
				 				if (min(table(cut(cum_synthetic[cum_synthetic > 0],
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					for (j in (length(probs) - 2):min(2, length(probs) - 2)) {
				 						quantile.tmp <-
				 							c(quantile(cum_synthetic[cum_synthetic > 0], seq(0, 1, 1 / j)),
				 								rep(max(cum_synthetic), length(covariate.probs) - (j + 1)))
				 						if (min(table(cut(
				 							cum_synthetic[cum_synthetic > 0],	unique(quantile.tmp)
				 						)) >= 15)) {
				 							break
				 						}
				 					}
				 					quantile.tmp
				 				}
				 				if (min(table(cut(cum_synthetic[cum_synthetic > 0],	unique(quantile.tmp))) < 10)) {
				 					c(quantile(cum_synthetic[cum_synthetic > 0], seq(0, 1, 1)),
				 						rep(max(cum_synthetic), length(covariate.probs) - 2))
				 				} else {
				 					quantile.tmp
				 				}
				 			} else {
				 				0
				 			},
				 			year          = {
				 				quantile.tmp <-	quantile(year, covariate.probs)
				 				if (min(table(cut(year,
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					quantile.tmp <- c(quantile(year, seq(0, 1, 1 / 3)),
				 														rep(max(year), 3))
				 					if (min(table(cut(year,
				 														unique(quantile.tmp)))) >= 15) {
				 						quantile.tmp
				 					} else {
				 						quantile.tmp <- c(quantile(year, seq(0, 1, 1 / 2)),
				 															rep(max(year), 4))
				 						if (min(table(cut(year,
				 															unique(quantile.tmp))) >= 10)) {
				 							quantile.tmp
				 						} else {
				 							c(quantile(year, seq(0, 1, 1)),
				 								rep(max(year), 5))
				 						}
				 					}
				 				}
				 			},
				 			yearwork      = {
				 				quantile.tmp <- quantile(yearwork, covariate.probs)
				 				if (min(table(cut(yearwork,
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					quantile.tmp <- c(quantile(yearwork, seq(0, 1, 1 / 3)),
				 														rep(max(yearwork), 3))
				 					if (min(table(cut(yearwork,
				 														unique(quantile.tmp)))) >= 15) {
				 						quantile.tmp
				 					} else {
				 						quantile.tmp <- c(quantile(yearwork, seq(0, 1, 1 / 2)),
				 															rep(max(yearwork), 4))
				 						if (min(table(cut(yearwork,
				 															unique(quantile.tmp))) >= 10)) {
				 							quantile.tmp
				 						} else {
				 							c(quantile(yearwork, seq(0, 1, 1)),
				 								rep(max(yearwork), 5))
				 						}
				 					}
				 				}
				 			},
				 			yin         = {
				 				quantile.tmp <- quantile(year(yin), covariate.probs)
				 				if (min(table(cut(year(yin),
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					quantile.tmp <- c(quantile(year(yin), seq(0, 1, 1 / 3)),
				 														rep(max(year(yin)), 3))
				 					if (min(table(cut(year(yin),
				 														unique(quantile.tmp)))) >= 15) {
				 						quantile.tmp
				 					} else {
				 						quantile.tmp <- c(quantile(year(yin), seq(0, 1, 1 / 2)),
				 															rep(max(year(yin)), 4))
				 						if (min(table(cut(year(yin),
				 															unique(quantile.tmp))) >= 10)) {
				 							quantile.tmp
				 						} else {
				 							c(quantile(year(yin), seq(0, 1, 1)),
				 								rep(max(year(yin)), 5))
				 						}
				 					}
				 				}
				 			},
				 			yob           = {
				 				quantile.tmp <- quantile(year(yob), covariate.probs)
				 				if (min(table(cut(year(yob),
				 													unique(quantile.tmp)))) >= 20) {
				 					quantile.tmp
				 				} else {
				 					quantile.tmp <- c(quantile(year(yob), seq(0, 1, 1 / 3)),
				 														rep(max(year(yob)), 3))
				 					if (min(table(cut(year(yob),
				 														unique(quantile.tmp)))) >= 15) {
				 						quantile.tmp
				 					} else {
				 						quantile.tmp <- c(quantile(year(yob), seq(0, 1, 1 / 2)),
				 															rep(max(year(yob)), 4))
				 						if (min(table(cut(year(yob),
				 															unique(quantile.tmp))) >= 10)) {
				 							quantile.tmp
				 						} else {
				 							c(quantile(year(yob), seq(0, 1, 1)),
				 								rep(max(year(yob)), 5))
				 						}
				 					}
				 				}
				 			}
				 		)]

				 		# quantiles.exposure

				 		# Ready for coxph
				 		tmp.cohort_prepped <-
				 			cohort_py[
				 				immortal == 0 & nohist == 0 & wh == 1 &
				 					right.censored != 1 &
				 					!is.na(race)&
				 					{if (outcome_type == "incidence") {
				 						year >= 1985
				 					} else !is.na(year)}, .(
				 						py,
				 						Race = factor(race, levels = c("White", "Black")),
				 						Sex = factor(
				 							sex,
				 							levels = c("M", "F"),
				 							labels = c("Male", "Female")
				 						),
				 						Plant = factor(
				 							plant,
				 							levels = c(1, 2, 3),
				 							labels = c("Gear \\& Axle", "HydraMatic", "Saginaw")
				 						),
				 						event = get(outcome),
				 						ddiag = {
				 							if (outcome_type == 'incidence') {
				 								get(gsub("canc_", "ddiag_", outcome))
				 							}
				 						},
				 						studyno,
				 						Soluble = cum_soluble,
				 						Straight = cum_straight,
				 						Synthetic = cum_synthetic,
				 						Soluble.cat   = if (length(unique(quantiles.exposure$cum_soluble)) > 1) {cut(cum_soluble,
				 																																												 c(
				 																																												 	-Inf, 0, unique(quantiles.exposure[, cum_soluble])[-c(1, length(unique(quantiles.exposure$cum_soluble)))],
				 																																												 	max(cum_soluble)
				 																																												 ))} else {NA},
				 						Soluble500.cat   = if (length(unique(quantiles.exposure$cum_soluble500)) > 1) {cut(cum_soluble,
				 																																															 c(
				 																																															 	-Inf, 5, unique(quantiles.exposure[, cum_soluble500])[-c(1, length(unique(quantiles.exposure$cum_soluble500)))],
				 																																															 	max(cum_soluble)
				 																																															 ))} else {NA},
				 						Soluble10.cat   = if (length(unique(quantiles.exposure$cum_soluble10)) > 1) {cut(cum_soluble,
				 																																														 c(
				 																																														 	-Inf, 0.1, unique(quantiles.exposure[, cum_soluble10])[-c(1, length(unique(quantiles.exposure$cum_soluble10)))],
				 																																														 	max(cum_soluble)
				 																																														 ))} else {NA},
				 						Soluble5.cat   = if (length(unique(quantiles.exposure$cum_soluble5)) > 1) {cut(cum_soluble,
				 																																													 c(
				 																																													 	-Inf, 0.05, unique(quantiles.exposure[, cum_soluble5])[-c(1, length(unique(quantiles.exposure$cum_soluble5)))],
				 																																													 	max(cum_soluble)
				 																																													 ))} else {NA},
				 						Straight.cat  = if (length(unique(quantiles.exposure$cum_straight)) > 1) {cut(cum_straight,
				 																																													c(
				 																																														-Inf, 0, unique(quantiles.exposure[, cum_straight])[-c(1, length(unique(quantiles.exposure$cum_straight)))],
				 																																														max(cum_straight)
				 																																													))} else {NA},
				 						Synthetic.cat = if (length(unique(quantiles.exposure$cum_synthetic)) > 1) {cut(cum_synthetic,
				 																																													 c(
				 																																													 	-Inf, 0, unique(quantiles.exposure[, cum_synthetic])[-c(1, length(unique(quantiles.exposure$cum_synthetic)))],
				 																																													 	max(cum_synthetic)
				 																																													 ))} else {NA},
				 						Year = year,
				 						Year.cat      = cut(
				 							year,
				 							c(min(year),
				 								unique(quantiles.exposure[, year])[-c(1, length(unique(quantiles.exposure$year)))],
				 								max(year)),
				 							dig.lab = 4,
				 							include.lowest = T
				 						),
				 						`Years since hire` = yearwork,
				 						`Years since hire.cat`  = cut(
				 							yearwork,
				 							c(min(yearwork), unique(quantiles.exposure[, yearwork])[-c(1, length(unique(quantiles.exposure$yearwork)))],
				 								max(yearwork)),
				 							dig.lab = 4,
				 							include.lowest = T
				 						),
				 						age,
				 						age.year = ceiling(year - year(yob)),
				 						age.year1,
				 						age.year2 = {
				 							if (outcome_type  == "incidence")
				 							{ifelse(
				 								!is.na(get(gsub("canc_", "ddiag_", outcome))) &
				 									year == year(get(gsub("canc_", "ddiag_", outcome))),
				 								time_length(difftime(
				 									get(gsub("canc_", "ddiag_", outcome)),
				 									yob),
				 									'years'), age.year2)
				 							} else {
				 								age.year2}
				 						},
				 						`Year of hire` = yin,
				 						`Year of hire.cat`     = cut(
				 							year(yin),
				 							c(
				 								min(year(yin)),
				 								unique(quantiles.exposure[, yin])[-c(1, length(unique(quantiles.exposure$yin)))],
				 								max(year(yin))
				 							),
				 							dig.lab = 4,
				 							include.lowest = T
				 						),
				 						`Year of birth` = yob,
				 						`Year of birth.cat`      = cut(
				 							year(yob),
				 							c(
				 								min(year(yob)),
				 								unique(quantiles.exposure[, yob])[-c(1, length(unique(quantiles.exposure$yob)))],
				 								max(year(yob))
				 							),
				 							dig.lab = 4,
				 							include.lowest = T
				 						)
				 					)][age.year1 < age.year2 & event != 2]

				 		if (min(table(tmp.cohort_prepped[event == 1]$Sex)) <= 10) {
				 			tmp.cohort_prepped <- tmp.cohort_prepped[Sex == levels(Sex)[which.max(table(tmp.cohort_prepped[event == 1]$Sex))]]
				 		}

				 		assign(paste0(
				 			gsub(" ", "_", outcome),
				 			cohort_prepped.suffix
				 		),
				 		tmp.cohort_prepped, inherits = T)
				 	} else {
				 		tmp.cohort_prepped <- get(paste0(
				 			gsub(" ", "_", outcome),
				 			cohort_prepped.suffix))
				 	}

				 	if (is.null(special.interaction)) {
				 		basic.formula <- 	paste(
				 			"Surv(age.year1, age.year2, event) ~",
				 			if (n_distinct(tmp.cohort_prepped$Straight.cat) > 1) {
				 				"Straight.cat +"
				 			},
				 			if (n_distinct(tmp.cohort_prepped$Soluble.cat) > 1) {
				 				"Soluble.cat +"
				 			},
				 			if (n_distinct(tmp.cohort_prepped$Synthetic.cat) > 1) {
				 				"Synthetic.cat +"
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$Year.cat)) > 1 &
				 					length(unique(tmp.cohort_prepped[event == 1]$Year.cat)) > 1) {
				 				"Year.cat +"
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$`Year of hire.cat`)) > 1 &
				 					length(unique(tmp.cohort_prepped[event == 1]$`Year of hire.cat`)) > 1) {
				 				"`Year of hire.cat` +"
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$`Race`)) > 10 &
				 					length(unique(tmp.cohort_prepped[event == 1]$`Race`)) > 1) {
				 				if (stratify.baseline) {
				 					"strata(Race) +"
				 				} else {
				 					"Race +"
				 				}
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$Sex)) > 10 &
				 					length(unique(tmp.cohort_prepped[event == 1]$Sex)) > 1) {
				 				"Sex +"
				 			},
				 			if (stratify.baseline) {
				 				"strata(Plant)"
				 			} else {
				 				'Plant'
				 			}
				 		)
				 	} else {
				 		basic.formula <- 	paste(
				 			"Surv(age.year1, age.year2, event) ~",
				 			if (n_distinct(tmp.cohort_prepped$Straight.cat) > 1) {
				 				paste0("Straight.cat + ", ifelse(
				 					special.interaction == "Straight", "factor(as.numeric(Straight.cat) == 1 & as.numeric(Soluble.cat) != 1 & as.numeric(Synthetic.cat) != 1) +", "")
				 				)
				 			},
				 			if (n_distinct(tmp.cohort_prepped$Soluble.cat) > 1) {
				 				paste0("Soluble.cat +", ifelse(
				 					special.interaction == "Soluble", "factor(as.numeric(Soluble.cat) == 1 & as.numeric(Straight.cat) != 1 & as.numeric(Synthetic.cat) != 1) +", "")
				 				)
				 			},
				 			if (n_distinct(tmp.cohort_prepped$Synthetic.cat) > 1) {
				 				paste0("Synthetic.cat +", ifelse(
				 					special.interaction == "Synthetic", "factor(as.numeric(Synthetic.cat) == 1 & as.numeric(Straight.cat) != 1 & as.numeric(Soluble.cat) != 1) +", "")
				 				)
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$Year.cat)) > 1 &
				 					length(unique(tmp.cohort_prepped[event == 1]$Year.cat)) > 1) {
				 				"Year.cat +"
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$`Year of hire.cat`)) > 1 &
				 					length(unique(tmp.cohort_prepped[event == 1]$`Year of hire.cat`)) > 1) {
				 				"`Year of hire.cat` +"
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$`Race`)) > 10 &
				 					length(unique(tmp.cohort_prepped[event == 1]$`Race`)) > 1) {
				 				if (stratify.baseline) {
				 					"strata(Race) +"
				 				} else {
				 					"Race +"
				 				}
				 			},
				 			if (min(table(tmp.cohort_prepped[event == 1]$Sex)) > 10 &
				 					length(unique(tmp.cohort_prepped[event == 1]$Sex)) > 1) {
				 				"Sex +"
				 			},
				 			if (stratify.baseline) {
				 				"strata(Plant)"
				 			} else {
				 				'Plant'
				 			}
				 		)
				 	}

				 	# Cox models
				 	if (run_coxph) {
				 		message(paste0("\nCox model for ", outcome, "..."))
				 		tmp.coxph <-
				 			coxph(as.formula(paste(basic.formula, formula.append)),
				 						data = tmp.cohort_prepped,
				 						ties = 'efron', ...)
				 		print(summary(tmp.coxph)$coefficients[,c(2,4,5)])
				 		assign(paste0(
				 			gsub(" ", "_", outcome),
				 			".coxph"
				 		),
				 		tmp.coxph, inherits = T)

				 		saveRDS(tmp.coxph,
				 						file = to_drive_D(here::here(
				 							paste0("resources",
				 										 ifelse(outcome.type == 'incidence',
				 										 			 '/incidence',
				 										 			 paste0(paste0('/lag ', exposure.lag))),
				 										 ifelse(length(probs) > 4, '/verbose', '')),
				 							paste0(paste0(gsub(" ", "_", outcome), ".coxph"), '.rds')))
				 		)
				 	}

				 	if (sum(grepl('soluble', run_messy, ignore.case = T)) > 0) {
				 		# Soluble exposure at or below NIOSH limit allowed into reference
				 		if (is.null(specific_messy) | sum(grepl("niosh", specific_messy, ignore.case = T))) {
				 			message(paste0("\nCox model for ", outcome, " allowing soluble exposure below NIOSH limit into the referent group..."))
				 			tmp.coxph <-
				 				coxph(as.formula(paste(gsub("Soluble.cat", "Soluble500.cat", basic.formula), formula.append)),
				 							data = tmp.cohort_prepped,
				 							ties = 'efron')
				 			print(summary(tmp.coxph)$coefficients[,c(2,4,5)])
				 			assign(paste0(
				 				gsub(" ", "_", outcome),
				 				"_sol500.coxph"
				 			),
				 			tmp.coxph, inherits = T)
				 		}
				 		# Soluble exposure at or below ambient levels allowed into reference
				 		if (is.null(specific_messy) | sum(grepl("ambie", specific_messy, ignore.case = T))) {
				 			message(paste0("\nCox model for ", outcome, " allowing soluble exposure below ambient level into the referent group..."))
				 			tmp.coxph <-
				 				coxph(as.formula(paste(gsub("Soluble.cat", "Soluble10.cat", basic.formula), formula.append)),
				 							data = tmp.cohort_prepped,
				 							ties = 'efron')
				 			print(summary(tmp.coxph)$coefficients[,c(2,4,5)])
				 			assign(paste0(
				 				gsub(" ", "_", outcome),
				 				"_sol10.coxph"
				 			),
				 			tmp.coxph, inherits = T)
				 		}
				 		# Soluble exposure at or below 1 percent of limit allowed into referent group
				 		if (is.null(specific_messy) | sum(grepl("low|first", specific_messy, ignore.case = T))) {
				 			message(paste0("\nCox model for ", outcome, " allowing soluble exposure below 1 percent of limit among cases into the referent group..."))
				 			tmp.coxph <-
				 				coxph(as.formula(paste(gsub("Soluble.cat", "Soluble5.cat", basic.formula), formula.append)),
				 							data = tmp.cohort_prepped,
				 							ties = 'efron')
				 			print(summary(tmp.coxph)$coefficients[,c(2,4,5)])
				 			assign(paste0(
				 				gsub(" ", "_", outcome),
				 				"_sol5.coxph"
				 			),
				 			tmp.coxph, inherits = T)
				 		}

				 		lapply(c(paste0(gsub(" ", "_", outcome), "_sol500.coxph"),
				 						 paste0(gsub(" ", "_", outcome), "_sol10.coxph"),
				 						 paste0(gsub(" ", "_", outcome), "_sol5.coxph")),
				 					 function(tmp.coxph) {
				 					 	saveRDS(get(tmp.coxph, envir = .GlobalEnv),
				 					 					file = to_drive_D(here::here(
				 					 						paste0("resources",
				 					 									 ifelse(outcome.type == 'incidence',
				 					 									 			 '/incidence',
				 					 									 			 paste0(paste0('/lag ', exposure.lag))),
				 					 									 ifelse(length(probs) > 4, '/verbose', '')),
				 					 						paste0(tmp.coxph, '.rds')))
				 					 	)
				 					 	message(paste0("Saved ", tmp.coxph))
				 					 })
				 	}
				 }
	)}

outcome <- grep(
	'esoph|stomach|colon|^rectal|canc_rectal|^bladder|pancrea|^liver|Laryn|Lung|skin|brain|prosta|leuk|breast',
	names(cohort_analytic),
	ignore.case = T,
	value = T)

if (outcome.type == 'mortality') {
	outcome.selected <- c(
		"Laryngeal cancer",	"Lung cancer",
		"Esophageal cancer", "Stomach cancer",
		"Colon cancer",
		"Rectal cancer", "Bladder cancer",
		"Liver cancer",
		"Pancreatic cancer", "Skin cancer",
		"Prostate cancer", "Brain and nervous system cancers",
		"Leukemia", "Breast cancer"
	)
} else {
	outcome <- grep('canc_', outcome, value = T)
}

# # Run run! ####
# get.mod(
# 	cohort_py = cohort_analytic,
# 	outcome = outcome.selected,
# 	probs = probs,
# 	outcome_type = outcome.type,
# 	run_coxph = T
# )

# Run messy! ####
# get.mod(
# 	cohort_py = cohort_analytic,
# 	outcome = outcome.selected,
# 	probs = probs,
# 	outcome_type = outcome.type,
# 	run_coxph = F,
# 	run_messy = 'Soluble',
# 	rm_stepwise = T
# )
