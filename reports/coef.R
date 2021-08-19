# Coefficient table ####
# Novmber 6, 2019
# Kevin Chen

# Column widths in inches should be
# 1.5   0.94   1.2   0.42   0.42   1   0.22

# Make table file ####
get.ctab <- function(
	outcome_selected = outcome.selected,
	coxph.dir  = paste0('resources/mortality/lag ', exposure.lag),
	output.dir = paste0('reports/resources/lag ', exposure.lag),
	save_coxph.tab = T,
	mwf.vec = NULL,
	messy_ref = NULL,
	clean_referent = F,
	additional_term = F
) {

	get.coxph.tab <- function(mwf = "") {
			rbindlist(
				lapply(outcome_selected, function(outcome_i = outcome_selected[1]) {
					tmp.coxph <- readRDS(to_drive_D(here::here(
						paste0(coxph.dir, mwf),
						paste0(
							gsub(" ", "_", outcome_i), messy_ref, ".coxph",'.rds'))))

					if (clean_referent) {
						get.mod(
							cohort_py = as.data.table(
								as.data.frame(cohort_analytic[(
								cum_straight == 0 &
									{if (is.null(messy_ref)) {
										cum_soluble == 0
										} else {
											if (messy_ref == "_sol500") {
												cum_soluble <= 5
											} else {
												if (messy_ref == "_sol10") {
													cum_soluble <= 0.1
												} else {
													if (messy_ref == "_sol5") {
														cum_soluble <= .05
														}
													}
												}}} &
									cum_synthetic == 0) |
									(get(paste0('cum_', tolower(mwf))) != 0)])),
							outcome = outcome_i,
							probs = probs,
							outcome_type = outcome.type,
							specific.race = NULL,
							run_coxph = F,
							stratify.baseline = T
						)
					}

					tmp.cohort_prepped <- get(paste0(
						gsub(" ", "_", outcome_i), ".cohort_prepped"
					))

					cbind(summary(tmp.coxph)$coefficients[, -c(1, 3, 4)],
								summary(tmp.coxph)$conf.int[, 3:4]) %>%
						as.data.frame() %>% cbind(
							name = substr(rownames(.), 1, unlist(
								gregexpr(
									'\\.cat`\\(|\\.cat\\(|Not |Female$|2$|3$|Hydra|Sagin',
									rownames(.)
								)
							) - 1),
							level = gsub(',', ", ", substring(rownames(.), unlist(
								gregexpr('t`\\(|t\\(|eNot|xF|t2|t3|tHydra|tSagin', rownames(.))
							) + 1)),
							n = NaN,
							.
						) %>% mutate(" " = ifelse(
							`Pr(>|z|)` < 0.05, "$*$",
							ifelse(`Pr(>|z|)` < 0.1, "$\\cdot$", NA))) %>% apply(
								2, function(x) {
									gsub("`", "", as.character(x))
								}) %>% as.data.frame -> coxph.tab

					coxph.tab[, 3:(ncol(coxph.tab) - 1)] <- apply(coxph.tab[, 3:(ncol(coxph.tab) - 1)], 2, as.numeric)

					coxph.tab <- coxph.tab[grep(
						ifelse(is.null(mwf.vec), "^strai|^sol|^synth", paste0('^', mwf)),
						coxph.tab$name, ignore.case = T),]

					coxph.tab$name <- factor(coxph.tab$name)

					coxph.tab$name <- with(coxph.tab, gsub("500|10|5", "", levels(name)[as.numeric(name)]))

					if (is.null(mwf.vec)) {
						coxph.tab$n <- as.vector(unlist(
							sapply(tmp.cohort_prepped[event == 1, .(
								Straight.cat,
								get(paste0('Soluble',
													 ifelse(
													 	!is.null(messy_ref),
													 	substring(messy_ref, 5), ''), '.cat')),
								Synthetic.cat)], function(x) {
									table(x)[-1]
								})
						))} else {
							coxph.tab$n <- as.vector(unlist(
								sapply(tmp.cohort_prepped[
									event == 1,
									paste0(mwf, ifelse(
										paste0(mwf) == "Soluble" & !is.null(messy_ref),
										substring(messy_ref, 5), ""), '.cat'),
																					with = F], function(x) {
																						table(x)[-1]
																					})
								))
						}

					trend.df <- data.frame(
						mwf = substr(
							names(coef(tmp.coxph)[grep(
								ifelse(is.null(mwf.vec), "^strai|^sol|^synth",
											 paste0('^', mwf)), names(coef(tmp.coxph)),
								ignore.case = T)]),
							1, unlist(gregexpr('.cat', names(coef(tmp.coxph)[grep(
								ifelse(is.null(mwf.vec), "^strai|^sol|^synth",
											 paste0('^', mwf)), names(coef(tmp.coxph)),
								ignore.case = T)]))) - 1),
						level = substring(
							names(coef(tmp.coxph)[grep(
								ifelse(is.null(mwf.vec), "^strai|^sol|^synth",
											 paste0('^', mwf)), names(coef(tmp.coxph)),
								ignore.case = T)]),
							unlist(gregexpr('.cat', names(coef(tmp.coxph)[grep(
								ifelse(is.null(mwf.vec), "^strai|^sol|^synth",
											 paste0('^', mwf)), names(coef(tmp.coxph)),
								ignore.case = T)]))) + 4),
						HR = c(exp(coef(tmp.coxph))[grep(
							ifelse(is.null(mwf.vec), "(^strai|^sol|^synth)",
											 paste0('^', mwf)), names(coef(tmp.coxph)),
							ignore.case = T)]
						)
					)

					trend.df$n <- coxph.tab$n

					trend.df$mwf <- with(trend.df, gsub("500|10|5", "", mwf))

					if (class(trend.df$level) == "factor") {
						trend.df$level <- levels(trend.df$level)[as.numeric(trend.df$level)]
					}

					trend.df <- rbind(trend.df,
														data.frame(
															mwf =	{if (is.null(mwf.vec)) {
																c("Straight", "Soluble", "Synthetic")
																} else {paste(mwf)}},
															level = {if (is.null(mwf.vec)) {
																sapply(tmp.cohort_prepped[event == 1, .(
																	Straight.cat,
																	get(paste0('Soluble', ifelse(
																		!is.null(messy_ref),
																		substring(messy_ref, 5), ""), '.cat')),
																	Synthetic.cat)],
																	function(x) {
																		names(table(x)[1])
																	})
																} else {
																		sapply(tmp.cohort_prepped[
																			event == 1, paste0(mwf, ifelse(
																				!is.null(messy_ref) & mwf == "Soluble",
																				substring(messy_ref, 5), ""), '.cat')
																		, with = F],
																		function(x) {
																			names(table(x)[1])
																		})
																	}},
															HR = 1,
															n = {if (is.null(mwf.vec)) {
																sapply(tmp.cohort_prepped[event == 1, .(
																	Straight.cat,
																	get(paste0('Soluble', ifelse(
																		!is.null(messy_ref),
																		substring(messy_ref, 5), ""), '.cat')),
																	Synthetic.cat)],
																	function(x) {
																		table(x)[1]
																	})} else {
																		sapply(tmp.cohort_prepped[event == 1,
																			paste0(mwf, ifelse(
																				!is.null(messy_ref) & mwf == "Soluble",
																				substring(messy_ref, 5), ""), '.cat')
																		, with = F],
																		function(x) {
																			table(x)[1]
																		})
																	}}
														)
														)

					trend.df$lower <- as.numeric(substr(trend.df$level, 2, unlist(gregexpr(',', trend.df$level)) - 1))
					trend.df$lower[trend.df$lower == -Inf] <- 0
					trend.df$upper <- as.numeric(substr(trend.df$level, unlist(gregexpr(',', trend.df$level)) + 1,
																							nchar(trend.df$level) - 1))
					trend.df$mid <- (trend.df$upper - trend.df$lower)/2
					trend.df[is.na(trend.df)] <- 0

					setDT(trend.df)

					if (is.null(mwf.vec) | mwf == "Straight") {
						trend.straight <- lm(
							HR ~ mid, data = trend.df[mwf == "Straight"], weights = n)
						if (is.null(summary(trend.straight)$fstatistic[1])) {
							p.straight <- NA} else {
							p.straight <- pf(summary(trend.straight)$fstatistic[1],
															 summary(trend.straight)$fstatistic[2],
															 summary(trend.straight)$fstatistic[3],
															 lower.tail = F)}
					}

					if (is.null(mwf.vec) | mwf == "Soluble") {
						trend.soluble <- lm(
							HR ~ mid, data = trend.df[mwf == "Soluble"], weights = n)
						if (is.null(summary(trend.soluble)$fstatistic[1])) {
							p.soluble <- NA} else {
							p.soluble <- pf(summary(trend.soluble)$fstatistic[1],
															summary(trend.soluble)$fstatistic[2],
															summary(trend.soluble)$fstatistic[3],
															lower.tail = F)}
					}

					if (is.null(mwf.vec) | mwf == "Synthetic") {
						trend.synthetic <- lm(
							HR ~ mid, data = trend.df[mwf == "Synthetic"], weights = n)
						if (is.null(summary(trend.synthetic)$fstatistic[1])) {
							p.synthetic <- NA} else {
							p.synthetic <- pf(summary(trend.synthetic)$fstatistic[1],
																summary(trend.synthetic)$fstatistic[2],
																summary(trend.synthetic)$fstatistic[3],
																lower.tail = F)}
					}

					trend.tab <- data.table()
					trend.tab[,	(names(coxph.tab)) := list(
						if (is.null(mwf.vec)) {
							c("Straight", "Soluble", "Synthetic")
						} else {paste(mwf)},
						"Trend",
						NA,
						NA,
						# p-value
						if (is.null(mwf.vec)) {
							c(p.straight, p.soluble, p.synthetic)
						} else {get(paste0('p.', tolower(mwf)))},
						NA, NA, as.character(NA)
					)]

					trend.tab[`Pr(>|z|)` < 0.1, " " := "$\\cdot$"]
					trend.tab[`Pr(>|z|)` < 0.05, " " := "$*$"]

					trend.tab <- trend.tab[!is.na(`Pr(>|z|)`)]

					coxph.tab <- rbindlist(
						list(coxph.tab, trend.tab))

					referent.tab <- data.table()
					referent.tab[,	(names(coxph.tab)) := list(
						# name
						if (is.null(mwf.vec)) {
							c("Straight", "Soluble", "Synthetic")
						} else {paste(mwf)},
						# level
						{if (is.null(mwf.vec)) {sapply(tmp.cohort_prepped[event == 1, .(
																Straight.cat,
																get(paste0('Soluble', ifelse(
																	!is.null(messy_ref),
																	substring(messy_ref, 5), ""), '.cat')),
																Synthetic.cat)],
																function(x) {
																	names(table(x)[1])
																})} else {
																	sapply(tmp.cohort_prepped[
																		event == 1, paste0(mwf, ifelse(
																	!is.null(messy_ref) & mwf == "Soluble",
																	substring(messy_ref, 5), ""), '.cat'),
																	with = F],
																	function(x) {
																		names(table(x)[1])
																	})
																}},
						# n
						{if (is.null(mwf.vec)) {sapply(tmp.cohort_prepped[event == 1, .(
							Straight.cat,
							get(paste0('Soluble', ifelse(
								!is.null(messy_ref),
								substring(messy_ref, 5), ""), '.cat')),
							Synthetic.cat)],
							function(x) {
								table(x)[1]
							})} else {
								sapply(
									tmp.cohort_prepped[
										event == 1,
										paste0(mwf, ifelse(
								!is.null(messy_ref) & mwf == "Soluble",
								substring(messy_ref, 5), ""), '.cat'), with = F],
											 function(x) {
											 	table(x)[1]
											 })
							}
						},
						NA,
						NA,
						NA,
						NA,
						NA
					)]

					referent.tab[, level := gsub(",", ", ", level)]

					coxph.tab <- rbindlist(
						list(coxph.tab, referent.tab))

					coxph.tab[!is.na(`lower .95`), `95\\% CI` := paste0(
						"(",
						formatC(`lower .95`, digits = 2, format = 'f'),
						", ",
						formatC(`upper .95`, digits = 2, format = 'f'),
						")"
					)]

					coxph.tab <- coxph.tab[,c(1:5, 9, 8)]

					if (class(coxph.tab$level) == "factor") {
						coxph.tab[, level := levels(level)[as.numeric(level)]]
					}

					coxph.tab[level != "Trend", `:=`(
						exposure.lower = as.numeric(substr(
							level, 2, unlist(regexpr(",", level)) - 1)),
						exposure.upper = as.numeric(ifelse(grepl("]", level), substr(
							level, unlist(regexpr(", ", level)) + 2,
							unlist(regexpr("]", level)) - 1), NA))
					)]

					coxph.tab[level == "Trend", exposure.lower := Inf]


					setorder(coxph.tab, name, exposure.lower)

					coxph.tab[level != "Trend", `:=`(
						level =
							if (.N > 2) {
								c(
									ifelse(exposure.upper[1] == 0, '$0$',
												 paste0("$0$ to $",
												 			 exposure.upper[1], "$"
												 )),
									paste0(paste0(
										"$>",
										{if (exposure.lower[2] == .05) {
											sapply(1:(.N - 2),
														 function(i) {
														 	ifelse(
														 		i == 1,
														 		exposure.lower[-c(1, .N)][i],
														 		round(exposure.lower[-c(1, .N)][i], 1))

														 })
										} else {
											round(exposure.lower[-c(1, .N)], 1)
										}}),
										"$ to $",
										round(
											exposure.upper[-c(1, .N)], 1), "$ "
									),
									paste0("$>", round(exposure.lower[.N], 1), "$")
								)} else {c(
									ifelse(exposure.upper[1] == 0, '$0$',
												 paste0("$0$ to $",
												 			 exposure.upper[1], "$ "
												 )), paste0("$>", round(exposure.lower[.N], 1), "$"))}
					), by = .(name)]

					# make column for mwf tpe
					coxph.tab[, mwf := name]
					# make name column pretty
					coxph.tab[duplicated(name), name := NA]

					# Add row for outcome names and counts
						outcome_names.tab <- data.table()

						outcome_names.tab[
							,	(c(names(coxph.tab))) := list(
								# Name
								paste0(
									'\\multicolumn{6}{l}{', outcome_i,
									" ($",
									prettyNum(nrow(tmp.cohort_prepped[event == 1]), '\\\\,'),
									"$ cases)}\\\\%"),
								# Level
								NA,
								# n
								NA,
								# exp(coef)
								NA,
								# Pr(>|z|)
								NA,
								# 95\\% CI
								NA,
								# asterisk
								NA, -Inf, NA, NA)]

					if (mwf == "") {
						outcome_names.tab <- rbindlist(
							list(outcome_names.tab,
									 outcome_names.tab,
									 outcome_names.tab))
						outcome_names.tab$mwf <- c("Straight", "Soluble", "Synthetic")
						} else {outcome_names.tab$mwf <- mwf}

					coxph.tab <- rbindlist(list(
						outcome_names.tab,
						coxph.tab),
						use.names = T)

					# Order rows
					setorder(coxph.tab, mwf, exposure.lower)

					coxph.tab <- coxph.tab[,-c('exposure.lower', 'exposure.upper')]

					# Add column for units
					coxph.tab <- cbind(coxph.tab[,1:2],
														 			 'mg/m$^3\\cdot$years',
														 coxph.tab[,3:ncol(coxph.tab)])
					coxph.tab[level == "Trend" | is.na(level), V2 := NA]

					# Column names
					names(coxph.tab) <- c(
						"name",
						"exposure",
						"   ",
						"Number of cases",
						"HR",
						# "$\\exp\\left(\\hat{\\beta}\\right)$",
						"$p$",
						# "$\\Prob{Z > |z|}$",
						"95\\% CI",
						"    ",
						'mwf'
					)

					coxph.tab$HR <- formatC(round(coxph.tab$HR, digits = 2), format = 'f', digits = 2)
					coxph.tab$HR[grep('NA', coxph.tab$HR)] <- NA
					coxph.tab$`$p$` <- formatC(round(coxph.tab$`$p$`, digits = 2), format = 'f', digits = 2)

					coxph.tab$`$p$` <- gsub("1.00", "$0.99$", coxph.tab$`$p$`)

					coxph.tab$`$p$`[grep('NA', coxph.tab$`$p$`)] <- NA

					coxph.tab[, `:=`(outcome = paste(outcome_i))]

					coxph.tab[name %in% c("Straight", "Soluble", "Synthetic"),
										name := NA]

					return(coxph.tab)

					rm(list = paste0(gsub(" ", "_", outcome_i), ".coxph"))
					if (additional_term | clean_referent) {
					rm(list = paste0(gsub(" ", "_", outcome_i), ".cohort_prepped"))
						}
				})
				, use.names = T)
	}

	if (is.null(mwf.vec)) {
		coxph.tab <- get.coxph.tab("") } else {
		coxph.tab <- rbindlist(
			lapply(mwf.vec, function(x) {
									get.coxph.tab(x)
			})
			, use.names = T)
		}

	if (save_coxph.tab) {
		saveRDS(coxph.tab, file = here::here(
			output.dir,	paste0(
				ifelse(!is.null(messy_ref),
							 paste0(substring(messy_ref, 2), "."), ""), 'coxph.tab', '.rds')))
	}

}


# Render ####
render.ctab <- function(
	mwf_which = mwf,
	tmp.tab = as.data.table(
		as.data.frame(get('coxph.tab', envir = .GlobalEnv))),
	tab.caption = NULL,
	include_caption = T,
	table_engine = table.engine,
	include_p = F
) {

	if (is.null(tab.caption)) {
		tab.caption <- paste0(
		"Cox model estimates of the hazard ratio for selected cancer outcomes associated with exposure to ", ifelse(table.engine == 'xtable', "\\textbf{", "**"), tolower(mwf_which), ifelse(table.engine == 'xtable', "}", "**") ," metalworking fluids, controlling for other fluid types, calendar year, calendar year of hire, age, race, sex, and plant, when possible."
	)
	}
	names(tmp.tab)[1:2] <- c(' ', '  ')
		# LaTeX math
	tmp.tab[,`:=`(
		`Number of cases` = as.character(
			ifelse(is.na(`Number of cases`),
						 NA,
						 paste0("$", `Number of cases`, "$"))),
		`HR` = as.character(
			ifelse(is.na(`HR`),
						 NA,
						 paste0("$", `HR`, "$"))),
		`$p$` = as.character(
			ifelse(is.na(`$p$`),
						 NA,
						 paste0("$", `$p$`, "$"))),
		`95\\% CI` = as.character(
			ifelse(is.na(`95\\% CI`),
						 NA,
						 gsub(", ", ",\\\\ ", paste0("$", `95\\% CI`, "$"))
			))
	)]

	if (!include_p) {
		tmp.tab <- tmp.tab[, -"$p$"]
	}

	if (table_engine == 'pander') {
		# tmp.tab[!is.na(`  `),
		# 				`  ` := paste0("&#9;", `  `)]
		tmp.tab[, `:=`(
			` ` = gsub("\\\\multicolumn\\{6\\}\\{l\\}\\{|\\}\\\\\\\\%", "", ` `),
			# pander/html style cdot
			`   ` = gsub('\\$\\^3\\\\cdot\\$years', '^3^&middot;years', `   `))]
	} else {
		# tmp.tab[!is.na(`  `),
		# 				`  ` := paste0("\\hspace{1cm}", `  `)]
	}


		if (table_engine == 'pander') {
			tmp.tab[mwf == mwf_which, -c('mwf', 'outcome')] %>% pander(
				missing = "",
				justify = as.vector(na.exclude(
					c("left", "left", "left", "center", "right", ifelse(include_p, "right", NA), "right","center"))),
				if (include_caption) {caption = tab.caption}
				) %>% cat
			} else {
					tmp.tab[mwf == mwf_which, -c('mwf', 'outcome')] %>% xtable(
						if (include_caption) {caption = tab.caption},
						digits = 3,
						label = paste0('tab:', gsub(" ", "_", mwf_which), '_signif.coxph'),
						align = paste0("clllcr",
													 ifelse(include_p, 'r', ''),
													 "rc")
					) %>% print.xtable(
						# include.colnames = F,
						hline.after = c(-1, nrow(.)),
						tabular.environment = 'longtable',
						floating = F,
						add.to.row = list(pos = list(0),
															command = c(
																paste(
																	'\\hline \n',
																	'\\endhead \n',
																	'\\hline \n',
																	'\\endfoot \n',
																	'\\endlastfoot \n'
																)
															)))
				}
}
