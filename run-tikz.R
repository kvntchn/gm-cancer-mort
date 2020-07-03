# Attempt at splining exposure ####
# August 30, 2019

library(here)
source(here::here('wrangling', '00-my-theme.R'))

if (sum(grepl('.cohort_prepped', ls())) == 0) {
	source(here::here('mortality modeling', 'modeling.R'))
	names(outcome) <- 1:length(outcome)
}

upper.percentile <- 0.99
# upper.percentile <- 1

# Esophageal cancer ####

# rm(list = ls()[-grep("cohort_analytic|cohort$|cohort_prepped|mytheme|upper.percentile|mwf.ggdf|cohort.ggdf", ls())])
#
# outcome <- "Esophageal cancer"
# # outcome <- "Leukemia"
# # outcome <- "Rectal cancer"
# # outcome <- "Skin cancer"
# # outcome <- "Stomach cancer"
#
# spline.which <- "Synthetic"
# # spline.which <- "Synthetic"
# # spline.which <- "Soluble"
# # spline.which <- "Soluble"
# # spline.which <- "Straight"
#
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df0.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df2.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df3.coxph.Rdata"))))
#
# termplot.which <- ifelse(spline.which == "Soluble", 5,
# 												 ifelse(spline.which == "Straight", 6, 7))
#
# df0.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph')),
# 	se = T, plot = F)
#
# df2.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df2.coxph')),
# 	se = T, plot = F)
#
# df3.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df3.coxph')),
# 	se = T, plot = F)
#
# # df4.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df4.coxph')),
# # 										 se = T, plot = F)
# #
# # df5.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df5.coxph')),
# # 										 se = T, plot = F)
#
# mwf_df0.spline <- data.frame(
# 	mwf = df0.termplot[[termplot.which]]$x,
# 	fit = df0.termplot[[termplot.which]]$y,
# 	se  = df0.termplot[[termplot.which]]$se,
# 	ref = with(df0.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = round(get(paste0(
# 		gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph'))$df[termplot.which], 1),
# 	cAIC = '(cAIC)'
# )
#
# mwf_df2.spline <- data.frame(
# 	mwf = df2.termplot[[termplot.which]]$x,
# 	fit = df2.termplot[[termplot.which]]$y,
# 	se  = df2.termplot[[termplot.which]]$se,
# 	ref = with(df2.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = '2',
# 	cAIC = ""
# )
#
# mwf_df3.spline <- data.frame(
# 	mwf = df3.termplot[[termplot.which]]$x,
# 	fit = df3.termplot[[termplot.which]]$y,
# 	se  = df3.termplot[[termplot.which]]$se,
# 	ref = with(df3.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = 3,
# 	cAIC = ""
# )
#
# mwf.spline <- rbindlist(list(
# 	mwf_df0.spline,
# 	mwf_df2.spline,
# 	mwf_df3.spline))
#
# cohort.ggdf <- melt(
# 	get(paste0(gsub(" ", "_", outcome), ".cohort_prepped")),
# 	id.vars = c('studyno', 'age.year2', 'event'),
# 	measure.vars = list(c("Soluble", "Straight", "Synthetic")),
# 	value.name = c('mwf'),
# 	variable.name = c('type')
# )
#
# cohort.ggdf[,
# 						`:=`(mwf.upper = quantile(mwf, upper.percentile)),
# 						by = .(type)]
#
# mwf.ggdf <- as.data.table(merge(mwf.spline,
# 																cohort.ggdf[, .(mwf.upper = unique(mwf.upper)), by = .(type)],
# 																on = .(type)))
#
# if (upper.percentile == 1) {
# 	hr.min <- 0
# 	if (outcome == "Esophageal cancer") {
# 		hr.max <- 6}
# 	if (outcome == "Leukemia") {
# 		hr.max <- 3.25}
# 	if (outcome == "Rectal cancer") {
# 		hr.max <- 1.75}
# 	if (outcome == "Skin cancer") {
# 		hr.max <- 3.25}
# 	if (outcome == "Stomach cancer") {
# 		hr.max <- 3.25}
# } else {
# 	if (outcome == "Esophageal cancer") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Leukemia") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Rectal cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 1.425}
# 	if (outcome == "Skin cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 2}
# 	if (outcome == "Stomach cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 3.65}
# }
#
# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'cohort.ggdf'), cohort.ggdf, inherits = T)
# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'mwf.ggdf'), mwf.ggdf, inherits = T)
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splinedCI_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 7, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref))) +
# 	geom_line() +
# 	geom_ribbon(aes(
# 		ymin = {
# 			lower <- exp(fit - ref - qnorm(0.975) * se)
# 			ifelse(lower <= hr.min, hr.min, lower)
# 			},
# 		ymax = {
# 			upper <- exp(fit - ref + qnorm(0.975) * se)
# 			ifelse(upper > hr.max, hr.max, upper)
# 		}),
# 		alpha = 0.1) +
# 			scale_y_continuous(limits = c(hr.min, hr.max)) +
# 		facet_wrap(. ~ spline.df,
# 						 labeller = labeller(
# 						 	spline.df = {
# 						 		df.label <- with(mwf.ggdf, sapply(1:3, function(i) {
# 						 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 						 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 						 		}))
# 						 		names(df.label) <- sapply(1:3, function(i) {
# 						 			levels(mwf.ggdf$spline.df)[i]
# 						 		})
# 						 		df.label
# 						 })) +
# 	geom_rug(data = cohort.ggdf[
# 		event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cumulative exposure to ", tolower(spline.which)," metal-working fluids (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splined_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 5, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 	geom_line() +
# 	scale_y_continuous(limits = c(hr.min, hr.max)) +
# 	geom_rug(data = cohort.ggdf[event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

# Leukemia ####
# Sys.sleep(5)
# rm(list = ls()[-grep("cohort_analytic|cohort$|cohort_prepped|mytheme|upper.percentile|mwf.ggdf|cohort.ggdf", ls())])
#
# # outcome <- "Esophageal cancer"
# outcome <- "Leukemia"
# # outcome <- "Rectal cancer"
# # outcome <- "Skin cancer"
# # outcome <- "Stomach cancer"
#
# # spline.which <- "Synthetic"
# spline.which <- "Synthetic"
# # spline.which <- "Soluble"
# # spline.which <- "Soluble"
# # spline.which <- "Straight"
#
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df0.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df2.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df3.coxph.Rdata"))))
#
# termplot.which <- ifelse(spline.which == "Soluble", 5,
# 												 ifelse(spline.which == "Straight", 6, 7))
#
# df0.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph')),
# 										 se = T, plot = F)
#
# df2.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df2.coxph')),
# 										 se = T, plot = F)
#
# df3.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df3.coxph')),
# 										 se = T, plot = F)
#
# # df4.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df4.coxph')),
# # 										 se = T, plot = F)
# #
# # df5.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df5.coxph')),
# # 										 se = T, plot = F)
#
# mwf_df0.spline <- data.frame(
# 	mwf = df0.termplot[[termplot.which]]$x,
# 	fit = df0.termplot[[termplot.which]]$y,
# 	se  = df0.termplot[[termplot.which]]$se,
# 	ref = with(df0.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = round(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph'))$df[termplot.which], 1),
# 	cAIC = '(cAIC)'
# 	)
#
# mwf_df2.spline <- data.frame(
# 	mwf = df2.termplot[[termplot.which]]$x,
# 	fit = df2.termplot[[termplot.which]]$y,
# 	se  = df2.termplot[[termplot.which]]$se,
# 	ref = with(df2.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = '2',
# 	cAIC = ""
# 	)
#
# mwf_df3.spline <- data.frame(
# 	mwf = df3.termplot[[termplot.which]]$x,
# 	fit = df3.termplot[[termplot.which]]$y,
# 	se  = df3.termplot[[termplot.which]]$se,
# 	ref = with(df3.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = 3,
# 	cAIC = ""
# 	)
#
# mwf.spline <- rbindlist(list(
# 	mwf_df0.spline,
# 	mwf_df2.spline,
# 	mwf_df3.spline))
#
#
#
# cohort.ggdf <- melt(
# 			get(paste0(gsub(" ", "_", outcome), ".cohort_prepped")),
# 			id.vars = c('studyno', 'age.year2', 'event'),
# 			measure.vars = list(c("Soluble", "Straight", "Synthetic")),
# 			value.name = c('mwf'),
# 			variable.name = c('type')
# 		)
#
# cohort.ggdf[,
# 	`:=`(mwf.upper = quantile(mwf, upper.percentile)),
# 	by = .(type)]
#
# mwf.ggdf <- as.data.table(merge(mwf.spline,
# 									cohort.ggdf[, .(mwf.upper = unique(mwf.upper)), by = .(type)],
# 									on = .(type)))
#
# if (upper.percentile == 1) {
# 	hr.min <- 0
# 	if (outcome == "Esophageal cancer") {
# 			hr.max <- 6}
# 	if (outcome == "Leukemia") {
# 			hr.max <- 3.25}
# 	if (outcome == "Rectal cancer") {
# 			hr.max <- 1.75}
# 	if (outcome == "Skin cancer") {
# 		hr.max <- 3.25}
# 	if (outcome == "Stomach cancer") {
# 		hr.max <- 3.25}
# 	} else {
# 	if (outcome == "Esophageal cancer") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Leukemia") {
# 		hr.min <- 0.875
# 			hr.max <- 1.425}
# 	if (outcome == "Rectal cancer") {
# 		hr.min <- 0.75
# 			hr.max <- 1.425}
# 	if (outcome == "Skin cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 2}
# 	if (outcome == "Stomach cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 3.65}
# 		}
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splinedCI_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 7, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref))) +
# 	geom_line() +
# 	geom_ribbon(aes(
# 		ymin = {
# 			lower <- exp(fit - ref - qnorm(0.975) * se)
# 			ifelse(lower <= hr.min, hr.min, lower)
# 			},
# 		ymax = {
# 			upper <- exp(fit - ref + qnorm(0.975) * se)
# 			ifelse(upper > hr.max, hr.max, upper)
# 		}),
# 		alpha = 0.1) +
# 			scale_y_continuous(limits = c(hr.min, hr.max)) +
# 		facet_wrap(. ~ spline.df,
# 						 labeller = labeller(
# 						 	spline.df = {
# 						 		df.label <- with(mwf.ggdf, sapply(1:3, function(i) {
# 						 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 						 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 						 		}))
# 						 		names(df.label) <- sapply(1:3, function(i) {
# 						 			levels(mwf.ggdf$spline.df)[i]
# 						 		})
# 						 		df.label
# 						 })) +
# 	geom_rug(data = cohort.ggdf[
# 		event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cumulative exposure to ", tolower(spline.which)," metal-working fluids (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splined_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 5, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 	geom_line() +
# 	scale_y_continuous(limits = c(hr.min, hr.max)) +
# 	geom_rug(data = cohort.ggdf[event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

# Rectal Cancer ####
# Sys.sleep(5)
# rm(list = ls()[-grep("cohort_analytic|cohort$|cohort_prepped|mytheme|upper.percentile|mwf.ggdf|cohort.ggdf", ls())])
#
# # outcome <- "Esophageal cancer"
# # outcome <- "Leukemia"
# outcome <- "Rectal cancer"
# # outcome <- "Skin cancer"
# # outcome <- "Stomach cancer"
#
# # spline.which <- "Synthetic"
# # spline.which <- "Synthetic"
# spline.which <- "Soluble"
# # spline.which <- "Soluble"
# # spline.which <- "Straight"
#
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df0.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df2.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df3.coxph.Rdata"))))
#
# termplot.which <- ifelse(spline.which == "Soluble", 5,
# 												 ifelse(spline.which == "Straight", 6, 7))
#
# df0.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph')),
# 	se = T, plot = F)
#
# df2.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df2.coxph')),
# 	se = T, plot = F)
#
# df3.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df3.coxph')),
# 	se = T, plot = F)
#
# # df4.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df4.coxph')),
# # 										 se = T, plot = F)
# #
# # df5.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df5.coxph')),
# # 										 se = T, plot = F)
#
# mwf_df0.spline <- data.frame(
# 	mwf = df0.termplot[[termplot.which]]$x,
# 	fit = df0.termplot[[termplot.which]]$y,
# 	se  = df0.termplot[[termplot.which]]$se,
# 	ref = with(df0.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = round(get(paste0(
# 		gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph'))$df[termplot.which], 1),
# 	cAIC = '(cAIC)'
# )
#
# mwf_df2.spline <- data.frame(
# 	mwf = df2.termplot[[termplot.which]]$x,
# 	fit = df2.termplot[[termplot.which]]$y,
# 	se  = df2.termplot[[termplot.which]]$se,
# 	ref = with(df2.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = '2',
# 	cAIC = ""
# )
#
# mwf_df3.spline <- data.frame(
# 	mwf = df3.termplot[[termplot.which]]$x,
# 	fit = df3.termplot[[termplot.which]]$y,
# 	se  = df3.termplot[[termplot.which]]$se,
# 	ref = with(df3.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = 3,
# 	cAIC = ""
# )
#
# mwf.spline <- rbindlist(list(
# 	mwf_df0.spline,
# 	mwf_df2.spline,
# 	mwf_df3.spline))
#
#
#
# cohort.ggdf <- melt(
# 	get(paste0(gsub(" ", "_", outcome), ".cohort_prepped")),
# 	id.vars = c('studyno', 'age.year2', 'event'),
# 	measure.vars = list(c("Soluble", "Straight", "Synthetic")),
# 	value.name = c('mwf'),
# 	variable.name = c('type')
# )
#
# cohort.ggdf[,
# 						`:=`(mwf.upper = quantile(mwf, upper.percentile)),
# 						by = .(type)]
#
# mwf.ggdf <- as.data.table(merge(mwf.spline,
# 																cohort.ggdf[, .(mwf.upper = unique(mwf.upper)), by = .(type)],
# 																on = .(type)))
#
# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'cohort.ggdf'), cohort.ggdf, inherits = T)
# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'mwf.ggdf'), mwf.ggdf, inherits = T)
#
# if (upper.percentile == 1) {
# 	hr.min <- 0
# 	if (outcome == "Esophageal cancer") {
# 		hr.max <- 6}
# 	if (outcome == "Leukemia") {
# 		hr.max <- 3.25}
# 	if (outcome == "Rectal cancer") {
# 		hr.max <- 1.75}
# 	if (outcome == "Skin cancer") {
# 		hr.max <- 3.25}
# 	if (outcome == "Stomach cancer") {
# 		hr.max <- 3.25}
# } else {
# 	if (outcome == "Esophageal cancer") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Leukemia") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Rectal cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 1.425}
# 	if (outcome == "Skin cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 2}
# 	if (outcome == "Stomach cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 3.65}
# }
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splinedCI_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 7, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref))) +
# 	geom_line() +
# 	geom_ribbon(aes(
# 		ymin = {
# 			lower <- exp(fit - ref - qnorm(0.975) * se)
# 			ifelse(lower <= hr.min, hr.min, lower)
# 			},
# 		ymax = {
# 			upper <- exp(fit - ref + qnorm(0.975) * se)
# 			ifelse(upper > hr.max, hr.max, upper)
# 		}),
# 		alpha = 0.1) +
# 			scale_y_continuous(limits = c(hr.min, hr.max)) +
# 		facet_wrap(. ~ spline.df,
# 						 labeller = labeller(
# 						 	spline.df = {
# 						 		df.label <- with(mwf.ggdf, sapply(1:3, function(i) {
# 						 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 						 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 						 		}))
# 						 		names(df.label) <- sapply(1:3, function(i) {
# 						 			levels(mwf.ggdf$spline.df)[i]
# 						 		})
# 						 		df.label
# 						 })) +
# 	geom_rug(data = cohort.ggdf[
# 		event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cumulative exposure to ", tolower(spline.which)," metal-working fluids (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splined_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 5, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 	geom_line() +
# 	scale_y_continuous(limits = c(hr.min, hr.max)) +
# 	geom_rug(data = cohort.ggdf[event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

# Skin cancer ####
# Sys.sleep(5)
# rm(list = ls()[-grep("cohort_analytic|cohort$|cohort_prepped|mytheme|upper.percentile|mwf.ggdf|cohort.ggdf", ls())])
#
# # outcome <- "Esophageal cancer"
# # outcome <- "Leukemia"
# # outcome <- "Rectal cancer"
# outcome <- "Skin cancer"
# # outcome <- "Stomach cancer"
#
# # spline.which <- "Synthetic"
# # spline.which <- "Synthetic"
# # spline.which <- "Soluble"
# spline.which <- "Soluble"
# # spline.which <- "Straight"
#
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df0.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df2.coxph.Rdata"))))
# load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df3.coxph.Rdata"))))
#
# termplot.which <- ifelse(spline.which == "Soluble", 5,
# 												 ifelse(spline.which == "Straight", 6, 7))
#
# df0.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph')),
# 										 se = T, plot = F)
#
# df2.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df2.coxph')),
# 										 se = T, plot = F)
#
# df3.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df3.coxph')),
# 										 se = T, plot = F)
#
# # df4.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df4.coxph')),
# # 										 se = T, plot = F)
# #
# # df5.termplot <- termplot(get(paste0(
# # 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df5.coxph')),
# # 										 se = T, plot = F)
#
# mwf_df0.spline <- data.frame(
# 	mwf = df0.termplot[[termplot.which]]$x,
# 	fit = df0.termplot[[termplot.which]]$y,
# 	se  = df0.termplot[[termplot.which]]$se,
# 	ref = with(df0.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = round(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph'))$df[termplot.which], 1),
# 	cAIC = '(cAIC)'
# 	)
#
# mwf_df2.spline <- data.frame(
# 	mwf = df2.termplot[[termplot.which]]$x,
# 	fit = df2.termplot[[termplot.which]]$y,
# 	se  = df2.termplot[[termplot.which]]$se,
# 	ref = with(df2.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = '2',
# 	cAIC = ""
# 	)
#
# mwf_df3.spline <- data.frame(
# 	mwf = df3.termplot[[termplot.which]]$x,
# 	fit = df3.termplot[[termplot.which]]$y,
# 	se  = df3.termplot[[termplot.which]]$se,
# 	ref = with(df3.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = 3,
# 	cAIC = ""
# 	)
#
# mwf.spline <- rbindlist(list(
# 	mwf_df0.spline,
# 	mwf_df2.spline,
# 	mwf_df3.spline))
#
#
#
# cohort.ggdf <- melt(
# 			get(paste0(gsub(" ", "_", outcome), ".cohort_prepped")),
# 			id.vars = c('studyno', 'age.year2', 'event'),
# 			measure.vars = list(c("Soluble", "Straight", "Synthetic")),
# 			value.name = c('mwf'),
# 			variable.name = c('type')
# 		)
#
# cohort.ggdf[,
# 	`:=`(mwf.upper = quantile(mwf, upper.percentile)),
# 	by = .(type)]
#
# mwf.ggdf <- as.data.table(merge(mwf.spline,
# 									cohort.ggdf[, .(mwf.upper = unique(mwf.upper)), by = .(type)],
# 									on = .(type)))
#
# if (upper.percentile == 1) {
# 	hr.min <- 0
# 	if (outcome == "Esophageal cancer") {
# 			hr.max <- 6}
# 	if (outcome == "Leukemia") {
# 			hr.max <- 3.25}
# 	if (outcome == "Rectal cancer") {
# 			hr.max <- 1.75}
# 	if (outcome == "Skin cancer") {
# 		hr.max <- 3.25}
# 	if (outcome == "Stomach cancer") {
# 		hr.max <- 3.25}
# 	} else {
# 	if (outcome == "Esophageal cancer") {
# 		hr.min <- 0.875
# 		hr.max <- 1.425}
# 	if (outcome == "Leukemia") {
# 		hr.min <- 0.875
# 			hr.max <- 1.425}
# 	if (outcome == "Rectal cancer") {
# 		hr.min <- 0.75
# 			hr.max <- 1.425}
# 	if (outcome == "Skin cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 2}
# if (outcome == "Stomach cancer") {
# 		hr.min <- 0.75
# 		hr.max <- 3.65}
# 	}
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splinedCI_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 7, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref))) +
# 	geom_line() +
# 	geom_ribbon(aes(
# 		ymin = {
# 			lower <- exp(fit - ref - qnorm(0.975) * se)
# 			ifelse(lower <= hr.min, hr.min, lower)
# 			},
# 		ymax = {
# 			upper <- exp(fit - ref + qnorm(0.975) * se)
# 			ifelse(upper > hr.max, hr.max, upper)
# 		}),
# 		alpha = 0.1) +
# 			scale_y_continuous(limits = c(hr.min, hr.max)) +
# 		facet_wrap(. ~ spline.df,
# 						 labeller = labeller(
# 						 	spline.df = {
# 						 		df.label <- with(mwf.ggdf, sapply(1:3, function(i) {
# 						 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 						 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 						 		}))
# 						 		names(df.label) <- sapply(1:3, function(i) {
# 						 			levels(mwf.ggdf$spline.df)[i]
# 						 		})
# 						 		df.label
# 						 })) +
# 	geom_rug(data = cohort.ggdf[
# 		event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cumulative exposure to ", tolower(spline.which)," metal-working fluids (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splined_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 5, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 	geom_line() +
# 	scale_y_continuous(limits = c(hr.min, hr.max)) +
# 	geom_rug(data = cohort.ggdf[event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

# Stomach cancer ####
Sys.sleep(5)
rm(list = ls()[-grep("cohort_analytic|cohort$|cohort_prepped|mytheme|upper.percentile|mwf.ggdf|cohort.ggdf", ls())])

# outcome <- "Esophageal cancer"
# outcome <- "Leukemia"
# outcome <- "Rectal cancer"
# outcome <- "Skin cancer"
outcome <- "Stomach cancer"

# spline.which <- "Synthetic"
# spline.which <- "Synthetic"
# spline.which <- "Soluble"
# spline.which <- "Soluble"
spline.which <- "Straight"

load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df0.coxph.Rdata"))))
load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df2.coxph.Rdata"))))
load(to_drive_D(here::here('mortality modeling', paste0("resources/", gsub(" ", "_", outcome), "_splined_", tolower(spline.which), "_df3.coxph.Rdata"))))

termplot.which <- ifelse(spline.which == "Soluble", 5,
												 ifelse(spline.which == "Straight", 6, 7))

df0.termplot <- termplot(get(paste0(
	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph')),
	se = T, plot = F)

# df2.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df2.coxph')),
# 	se = T, plot = F)
#
# df3.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df3.coxph')),
# 	se = T, plot = F)
#
# df4.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df4.coxph')),
# 										 se = T, plot = F)
#
# df5.termplot <- termplot(get(paste0(
# 	gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df5.coxph')),
# 										 se = T, plot = F)

mwf_df0.spline <- data.frame(
	mwf = df0.termplot[[termplot.which]]$x,
	fit = df0.termplot[[termplot.which]]$y,
	se  = df0.termplot[[termplot.which]]$se,
	ref = with(df0.termplot[[termplot.which]], y[x == 0]),
	type = spline.which,
	spline.df = round(get(paste0(
		gsub(" ", "_", outcome), '_splined_', tolower(spline.which), '_df0.coxph'))$df[termplot.which], 1),
	cAIC = '(cAIC)'
)

# mwf_df2.spline <- data.frame(
# 	mwf = df2.termplot[[termplot.which]]$x,
# 	fit = df2.termplot[[termplot.which]]$y,
# 	se  = df2.termplot[[termplot.which]]$se,
# 	ref = with(df2.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = '2',
# 	cAIC = ""
# )
#
# mwf_df3.spline <- data.frame(
# 	mwf = df3.termplot[[termplot.which]]$x,
# 	fit = df3.termplot[[termplot.which]]$y,
# 	se  = df3.termplot[[termplot.which]]$se,
# 	ref = with(df3.termplot[[termplot.which]], y[x == 0]),
# 	type = spline.which,
# 	spline.df = 3,
# 	cAIC = ""
# )

mwf.spline <- #rbindlist(list(
	mwf_df0.spline#,
	# mwf_df2.spline,
	# mwf_df3.spline))



cohort.ggdf <- melt(
	get(paste0(gsub(" ", "_", outcome), ".cohort_prepped")),
	id.vars = c('studyno', 'age.year2', 'event'),
	measure.vars = list(c("Soluble", "Straight", "Synthetic")),
	value.name = c('mwf'),
	variable.name = c('type')
)

cohort.ggdf[,
						`:=`(mwf.upper = quantile(mwf, upper.percentile)),
						by = .(type)]

mwf.ggdf <- as.data.table(merge(mwf.spline,
																cohort.ggdf[, .(mwf.upper = unique(mwf.upper)), by = .(type)],
																on = .(type)))

if (upper.percentile == 1) {
	hr.min <- 0
	if (outcome == "Esophageal cancer") {
		hr.max <- 6}
	if (outcome == "Leukemia") {
		hr.max <- 3.25}
	if (outcome == "Rectal cancer") {
		hr.max <- 1.75}
	if (outcome == "Skin cancer") {
		hr.max <- 3.25}
	if (outcome == "Stomach cancer") {
		hr.max <- 3.25}
} else {
	if (outcome == "Esophageal cancer") {
		hr.min <- 0.875
		hr.max <- 1.425}
	if (outcome == "Leukemia") {
		hr.min <- 0.875
		hr.max <- 1.425}
	if (outcome == "Rectal cancer") {
		hr.min <- 0.75
		hr.max <- 1.425}
	if (outcome == "Skin cancer") {
		hr.min <- 0.75
		hr.max <- 2}
	if (outcome == "Stomach cancer") {
		hr.min <- 0.75
		hr.max <- 3.65}
}

# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'cohort.ggdf'), cohort.ggdf, inherits = T)
# assign(paste0(gsub(" ", "_", outcome), "_", spline.which, "_", 'mwf.ggdf'), mwf.ggdf, inherits = T)

# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splinedCI_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 7, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref))) +
# 	geom_line() +
# 	geom_ribbon(aes(
# 		ymin = {
# 			lower <- exp(fit - ref - qnorm(0.975) * se)
# 			ifelse(lower <= hr.min, hr.min, lower)
# 			},
# 		ymax = {
# 			upper <- exp(fit - ref + qnorm(0.975) * se)
# 			ifelse(upper > hr.max, hr.max, upper)
# 		}),
# 		alpha = 0.1) +
# 			scale_y_continuous(limits = c(hr.min, hr.max)) +
# 		facet_wrap(. ~ spline.df,
# 						 labeller = labeller(
# 						 	spline.df = {
# 						 		df.label <- with(mwf.ggdf, sapply(1:3, function(i) {
# 						 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 						 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 						 		}))
# 						 		names(df.label) <- sapply(1:3, function(i) {
# 						 			levels(mwf.ggdf$spline.df)[i]
# 						 		})
# 						 		df.label
# 						 })) +
# 	geom_rug(data = cohort.ggdf[
# 		event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cumulative exposure to ", tolower(spline.which)," metal-working fluids (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
					paste0(gsub(" ", "_", outcome), "_splined_cAIC_", tolower(spline.which), ".tex")),
		 standAlone = T, width = 3.5, height = 3)
ggplot(mwf.ggdf[mwf <= mwf.upper &
									spline.df == unique(spline.df)[1],],
	aes(x = mwf, y = exp(fit - ref))) +
	geom_line() +
	geom_ribbon(aes(
		ymin = {
			lower <- exp(fit - ref - qnorm(0.975) * se)
			ifelse(lower <= hr.min, hr.min, lower)
			},
		ymax = {
			upper <- exp(fit - ref + qnorm(0.975) * se)
			ifelse(upper > hr.max, hr.max, upper)
		}),
		alpha = 0.1) +
			scale_y_continuous(limits = c(hr.min, hr.max)) +
		facet_wrap(. ~ spline.df,
						 labeller = labeller(
						 	spline.df = {
						 		df.label <- with(mwf.ggdf, sapply(1, function(i) {
						 			paste0("$df = ", unique(spline.df)[i], "$ ",
						 						 unique(cAIC[spline.df == unique(spline.df)[i]]))
						 		}))
						 		names(df.label) <- sapply(1, function(i) {
						 			unique(mwf.ggdf$spline.df)[i]
						 		})
						 		df.label
						 })) +
	geom_rug(data = cohort.ggdf[
		event == 1 & mwf <= mwf.upper & type == spline.which,],
		aes(x = mwf, y = 1, linetype = NULL)
	) +
	stat_function(
		fun = function(x) {
			1
		},
		linetype = "3313"
	) +
	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
			 y = "Hazard ratio",
			 linetype = "$df$") +
	mytheme
dev.off()
#
# tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', 'reports/mortality modeling/splined resources'),
# 					paste0(gsub(" ", "_", outcome), "_splined_", tolower(spline.which), ".tex")),
# 		 standAlone = T, width = 5, height = 3)
# ggplot(mwf.ggdf[mwf <= mwf.upper,],
# 	aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 	geom_line() +
# 	scale_y_continuous(limits = c(hr.min, hr.max)) +
# 	geom_rug(data = cohort.ggdf[event == 1 & mwf <= mwf.upper & type == spline.which,],
# 		aes(x = mwf, y = 1, linetype = NULL)
# 	) +
# 	stat_function(
# 		fun = function(x) {
# 			1
# 		},
# 		linetype = "3313"
# 	) +
# 	labs(x = paste0("Cum. exposure to ", tolower(spline.which)," MWFs (mg/m$^3\\cdot$yr)"),
# 			 y = "Hazard ratio",
# 			 linetype = "$df$") +
# 	mytheme
# dev.off()

# Digestive cancers -- Combined plot ####

# Esophageal_cancer_Synthetic_mwf.ggdf[, `:=`(
# 	outcome = "Esophageal cancer",
# 	hr.min = 0.875,
# 	hr.max = 1.425)]
# Stomach_cancer_Straight_mwf.ggdf[, `:=`(
# 	outcome = "Stomach cancer",
# 	hr.min = 0.75,
# 	hr.max = 3.65)]
# Rectal_cancer_Soluble_mwf.ggdf[, `:=`(
# 	outcome = "Rectal cancer",
# 	hr.min = 0.75,
# 	hr.max = 1.425)]
#
# Esophageal_cancer_Synthetic_mwf.ggdf <- Esophageal_cancer_Synthetic_mwf.ggdf[
# 	type == "Synthetic"]
# Stomach_cancer_Straight_mwf.ggdf <- Stomach_cancer_Straight_mwf.ggdf[
# 	type == "Straight"]
# Rectal_cancer_Soluble_mwf.ggdf <- Rectal_cancer_Soluble_mwf.ggdf[
# 	type == "Soluble"]
#
# Esophageal_cancer_Synthetic_cohort.ggdf <- Esophageal_cancer_Synthetic_cohort.ggdf[
# 	type == "Synthetic"]
# Stomach_cancer_Straight_cohort.ggdf <- Stomach_cancer_Straight_cohort.ggdf[
# 	type == "Straight"]
# Rectal_cancer_Soluble_cohort.ggdf <- Rectal_cancer_Soluble_cohort.ggdf[
# 	type == "Soluble"]
#
# 	# No confidence bands ####
# 	tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', "reports/mortality modeling/splined resources/combined_splined.tex")),
# 			 standAlone = T, width = 5, height = 9)
# 	cowplot::plot_grid(
# 		# Esophageal cancer syntheteic
# 		ggplot(Esophageal_cancer_Synthetic_mwf.ggdf[mwf <= mwf.upper,],
# 					 aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 			geom_line() +
# 			geom_rug(data = Esophageal_cancer_Synthetic_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ type, scales = 'free_x') +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 				"Lagged cum. exposure to synthetic MWF (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		# Stomach cancer straight
# 		ggplot(Stomach_cancer_Straight_mwf.ggdf[mwf <= mwf.upper,],
# 					 aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 			geom_line() +
# 			geom_rug(data = Stomach_cancer_Straight_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ type, scales = 'free_x') +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 				"Lagged cum. exposure to straight MWF (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		# Rectal cancer soluble
# 		ggplot(Rectal_cancer_Soluble_mwf.ggdf[mwf <= mwf.upper,],
# 					 aes(x = mwf, y = exp(fit - ref), linetype = paste(spline.df, cAIC))) +
# 			geom_line() +
# 			geom_rug(data = Rectal_cancer_Soluble_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ type, scales = 'free_x') +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 				"Lagged cum. exposure to soluble MWF (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		ncol = 1
# 	)
# 	dev.off()
#
# 	Esophageal_cancer_Synthetic_mwf.ggdf[, `:=`(
# 		hr.min = 0,
# 		hr.max = 5)]
# 	Stomach_cancer_Straight_mwf.ggdf[, `:=`(
# 		hr.min = 0,
# 		hr.max = 3.25)]
# 	Rectal_cancer_Soluble_mwf.ggdf[, `:=`(
# 		hr.min = 0.75,
# 		hr.max = 1.4)]
#
# 	# With confidence bands ####
# 	tikz(here::here(ifelse(upper.percentile == 1, 'reports/mortality modeling/splined resources/full window', "reports/mortality modeling/splined resources/combined_splinedCI.tex")),
# 			 standAlone = T, width = 7, height = 9)
# 	cowplot::plot_grid(
# 		# Esophageal cancer synthetic
# 		ggplot(Esophageal_cancer_Synthetic_mwf.ggdf[mwf <= mwf.upper,],
# 					 aes(x = mwf, y = exp(fit - ref))) +
# 			geom_line() +
# 			geom_ribbon(aes(
# 				ymin = {
# 					lower <- exp(fit - ref - qnorm(0.975) * se)
# 					ifelse(lower <= hr.min, hr.min, lower)
# 				},
# 				ymax = {
# 					upper <- exp(fit - ref + qnorm(0.975) * se)
# 					ifelse(upper > hr.max, hr.max, upper)
# 				}),
# 				alpha = 0.1) +
# 			geom_rug(data = Esophageal_cancer_Synthetic_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ spline.df,
# 								 labeller = labeller(
# 								 	spline.df = {
# 								 		df.label <- with(Esophageal_cancer_Synthetic_mwf.ggdf[mwf <= mwf.upper], sapply(1:3, function(i) {
# 								 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 								 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 								 		}))
# 								 		names(df.label) <- sapply(1:3, function(i) {
# 								 			levels(Esophageal_cancer_Synthetic_mwf.ggdf[mwf <= mwf.upper]$spline.df)[i]
# 								 		})
# 								 		df.label
# 								 	})) +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 				"Lagged cumulative exposure to synthetic metalworking fluids (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		# Stomach cancer straight
# 		ggplot(Stomach_cancer_Straight_mwf.ggdf[mwf <= mwf.upper],
# 					 aes(x = mwf, y = exp(fit - ref))) +
# 			geom_line() +
# 			geom_ribbon(aes(
# 				ymin = {
# 					lower <- exp(fit - ref - qnorm(0.975) * se)
# 					ifelse(lower <= hr.min, hr.min, lower)
# 				},
# 				ymax = {
# 					upper <- exp(fit - ref + qnorm(0.975) * se)
# 					ifelse(upper > hr.max, hr.max, upper)
# 				}),
# 				alpha = 0.1) +
# 			geom_rug(data = Stomach_cancer_Straight_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ spline.df,
# 								 labeller = labeller(
# 								 	spline.df = {
# 								 		df.label <- with(Stomach_cancer_Straight_mwf.ggdf[mwf <= mwf.upper], sapply(1:3, function(i) {
# 								 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 								 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 								 		}))
# 								 		names(df.label) <- sapply(1:3, function(i) {
# 								 			levels(Stomach_cancer_Straight_mwf.ggdf[mwf <= mwf.upper]$spline.df)[i]
# 								 		})
# 								 		df.label
# 								 	})) +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 				"Lagged cumulative exposure to straight metalworking fluids (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		# Rectal cancer synthetic
# 		ggplot(Rectal_cancer_Soluble_mwf.ggdf[mwf <= mwf.upper,],
# 					 aes(x = mwf, y = exp(fit - ref))) +
# 			geom_line() +
# 			geom_ribbon(aes(
# 				ymin = {
# 					lower <- exp(fit - ref - qnorm(0.975) * se)
# 					ifelse(lower <= hr.min, hr.min, lower)
# 				},
# 				ymax = {
# 					upper <- exp(fit - ref + qnorm(0.975) * se)
# 					ifelse(upper > hr.max, hr.max, upper)
# 				}),
# 				alpha = 0.1) +
# 			geom_rug(data = Rectal_cancer_Soluble_cohort.ggdf[
# 				event == 1 & mwf <= mwf.upper],
# 				aes(x = mwf, y = NULL, linetype = NULL)
# 			) +
# 			facet_grid(outcome ~ spline.df,
# 								 labeller = labeller(
# 								 	spline.df = {
# 								 		df.label <- with(Rectal_cancer_Soluble_mwf.ggdf[mwf <= mwf.upper], sapply(1:3, function(i) {
# 								 			paste0("$df = ", levels(spline.df)[i], "$ ",
# 								 						 unique(cAIC[spline.df == levels(spline.df)[i]]))
# 								 		}))
# 								 		names(df.label) <- sapply(1:3, function(i) {
# 								 			levels(Rectal_cancer_Soluble_mwf.ggdf[mwf <= mwf.upper]$spline.df)[i]
# 								 		})
# 								 		df.label
# 								 	})) +
# 			stat_function(
# 				fun = function(x) {
# 					1
# 				},
# 				linetype = "3313"
# 			) +
# 			labs(x = paste0(
# 			"Lagged cumulative exposure to soluble metalworking fluids (mg/m$^3\\cdot$yr)"),
# 				y = "Hazard ratio",
# 				linetype = "$df$") +
# 			mytheme,
# 		ncol = 1
# 	)
# 	dev.off()