# Testing covariate-time interactions to assess ####
# Proportional hazards assumption

library(here)
source('~/headRs/00-my-theme.R')

# rm(list = ls()[-grep("cohort", ls())])
if (sum(grepl("\\.cohort_prepped", ls())) == 0) {
	source(here::here('cancer mortality/modeling.R'))
}

for (outcome_i in outcome.selected) {
	tmp.cohort_prepped <- get(paste0(gsub(" ", "_", outcome_i), ".cohort_prepped"))

	basic.formula <- 	paste(
				 		"Surv(age.year1, age.year2, event) ~",
				 		# "soluble.ever", +
				 		"Soluble +",
				 		# "straight.ever +",
				 		"Straight +",
				 		# "synthetic.ever +",
				 		"Synthetic +",
				 		if (min(table(tmp.cohort_prepped[event == 1]$Year.cat)) > 1) {
				 			"Year +"
				 		},
				 		if (min(table(tmp.cohort_prepped[event == 1]$`Year of hire.cat`)) > 1) {
				 			"`Year of hire` +"
				 		},
				 		if (min(table(tmp.cohort_prepped[event == 1]$`Race`)) > 10) {
				 			"Race +"
				 		},
				 		if (min(table(tmp.cohort_prepped[event == 1]$Sex)) > 10) {
				 			"Sex +"
				 		},
				 		"Plant"
				 	)

				 	# Run Cox models
				 		tmp.coxph <-
				 			coxph(as.formula(basic.formula),
				 						data = {
				 							if (min(table(tmp.cohort_prepped[event == 1]$Sex)) <= 10) {
				 								tmp.cohort_prepped[Sex == levels(Sex)[which.max(table(tmp.cohort_prepped[event == 1]$Sex))]]
				 							} else
				 								tmp.cohort_prepped
				 						}
				 						,
				 						ties = 'efron')

	model.resid <- resid(tmp.coxph,	"scaledsch")

	signif <- data.frame(matrix(nrow = 0, ncol = 6))
	signif.colnames <- c("Covariate", "$\\widehat{m}$", "$\\widehat{\\mathrm{SE}}\\left(m\\right)$", "$t$", "$\\Prob{>|t|}$", " ")
	colnames(signif) <- signif.colnames

	b <- coef(tmp.coxph)

	residual.df <- data.frame(
		time = rep(NaN, nrow(model.resid) * ncol(model.resid)),
		schoenfeld = rep(NaN, nrow(model.resid) * ncol(model.resid)),
		covariate = rep(as.character(NA), nrow(model.resid) * ncol(model.resid)),
		beta = rep(NaN, nrow(model.resid) * ncol(model.resid)),
		stringsAsFactors = F
	)

	for (j in 1:ncol(model.resid)) {
		residual.df[seq(1, nrow(residual.df),
										nrow(model.resid))[j]:(seq(1, nrow(residual.df),
										nrow(model.resid))[j] + nrow(model.resid) - 1),
								] <- data.frame(
									time = as.numeric(rownames(model.resid)),
									schoenfeld = unname(model.resid[, j]),
									covariate = rep(names(
										coef(tmp.coxph)[j]),
										nrow(model.resid)),
									beta = unname(coef(tmp.coxph)[j]),
									stringsAsFactors = F)

		residual.test <- summary(lm(
			schoenfeld + b[j] ~ rank(time),
			residual.df[seq(1, nrow(residual.df),
										nrow(model.resid))[j]:(seq(1, nrow(residual.df),
										nrow(model.resid))[j] + nrow(model.resid) - 1),
								]
		))$coefficients[2, ]
		if (residual.test[4] < 0.15) {

			Covariate <- names(coef(tmp.coxph))[j]

			residual.test <- c(
			Covariate,
			residual.test,
			ifelse(residual.test[4] < 0.05, "$*$", NA))

			names(residual.test) <- signif.colnames

			signif <- rbindlist(
				list(signif,
						 	as.data.frame(t(residual.test))
				))
			signif <- as.data.frame(signif)
			signif[,-1] <- apply(signif[,-1], 2, as.numeric)
			}
	}

	if (nrow(signif) > 0) {

		# message('--------------------------')
		# message(outcome_i)
		# message('--------------------------')
		signif[,3:6] <- apply(signif[,3:6], 2, function(x) {round(as.numeric(x), 3)})

		print.xtable(
			xtable(signif,
						 caption = paste0("Selected Schoenfeld residual tests for the Cox model for ", tolower(outcome_i), "."),
						 digits = 3),
		)

		cat("\n\n")

		residual.df <- merge(as.data.table(residual.df),
												 {
												 	names(signif) <- c('covariate', 'm', 'se(m)', 't', 'P(>|z|)', 'significant')
												 	as.data.table(signif)
												 },
												 on = c("covariate"),
												 all.x =  T)


		assign(paste0(gsub(' ', '_', outcome_i), '.schoenfeld'),
					 residual.df)

	}

}

Bladder_and_urinary_organ_cancers.schoenfeld[
	covariate == 'Synthetic'] %>% ggplot(
			aes(x = time, y = schoenfeld)) +
	geom_point() +
	geom_smooth(method = 'lm', se = F, linetype = 2, color = 'red') +
	geom_smooth(method = 'loess', se = F, linetype = 1) + mytheme

