# Attempt at splining exposure ####
# August 30, 2019

rm(list = ls()[-grep("cohort$|cohort_prepped", ls())])
library(here)
source(here('../gm-wrangling/wrangling', '00-my-theme.R'))

if (sum(grepl('.cohort_prepped', ls())) == 0) {
	source(here('mortality modeling', 'modeling.R'))
	names(outcome) <- 1:length(outcome)
}

get.spline <- function(outcome = "Rectal cancer",
											 spline.which = "Synthetic",
											 spline.df = 0,
											 save_fit = T) {
	assign(
		paste0(gsub(" ", "_", outcome),
					 '_splined_', tolower(spline.which), '_df', spline.df,	'.coxph'),
		coxph(
			as.formula(
				paste0(
					"Surv(age.year1, age.year2, event) ~ Year.cat +	`Year of hire.cat` + Race +	Plant + ",
					if (spline.which == "Soluble") {
						if (spline.which == 0) {
							"pspline(Soluble, df = 0, caic = T) + "
						} else
							paste0("pspline(Soluble, df = ", spline.df, ") + ")
					} else {
						"Soluble.cat + "
					},
					if (spline.which == "Straight") {
						if (spline.which == 0) {
							"pspline(Straight, df = 0, caic = T) + "
						} else
							paste0("pspline(Straight, df = ", spline.df, ") + ")
					} else {
						"Straight.cat + "
					},
					if (spline.which == "Synthetic") {
						if (spline.which == 0) {
							"pspline(Synthetic, df = 0, caic = T)"
						} else
							paste0("pspline(Synthetic, df = ", spline.df, ")")
					} else {
						"Synthetic.cat "
					}
				)
			)
			,
			data = {
				if (min(table(get(paste0(
					gsub(" ", "_", outcome), ".cohort_prepped"
				))[event == 1]$Sex)) <= 10) {
					get(paste0(gsub(" ", "_", outcome), ".cohort_prepped"))[Sex == levels(Sex)[which.max(table(get(paste0(
						gsub(" ", "_", outcome), ".cohort_prepped"
					))[event == 1]$Sex))]]
				} else
					get(paste0(gsub(" ", "_", outcome), ".cohort_prepped"))
			}
			,
			ties = 'efron',
			x = T
		),
		inherits = T
	)

	get(paste0(gsub(" ", "_", outcome),
									'_splined_',	tolower(spline.which), '_df',	spline.df, '.coxph'))

	if (save_fit) {
	save(
		list = paste0(gsub(" ", "_", outcome),
									'_splined_',	tolower(spline.which), '_df',	spline.df, '.coxph'),
		file =  to_drive_D(here('modeling/mortality/resources/splined',
								 paste0(gsub(" ", "_", outcome), '_splined_', tolower(spline.which),
								 			 '_df', spline.df,	'.coxph',	".Rdata")))
	)
		}
}

# Run splined models!!! ####
lapply(sort(c(
	# "Skin cancer",
	"Stomach cancer",
	"Stomach cancer"
	# "Rectal cancer", "Leukemia",
	# "Esophageal cancer"
	)), function(outcome) {

			 	if (outcome == "Rectal cancer" | outcome == "Skin cancer") {
			 		spline.which <- "Soluble"
			 	}

			 	if (outcome == "Leukemia" | outcome == "Esophageal cancer") {
			 		spline.which <- "Synthetic"
			 	}

			 	if (outcome == "Stomach cancer") {
			 		spline.which <- c("Straight", "Soluble")
			 	}

		sapply(spline.which, function(spline.which) {
				sapply(c(0, 2:4), function(i) {
				rm(list = ls()[-grep("cohort$|cohort_prepped", ls())])
				get.spline(outcome = outcome,
									 spline.which = spline.which,
									 spline.df = i)
				})
		})
})

