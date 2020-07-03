icd9 = c(
	`All causes` = "--",
	`All natural causes` =
		"001--799",
	`All cancer` =
		"140--239",
	`Esophageal cancer` =
		"150",
	`Stomach cancer` =
		"151",
	`Intestinal cancer` =
		"152--153",
	`Rectal cancer` =
		"154",
	`Kidney cancer` =
		"189.0, 189.2",
	`Bladder and uretic cancers` =
		"188, 189.3--189.9",
	`Bile duct, liver, and gallbladder cancers` =
		"155--156",
	`Pancreatic cancer` =
		"157",
	`Laryngeal cancer` =
		"161",
	`Lung cancer` =
		"162",
	`Skin cancer` =
		"172--173",
	`Prostate cancer` =
		"185",
	`Brain and nervous system cancers` =
		"191--192",
	`Leukemia` =
		"204--208",
	`Breast cancer` =
		"174--175",
	`All nonmalignant respiratory diseases` =
		paste(
			`Acute Respiratory Infection` =
				"460--466",
			`Pneumoconiosis and other` =
				"470--478",
			`Pneumonia and flu` =
				"480--487",
			`COPD and asthma and other` =
				"490--495, 496--519",
			sep = ', '
		),
	`Chronic obstructive pulmonary disease` =
		"490--492, 496",
	`Pneumonia` =
		"480--486",
	`Cirrhosis and other chronic liver disease` =
		"571",
	`All heart diseases` =
		paste("390--398",
					"402, 404",
					"410--414",
					"420--429",
					sep = ', '),
	`IHD` =
		"410--414",
	`RHD` =
		"390--398",
	`Cerebrovascular disease` =
		"430--438",
	`All external causes` =
		paste("E800--E848",
					"E850--E888",
					"E929--E978",
					"E980--E999",
					sep = ', ')
)

# ICD-10 ####
icd10 = c(
	`All causes` =
		"--",
	`All natural causes` =
		"A00--R99, U00--Z99",
	`All cancer` =
		"C00--C99, D00--D49",
	`Esophageal cancer` =
		"C15",
	`Stomach cancer` =
		"C16",
	`Intestinal cancer` =
		"C17--C18",
	`Rectal cancer` =
		"C19--C21",
	`Kidney cancer` =
		"C64--C66",
	`Bladder and uretic cancers` =
		"C67--C68",
	`Bile duct, liver, and gallbladder cancers` =
		"C22--C24",
	`Pancreatic cancer` =
		"C25",
	`Laryngeal cancer` =
		"C32",
	`Lung cancer` =
		"C33--C34",
	`Skin cancer` =
		paste("C43--C44",
					"C46.0, C46.9",
					sep = ', '),
	`Prostate cancer` =
		"C61",
	`Brain and nervous system cancers` =
		"C47, C70--C72",
	`Leukemia` =
		paste("C91.0--C91.3",
					"C91.5--C91.9",
					"C92--C95",
					sep = ", "),
	`Breast cancer` =
		('C50'),
	`All nonmalignant respiratory diseases` =
		paste(
			'A48.1',
			"J00--J01",
			"J02.8--J02.9",
			"J03.8--J03.9",
			"J04--J06",
			"J10--J18",
			"J20--J22",
			"J40--J46",
			"J30--J33",
			"J34.1--J34.8",
			"J35--J39",
			"J47",
			"J60:J95",
			"J98",
			"R09.1",
			sep = ", "
		),
	`Chronic obstructive pulmonary disease` =
		"J40--J44",
	`Pneumonia` =
		paste('A48.1',
					"J12--J18",
					sep = ', '),
	`Cirrhosis and other chronic liver disease` =
		paste('K70',
					'K73--K74',
					"K76.0",
					sep = ', '),
	`All heart diseases` =
		paste(
			"I00--I09",
			"I11--I13",
			"I20--I22, I24--I25",
			"I30--I38",
			"I40, I42, I44--I52",
			"I97.0--I97.1, I97.8--I97.9",
			"R00.1",
			"R00.8",
			sep = ', '
		),
	`IHD (excluding complications arising from IHD)` =
		paste("I20--I22",
					"I24--I25",
					"I51.3", "I51.6",
					sep = ', '),
	`RHD` =
		"I00--I09",
	`Cerebrovascular disease` =
		paste("G45.0--G45.2, G45.4--G45.9",
					"I60--I69",
					sep = ', '),
	`All external causes` =
		paste(
			"V00--V99",
			"W00-W99",
			"X00--X84",
			"X85--X99",
			"Y00--Y36, Y40--Y89",
			sep = ", "
		)
)
