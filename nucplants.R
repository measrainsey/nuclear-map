files.wd		= '/Users/MEAS/GitHub/nuclear-map'
nuc.file 		= 'nucgenlist.csv'
plants.file 	= 'allplants.csv'

library(data.table)

setwd(files.wd)

nuclist = fread(nuc.file, header = TRUE)
nuclist = nuclist[, c("plant_code", "nameplate_capacity", "status", "operating_year", "retirement_year"), with = FALSE]

nuclist = nuclist[, plant_capacity := sum(nameplate_capacity), by = plant_code]
nuclist = nuclist[, c("plant_code", "plant_capacity"), with = FALSE]

nucplants = na.omit(unique(nuclist))

plantlist = fread(plants.file, header = TRUE)

nucplants = nucplants[plantlist, on = "plant_code", nomatch = 0]

write.csv(nucplants, "nucplantlist.csv", row.names = FALSE)