eia.dir   = '/Users/MEAS/Google Drive/data/eia/860/eia860'
gen.fil   = c('GeneratorY2011.xlsx', 'GeneratorY2012.xlsx', '3_1_Generator_Y2013.xlsx', 
              '3_1_Generator_Y2014.xlsx', '3_1_Generator_Y2015.xlsx', '3_1_Generator_Y2016.xlsx')
plt.fil   = c('Plant.xlsx', 'PlantY2012.xlsx', '2___Plant_Y2013.xlsx', 
              '2___Plant_Y2014.xlsx', '2___Plant_Y2015.xlsx', '2___Plant_Y2016.xlsx')
add.dir   = '/Users/MEAS/Google Drive/data/eia/electric-power-monthly/february2018'
add.fil   = 'Table_6_05.xlsx'
out.dir   = '/Users/MEAS/GitHub/nuclear-map'

# ------------------------------------------------------------------------------------

# load libraries ------

library(data.table)
library(lubridate)
library(openxlsx)
library(stringr)

# list of years -------
  years = 2011:2016

# read in operating generators -------

  list_cols = list(c(3:4,7,10,14:16,48:49),
                   c(3:4,7,10,14:16,48:49),
                   c(3:4,7,15,25:28,33),
                   c(3:4,7,16,26:29,34),
                   c(3:4,7,16,26:29,34),
                   c(3:4,7,16,26:29,34)
  )
  
  list_names = list(c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source", "ret_month", "ret_year"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source", "ret_month", "ret_year"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"))
  
  ncols = c("capacity", "op_month", "op_year", "ret_month", "ret_year")
  
  list_op = list()
  
  for (i in seq_along(years)) {
    setwd(paste0(eia.dir, years[i]))
    
    gen.temp = as.data.table(read.xlsx(gen.fil[i], 
                                       colNames = TRUE, 
                                       startRow = 2,
                                       cols = list_cols[[i]]))
    
    colnames(gen.temp) = list_names[[i]]
    
    gen.temp = gen.temp[ energy_source == "NUC" ]
    
    gen.temp[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
    
    list_op[[i]] = gen.temp
    
    setcolorder(gen.temp, 
                c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"))
    
    rm(gen.temp)
    
  }
  
  op_all = rbindlist(list_op)


# read in retired generators -------

  list_cols = list(c(3:4,7,10,14:18),
                   c(3:4,7,10,14:16,48:49),
                   c(3:4,7,15,25:28,33),
                   c(3:4,7,16,26:29,34),
                   c(3:4,7,16,26:29,34),
                   c(3:4,7,16,26:29,34)
  )
  
  list_names = list(c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source", "ret_month", "ret_year"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"))
  
  ncols = c("capacity", "op_month", "op_year", "ret_month", "ret_year")
  
  list_ret = list()
  
  
  for (i in seq_along(years)) {
    setwd(paste0(eia.dir, years[i]))
    
    gen.temp = as.data.table(read.xlsx(gen.fil[i], 
                                       sheet = 3,
                                       colNames = TRUE, 
                                       startRow = 2,
                                       cols = list_cols[[i]]))
    
    colnames(gen.temp) = list_names[[i]]
    
    gen.temp = gen.temp[ energy_source == "NUC" ]
    
    gen.temp[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
    
    list_ret[[i]] = gen.temp
    
    setcolorder(gen.temp, 
                c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "ret_month", "ret_year", "energy_source"))
    
    rm(gen.temp)
    
  }
  
  ret_2016 = rbindlist(list_ret)
  ret_2016 = unique(ret_2016)

# filter out retirements up to 2018 ------

  ret_2018 = op_all[ ret_year <= 2018 ]
  
  ret_current = rbindlist(list(ret_2016, ret_2018))
  ret_current = unique(ret_current)
  
# remove retired generators from operating generators -----

  op_current = op_all[!ret_2018, on = c("plant_code", "gen_id")]

# filter out planned retirements ------

  ret_planned = op_all[ ret_year > 2018 ]

# add planned installments (up to 2016) ------

  list_cols = list(c(3:4,7,10,14:15,18),
                   c(3:4,7,10,14:15,18),
                   c(3:4,7,16,21:22,28),
                   c(3:4,7,16,21:22,29),
                   c(3:4,7,16,21:22,29),
                   c(3:4,7,16,21:22,29)
  )
  
  list_names = list(c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_year", "op_month", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"),
                    c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"))
  
  ncols = c("capacity", "op_month", "op_year")
  
  list_prop = list()
  
  
  for (i in seq_along(years)) {
    setwd(paste0(eia.dir, years[i]))
    
    gen.temp = as.data.table(read.xlsx(gen.fil[i], 
                                       sheet = 2,
                                       colNames = TRUE, 
                                       startRow = 2,
                                       cols = list_cols[[i]]))
    
    colnames(gen.temp) = list_names[[i]]
    
    gen.temp = gen.temp[ energy_source == "NUC" ]
    
    gen.temp[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
    
    list_prop[[i]] = gen.temp
    
    setcolorder(gen.temp, 
                c("plant_code", "plant_name", "gen_id", "capacity", "op_month", "op_year", "energy_source"))
    
    rm(gen.temp,list_cols,list_names)
    
  }
  
  prop_2016 = rbindlist(list_prop)
  prop_2016 = unique(prop_2016)


# add planned installments (more recent) ------

  setwd(add.dir)
  
  op_planned = as.data.table(read.xlsx(add.fil, startRow = 2, cols = c(1,6,8:9,11,15)))[ Technology == "Nuclear" ]
  
  colnames(op_planned) = c("op_year", "plane_name", "plant_code", "gen_id", "technology", "capacity")
  
  prop_2016 = prop_2016[!op_planned, on = c("plant_code", "gen_id")] # all remaining planned generators in this dataset were cancelled

