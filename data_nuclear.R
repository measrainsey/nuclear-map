gen.dir   = '/Users/MEAS/Google Drive/data/eia/860/eia8602016'
gen.fil   = '3_1_Generator_Y2016.xlsx'
plant.fil = '2___Plant_Y2016.xlsx'
add.dir   = '/Users/MEAS/Google Drive/data/eia/electric-power-monthly/february2018'
add.fil   = 'Table_6_05.xlsx'
out.dir   = '/Users/MEAS/GitHub/nuclear-map'

# ------------------------------------------------------------------------------------

# load libraries ------

  library(data.table)
  library(lubridate)
  library(openxlsx)
  library(stringr)

# load current generators ------
  
  setwd(gen.dir)


  cur_op = as.data.table(read.xlsx(gen.fil, sheet = "Operable", startRow = 2, rows = 2:20726, 
                                   cols = c(3,7:8,16,26:29)))[ Technology == "Nuclear"]
    ncols = colnames(cur_op)[4:8]
    cur_op[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
    colnames(cur_op) = c("plant_code", "gen_id", "technology", "capacity", "op_month", "op_year", "ret_month", "ret_year")
    
  cur_ret = as.data.table(read.xlsx(gen.fil, sheet = "Retired and Canceled",
                                    startRow = 2, cols = c(3,7:8,16,26:29)))[ Technology == "Nuclear"]
    ncols = colnames(cur_ret)[4:8]
    cur_ret[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
    colnames(cur_ret) = c("plant_code", "gen_id", "technology", "capacity", "op_month", "op_year", "ret_month", "ret_year")
    cur_ret = cur_ret[, c("plant_code", "capacity", "ret_year")]
    cur_ret = cur_ret[!is.na(ret_year)]
  
  plan_ret = cur_op[ !is.na(ret_year), c("plant_code", "gen_id", "technology", "capacity","ret_year")]
  cur_op = cur_op[!plan_ret, on = c("plant_code", "gen_id"), 
                  c("plant_code", "capacity", "op_year")]
  plan_ret = plan_ret[, c("plant_code", "capacity","ret_year")]
  
  colnames(cur_op) = c("plant_code", "capacity", "year")
  colnames(cur_ret) = c("plant_code", "capacity", "year")
  colnames(plan_ret) = c("plant_code", "capacity", "year")

  # cur_prop = as.data.table(read.xlsx(gen.fil, sheet = "Proposed", startRow = 2, cols = c(3,7:8,16,21:24)))[ Technology == "Nuclear"]
  # 
  #   ncols = colnames(cur_prop)[4:8]
  #   cur_prop[, (ncols) := lapply(.SD, as.numeric), .SDcols = ncols ]
  
  setwd(add.dir)
  plan_prop = as.data.table(read.xlsx(add.fil, startRow = 2, cols = c(1,8,11,15)))[ Technology == "Nuclear" ]
    colnames(plan_prop) = c("year", "plant_code", "technology", "capacity")
    plan_prop = plan_prop[, c("year", "plant_code", "capacity")]
    setcolorder(plan_prop, c("plant_code", "capacity", "year"))
  
  setwd(gen.dir)
  
  plant_info = as.data.table(read.xlsx(plant.fil, startRow = 2, cols = c(3:4,6:7,10:11)))
    colnames(plant_info) = c("plant_code", "plant_name", "city", "state", "latitude", "longitude")
  plant_info = plant_info[ plant_code %in% cur_op[, plant_code] | 
                             plant_code %in% cur_ret[, plant_code] | 
                             plant_code %in% plan_prop[, plant_code] ]
  
  
# set retirements as negative ------
  
  cur_ret[, capacity := -capacity]
  plan_ret[, capacity := -capacity]
  
# all generators together -----
  
  dt_all = rbindlist(list(cur_op, cur_ret, plan_ret, plan_prop))
  dt_all[, (2:3) := lapply(.SD, as.numeric), .SDcols = 2:3 ]
  dt_all = dt_all[, lapply(.SD, sum), by = c("plant_code", "year")]
  
  dt_all = dt_all[plant_info, on = "plant_code"]
  
  dt_1986 = dt_all[ year == "1986"]
  
# save files ------
  
  setwd(out.dir)
  
  fwrite(dt_all, "dt_all.csv", row.names = FALSE)
  fwrite(dt_1986, "dt_1986.csv", row.names = FALSE)
  