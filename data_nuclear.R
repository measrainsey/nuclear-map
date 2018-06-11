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
  ret_current = ret_current[!is.na(ret_year)]

# remove retired generators from operating generators -----

  op_current = op_all[!ret_2018, on = c("plant_code", "gen_id")]


# filter out planned retirements ------

  ret_planned = op_all[ ret_year > 2018 ]

# add planned installments listed in eia 860 (up to 2016) ------

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

    rm(gen.temp)

  }

  prop_2016 = rbindlist(list_prop)
  prop_2016 = unique(prop_2016)


# add planned installments (more recent) ------

  setwd(add.dir)

  op_planned = as.data.table(read.xlsx(add.fil, startRow = 2, cols = c(1,6,8:9,11,15)))[ Technology == "Nuclear" ]

  colnames(op_planned) = c("op_year", "plant_name", "plant_code", "gen_id", "technology", "capacity")

  prop_2016 = prop_2016[!op_planned, on = c("plant_code", "gen_id")] # all remaining planned generators in this dataset were cancelled
  op_planned = op_planned[, c("plant_code", "plant_name", "gen_id", "capacity", "op_year")]

# remove unneeded data tables -------

  rm(op_all,prop_2016,ret_2016,ret_2018)

# get only unique operating units ------

  op_current = op_current[, c("plant_code", "plant_name", "gen_id", "capacity", "op_year")]
  op_current[ plant_name %like% "Susquehanna", plant_name := "Susquehanna Steam Electric Station"]
  op_current[ plant_name %like% "Brunswick", plant_name := "Brunswick Nuclear"]
  op_current[ plant_name %like% "Cooper", plant_name := "Cooper Nuclear Station"]
  op_current[ plant_name %like% "Monticello", plant_name := "Monticello Nuclear Facility"]
  op_current = unique(op_current)

  # no_current = op_current[, .(count = uniqueN(capacity)),  by = c("plant_code", "gen_id")]
  # op_current = op_current[no_current, on = c("plant_code", "gen_id")]

  op_current = op_current[, .SD[which.max(capacity)], by = c("plant_code", "plant_name", "gen_id", "op_year")]

# get only unique retiring units -------

  ret_planned = ret_planned[, c("plant_code", "plant_name", "gen_id", "capacity", "ret_year")]
  ret_planned = unique(ret_planned)

# change retirements to negative ------

  # ret_current[, capacity := -capacity ]
  # ret_planned[, capacity := -capacity ]

# get plant locations -----

  list_cols = list(c(),
                   c(2:3,5,7,27:28),
                   c(3:4,6:7,10:11),
                   c(),
                   c(),
                   c(3:4,6:7,10:11)
  )


  list_info = list()

  for (i in c(3,6)) {
    setwd(paste0(eia.dir, years[i]))

    plt.temp = as.data.table(read.xlsx(plt.fil[i], startRow = 2, cols = list_cols[[i]]))
    colnames(plt.temp) = c("plant_code", "plant_name", "city", "state", "latitude", "longitude")

    list_info[[i]] = plt.temp

    rm(plt.temp)

  }

  # for (i in 2) {
  #
  #   setwd(paste0(eia.dir, years[i]))
  #
  #   plt.temp = as.data.table(read.xlsx(plt.fil[i], startRow = 2, cols = list_cols[[i]]))
  #   colnames(plt.temp) = c("plant_code", "plant_name", "city", "state", "latitude", "longitude")
  #
  #   plt.temp = plt.temp[ plant_code == "6103"]
  #   list_info[[i]] = plt.temp
  #
  #   rm(plt.temp)
  #
  # }

  plant_info = rbindlist(list_info)

  plant_info = plant_info[ plant_code %in% op_current[, plant_code] |
                             plant_name %in% op_planned[, plant_name] |
                             plant_code %in% ret_current[, plant_code] |
                             plant_code %in% ret_planned[, plant_code]]

  plant_info[ plant_name %like% "Susquehanna", plant_name := "Susquehanna Steam Electric Station"]

  plant_info = unique(plant_info)

  op_current = op_current[plant_info[, c("plant_code", "city", "state", "latitude", "longitude")], on = "plant_code", nomatch = 0]
  op_planned = op_planned[plant_info[, c("plant_code", "city", "state", "latitude", "longitude")], on = "plant_code", nomatch = 0]

  ret_current = ret_current[plant_info[, c("plant_code", "city", "state", "latitude", "longitude")], on = "plant_code", nomatch = 0]
  ret_planned = ret_planned[plant_info[, c("plant_code", "city", "state", "latitude", "longitude")], on = "plant_code", nomatch = 0]

  # dt_nomatch = op_current_2[!op_current, on = "plant_name"]
  # no_match = op_current_2[, .(count = uniqueN(latitude)),  by = c("plant_code", "gen_id")]

# create data table for initial installations -------

  op_current_first = op_current[, .SD[which.min(op_year)], by = c("plant_code")]
  op_planned_first = op_planned[, .SD[which.min(op_year)], by = c("plant_code")]
    setcolorder(op_planned_first, c("plant_code", "plant_name", "gen_id", "op_year",
                                    "capacity", "city", "state", "latitude", "longitude"))

  ret_current_first = ret_current[, c("plant_code", "plant_name", "gen_id", "op_year",
                                      "capacity", "city", "state", "latitude", "longitude")][, .SD[which.min(op_year)],
                                                                                             by = c("plant_code")]

  dt_first = rbindlist(list(op_current_first,ret_current_first,op_planned_first))

  dt_first = dt_first[, .SD[which.min(op_year)], by = c("plant_code")]


  rm(op_current_first,ret_current_first,op_planned_first)


# keep releveant columns ------

  ret_current = ret_current[, c("plant_code", "plant_name", "gen_id", "capacity", "op_year", "ret_year",
                                "city", "state", "latitude", "longitude")]
  ret_current = unique(ret_current)

  ret_planned = ret_planned[, c("plant_code", "plant_name", "gen_id", "capacity", "ret_year", "city", "state", "latitude", "longitude")]

# match planned retirements with operating year -----

  ret_planned = ret_planned[op_current[, c("plant_code", "gen_id", "op_year")], on = c("plant_code", "gen_id"), nomatch = 0]

# melt data from wide to long -------

  ret_planned = melt(ret_planned, measure.vars = c("op_year", "ret_year"),
                     variable.name = "category", value.name = "year")

  ret_planned[category == "ret_year", capacity := -capacity]

  ret_current = melt(ret_current[, c("plant_code", "plant_name", "gen_id", "capacity", "op_year", "ret_year",
                                     "city", "state", "latitude", "longitude")],
                     measure.vars = c("op_year", "ret_year"),
                     variable.name = "category", value.name = "year")

  ret_current[category == "ret_year", capacity := -capacity]

# current plants that have no planned retirements yet --------

  op_current_all = copy(op_current)
  op_current = op_current_all[!ret_planned, on = c("plant_code", "gen_id")]
  setnames(op_current, "op_year", "year")
  setnames(op_planned, "op_year", "year")

# sum retirements that are in the same year ------

  ret_current = ret_current[, .(capacity = sum(capacity)), by = c("plant_code", "plant_name", "year",
                                                                  "city", "state", "latitude", "longitude")]

  ret_planned = ret_planned[, .(capacity = sum(capacity)), by = c("plant_code", "plant_name", "year",
                                                                  "city", "state", "latitude", "longitude")]
# data table of all years ------

  dt.years = data.table(year = 1969:2027)

# sequence of retired plants -----

  list_ret_current = list()
  dt.un = unique(ret_current[, c("plant_code")])

  for (i in 1:nrow(dt.un)) {
    dt.temp = ret_current[plant_code == dt.un[, plant_code][i]]
    first.temp = dt_first[plant_code == dt.un[, plant_code][i]]
    dt.temp = dt.temp[dt.years, on = "year"]
    dt.temp = dt.temp[ year >= first.temp[, op_year]]

    dt.temp[, plant_code := first.temp[, plant_code]]
    dt.temp[, plant_name := first.temp[, plant_name]]
    dt.temp[, city := first.temp[, city]]
    dt.temp[, state := first.temp[, state]]
    dt.temp[, latitude := first.temp[, latitude]]
    dt.temp[, longitude := first.temp[, longitude]]
    dt.temp[is.na(capacity), capacity := 0]

    dt.temp = dt.temp[ , c("year", "plant_code", "plant_name", "capacity", "city", "state", "latitude", "longitude"),
                       with = FALSE][, lapply(.SD,sum),
                                     by = list(plant_code, plant_name, year, city, state, latitude, longitude)]
    
    dt.temp[year < min(ret_current[plant_code == dt.un[, plant_code][i] & capacity < 0, year]), status := "operating"]
    dt.temp[year >= min(ret_current[plant_code == dt.un[, plant_code][i] & capacity < 0, year]), status := "retired"]
    dt.temp[, cum_capacity := cumsum(capacity), by = list(status)]
    dt.temp[, max_capacity := max(cum_capacity)]
    dt.temp[status == "operating", net_capacity := cum_capacity]
    dt.temp[status == "retired", net_capacity := cum_capacity + max_capacity]
    
    # dt.temp = dt.temp[ , c("year", "plant_code", "plant_name", "capacity", "city", "state", "latitude", "longitude"),
    #                    with = FALSE][, lapply(.SD,sum), 
    #                                  by = list(plant_code, plant_name, year, city, state, latitude, longitude)]
    # 
    # dt.temp[, cum_capacity := cumsum(capacity)]
    dt.temp = dt.temp[, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity",
                          "city", "state", "latitude", "longitude")]
    setcolorder(dt.temp, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity", 
                           "city", "state", "latitude", "longitude"))

    list_ret_current[[i]] = dt.temp

  }

  rm(dt.temp,dt.un,first.temp)


# sequence of planned retired plants -----

  list_ret_planned = list()
  dt.un = unique(ret_planned[, c("plant_code")])

  for (i in 1:nrow(dt.un)) {
    dt.temp = ret_planned[plant_code == dt.un[, plant_code][i]]
    first.temp = dt_first[plant_code == dt.un[, plant_code][i]]
    dt.temp = dt.temp[dt.years, on = "year"]
    dt.temp = dt.temp[ year >= first.temp[, op_year]]

    dt.temp[, plant_code := first.temp[, plant_code]]
    dt.temp[, plant_name := first.temp[, plant_name]]
    dt.temp[, city := first.temp[, city]]
    dt.temp[, state := first.temp[, state]]
    dt.temp[, latitude := first.temp[, latitude]]
    dt.temp[, longitude := first.temp[, longitude]]
    dt.temp[is.na(capacity), capacity := 0]
    
    dt.temp = dt.temp[ , c("year", "plant_code", "plant_name", "capacity", "city", "state", "latitude", "longitude"),
                       with = FALSE][, lapply(.SD,sum),
                                     by = list(plant_code, plant_name, year, city, state, latitude, longitude)]

    dt.temp[year < min(ret_planned[plant_code == dt.un[, plant_code][i] & capacity < 0, year]), status := "operating"]
    dt.temp[year >= min(ret_planned[plant_code == dt.un[, plant_code][i] & capacity < 0, year]), status := "retired"]
    dt.temp[, cum_capacity := cumsum(capacity), by = list(status)]
    dt.temp[, max_capacity := max(cum_capacity)]
    dt.temp[status == "operating", net_capacity := cum_capacity]
    dt.temp[status == "retired", net_capacity := cum_capacity + max_capacity]
    
    # dt.temp = dt.temp[ , c("year", "plant_code", "plant_name", "capacity", "city", "state", "latitude", "longitude"),
    #                    with = FALSE][, lapply(.SD,sum), 
    #                                  by = list(plant_code, plant_name, year, city, state, latitude, longitude)]
    
    # dt.temp[, cum_capacity := cumsum(capacity)]
    dt.temp = dt.temp[, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity",
                          "city", "state", "latitude", "longitude")]
    setcolorder(dt.temp, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity", 
                           "city", "state", "latitude", "longitude"))
    
    list_ret_planned[[i]] = dt.temp

  }
  rm(dt.temp,dt.un,first.temp)


# sequence of current and planned operating plants -------

  setcolorder(op_planned, colnames(op_current))
  op_all = rbindlist(list(op_current,op_planned))
  op_all[, year := as.numeric(year)]
  op_all[, capacity := as.numeric(capacity)]

  list_op_all = list()
  dt.un = unique(op_all[, c("plant_code")])

  for (i in 1:nrow(dt.un)) {
    dt.temp = op_all[plant_code == dt.un[, plant_code][i]]
    first.temp = dt_first[plant_code == dt.un[, plant_code][i]]
    dt.temp = dt.temp[dt.years, on = "year"]
    dt.temp = dt.temp[ year >= first.temp[, op_year]]

    dt.temp[, plant_code := first.temp[, plant_code]]
    dt.temp[, plant_name := first.temp[, plant_name]]
    dt.temp[, city := first.temp[, city]]
    dt.temp[, state := first.temp[, state]]
    dt.temp[, latitude := first.temp[, latitude]]
    dt.temp[, longitude := first.temp[, longitude]]
    dt.temp[is.na(capacity), capacity := 0]
    
    dt.temp = dt.temp[ , c("year", "plant_code", "plant_name", "capacity", "city", "state", "latitude", "longitude"),
                       with = FALSE][, lapply(.SD,sum), 
                                     by = list(plant_code, plant_name, year, city, state, latitude, longitude)]

    dt.temp[, cum_capacity := cumsum(capacity)]
    dt.temp[, max_capacity := max(cum_capacity)]
    dt.temp[, net_capacity := cum_capacity]

    dt.temp = dt.temp[, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity",
                          "city", "state", "latitude", "longitude")]
    setcolorder(dt.temp, c("year", "plant_code", "plant_name", "capacity", "cum_capacity", "max_capacity", "net_capacity", 
                           "city", "state", "latitude", "longitude"))
    
    list_op_all[[i]] = dt.temp

  }
  rm(dt.temp,dt.un,first.temp)


# combine all data tables -------

  dt_sequence = rbindlist(c(list_ret_current,list_ret_planned,list_op_all))
  dt_sequence = dt_sequence[, c("year", "plant_code", "plant_name", "capacity", "cum_capacity",  "max_capacity", "net_capacity", 
                                "city", "state", "latitude", "longitude")]


# export data ------

  setwd(out.dir)

  fwrite(dt_first, "nuc_first.csv", row.names = FALSE)
  fwrite(dt_sequence, "nuc_sequence_4.csv", row.names = FALSE)
  # fwrite(dt_sequence[year == 2018], "nuc_2018.csv", row.names = FALSE)
