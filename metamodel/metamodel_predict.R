# Predict -----------------------------------------------------------------
predict_metamodel <- function(parameters = parametrs) {
metamodel_input <- read_fst("./metamodel/metamodel_input.fst", as.data.table = TRUE)
metamodel_input[, `:=` (
  cpp_chd = cpp_chd + 4L,
  cpp_stroke = cpp_stroke + 3L,
  dpp_chd = dpp_chd + 2L,
  dpp_stroke = dpp_stroke + 2L,
  dpp_other = dpp_other + 5L
)]

all_files <- as.list(
  dir(
    path = "./metamodel",
    pattern = glob2rx("mod_*.rds"),
    full.names = TRUE,
    recursive = FALSE
  )
)
metamodels <- lapply(all_files, readRDS)
all_files <- as.list(
  dir(
    path = "./metamodel",
    pattern = glob2rx("mod_*.rds"),
    full.names = FALSE,
    recursive = FALSE
  )
)
names(metamodels) <- gsub(".rds$", "", all_files)


# parameters_unf <- setDT(readRDS("./output/input_parameters_unformatted.rds"))
# parameters     <- setDT(readRDS("./output/input_parameters.rds"))
tt <- parameters["simulation_period_slider", value]
out <- CJ(year = seq(tt[[1]][[1]], tt[[1]][[2]]) - 2000L,
          scenario = parameters[scenario != "global", unique(scenario)],
          qimd = factor(1:5))
tt <- parameters["friendly_name", .(scenario, friendly_name = unique(unlist(value)))]
parameters[tt, on = "scenario", friendly_name := i.friendly_name]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "baseline", .(scenario, baseline = unlist(value))]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "serial_ensemble_checkbox", .(scenario, serial_ensemble = unlist(value))]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "parallel_ensemble_checkbox", .(scenario, parallel_ensemble = unlist(value))]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "parallel_ensemble_slider", .(scenario, parallel_ensemble_wt = unlist(value))]
out <- out[tt, on = "scenario"]
out[!(parallel_ensemble), parallel_ensemble_wt := 100] # TODO better logic for parallel ensemble
tt <- parameters[input_names %like% "ininit_year_slider", .(scenario, init_year = unlist(value) - 2000L)]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "cancel_program_checkbox", .(scenario, cancel_program = unlist(value))]
out <- out[tt, on = "scenario"]
tt <- parameters[input_names %like% "coverage_qimd0_slider",   .(scenario, annual_coverage = unlist(value)/100)]
out <- out[tt, on = "scenario"]
out[(cancel_program), annual_coverage := 0]
# out[year < init_year] TODO better logic for scenarios not starting
tt <- parameters[input_names %like% "coverage_cost_qimd0",   .(scenario, annual_coverage_cost = unlist(value))]
out <- out[tt, on = "scenario"]
out[(cancel_program), annual_coverage_cost := 0]
tt <- parameters[input_names %like% "uptake_slider",   .(scenario, annual_uptake = unlist(value)/100)]
out <- out[tt, on = "scenario"]
out[(cancel_program), annual_uptake := 0]
tt <- parameters[input_names %like% "uptake_cost",   .(scenario, annual_uptake_cost = unlist(value))]
out <- out[tt, on = "scenario"]
out[(cancel_program), annual_uptake_cost := 0]
tt <- parameters[input_names %like% "antihtn_px_slider",   .(scenario, antihtn_px = unlist(value)/100)]
out <- out[tt, on = "scenario"]
out[(cancel_program), antihtn_px := 0]
tt <- parameters[input_names %like% "statin_px_slider",   .(scenario, statin_px = unlist(value)/100)]
out <- out[tt, on = "scenario"]
out[(cancel_program), statin_px := 0]
out[, prescription_target := (statin_px + antihtn_px)/2]
parameters[input_names %like% "px", ]
tt <- parameters[input_names %like% "smkcess_slider",   .(scenario, lifestyle_target = unlist(value)/100)]
out <- out[tt, on = "scenario"]
out[(cancel_program), lifestyle_target := 0]
tt <- parameters[input_names %like% "smkcess_cost$",   .(scenario, lifestyle_target_cost = unlist(value))]
out <- out[tt, on = "scenario"]
out[(cancel_program), lifestyle_target_cost := 0]

out <- clone_dt(out, 30L) # increase to 100L

out[, rank_cpp_chd := runif(.N)]
guess_gamlss(out, metamodels$mod_cpp_chd, metamodel_input, 1)
out[, rank_cpp_chd := NULL]
out[, rank_dpp_chd := runif(.N)]
guess_gamlss(out, metamodels$mod_dpp_chd, metamodel_input, 1)
out[, rank_dpp_chd := NULL]
out[, rank_cpp_stroke := runif(.N)]
guess_gamlss(out, metamodels$mod_cpp_stroke, metamodel_input, 1)
out[, rank_cpp_stroke := NULL]
out[, rank_dpp_stroke := runif(.N)]
guess_gamlss(out, metamodels$mod_dpp_stroke, metamodel_input, 1)
out[, rank_dpp_stroke := NULL]
out[, rank_dpp_other := runif(.N)]
guess_gamlss(out, metamodels$mod_dpp_other, metamodel_input, 1)
out[, rank_dpp_other := NULL]
out[, rank_net_utility := runif(.N)]
guess_gamlss(out, metamodels$mod_net_utility, metamodel_input, 1)
out[, rank_net_utility := NULL]
out[, rank_net_cost := runif(.N)]
guess_gamlss(out, metamodels$mod_net_cost, metamodel_input, 1)
out[, rank_net_cost := NULL]
out[, rank_sei := runif(.N)] # Need to be same per qimd
guess_gamlss(out, metamodels$mod_sei, metamodel_input, 1)
out[, rank_sei := NULL]
out[, rank_rei := runif(.N)] # Need to be same per qimd
guess_gamlss(out, metamodels$mod_rei, metamodel_input, 1)
out[, rank_rei := NULL]
out[, `:=` (
  cpp_chd    = cpp_chd - 4L,
  cpp_stroke = cpp_stroke - 3L,
  dpp_chd    = dpp_chd - 2L,
  dpp_stroke = dpp_stroke - 2L,
  dpp_other  = dpp_other - 5L,

  cpp_t2dm   = (cpp_chd - 4L) * 0.5,
  cpp_af     = (cpp_stroke - 3L) * 0.5,
  cpp_lc     = (cpp_chd - 4L) * 0.1
)]

# parallel_ensemble logic -----
# out[, parallel_ensemble_wt := as.numeric(parallel_ensemble_wt)]
# out[scenario %in% paste0("sc", 3:4), parallel_ensemble := TRUE]
# out[scenario %in% paste0("sc", 3), parallel_ensemble_wt := 0.8]
# out[scenario %in% paste0("sc", 4), parallel_ensemble_wt := 0.2]

if (any(out$parallel_ensemble)) {
  tt <- out[(parallel_ensemble), ]
  out <-  out[!(parallel_ensemble), ]
  tt <- split(tt, tt$scenario)

  tt[[1]][tt[[2]], on  = c("year", "qimd", ".id"), `:=` (
    cpp_chd     = cpp_chd * parallel_ensemble_wt/100 + i.cpp_chd * i.parallel_ensemble_wt/100,
    cpp_stroke  = cpp_stroke * parallel_ensemble_wt/100 + i.cpp_stroke * i.parallel_ensemble_wt/100,
    cpp_t2dm    = cpp_t2dm * parallel_ensemble_wt/100 + i.cpp_t2dm * i.parallel_ensemble_wt/100,
    cpp_af      = cpp_af * parallel_ensemble_wt/100 + i.cpp_af * i.parallel_ensemble_wt/100,
    cpp_lc      = cpp_lc * parallel_ensemble_wt/100 + i.cpp_lc * i.parallel_ensemble_wt/100,
    dpp_chd     = dpp_chd * parallel_ensemble_wt/100 + i.dpp_chd * i.parallel_ensemble_wt/100,
    dpp_stroke  = dpp_stroke * parallel_ensemble_wt/100 + i.dpp_stroke * i.parallel_ensemble_wt/100,
    dpp_other   = dpp_other * parallel_ensemble_wt/100 + i.dpp_other * i.parallel_ensemble_wt/100,
    net_utility = net_utility * parallel_ensemble_wt/100 + i.net_utility * i.parallel_ensemble_wt/100,
    net_cost    = net_cost * parallel_ensemble_wt/100 + i.net_cost * i.parallel_ensemble_wt/100,
    sei         = sei * parallel_ensemble_wt/100 + i.sei * i.parallel_ensemble_wt/100,
    rei         = rei * parallel_ensemble_wt/100 + i.rei * i.parallel_ensemble_wt/100
  )]

  tt[[1]][, friendly_name := paste0(friendly_name, "_ens")]
  out <- rbind(out,  tt[[1]])
  rm(tt)
}

if (any(out$baseline)) {
  tt <- out[(baseline), ]
  tt[(cancel_program), `:=` (
    cpp_chd     = 0L,
    cpp_stroke  = 0L,
    cpp_t2dm    = 0L,
    cpp_af      = 0L,
    cpp_lc      = 0L,
    dpp_chd     = 0L,
    dpp_stroke  = 0L,
    dpp_other   = 0L,
    net_utility = 0,
    net_cost    = 0,
    sei         = 0,
    rei         = 0
  )]
  out <-  out[!(baseline), ]
  out[tt, on = c("year", "qimd", ".id"), `:=` (
    cpp_chd = cpp_chd - i.cpp_chd,
    cpp_stroke  = cpp_stroke - i.cpp_stroke,
    cpp_t2dm    = cpp_t2dm - i.cpp_t2dm,
    cpp_af      = cpp_af - i.cpp_af,
    cpp_lc      = cpp_lc - i.cpp_lc,
    dpp_chd     = dpp_chd - i.dpp_chd,
    dpp_stroke  = dpp_stroke - i.dpp_stroke,
    dpp_other   = dpp_other - i.dpp_other,
    net_utility = net_utility - i.net_utility,
    net_cost    = net_cost - i.net_cost,
    sei         = sei - i.sei,
    rei         = rei - i.rei
  )]
  rm(tt)

}
out[, year := year + 2000L]
setkey(out, year)
return(out)
}
# write_fst(out, "./output/out.fst")
