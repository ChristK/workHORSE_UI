## required for shinnyapps.io to work
library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyBS) # TODO use https://ijlyttle.github.io/bsplus/index.html
library(bsplus)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(fst)
library(DT)
library(plotly)
# library(promises)
# library(future)
library(gamlss)
library(parallel)
# remotes::install_github("ChristK/CKutils")
library(CKutils)

# plan(multiprocess)
# options(future.globals.maxSize = 6 * 1024^3)

mythemeSelector <- function() {
  # from https://stackoverflow.com/questions/47827337/prodution-ready-themeselector-for-shiny
  div(
    div(
      # tags$style(type='text/css', ".shiny-input-container { height: 20px; line-height: 0.86; font-size: 12px; margin: 0px; padding: 0px; border: 0px;}"),
      #
      # tags$style(type='text/css', ".form-control { height: 20px; line-height: 1.1; font-size: 12px; margin-left: 20px; padding: 0px; border: 0px;}"),
      selectInput("shinytheme_selector", "Please select another theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE, width = "100%"
      )
    ),
    tags$script(
      "$('#shinytheme_selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}


# Produce scenario tabs 2-9 using scenario 1 as a template if not exist ------
for (i in 2:9) {
  if (!file.exists(paste0("ui/scenario", i, "_tab.R")) ||
      !identical(file.size("ui/scenario1_tab.R"), file.size(paste0("ui/scenario", i, "_tab.R")))) {
    tt <- readLines(file.path("ui", "scenario1_tab.R"))
    tt <- gsub("Scenario 1",
               paste0("Scenario ", i),
               gsub(
                 "input.level == 1",
                 paste0("input.level == ", i),
                 gsub("_sc1",  paste0("_sc", i), tt)
               ))
    writeLines(tt, paste0("ui/scenario", i, "_tab.R"))
  }
}

for (i in 2:9) {
  if (!file.exists(paste0("server/tooltips_sc", i, ".R")) ||
      !identical(file.size("server/tooltips_sc1.R"), file.size(paste0("server/tooltips_sc", i, ".R")))) {
    tt <- readLines(file.path("server", "tooltips_sc1.R"))
    tt <- gsub("_sc1",  paste0("_sc", i), tt)
    writeLines(tt, paste0("server/tooltips_sc", i, ".R"))
  }
}

# load localities structure
localities_indx <- read_fst("./synthpop/localities_indx.fst", as.data.table = TRUE)

# Extract statin table from inputs ----
extract_statin_table <- function(param_tb, scenario_num) {
  out <- param_tb[input_names %like% "statin_px_qrisk" & scenario == paste0("sc", scenario_num), .(input_names, "val" = unlist(value))]
  if (nrow(out) == 0L) stop("Parameters don't exist.")
  out[, c("qrisk", "qimd") := tstrsplit(input_names, "_", fixed = TRUE, keep = 4:5)]
  replace_from_table(out, "qimd", as.character(1:5), c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"))
  return(out)
  }
# extract_statin_table(setDT(readRDS("./output/input_parameters.rds")), 2)[]
# setcolorder(
#   dcast(extract_statin_table(setDT(
#     readRDS("./output/input_parameters.rds")
#   ), 2), qimd ~ qrisk, value.var = "val"),
#   c("qimd", "low", "mid", "high")
# )[]

# Extract antihtn table from inputs ----
extract_antihtn_table <- function(param_tb, scenario_num) {
  out <- param_tb[input_names %like% "antihtn_px_qrisk" & scenario == paste0("sc", scenario_num), .(input_names, "val" = unlist(value))]
  if (nrow(out) == 0L) stop("Parameters don't exist.")
  out[, c("qrisk", "qimd") := tstrsplit(input_names, "_", fixed = TRUE, keep = 4:5)]
  replace_from_table(out, "qimd", as.character(1:5), c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"))
  return(out)
}
# extract_antihtn_table(setDT(readRDS("./output/input_parameters.rds")), 2)[]
# setcolorder(
#   dcast(extract_antihtn_table(setDT(
#     readRDS("./output/input_parameters.rds")
#   ), 2), qimd ~ qrisk, value.var = "val"),
#   c("qimd", "low", "mid", "high")
# )[]

# Extract uptake table from inputs ----
extract_uptake_table <- function(param_tb, scenario_num) {
  sc_nam <- paste0("sc", scenario_num)
  # recreate age groups
  if (param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[2]]] - param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[1]]] < 11) {
    agegrp_nam <-
      paste0(param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[1]]], "-", param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[2]]])
  } else {
    agegrp_nam <-
      agegrp_name(as.integer(param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[1]]] /
                               10) * 10L,
                  as.integer(param_tb[input_names == "age_eligibility_slider" & scenario == sc_nam, value[[1]][[2]]] /
                               10) * 10L,
                  10L)
  }

  tt <- CJ(qimd = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
           sex = c("men", "women"),
           ages = agegrp_nam)

  # out <- param_tb[input_names %like% "uptake_qrisk_", .(input_names, "val" = unlist(get(sc_nam)))]
  # if (!"val" %in% names(out)) out[, val := NA_real_]
  out <- param_tb[input_names %like% "uptake_qrisk_" & scenario == sc_nam, .(input_names, "val" = unlist(value))]
  if (nrow(out) == 0L) stop("Parameters don't exist.")

  out[, c("qrisk", "ord") := tstrsplit(input_names, "_", fixed = TRUE, keep = 3:4)]
  out <- setcolorder(dcast(out, as.integer(ord)~qrisk, value.var = "val"), c("ord", "low", "mid", "high"))
  if (is.unsorted(out$ord)) setkey(out, ord)
  out[, ord := NULL]
  out <- cbind(tt, out)
  out <- melt(out, 1:3, variable.name = "qrisk",  value.nam = "val")
  return(out)
}

most_cost_effective <- function(dt) {
  names(dt[year == max(year),
            .(
              nmb_cml = sum(nmb_cml)
            ), by = .(.id, friendly_name)
            ][, friendly_name[which.max(nmb_cml)], by = .id][, head(sort(counts(V1), decreasing = TRUE), 1)])
}

most_effective <- function(dt) {
  names(dt[year == max(year),
            .(
              # cpp_cml = sum(cpp_chd_cml + cpp_stroke_cml + cpp_t2dm_cml + cpp_af_cml + cpp_lc_cml)
              cpp_cml = sum(net_utility_cml)

            ), by = .(.id, friendly_name)
            ][, friendly_name[which.max(cpp_cml)], by = .id][, head(sort(counts(V1), decreasing = TRUE), 1)])
}

most_equitable <- function(dt) {
  names(dt[year == max(year),
            .(
              sei_cml = mean(sei)
            ), by = .(.id, friendly_name)
            ][, friendly_name[which.max(sei_cml)], by = .id][, head(sort(counts(V1), decreasing = TRUE), 1)])

}

source(file.path("metamodel", "metamodel_predict.R"), local = TRUE)

# sum(gtools::rdirichlet(1, rep(1, 180))) # to simulate a 180 length vector that sums to 1
# extract_uptake_table(setDT(readRDS("./output/input_parameters.rds")), 2)[]
# To write when running from shiny server or docker see https://groups.google.com/forum/#!topic/shiny-discuss/srWETT6uL-I

# out <- read_fst(file.path("output", "out.fst"), as.data.table = TRUE)
# out[,
#     `:=` (
#       cpp_chd_cml     = round(cumsum(cpp_chd)),
#       cpp_stroke_cml  = round(cumsum(cpp_stroke)),
#       cpp_t2dm_cml    = round(cumsum(cpp_t2dm)),
#       cpp_af_cml      = round(cumsum(cpp_af)),
#       cpp_lc_cml      = round(cumsum(cpp_lc)),
#       dpp_chd_cml     = round(cumsum(dpp_chd)),
#       dpp_stroke_cml  = round(cumsum(dpp_stroke)),
#       dpp_other_cml   = round(cumsum(dpp_other)),
#       net_utility_cml = cumsum(net_utility),
#       net_cost_cml    = cumsum(net_cost)
#     ),
#     by = .(.id, friendly_name, qimd)
#     ][, nmb_cml := net_cost_cml + net_utility_cml * 20000]
#
#
# out[, sum(nmb_cml), by = .(.id, friendly_name, year)
#     ][, .(prop_if(V1 > 0)), by = .(friendly_name, year)
#       ][, V2 := predict(loess(V1 ~ year)), by = friendly_name]
#
# p <- plot_ly(out[, sum(nmb_cml), by = .(.id, friendly_name, year)
#             ][, .(prop_if(V1 > 0)), by = .(friendly_name, year)
#               ][, V2 := predict(loess(V1 ~ year)), by = friendly_name],
#              x = ~year, y = ~V2, color = ~ friendly_name, type = 'scatter',
#             mode = 'lines', line = list(shape = "spline", smoothing = 1.3)) %>%
#   add_lines(y = 0.8, name = "Decision aid", color = NULL,
#             line = list(color = "black", dash = "dot")) %>%
#   layout(
#     yaxis = list(title = "Probability of cost-effective policy", range = c(0, 1),
#                  tickformat= ",.0%"),
#     xaxis = list(title = "Year")
#   )
# p
