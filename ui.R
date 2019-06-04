#!/usr/bin/Rscript
## IMPACTncd: A decision support tool for primary prevention of NCDs
## Copyright (C) 2018  Chris Kypridemos
## IMPACTncd is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>
## or write to the Free Software Foundation, Inc., 51 Franklin Street,
## Fifth Floor, Boston, MA 02110-1301  USA.

myenv <- environment()
ui = tagList(
# shinyBS:::shinyBSDep, # for tooltips to work
shinyjs::useShinyjs(), # for conditional deactivating inputs
navbarPage(
theme = shinytheme("paper"), #cosmo
title = "workHORSE",
id = "inTabset",
# Welcome tab --------------------------
source(file.path("ui", "welcome_tab.R"), local = TRUE)$value,

# Simulation parameters tab --------------------------
source(file.path("ui", "simulation_parameters_tab.R"), local = TRUE)$value,

# Scenario parameters tab --------------------------
tabPanel("Scenario parameters",
  uiOutput("scenario_tabs"),
  source(file.path("ui", "scenario1_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario2_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario3_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario4_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario5_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario6_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario7_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario8_tab.R"), local = TRUE)$value,
  source(file.path("ui", "scenario9_tab.R"), local = TRUE)$value
),
# Output tab --------------------------
source(file.path("ui", "output_tab.R"), local = TRUE)$value,

#inputs = mythemeSelector(),

navbarMenu("More",
# Advance settings tab --------------------------
source(file.path("ui", "advanced_settings_tab.R"), local = TRUE)$value,

# Documentation tab --------------------------
source(file.path("ui", "documentation_tab.R"), local = TRUE)$value,

# About tab --------------------------
source(file.path("ui", "about_tab.R"), local = TRUE)$value
)),


# Add version number to the right
HTML("<script>var parent = document.getElementsByClassName('navbar-nav');
parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\">v0.9</a></li></ul>' );</script>")
)

