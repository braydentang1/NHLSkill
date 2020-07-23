library(lavaan)
library(tidyverse)

model <- '
group: F
	level: 1
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel

		x_gf_60 ~~ scf_60_rel
		
	level: 2
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel
		
		x_gf_60 ~~ scf_60_rel
		total_points_60 ~~ def_gar_60 + x_ga_60

group: D
		level: 1
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60 
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel 
		
		x_gf_60 ~~ scf_60_rel
		
	level: 2
		off_skill =~ off_gar_60 + x_gf_60 + scf_60_rel + total_points_60
		def_skill =~ def_gar_60 + x_ga_60 + sca_60_rel
		
		x_gf_60 ~~ scf_60_rel
		total_points_60 ~~ def_gar_60 + x_ga_60
		
'

fit <- sem(model = model, data = all_data_processed, cluster = "player", group = "position")
test <- tibble(player = unique(all_data_processed %>% filter(position == "D") %>% select(player) %>% pull()), off_score = lavPredict(fit, level = 2)$D[, 1], def_score = lavPredict(fit, level = 2)$D[, 2])