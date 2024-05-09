library(tidyverse)
library(baseballr)

load("data/player_df.rda")

load("data/ncaa_hitting_050624.rda")

df <- hit_df %>% 
  mutate(X1B = H - `2B` - `3B` - HR) %>% 
  select(player_name, Pos, team_name, conference, H, GP, GS,
         uBB = BB, X1B, X2B = `2B`, X3B = `3B`, HR, HBP, AB, SH, SF, SO = K, SB, CS, TB) %>% 
  summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), count = n()) %>% 
  mutate(season = 2024) %>% 
  ungroup() %>% 
  woba_plus() %>% 
  mutate(runs_created = ((H + uBB + HBP - CS) * (TB + 0.7*SB))/(AB + uBB + HBP + CS),
         PA = AB + uBB + HBP + SH + SF,
         lgOBP = round((H + uBB + HBP) / (AB + uBB + HBP + SF), digits = 3),
         lgSLG = round((X1B + 2*X2B + 3*X3B + 4*HR)/AB, digits = 3))

lgSLG <- df[["lgSLG"]]
lgOBP <- df[["lgOBP"]]

players_clean <- player_df %>% 
  group_by(team_name) %>% 
  mutate(max_gp = max(GP)) %>% 
  ungroup() %>% 
  mutate(qualify = if_else(PA/max_gp >= 2 & GP/max_gp >= .75, 1, 0)) %>% 
  filter(qualify == 1)
  

Statcast_MaxMetrics <- players_clean %>% 
  group_by(team_name, player_name) %>% 
  mutate(
    ba = round(H/AB, digits = 3),
    obp = round((H + uBB + HBP) / (AB + uBB + HBP + SF), digits = 3),
    slg = round((X1B + 2*X2B + 3*X3B + 4*HR)/AB, digits = 3),
    ops = obp + slg,
    bb_to_k = round((uBB/SO), digits = 3),
    runs_created = round(runs_created, digits = 0),
    babip = round((H - HR) / (AB - SO - HR + SF), digits = 3)
  ) %>% 
  mutate(
    ops_plus = round(100 * (obp/lgOBP + slg/lgSLG - 1), digits = 0),
    iso = round(slg - ba, digits = 3)
  ) %>% 
  ungroup() %>% 
  mutate(
    max_ba = round(max(ba, na.rm = T), digits = 3),
    max_obp = round(max(obp, na.rm = T), digits = 3),
    max_slg = round(max(slg, na.rm = T), digits = 3),
    max_ops = round(max(ops, na.rm = T), digits = 3),
    max_wOBA = round(max(wOBA, na.rm = T), digits = 3),
    max_wOBA_CON = round(max(wOBA_CON, na.rm = T), digits = 3),
    max_RC = round(max(runs_created, na.rm = T), digits = 0),
    max_BB_to_K = round(max(bb_to_k, na.rm = T), digits = 3),
    max_babip = round(max(babip, na.rm = T), digits = 3),
    max_ops_plus = round(max(ops_plus, na.rm = T), digits = 3),
    max_iso = round(max(iso, na.rm = T), digits = 3)
  ) %>% 
  ungroup()

SC_MaxMetrics <- Statcast_MaxMetrics %>% 
  ungroup() %>% 
  mutate(
    MaxBA_percentile = round(max(max_ba, na.rm = T), digits = 3),
    MaxOBP_percentile = round(max(max_obp, na.rm = T), digits = 3),
    MaxSLG_percentile = round(max(max_slg, na.rm = T), digits = 3),
    MaxOPS_percentile = round(max(max_ops, na.rm = T), digits = 3),
    MaxWOBA_percentile = round(max(max_wOBA, na.rm = T), digits = 3),
    MaxWOBA_CON_percentile = round(max(max_wOBA_CON, na.rm = T), digits = 3),
    MaxRC_percentile = round(max(max_RC, na.rm = T), digits = 0),
    MaxBB_to_K_percentile = round(max(max_BB_to_K, na.rm = T), digits = 3),
    MaxBABIP_percentile = round(max(max_babip, na.rm = T), digits = 3),
    MaxOPS_Plus_percentile = round(max(max_ops_plus, na.rm = T), digits = 3),
    MaxISO_percentile = round(max(max_iso, na.rm = T), digits = 3)
  )

Statcast_MaxBA_percentile <- SC_MaxMetrics %>%
  arrange(-ba) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(savant_y = "BA", type = "standard")

Statcast_MaxOBP_percentile <- SC_MaxMetrics %>%
  arrange(-obp) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(savant_y = "OBP", type = "standard")
Statcast_MaxSLG_percentile <- SC_MaxMetrics %>%
  arrange(-slg) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "SLG", type = "standard"
  )
Statcast_MaxOPS_percentile <- SC_MaxMetrics %>%
  arrange(-ops) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "OPS", type = "standard"
  )
Statcast_MaxWOBA_percentile <- SC_MaxMetrics %>%
  arrange(-wOBA) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "wOBA", type = "advanced"
  )
Statcast_MaxWOBA_CON_percentile <- SC_MaxMetrics %>%
  arrange(-wOBA_CON) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "wOBA_CON",  type = "advanced"
  )
Statcast_MaxRC_percentile <- SC_MaxMetrics %>%
  arrange(-runs_created) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "RC",  type = "advanced"
  )
Statcast_MaxBB_to_K_percentile <- SC_MaxMetrics %>%
  arrange(-bb_to_k) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "BB/K", type = "standard"
  )
Statcast_MaxBABIP_percentile <- SC_MaxMetrics %>%
  arrange(-babip) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "BABIP", type = "advanced"
  )
Statcast_MaxOPS_Plus_percentile <- SC_MaxMetrics %>%
  arrange(-ops_plus) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "OPS+",  type = "advanced"
  )
Statcast_MaxISO_percentile <- SC_MaxMetrics %>%
  arrange(-iso) %>%
  mutate(ranking = row_number()) %>%
  mutate(percentile = round(1 - (ranking / max(ranking, na.rm = TRUE)), digits = 2) * 100) %>%
  mutate(
    savant_y = "ISO",  type = "advanced"
  )

Statcast_Percentiles <- bind_rows(Statcast_MaxBA_percentile,
                                  Statcast_MaxOBP_percentile,
                                  Statcast_MaxSLG_percentile,
                                  Statcast_MaxOPS_percentile,
                                  Statcast_MaxWOBA_percentile,
                                  Statcast_MaxWOBA_CON_percentile,
                                  Statcast_MaxRC_percentile,
                                  Statcast_MaxBB_to_K_percentile,
                                  Statcast_MaxBABIP_percentile,
                                  Statcast_MaxOPS_Plus_percentile,
                                  Statcast_MaxISO_percentile)

SC_Percentiles_1 <- Statcast_Percentiles %>% 
  ungroup() %>% 
  group_by(player_name) %>% 
  summarise(
    percentile = round(mean(percentile), digits = 0)
  ) %>% 
  unique() %>% 
  ungroup() %>% 
  arrange(-percentile) %>% 
  mutate(ranking = row_number()) %>%
  mutate(savant_y = "Overall") %>% 
  arrange(-percentile)

SC_Percentiles <- bind_rows(Statcast_Percentiles, SC_Percentiles_1) %>% 
  group_by(player_name, savant_y) %>% 
  mutate(
    savant_label = case_when(
      savant_y == "BA" ~ ba,
      savant_y == "OBP" ~ obp,
      savant_y == "SLG" ~ slg,
      savant_y == "OPS" ~ ops,
      savant_y == "wOBA" ~ wOBA,
      savant_y == "wOBA_CON" ~ wOBA_CON,
      savant_y == "RC" ~ runs_created,
      savant_y == "BB/K" ~ bb_to_k,
      savant_y == "BABIP" ~ babip,
      savant_y == "OPS+" ~ ops_plus,
      savant_y == "ISO" ~ iso,
      savant_y == "Overall" ~ ranking
    )
  ) %>% 
  mutate(
    savant_y = factor(savant_y, levels = c("wOBA", "wOBA_CON", "BA", "SLG",
                                           "OBP", "OPS", "OPS+", "BABIP", "ISO",
                                           "RC", "BB/K", "Overall"))
  ) %>% 
  mutate(
    player_name = str_split(player_name, ", ", simplify = TRUE) %>% 
      apply(1, function(name) paste(name[2], name[1]))
  )

# Visualize ----
player_percentile_graph <- function(player_id) {
  
  graph_df <- SC_Percentiles %>% 
    filter(player_name == player_id, savant_y != "Overall", type == "advanced")
  
  graph_df %>% 
    ggplot(SC_Percentiles, mapping = aes(percentile, fct_rev(savant_y), color = percentile)) +
    xlim(-2.5, 110) +
    labs(x = NULL, y = NULL,
         title = "NCAA Percentile Rankings",
         subtitle = paste(graph_df$player_name, "-", graph_df$team_name)) +
    geom_segment(aes(x = 0, xend = 100, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "#B9CED0", size = 2) +
    geom_segment(aes(x = -2.5, y = fct_rev(savant_y), xend = percentile, yend = fct_rev(savant_y),
                     color = percentile), linewidth = 10) +
    geom_segment(aes(x = 4.5, xend = 5.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_segment(aes(x = 49.5, xend = 50.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_segment(aes(x = 94.5, xend = 95.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_point(aes(x = percentile, y = fct_rev(savant_y), fill = percentile),
               shape = 21, color = "white", size = 10, stroke = 2) +
    geom_line(aes(x = percentile, y = fct_rev(savant_y)), color = "#010101", size = 8) +
    geom_text(aes(label = percentile), color = "white", hjust = .5, vjust = .4,
              size = 3.5, fontface = "bold", family = "sans") + 
    geom_text(aes(x = 109, label = savant_label), color = "grey20", hjust = .5, vjust = .4,
              size = 3.5, fontface = "bold", family = "sans") + 
    scale_color_gradientn(colours = c("#2166ac", '#B9CED0', '#b2182b'),
                          limits = c(0, 100), na.value = "#2166ac") +
    scale_fill_gradientn(colours = c("#2166ac",'#B9CED0', '#b2182b'),
                         limits = c(0, 100), na.value = "#2166ac") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(color = "grey20", size = 15, face = "bold",
                                    vjust = 0, hjust = 0.5, family = "mono"),
          plot.subtitle = element_text(color = "grey20", size = 12, face = "italic",
                                       vjust = -1, hjust = 0.5, family = "mono"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 9, face = "bold.italic", colour = "grey20", hjust = 1),
          legend.position = "none")
  
}

player_percentile_graph("Vance Honeycutt")

player_percentile_graph("Stephen Hrustich")
player_percentile_graph("Alex Calarco")
player_percentile_graph("Vincent Bianchina")
player_percentile_graph("Vance Honeycutt")
player_percentile_graph("Travis Bazzana")
player_percentile_graph("Charlie Condon")

library(teamcolors)
ncaa_colors <- teamcolors %>% 
  filter(league == "ncaa") %>% 
  rename(team_name = location)

SC <- left_join(SC_Percentiles, ncaa_colors, by = c("team_name"))

team_player_percentile_graph <- function(team_id) {
  
  graph_df <- SC %>% 
    filter(team_name == team_id, savant_y != "Overall", type == "advanced") %>% 
    mutate(
      primary = if_else(is.na(primary), "navy", primary),
      secondary = if_else(is.na(secondary), "white", secondary)
    )
  
  graph_df %>% 
    ggplot(SC_Percentiles, mapping = aes(percentile, fct_rev(savant_y), color = percentile)) +
    xlim(-2.5, 115) +
    labs(x = NULL, y = NULL,
         title = paste("NCAA Offensive Percentile Rankings -", graph_df$team_name)) +
    geom_segment(aes(x = 0, xend = 100, y = fct_rev(savant_y), yend = (fct_rev(savant_y))),
                 color = "#B9CED0", size = 2) +
    geom_segment(aes(x = -2.5, y = fct_rev(savant_y), xend = percentile, yend = fct_rev(savant_y),
                     color = percentile), linewidth = 10) +
    geom_segment(aes(x = 4.5, xend = 5.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_segment(aes(x = 49.5, xend = 50.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_segment(aes(x = 94.5, xend = 95.5, y = fct_rev(savant_y), yend = fct_rev(savant_y)),
                 color = "white", size = 10, alpha = 0.25) +
    geom_point(aes(x = percentile, y = fct_rev(savant_y), fill = percentile),
               shape = 21, color = "white", size = 10, stroke = 2) +
    geom_line(aes(x = percentile, y = fct_rev(savant_y)), color = "#010101", size = 8) +
    geom_text(aes(label = percentile), color = "white", hjust = .5, vjust = .4,
              size = 4, fontface = "bold", family = "sans") + 
    geom_text(aes(x = 115, label = savant_label), color = "grey20", hjust = 1, vjust = .4,
              size = 3.5, fontface = "bold", family = "sans") + 
    scale_color_gradientn(colours = c("#2166ac", '#B9CED0', '#b2182b'),
                          limits = c(0, 100), na.value = "#2166ac") +
    scale_fill_gradientn(colours = c("#2166ac",'#B9CED0', '#b2182b'),
                         limits = c(0, 100), na.value = "#2166ac") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(color = graph_df$primary, size = 15, face = "bold",
                                    vjust = 0, hjust = 0.5, family = "mono"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 9, face = "bold.italic", colour = "grey20", hjust = 1),
          legend.position = "none",
          strip.background = element_rect(
            fill = graph_df$primary, color = graph_df$secondary, size=1, linetype="solid"
          ),
          strip.text = element_text(color = "white", family = "sans",
                                    face = "bold.italic"),
          panel.border = element_rect(color = graph_df$secondary, fill=NA, size=1),
          panel.spacing = unit(0.2, "lines")
    ) +
    facet_wrap(~player_name, ncol = 2)
  
}

team_player_percentile_graph("Illinois")
team_player_percentile_graph("Michigan")
team_player_percentile_graph("Harvard")
team_player_percentile_graph("Rutgers")
