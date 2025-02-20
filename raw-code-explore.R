# ----------------------------------------
# Description:
#   This file contains rough exploratory
#   analysis code. It is not meant to be
#   run clean, and likely will not run
#   out of the box. Many dependencies
#   and libraries will be absent,
#   yet the bread crumbs here will be
#   sufficient for me to reproduce.
#   It is meant to be a historical record
#   to repeat some analysis down the road
# Author:
#   Stu Field
# ----------------------------------------

library(helpr)
library(wranglr)
library(dplyr)
library(tibble)
library(tidyr)
library(parsnip)
library(ggplot2)
library(patchwork)


# Data Prep ----
codebook <- readRDS("data/codebook.rds")
people <- read.csv("data/lahman_people.csv", header = TRUE) |> as_tibble()
savant_data <- readRDS("data/savant_data_2021_2023.rds")
ids <- read.csv("data/sample_submission.csv")$PLAYER_ID

playtime_by_player <- function(data, ...) {
  pos <- substitute(...)
  dplyr::group_by(data, ...) |>
    dplyr::summarise() |>
    dplyr::ungroup() |>
    dplyr::group_by(!!pos, game_year) |>
    dplyr::summarise(playing_time = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!pos) |>
    dplyr::summarise(playing_time = round(mean(playing_time)))
}

init_batter <- playtime_by_player(savant_data, batter, game_year,
                                  game_pk, at_bat_number) |>
  dplyr::filter(batter %in% ids)

init_pitcher <- playtime_by_player(savant_data, pitcher, game_year,
                                   game_pk, at_bat_number) |>
  dplyr::filter(pitcher %in% ids)

feat_by_batter <- function(x) {
  var <- rlang::sym(x)
  dplyr::group_by(savant_data, batter) |>
    tidyr::drop_na(!!var) |>
    dplyr::summarise(!!var := mean(!!var))
}

feat_by_pitcher <- function(x) {
  var <- rlang::sym(x)
  dplyr::group_by(savant_data, pitcher) |>
    tidyr::drop_na(!!var) |>
    dplyr::summarise(!!var := mean(!!var))
}

# vars from the people data set
people_vars <- c("birthCountry", "bats", "throws", "debut", "birthDate")
types <- enframe(sapply(savant_data, class), name = "feature", value = "class")
all_feats <- dplyr::filter(types, class != "character")$feature |>
  setdiff("sv_id")   # sv_id is uninformative

batter_vars  <- lapply(all_feats, feat_by_batter)
pitcher_vars <- lapply(all_feats, feat_by_pitcher)

batter_playtime <- be_hard(dplyr::left_join, by = "batter") |>
  Reduce(batter_vars, init_batter) |>
  imputeNAs() |>
  dplyr::filter(batter %in% ids) |>
  dplyr::left_join(people, by = c("batter" = "player_mlb_id")) |>
  # not all IDs for all features; impute where missing
  dplyr::select(-playerID_LAHMAN) |>
  dplyr::rename(player_id = "batter")

pitcher_playtime <- be_hard(dplyr::left_join, by = "pitcher") |>
  Reduce(pitcher_vars, init_pitcher) |>
  imputeNAs() |>
  dplyr::filter(pitcher %in% ids) |>
  dplyr::left_join(people, by = c("pitcher" = "player_mlb_id")) |>
  # not all IDs for all features; impute where missing
  dplyr::select(-playerID_LAHMAN) |>
  dplyr::rename(player_id = "pitcher")


# Combine pitcher and batter data
playtime_data <- list(
  pitcher = pitcher_playtime,
  batter  = batter_playtime
)

for ( i in people_vars ) {
  playtime_data$pitcher[[i]] <- factor(playtime_data$pitcher[[i]])
  playtime_data$batter[[i]] <- factor(playtime_data$batter[[i]])
}

# saveRDS(playtime_data, file = "data/playtime-data.rds")


# Start here ----

playtime_data <- readRDS("data/playtime-data.rds")
feats <- setdiff(names(playtime_data$pitcher),
                       c("player_id", "sv_id", "playing_time"))


# stability selection ----
ss <- stability_selection(
  center_scale(playtime_data$pitcher[, all_feats]),
  playtime_data$pitcher$playing_time,
  kernel = "lasso",
)

plot(ss)

uni_tbl <- calc_univariate(center_scale(playtime_data$pitcher),
                           var = "playing_time", test = "cor")

feat2 <- c("times_faced", "post_bat_score",
           "bat_score", "woba_value", "game_pk")

x <- center_scale(playtime_data$pitcher) |> feature_matrix(feat2)
y <- log(playtime_data$pitcher$playing_time)
model <- glmnet::glmnet(x, y, family = "gaussian")
preds <- predict(model, newx = x, type = "response", s = 0)[, 1L]
rmse <- sqrt(mean((exp(y) - exp(preds))^2))
rmse

ccc <- calc_ccc(preds, y)
ccc

SomaPlotr::plotConcord(
  playtime_data$pitcher$playing_time,
  exp(preds)
)


# parsnip pkg ----
library(parsnip)
library(broom)
coef_path_values <- c(0, 10^seq(-5, 1, length.out = 7L))
fit <- linear_reg(penalty = 0) |>   # ridge
  set_engine("glmnet", path_values = coef_path_values) |>
  fit(playing_time ~ ., data = data.frame(playing_time = y, x))

tidy(fit)

pred2 <- predict(fit, data.frame(x), penalty = 0)$.pred
sqrt(mean((exp(pred2) - exp(y))^2))

SomaPlotr::plotConcord(
  playtime_data$pitcher$playing_time, exp(pred2)
)
