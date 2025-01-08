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
library(tidyr)
library(purrr)
library(parsnip)
library(ggplot2)
library(patchwork)
library(randomForest)
library(nlme)
library(withr)
nym_blue <- "#002D72"
nym_orange <- "#FF5910"


# Question 1 ----

codebook <- readRDS("data/codebook.rds")
people <- read.csv("data/lahman_people.csv", header = TRUE) |>
  tibble::as_tibble()
savant_data <- readRDS("data/savant_data_2021_2023.rds")



playtime_by_batter <- function(data, ...) {
  dplyr::group_by(data, ...) |>
    dplyr::summarise() |>
    dplyr::ungroup() |>
    dplyr::group_by(batter, game_year) |>
    dplyr::summarise(playing_time = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(batter) |>
    dplyr::summarise(playing_time = round(mean(playing_time)))
}

init <- playtime_by_batter(savant_data, batter, game_year,
                           game_pk, at_bat_number)


feat_by_batter <- function(x) {
  var <- rlang::sym(x)
  dplyr::group_by(savant_data, batter) |>
    tidyr::drop_na(!!var) |>
    dplyr::summarise(!!var := mean(!!var))
}

ft_data <- lapply(feats, feat_by_batter)
ids <- read.csv("data/sample_submission.csv")$PLAYER_ID
playtime_data <- be_hard(dplyr::left_join, by = "batter") |>
  Reduce(ft_data, init) |>
  imputeNAs() |>
  dplyr::filter(batter %in% ids) |>
  dplyr::left_join(people, by = c("batter" = "player_mlb_id")) |>
  # not all IDs for all features; impute where missing
  dplyr::select(-playerID_LAHMAN)

playtime_data$birthCountry <- factor(playtime_data$birthCountry)
playtime_data$bats <- factor(playtime_data$bats)
playtime_data$throws <- factor(playtime_data$throws)
playtime_data$debut <- as.Date(playtime_data$debut)
playtime_data$birthDate <- as.Date(playtime_data$birthDate)

people_vars <- c("birthCountry", "bats", "throws",
                 "debut", "birthDate")
sapply(playtime_data[, people_vars], function(.x) any(is.na(.x)))

types <- tibble::enframe(sapply(playtime_data, class),
                         name = "feature",
                         value = "class")

feats <- setdiff(types$feature, c("playing_time", "batter"))

# stability selection ----
ss <- stability_selection(
  center_scale(playtime_data[, feats]),
  playtime_data$playing_time,
  kernel = "lasso"
)

plot(ss)

uni_tbl <- calc_univariate(center_scale(playtime_data),
                           var = "playing_time", test = "lm")

feat2 <- c("times_faced", "iso_value", "hit_location",
           "launch_speed_angle", "pitcher_at_bat_number",
           "delta_run_exp", "zone")

x <- center_scale(playtime_data) |> feature_matrix(feat2)
y <- log(playtime_data$playing_time)
model <- glmnet::glmnet(x, y, family = "gaussian")
preds <- predict(model, newx = x, type = "response", s = 0)[, 1L]
rmse <- sqrt(mean((exp(y) - exp(preds))^2))
rmse
ccc <- calc_ccc(preds, y)
ccc
output <- data.frame(
  actual = playtime_data$playing_time,
  predicted = exp(preds)
)

plot(output); abline(a = 0, b = 1, col = "red")

coef_path_values <- c(0, 10^seq(-5, 1, length.out = 7L))
fit <- linear_reg(penalty = 1) |>   # ridge
  set_engine("glmnet", path_values = coef_path_values) |>
  fit(playing_time ~ ., data = data.frame(playing_time = y, x))

broom::tidy(fit)
pred2 <- predict(fit, data.frame(x), penalty = 0)$.pred
pred2 <- predict(fit, data.frame(x))$.pred
sqrt(mean((pred2 - y)^2))
plot(y, pred2)
abline(a = 0, b = 1, col = 'red')
plot(preds, pred2)
abline(a = 0, b = 1, col = 'red')


# Feature selection ----
mt <- modelType_glm(response = "is_strike")
sm <- searchType_backwardModel()
sm <- searchType_forwardModel()
fs <- featureSelection(head(pitch_data, 1000), candidate.markers = all_feat,
                       model.type = mt, search.type = sm,
                       cost = "AUC", strat.column = "is_strike",
                       runs = 2, folds = 5)

fs_search <- Search(fs, num.cores = 2)
plot(fs_search)

# glmnet ----
glmfit <- glmnet::glmnet(
  x = pitch_data[, all_feat],
  y = as.factor(pitch_data$is_strike),
  alpha = 1,
  family = "binomial"
)
getModelCoef(glmfit)
coef(glmfit)


# stability selection ----
ss <- stabilitySelection(
  pitch_data[, all_feat],
  pitch_data$is_strike,
  num.iter = 250,
  parallel = TRUE
)
plot(ss)


# PCA ----
pca_data <- pitch_data
pca_data$is_strike <- as.factor(pca_data$is_strike)
int <- names(which(sapply(pca_data, is.integer)))
num <- names(which(sapply(pca_data, is.numeric))[-1L])
ft  <- setdiff(num, int)
for ( i in ft ) {
  pca_data[[i]] <- scale(pca_data[[i]], TRUE, FALSE)[, 1L]
}
for ( i in int ) {
  pca_data[[i]] <- as.factor(pca_data[[i]])
}
pca <- pca(pca_data, features = ft)
rot <- plot(pca, "r", identify = TRUE)
proj <- plot(pca, "p", color = is_lhp)
rot + proj
plotScree(pca)


# Fitting
feats <- c("strikes",
           "balls",
           "plate_location_z",
           "plate_location_x")


# logistic regression
form    <- as.formula(paste("is_strike ~", paste(feat, collapse = "+")))
lr_fit  <- stats::glm(form, family = "binomial", data = pitch_data)
lr_pred <- predict(lr_fit, newdata = pitch_data, type = "response")
calc_perf(pitch_data$is_strike, lr_pred)

# Naive Bayes
nb_fit <- e1071::naiveBayes(
  as.matrix(pitch_data[, feat]),
  as.factor(pitch_data$is_strike)
)
nb_pred <- predict(nb_fit, newdata = pitch_data[, feat], type = "raw")[, 2L]
calc_perf(pitch_data$is_strike, nb_pred)

# randomForest
rf_fit <- randomForest::randomForest(
  as.matrix(pitch_data[, feat]),
  as.factor(pitch_data$is_strike)
)
rf_pred <- predict(rf_fit, newdata = pitch_data[, feat], type = "prob")[, 2L]
calc_perf(pitch_data$is_strike, rf_pred)


pitch_data |>
  ggplot(aes(x = as.factor(is_strike), y = strikes)) +
  geom_boxplot()



# Question 2 ----
pitcher_so_data <- readRDS("questionnaire_data/pitcher_strikeout_data.rds")

pitcher_analysis <- tidyr::nest(pitcher_so_data, data = everything(),
                                .by = pitcher_id) |>
  dplyr::mutate(
    data  = setNames(data, pitcher_id),       # add names for downstream use
    data2 = map(data, function(.x) {
      dplyr::group_by(.x, year) |>
        dplyr::summarise(
          age        = min(age),
          mean_stuff = mean(stuff),
          walk_rate  = mean(is_walk),
          so_rate    = mean(is_strikeout),
          n          = n()
        )
    }),
    mean_stuff = map_dbl(data, ~ mean(.x$stuff)),         # all pitchers
    so_prop    = map_dbl(data, ~ mean(.x$is_strikeout)),  # all pitchers
    age_2025   = map_dbl(data2, ~ max(.x$age) + 1),
    lm_model   = map(data2, ~ stats::lm(mean_stuff ~ age, data = .x)),
    pred_stuff_2025_lm = map2_dbl(lm, age_2025, ~ predict(.x, data.frame(age = .y)))
  )


## does SO rate correlate with stuff? ----
# stuff vs SO
pitcher_analysis |>
  ggplot(aes(x = mean_stuff, y = so_prop)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method = "loess")

## strikeout linear model ----
so_model <- lm(so_prop ~ mean_stuff, data = pitcher_analysis)

pitcher_analysis <- pitcher_analysis |>
  dplyr::mutate(
    pred_so_2025_lm = predict(so_model,
                              data.frame(mean_stuff = pred_stuff_2025_lm))
  )

## boxplot: age vs stuff ----
pitcher_analysis$data[[100]] |>
  ggplot(aes(x = as.factor(age), y = stuff)) +
  geom_boxplot()


# what about temporal autocorrelation?
# Mixed-effects
fit_data <- dplyr::bind_rows(pitcher_analysis$data2, .id = "pitcher_id")
fit <- nlme::lme(mean_stuff ~ age, random = ~ 1 + age | pitcher_id,
                 data = fit_data)
summary(fit)


# Can we fit so rate directly?
fit2 <- nlme::lme(so_rate ~ age, random = ~ 1 + age | pitcher_id,
                  data = fit_data)
summary(fit2)


# plot "stuff" trajectories
fit_data |>
  ggplot(aes(x = age, y = mean_stuff, group = pitcher_id, colour = pitcher_id)) +
  geom_line() +
  geom_point(size = 2, shape = 19) +
  ylab("mean stuff") +
  theme(legend.position = "none") +
  ggtitle("Pitcher specific 'stuff' trajectory")

# plot "so rate" trajectories
fit_data |>
  ggplot(aes(x = age, y = so_rate, group = pitcher_id, colour = pitcher_id)) +
  geom_line() +
  geom_point(size = 2, shape = 19) +
  ylab("mean so rate") +
  theme(legend.position = "none") +
  ggtitle("Pitcher specific 'so_rate' trajectory")


pitcher_analysis <- pitcher_analysis |>
  dplyr::mutate(
    pred_so_2025_lme2 = as.numeric(
      predict(fit2, newdata = data.frame(age = age_2025, pitcher_id = pitcher_id))
    )
  )

coef(lme_fit) |>
  rownames_to_column("pitcher_id") |>
  dplyr::arrange(age) |>
  dplyr::select(pitcher_id, slope = age) |>
  as_tibble() |>
  head()

tmp <- pitcher_analysis |>
  dplyr::mutate(
    slope = map(lm_model, ~ coef(.x)["age"])
  )
lm_slope <- tmp$slope
lme_slope <- coef(lme_fit)[names(lm_slope), "age"]

# compare slopes lm vs lme
data.frame(lm_slope = as.numeric(lm_slope), lme_slope) |>
  ggplot(aes(x = lm_slope, y = lme_slope)) +
  geom_point(size = 2, shape = 21, color = nym_orange, fill = nym_blue) +
  geom_abline(slope = 1)

