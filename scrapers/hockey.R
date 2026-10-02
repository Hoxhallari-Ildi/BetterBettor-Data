conda_zoneinfo <- file.path(Sys.getenv("CONDA_PREFIX"), "share", "zoneinfo")
if (nzchar(Sys.getenv("CONDA_PREFIX")) && dir.exists(conda_zoneinfo)) Sys.setenv(TZDIR = conda_zoneinfo)
Sys.setenv(TZ = "UTC")

suppressPackageStartupMessages({
  library(fastRhockey)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(janitor)
})

current_nhl_season <- function() {
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%m"))
  if (month >= 10) year else year - 1
}

DEFAULT_TEAM <- "BOS"
DEFAULT_N_MATCHES <- 3
DEFAULT_OPPONENT_N <- 10
DEFAULT_SEASON <- current_nhl_season()

safe_numeric <- function(x) suppressWarnings(as.numeric(x))
safe_character <- function(x) as.character(x)

first_existing_column <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) return(NA_character_)
  found[[1]]
}

get_column <- function(df, candidates, default = NA) {
  col <- first_existing_column(df, candidates)
  if (is.na(col)) return(rep(default, nrow(df)))
  df[[col]]
}

nhl_season_string <- function(season) {
  season <- as.integer(season)
  paste0(season, season + 1)
}

expected_points_nhl <- function(xgf, xga, max_goals = 12) {
  xgf <- safe_numeric(xgf)
  xga <- safe_numeric(xga)
  if (length(xgf) == 0 || length(xga) == 0 || is.na(xgf) || is.na(xga)) return(NA_real_)
  xgf <- max(0, xgf)
  xga <- max(0, xga)
  goals <- 0:max_goals
  home_probs <- dpois(goals, xgf)
  away_probs <- dpois(goals, xga)
  home_win <- 0
  regulation_tie <- 0
  for (i in seq_along(goals)) {
    for (j in seq_along(goals)) {
      probability <- home_probs[i] * away_probs[j]
      if (goals[i] > goals[j]) home_win <- home_win + probability
      if (goals[i] == goals[j]) regulation_tie <- regulation_tie + probability
    }
  }
  2 * home_win + regulation_tie
}

get_team_games <- function(team, season) {
  season <- as.integer(season)
  team <- toupper(trimws(team))
  message("Getting NHL games for ", team, " season ", nhl_season_string(season), "...")
  games <- tryCatch(
    nhl_game_ids_by_season(season = season, team_abbr = team),
    error = function(e) {
      message("Unable to retrieve games for ", team, " / ", season, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(games) || nrow(as.data.frame(games)) == 0) return(tibble())
  as.data.frame(games)
}

normalize_game_list <- function(games, team, season) {
  if (is.null(games) || nrow(games) == 0) return(tibble())
  games <- as.data.frame(games)
  names(games) <- janitor::make_clean_names(names(games))
  id_col <- first_existing_column(games, c("game_id", "id"))
  date_col <- first_existing_column(games, c("game_date", "date", "game_date_time", "start_time_utc", "start_time"))
  home_col <- first_existing_column(games, c("home_team", "home_team_abbrev", "home_team_abbr", "home_abbr"))
  away_col <- first_existing_column(games, c("away_team", "away_team_abbrev", "away_team_abbr", "away_abbr"))
  if (is.na(id_col)) stop("Could not identify game ID column.")
  if (is.na(date_col)) stop("Could not identify game date column.")
  game_id <- as.character(games[[id_col]])
  raw_dates <- as.character(games[[date_col]])
  date <- suppressWarnings(as.Date(substr(raw_dates, 1, 10)))
  home <- if (!is.na(home_col)) toupper(as.character(games[[home_col]])) else rep(NA_character_, nrow(games))
  away <- if (!is.na(away_col)) toupper(as.character(games[[away_col]])) else rep(NA_character_, nrow(games))
  team <- toupper(trimws(team))
  h_a <- ifelse(home == team, "H", ifelse(away == team, "A", NA_character_))
  opponent <- ifelse(home == team, away, ifelse(away == team, home, NA_character_))
  tibble(game_id = game_id, date = date, season = as.integer(season), team = team, opponent = opponent, h_a = h_a) %>%
    filter(!is.na(game_id), !is.na(date), !is.na(opponent), !is.na(h_a)) %>%
    distinct(game_id, .keep_all = TRUE)
}

get_last_n_matches <- function(team, season, n_matches, before_date = NULL) {
  team <- toupper(trimws(team))
  season <- as.integer(season)
  n_matches <- as.integer(n_matches)
  message("Getting last ", n_matches, " games for ", team, "...")
  seasons <- season:(season - 1)
  all_games <- lapply(seasons, function(s) get_team_games(team, s))
  all_games <- all_games[vapply(all_games, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
  if (length(all_games) == 0) return(tibble())
  games <- bind_rows(lapply(seq_along(all_games), function(i) normalize_game_list(all_games[[i]], team, seasons[[i]])))
  if (nrow(games) == 0) return(tibble())
  if (is.null(before_date)) {
    games <- games %>% filter(date <= Sys.Date())
  } else {
    games <- games %>% filter(date < as.Date(before_date))
  }
  games %>% arrange(desc(date)) %>% slice_head(n = n_matches) %>% arrange(date)
}

get_previous_matches <- function(team, season, n_matches, before_date) {
  get_last_n_matches(team = team, season = season, n_matches = n_matches, before_date = before_date)
}

get_game_pbp <- function(game_id) {
  game_id <- as.numeric(game_id)
  pbp <- tryCatch(
    nhl_game_pbp(game_id = game_id),
    error = function(e) {
      message("PBP error for game ", game_id, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(pbp) || nrow(as.data.frame(pbp)) == 0) return(tibble())
  pbp <- as.data.frame(pbp)
  names(pbp) <- janitor::make_clean_names(names(pbp))
  as_tibble(pbp)
}

normalize_pbp <- function(pbp) {
  if (is.null(pbp) || nrow(pbp) == 0) return(tibble())
  pbp <- as_tibble(pbp)
  names(pbp) <- janitor::make_clean_names(names(pbp))
  if (!"event_type" %in% names(pbp)) stop("PBP does not contain event_type.")
  pbp <- pbp %>% mutate(event_type_clean = toupper(as.character(event_type)))
  team_col <- first_existing_column(pbp, c("event_team_abbr", "event_team", "team_abbr", "team"))
  player_id_col <- first_existing_column(pbp, c("event_player_1_id", "player_id", "event_player_id"))
  player_name_col <- first_existing_column(pbp, c("event_player_1_name", "player_name", "event_player_name"))
  xg_col <- first_existing_column(pbp, c("xg"))
  period_col <- first_existing_column(pbp, c("period_type", "period"))
  strength_col <- first_existing_column(pbp, c("strength_state", "strength"))
  if (is.na(team_col)) pbp$event_team_clean <- NA_character_ else pbp$event_team_clean <- toupper(as.character(pbp[[team_col]]))
  if (is.na(player_id_col)) pbp$player_id_clean <- NA_character_ else pbp$player_id_clean <- as.character(pbp[[player_id_col]])
  if (is.na(player_name_col)) pbp$player_clean <- NA_character_ else pbp$player_clean <- as.character(pbp[[player_name_col]])
  if (is.na(xg_col)) pbp$xg_clean <- NA_real_ else pbp$xg_clean <- safe_numeric(pbp[[xg_col]])
  if (is.na(period_col)) pbp$period_clean <- NA_character_ else pbp$period_clean <- as.character(pbp[[period_col]])
  if (is.na(strength_col)) pbp$strength_clean <- NA_character_ else pbp$strength_clean <- as.character(pbp[[strength_col]])
  pbp
}

build_team_event_metrics <- function(pbp, team, opponent) {
  pbp <- normalize_pbp(pbp)
  if (nrow(pbp) == 0) return(tibble())
  team <- toupper(team)
  opponent <- toupper(opponent)
  shot_events <- pbp$event_type_clean %in% c("SHOT", "GOAL")
  fenwick_events <- pbp$event_type_clean %in% c("SHOT", "GOAL", "MISSED_SHOT")
  corsi_events <- pbp$event_type_clean %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT")
  goal_events <- pbp$event_type_clean == "GOAL"
  missed_events <- pbp$event_type_clean == "MISSED_SHOT"
  blocked_events <- pbp$event_type_clean == "BLOCKED_SHOT"
  penalty_events <- pbp$event_type_clean == "PENALTY"
  five_v_five <- str_detect(toupper(pbp$strength_clean), "5V5|EVEN")
  team_shots <- shot_events & pbp$event_team_clean == team
  opp_shots <- shot_events & pbp$event_team_clean == opponent
  team_fenwick <- fenwick_events & pbp$event_team_clean == team
  opp_fenwick <- fenwick_events & pbp$event_team_clean == opponent
  team_corsi <- corsi_events & pbp$event_team_clean == team
  opp_corsi <- corsi_events & pbp$event_team_clean == opponent
  team_goals <- goal_events & pbp$event_team_clean == team
  opp_goals <- goal_events & pbp$event_team_clean == opponent
  team_xg <- sum(pbp$xg_clean[team_fenwick], na.rm = TRUE)
  opp_xg <- sum(pbp$xg_clean[opp_fenwick], na.rm = TRUE)
  team_corsi_5v5 <- sum(team_corsi & five_v_five, na.rm = TRUE)
  opp_corsi_5v5 <- sum(opp_corsi & five_v_five, na.rm = TRUE)
  team_fenwick_5v5 <- sum(team_fenwick & five_v_five, na.rm = TRUE)
  opp_fenwick_5v5 <- sum(opp_fenwick & five_v_five, na.rm = TRUE)
  team_xg_5v5 <- sum(pbp$xg_clean[team_fenwick & five_v_five], na.rm = TRUE)
  opp_xg_5v5 <- sum(pbp$xg_clean[opp_fenwick & five_v_five], na.rm = TRUE)
  pp_for <- team_corsi & str_detect(toupper(pbp$strength_clean), "5V4|5V3|4V3")
  pk_against <- opp_corsi & str_detect(toupper(pbp$strength_clean), "4V5|3V5|3V4")
  pp_opportunities <- sum(penalty_events & pbp$event_team_clean == opponent, na.rm = TRUE)
  pk_opportunities <- sum(penalty_events & pbp$event_team_clean == team, na.rm = TRUE)
  team_pp_goals <- sum(team_goals & str_detect(toupper(pbp$strength_clean), "5V4|5V3|4V3"), na.rm = TRUE)
  opp_pp_goals <- sum(opp_goals & str_detect(toupper(pbp$strength_clean), "5V4|5V3|4V3"), na.rm = TRUE)
  tibble(
    team = team,
    S = sum(team_shots, na.rm = TRUE),
    SA = sum(opp_shots, na.rm = TRUE),
    GF = sum(team_goals, na.rm = TRUE),
    GA = sum(opp_goals, na.rm = TRUE),
    xGF = team_xg,
    xGA = opp_xg,
    missed_shots = sum(missed_events & pbp$event_team_clean == team, na.rm = TRUE),
    missed_shots_against = sum(missed_events & pbp$event_team_clean == opponent, na.rm = TRUE),
    blocked_shot_attempts = sum(blocked_events & pbp$event_team_clean == team, na.rm = TRUE),
    blocked_shot_attempts_against = sum(blocked_events & pbp$event_team_clean == opponent, na.rm = TRUE),
    Corsi = sum(team_corsi, na.rm = TRUE),
    Corsi_Against = sum(opp_corsi, na.rm = TRUE),
    Fenwick = sum(team_fenwick, na.rm = TRUE),
    Fenwick_Against = sum(opp_fenwick, na.rm = TRUE),
    Corsi_5v5 = team_corsi_5v5,
    Corsi_Against_5v5 = opp_corsi_5v5,
    Fenwick_5v5 = team_fenwick_5v5,
    Fenwick_Against_5v5 = opp_fenwick_5v5,
    xGF_5v5 = team_xg_5v5,
    xGA_5v5 = opp_xg_5v5,
    PP_GF = team_pp_goals,
    PP_GF_against = opp_pp_goals,
    PP_opportunities = pp_opportunities,
    PK_opportunities = pk_opportunities,
    PK_GA = opp_pp_goals,
    penalties_taken = sum(penalty_events & pbp$event_team_clean == team, na.rm = TRUE),
    penalties_drawn = sum(penalty_events & pbp$event_team_clean == opponent, na.rm = TRUE)
  )
}

build_player_context <- function(pbp, game_id, date, season, target_team, target_opponent, h_a) {
  pbp <- normalize_pbp(pbp)
  if (nrow(pbp) == 0) return(tibble())
  relevant <- pbp %>% filter(event_type_clean %in% c("SHOT", "GOAL", "MISSED_SHOT", "BLOCKED_SHOT"))
  if (nrow(relevant) == 0) return(tibble())
  player_base <- relevant %>%
    filter(!is.na(player_id_clean), player_id_clean != "") %>%
    group_by(event_team_clean, player_id_clean, player_clean) %>%
    summarise(
      S = sum(event_type_clean %in% c("SHOT", "GOAL"), na.rm = TRUE),
      goals = sum(event_type_clean == "GOAL", na.rm = TRUE),
      xG = sum(xg_clean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(team = event_team_clean, player_id = player_id_clean, player = player_clean)
  goal_events <- pbp %>% filter(event_type_clean == "GOAL")
  assist_rows <- list()
  if (nrow(goal_events) > 0) {
    for (i in seq_len(nrow(goal_events))) {
      ids <- c(
        get_column(goal_events[i, , drop = FALSE], c("event_player_2_id"), NA_character_)[[1]],
        get_column(goal_events[i, , drop = FALSE], c("event_player_3_id"), NA_character_)[[1]]
      )
      names <- c(
        get_column(goal_events[i, , drop = FALSE], c("event_player_2_name"), NA_character_)[[1]],
        get_column(goal_events[i, , drop = FALSE], c("event_player_3_name"), NA_character_)[[1]]
      )
      keep <- !is.na(ids) & ids != ""
      if (any(keep)) {
        for (j in which(keep)) {
          assist_rows[[length(assist_rows) + 1]] <- tibble(
            player_id = as.character(ids[[j]]),
            player = as.character(names[[j]]),
            assists = 1,
            xA = safe_numeric(goal_events$xg_clean[[i]]) / sum(keep)
          )
        }
      }
    }
  }
  assists_df <- if (length(assist_rows) > 0) {
    bind_rows(assist_rows) %>%
      group_by(player_id, player) %>%
      summarise(assists = sum(assists), xA = sum(xA, na.rm = TRUE), .groups = "drop")
  } else {
    tibble(player_id = character(), player = character(), assists = numeric(), xA = numeric())
  }
  player_base %>%
    left_join(assists_df, by = c("player_id", "player")) %>%
    mutate(
      game_id = as.character(game_id),
      date = as.Date(date),
      season = as.integer(season),
      opponent = ifelse(team == toupper(target_team), toupper(target_opponent), toupper(target_team)),
      h_a = ifelse(team == toupper(target_team), h_a, ifelse(h_a == "H", "A", "H")),
      assists = coalesce(assists, 0),
      xA = coalesce(xA, 0),
      points = goals + assists,
      xG_involvement = xG + xA,
      xGChain = xG_involvement,
      xGBuildup = pmax(0, xGChain - xG),
      shooting_pct = ifelse(S > 0, goals / S, NA_real_),
      position = NA_character_,
      TOI = NA_real_,
      EV_TOI = NA_real_,
      PP_TOI = NA_real_,
      PK_TOI = NA_real_,
      plus_minus = NA_real_,
      PIM = NA_real_,
      hits = NA_real_,
      blocked_shots = NA_real_,
      takeaways = NA_real_,
      giveaways = NA_real_,
      shifts = NA_real_,
      faceoff_win_pct = NA_real_
    ) %>%
    select(
      game_id, date, season, team, opponent, player_id, player, position, h_a,
      TOI, EV_TOI, PP_TOI, PK_TOI, goals, assists, points, S, shooting_pct,
      xG, xA, xGChain, xGBuildup, xG_involvement, plus_minus, PIM, hits,
      blocked_shots, takeaways, giveaways, shifts, faceoff_win_pct
    )
}

build_game_metrics <- function(pbp, match) {
  game_id <- as.character(match$game_id[[1]])
  date <- as.Date(match$date[[1]])
  season <- as.integer(match$season[[1]])
  team <- toupper(as.character(match$team[[1]]))
  opponent <- toupper(as.character(match$opponent[[1]]))
  h_a <- as.character(match$h_a[[1]])
  all_team_metrics <- build_team_event_metrics(pbp, team, opponent)
  if (nrow(all_team_metrics) == 0) stop("No team metrics generated for game ", game_id)
  own <- all_team_metrics %>% filter(.data$team == team)
  if (nrow(own) == 0) stop("Could not find ", team, " in PBP for game ", game_id)
  own <- own[1, , drop = FALSE]
  xpts <- expected_points_nhl(own$xGF[[1]], own$xGA[[1]])
  baseline <- own %>%
    mutate(
      game_id = game_id,
      date = date,
      season = season,
      opponent = opponent,
      h_a = h_a,
      xPts = xpts,
      xG_diff = xGF - xGA,
      goal_diff = GF - GA,
      shot_diff = S - SA,
      shooting_pct = ifelse(S > 0, GF / S, NA_real_),
      save_pct_proxy = ifelse(SA > 0, 1 - GA / SA, NA_real_),
      Corsi_pct = ifelse(Corsi + Corsi_Against > 0, Corsi / (Corsi + Corsi_Against), NA_real_),
      Fenwick_pct = ifelse(Fenwick + Fenwick_Against > 0, Fenwick / (Fenwick + Fenwick_Against), NA_real_),
      Corsi_diff = Corsi - Corsi_Against,
      Fenwick_diff = Fenwick - Fenwick_Against,
      xGF_5v5_diff = xGF_5v5 - xGA_5v5,
      Corsi_pct_5v5 = ifelse(Corsi_5v5 + Corsi_Against_5v5 > 0, Corsi_5v5 / (Corsi_5v5 + Corsi_Against_5v5), NA_real_),
      Fenwick_pct_5v5 = ifelse(Fenwick_5v5 + Fenwick_Against_5v5 > 0, Fenwick_5v5 / (Fenwick_5v5 + Fenwick_Against_5v5), NA_real_),
      xGF_pct_5v5 = ifelse(xGF_5v5 + xGA_5v5 > 0, xGF_5v5 / (xGF_5v5 + xGA_5v5), NA_real_),
      PP_pct = ifelse(PP_opportunities > 0, PP_GF / PP_opportunities, NA_real_),
      PK_pct = ifelse(PK_opportunities > 0, 1 - PK_GA / PK_opportunities, NA_real_)
    ) %>%
    select(
      game_id, date, season, team, opponent, h_a,
      S, GF, xGF, SA, GA, xGA, xPts, xG_diff, goal_diff, shot_diff,
      shooting_pct, save_pct_proxy,
      Corsi, Corsi_Against, Corsi_pct, Corsi_diff,
      Fenwick, Fenwick_Against, Fenwick_pct, Fenwick_diff,
      missed_shots, missed_shots_against,
      blocked_shot_attempts, blocked_shot_attempts_against,
      Corsi_5v5, Corsi_Against_5v5, Corsi_pct_5v5,
      Fenwick_5v5, Fenwick_Against_5v5, Fenwick_pct_5v5,
      xGF_5v5, xGA_5v5, xGF_5v5_diff, xGF_pct_5v5,
      penalties_taken, penalties_drawn,
      PP_GF, PP_opportunities, PP_pct,
      PK_GA, PK_opportunities, PK_pct
    )
  context <- build_player_context(
    pbp = pbp,
    game_id = game_id,
    date = date,
    season = season,
    target_team = team,
    target_opponent = opponent,
    h_a = h_a
  )
  list(baseline = baseline, context = context)
}

process_game <- function(match) {
  game_id <- as.character(match$game_id[[1]])
  message("Processing ", game_id, " (", as.character(match$date[[1]]), ")...")
  pbp <- get_game_pbp(game_id)
  if (nrow(pbp) == 0) stop("No PBP returned for game ", game_id)
  build_game_metrics(pbp, match)
}

get_last_n_match_baselines <- function(team, season, n_matches) {
  matches <- get_last_n_matches(team = team, season = season, n_matches = n_matches)
  if (nrow(matches) == 0) stop("No completed games found for ", team, ".")
  message("Found ", nrow(matches), " completed matches.")
  baseline_list <- list()
  context_list <- list()
  for (i in seq_len(nrow(matches))) {
    result <- tryCatch(
      process_game(matches[i, , drop = FALSE]),
      error = function(e) {
        message("Could not process ", matches$game_id[[i]], ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) {
      if (nrow(result$baseline) > 0) baseline_list[[length(baseline_list) + 1]] <- result$baseline
      if (nrow(result$context) > 0) context_list[[length(context_list) + 1]] <- result$context
    }
  }
  if (length(baseline_list) == 0) stop("No game baselines were successfully processed.")
  list(
    baseline = bind_rows(baseline_list) %>% arrange(date),
    context = if (length(context_list) > 0) bind_rows(context_list) %>% arrange(date, team, desc(xG)) else tibble(),
    matches = matches
  )
}

build_opponent_baseline <- function(opponent, season, n_matches, target_game_id, target_date, target_team) {
  opponent <- toupper(trimws(opponent))
  target_date <- as.Date(target_date)
  target_team <- toupper(trimws(target_team))
  message("Getting previous ", n_matches, " games for opponent ", opponent, " before ", target_date, "...")
  matches <- get_previous_matches(
    team = opponent,
    season = season,
    n_matches = n_matches,
    before_date = target_date
  )
  if (nrow(matches) == 0) return(tibble())
  opponent_list <- list()
  for (i in seq_len(nrow(matches))) {
    match_row <- matches[i, , drop = FALSE]
    game_id <- as.character(match_row$game_id[[1]])
    message("Opponent game ", game_id, " (", match_row$date[[1]], ")...")
    result <- tryCatch({
      pbp <- get_game_pbp(game_id)
      if (nrow(pbp) == 0) stop("No PBP returned.")
      game_result <- build_game_metrics(pbp, match_row)
      game_result$baseline %>%
        mutate(
          target_game_id = as.character(target_game_id),
          target_date = target_date,
          target_team = target_team,
          target_opponent = opponent
        ) %>%
        select(
          target_game_id, target_date, target_team, target_opponent,
          everything()
        )
    }, error = function(e) {
      message("Could not process opponent game ", game_id, ": ", conditionMessage(e))
      NULL
    })
    if (!is.null(result) && nrow(result) > 0) opponent_list[[length(opponent_list) + 1]] <- result
  }
  if (length(opponent_list) == 0) return(tibble())
  bind_rows(opponent_list) %>% arrange(date)
}

args <- commandArgs(trailingOnly = TRUE)

season <- DEFAULT_SEASON
team <- DEFAULT_TEAM
n_matches <- DEFAULT_N_MATCHES
opponent_n <- DEFAULT_OPPONENT_N

if (length(args) >= 1) {
  season <- suppressWarnings(as.integer(args[[1]]))
  if (is.na(season)) stop("Invalid season: ", args[[1]])
}

if (length(args) >= 2) team <- toupper(trimws(args[[2]]))

if (length(args) >= 3) {
  n_matches <- suppressWarnings(as.integer(args[[3]]))
  if (is.na(n_matches) || n_matches < 1) stop("n_matches must be a positive integer.")
}

if (length(args) >= 4) {
  opponent_n <- suppressWarnings(as.integer(args[[4]]))
  if (is.na(opponent_n) || opponent_n < 1) stop("opponent_n must be a positive integer.")
}

message("==============================================")
message(" BetterBettor NHL baseline scraper")
message("==============================================")
message("Season: ", season, "-", season + 1)
message("Team: ", team)
message("Recent team games: ", n_matches)
message("Opponent games: ", opponent_n)
message("")

results <- get_last_n_match_baselines(
  team = team,
  season = season,
  n_matches = n_matches
)

baseline_df <- results$baseline
context_df <- results$context
matches_df <- results$matches

opponent_results <- list()

for (i in seq_len(nrow(matches_df))) {
  target_game_id <- as.character(matches_df$game_id[[i]])
  target_date <- as.Date(matches_df$date[[i]])
  target_opponent <- as.character(matches_df$opponent[[i]])
  opponent_df <- build_opponent_baseline(
    opponent = target_opponent,
    season = season,
    n_matches = opponent_n,
    target_game_id = target_game_id,
    target_date = target_date,
    target_team = team
  )
  if (!is.null(opponent_df) && nrow(opponent_df) > 0) opponent_results[[length(opponent_results) + 1]] <- opponent_df
}

opponents_df <- if (length(opponent_results) > 0) bind_rows(opponent_results) %>% arrange(target_date, team, date) else tibble()

output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(baseline_df, file.path(output_dir, "baseline.csv"))
write_csv(context_df, file.path(output_dir, "context.csv"))
write_csv(opponents_df, file.path(output_dir, "opponents.csv"))

message("")
message("==============================================")
message(" Scrape complete")
message("==============================================")
message("Baseline rows: ", nrow(baseline_df))
message("Context rows: ", nrow(context_df))
message("Opponent rows: ", nrow(opponents_df))
message("Wrote: ", file.path(output_dir, "baseline.csv"))
message("Wrote: ", file.path(output_dir, "context.csv"))
message("Wrote: ", file.path(output_dir, "opponents.csv"))
