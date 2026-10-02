# ============================================================
# NHL ROLLING MATCH BASELINE
#
# Python / Understat soccer implementation translated to:
#   R + fastRhockey + NHL
#
# Outputs:
#   output/baseline.csv
#   output/context.csv
#   output/opponents.csv
#
# ============================================================

suppressPackageStartupMessages({
  library(fastRhockey)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(readr)
})

# ============================================================
# CONFIG
# ============================================================

DEFAULT_SEASON <- 2025
DEFAULT_TEAM <- "BOS"
DEFAULT_N_MATCHES <- 3
DEFAULT_OPPONENT_N_MATCHES <- 10

# NHL season argument:
#
# 2025 = 2025-26
# 2024 = 2024-25
#
# fastRhockey's NHL roster documentation uses this
# four-digit season convention.
#
# ============================================================


# ============================================================
# 1. EXPECTED NHL STANDINGS POINTS
# ============================================================
#
# NHL:
#
#   Regulation/OT win = 2 standings points
#   OT/SO loss        = 1 standings point
#   regulation loss   = 0 standings points
#
# If final goals are modeled as independent Poisson variables,
# a tie after regulation is treated as an OT/SO outcome.
#
# Therefore:
#
#   xPts = 2 * P(win) + 1 * P(tie)
#
# This is NOT a prediction of the actual OT winner.
# ============================================================

expected_points_nhl <- function(
    xgf,
    xga,
    max_goals = 12
) {

  if (
    is.na(xgf) ||
    is.na(xga) ||
    xgf < 0 ||
    xga < 0
  ) {
    return(NA_real_)
  }

  gf_probs <- dpois(
    0:max_goals,
    lambda = xgf
  )

  ga_probs <- dpois(
    0:max_goals,
    lambda = xga
  )

  probability_matrix <- outer(
    gf_probs,
    ga_probs,
    "*"
  )

  win_prob <- 0
  tie_prob <- 0

  for (gf in 0:max_goals) {

    for (ga in 0:max_goals) {

      p <- probability_matrix[
        gf + 1,
        ga + 1
      ]

      if (gf > ga) {
        win_prob <- win_prob + p
      }

      if (gf == ga) {
        tie_prob <- tie_prob + p
      }
    }
  }

  2 * win_prob + tie_prob
}


# ============================================================
# 2. NHL SEASON STRING
# ============================================================

nhl_season_string <- function(season) {

  paste0(
    season,
    season + 1
  )
}


# ============================================================
# 3. GET TEAM GAME IDS
# ============================================================

get_team_games <- function(
    team,
    season
) {

  message(
    "Getting NHL games for ",
    team,
    " season ",
    nhl_season_string(season),
    "..."
  )

  games <- nhl_game_ids_by_season(
    season = season,
    team_abbr = team
  )

  if (is.null(games) || nrow(games) == 0) {
    return(tibble())
  }

  games
}


# ============================================================
# 4. NORMALIZE SCHEDULE / GAME DATA
# ============================================================
#
# fastRhockey has changed some returned structures over time.
# This helper attempts to standardize the fields we need.
#
# The resulting object is:
#
#   game_id
#   date
#   team
#   opponent
#   h_a
#   season
#
# ============================================================

normalize_game_list <- function(
    games,
    team,
    season
) {

  if (is.null(games) || nrow(games) == 0) {
    return(tibble())
  }

  names(games) <- janitor::make_clean_names(
    names(games)
  )

  # ----------------------------------------------------------
  # GAME ID
  # ----------------------------------------------------------

  id_col <- intersect(
    c(
      "game_id",
      "id"
    ),
    names(games)
  )[1]

  if (is.na(id_col)) {
    stop("Could not identify NHL game_id column.")
  }

  # ----------------------------------------------------------
  # DATE
  # ----------------------------------------------------------

  date_col <- intersect(
    c(
      "game_date",
      "date",
      "game_date_time",
      "start_time_utc"
    ),
    names(games)
  )[1]

  if (is.na(date_col)) {
    stop("Could not identify NHL game date column.")
  }

  # ----------------------------------------------------------
  # TEAM / OPPONENT
  # ----------------------------------------------------------
  #
  # Depending on the fastRhockey version, schedule-derived
  # data may expose home/away abbreviations or names.
  #
  # We therefore normalize them here.
  # ----------------------------------------------------------

  home_col <- intersect(
    c(
      "home_team",
      "home_team_abbrev",
      "home_team_abbr"
    ),
    names(games)
  )[1]

  away_col <- intersect(
    c(
      "away_team",
      "away_team_abbrev",
      "away_team_abbr"
    ),
    names(games)
  )[1]

  if (
    is.na(home_col) ||
    is.na(away_col)
  ) {

    stop(
      paste(
        "Could not identify home/away team columns.",
        "Inspect names(games) for your installed fastRhockey version."
      )
    )
  }

  result <- games %>%
    mutate(
      game_id = as.character(.data[[id_col]]),

      date = as.Date(
        substr(
          as.character(.data[[date_col]]),
          1,
          10
        )
      ),

      home_team = as.character(
        .data[[home_col]]
      ),

      away_team = as.character(
        .data[[away_col]]
      )
    ) %>%
    mutate(
      team = team,

      h_a = case_when(
        home_team == team ~ "h",
        away_team == team ~ "a",
        TRUE ~ NA_character_
      ),

      opponent = case_when(
        home_team == team ~ away_team,
        away_team == team ~ home_team,
        TRUE ~ NA_character_
      ),

      season = season
    ) %>%
    filter(
      !is.na(h_a),
      !is.na(opponent)
    ) %>%
    select(
      game_id,
      team,
      opponent,
      season,
      date,
      h_a
    ) %>%
    distinct()

  result
}


# ============================================================
# 5. GET LAST N COMPLETED GAMES
# ============================================================

get_last_n_matches <- function(
    team,
    season,
    n
) {

  seasons_to_check <- seq(
    season,
    season - 2
  )

  all_games <- map_dfr(
    seasons_to_check,
    function(s) {

      tryCatch({

        games <- get_team_games(
          team,
          s
        )

        if (
          is.null(games) ||
          nrow(games) == 0
        ) {
          return(tibble())
        }

        normalize_game_list(
          games,
          team,
          s
        )

      }, error = function(e) {

        message(
          "Could not retrieve season ",
          s,
          ": ",
          e$message
        )

        tibble()
      })
    }
  )

  if (nrow(all_games) == 0) {
    stop(
      "No NHL games found for ",
      team
    )
  }

  all_games %>%
    filter(
      !is.na(date),
      date <= Sys.Date()
    ) %>%
    arrange(desc(date)) %>%
    slice_head(n = n) %>%
    arrange(date)
}


# ============================================================
# 6. GET TEAM SEASON MATCHES
# ============================================================

get_team_season_matches <- function(
    team,
    season
) {

  message(
    "Getting match list for ",
    team,
    " season ",
    season,
    "..."
  )

  tryCatch({

    games <- get_team_games(
      team,
      season
    )

    if (
      is.null(games) ||
      nrow(games) == 0
    ) {
      return(tibble())
    }

    normalize_game_list(
      games,
      team,
      season
    )

  }, error = function(e) {

    message(
      "Could not retrieve ",
      team,
      " for season ",
      season,
      ": ",
      e$message
    )

    tibble()
  })
}


# ============================================================
# 7. PREVIOUS N GAMES FOR TEAM
# ============================================================

get_previous_matches <- function(
    team,
    target_date,
    current_season,
    n
) {

  target_date <- as.Date(target_date)

  current_season <- as.integer(
    current_season
  )

  previous_season <- current_season - 1

  message(
    "\nGetting previous ",
    n,
    " matches for ",
    team,
    " before ",
    target_date,
    "..."
  )

  # ----------------------------------------------------------
  # CURRENT SEASON
  # ----------------------------------------------------------

  current <- get_team_season_matches(
    team,
    current_season
  )

  if (nrow(current) > 0) {

    current <- current %>%
      filter(
        date < target_date
      )
  }

  # ----------------------------------------------------------
  # PREVIOUS SEASON IF NEEDED
  # ----------------------------------------------------------

  frames <- list()

  if (nrow(current) > 0) {
    frames[[length(frames) + 1]] <- current
  }

  current_count <- sum(
    map_int(
      frames,
      nrow
    )
  )

  if (current_count < n) {

    previous <- get_team_season_matches(
      team,
      previous_season
    )

    if (nrow(previous) > 0) {

      previous <- previous %>%
        filter(
          date < target_date
        )

      if (nrow(previous) > 0) {
        frames[[length(frames) + 1]] <- previous
      }
    }
  }

  if (length(frames) == 0) {

    message(
      "  No previous matches found."
    )

    return(tibble())
  }

  bind_rows(frames) %>%
    arrange(date) %>%
    slice_tail(n = n) %>%
    arrange(date)
}


# ============================================================
# 8. GAME FEED
# ============================================================
#
# fastRhockey:
#
#   nhl_game_feed()
#
# returns detailed game information including play-by-play,
# rosters and game information.
#
# We use the game feed as the raw equivalent of:
#
#   Understat match.get_roster_data()
#   Understat match.get_shot_data()
#
# ============================================================

get_raw_game_context <- function(
    game_id
) {

  message(
    "  Loading NHL game ",
    game_id,
    "..."
  )

  feed <- nhl_game_feed(
    game_id = as.numeric(game_id)
  )

  if (is.null(feed)) {
    stop(
      "No game feed returned for ",
      game_id
    )
  }

  feed
}


# ============================================================
# 9. EXTRACT PLAY-BY-PLAY
# ============================================================

get_game_pbp <- function(
    game_id
) {

  pbp <- nhl_game_pbp(
    game_id = as.numeric(game_id)
  )

  if (
    is.null(pbp) ||
    nrow(pbp) == 0
  ) {
    return(tibble())
  }

  pbp
}


# ============================================================
# 10. BUILD SHOT DATA
# ============================================================
#
# NHL shot events:
#
#   SHOT
#   GOAL
#   MISSED_SHOT
#
# Blocked shots are deliberately excluded from xG, consistent
# with the fastRhockey xG model documentation.
#
# fastRhockey's xG model is based on unblocked shots and uses
# separate 5v5 and special-teams models.
#
# ============================================================

build_shot_context <- function(
    pbp
) {

  if (
    is.null(pbp) ||
    nrow(pbp) == 0
  ) {
    return(tibble())
  }

  names(pbp) <- janitor::make_clean_names(
    names(pbp)
  )

  if (
    !"event_type" %in% names(pbp)
  ) {
    stop(
      "event_type is missing from NHL PBP."
    )
  }

  shots <- pbp %>%
    filter(
      event_type %in% c(
        "SHOT",
        "GOAL",
        "MISSED_SHOT"
      )
    )

  if (nrow(shots) == 0) {
    return(tibble())
  }

  # ----------------------------------------------------------
  # TEAM
  # ----------------------------------------------------------

  team_col <- intersect(
    c(
      "event_team_abbr",
      "team_abbr",
      "event_team"
    ),
    names(shots)
  )[1]

  # ----------------------------------------------------------
  # PLAYER
  # ----------------------------------------------------------

  player_id_col <- intersect(
    c(
      "event_player_1_id",
      "player_id"
    ),
    names(shots)
  )[1]

  player_name_col <- intersect(
    c(
      "event_player_1_name",
      "player_name"
    ),
    names(shots)
  )[1]

  if (is.na(team_col)) {
    stop(
      "Could not identify shooting team in PBP."
    )
  }

  shots <- shots %>%
    mutate(
      team = .data[[team_col]],

      player_id = if (
        !is.na(player_id_col)
      ) {
        suppressWarnings(
          as.numeric(.data[[player_id_col]])
        )
      } else {
        NA_real_
      },

      player = if (
        !is.na(player_name_col)
      ) {
        as.character(.data[[player_name_col]])
      } else {
        NA_character_
      },

      goal = as.integer(
        event_type == "GOAL"
      )
    )

  # ----------------------------------------------------------
  # xG
  # ----------------------------------------------------------
  #
  # If fastRhockey's PBP has already supplied xg, use it.
  #
  # This is preferable to rebuilding the model ourselves.
  # ----------------------------------------------------------

  if ("xg" %in% names(shots)) {

    shots <- shots %>%
      mutate(
        xg = as.numeric(xg)
      )

  } else {

    # --------------------------------------------------------
    # FALLBACK
    # --------------------------------------------------------
    #
    # We don't invent an xG model here.
    #
    # The current fastRhockey data pipeline has an NHL xG
    # model, but the exact prediction helper/API can differ
    # by package release.
    #
    # If xg is absent, fail explicitly rather than silently
    # substituting raw shooting percentage.
    # --------------------------------------------------------

    stop(
      paste(
        "NHL PBP does not contain xg.",
        "Use a fastRhockey release/PBP dataset containing",
        "the integrated xG field or plug in the fastRhockey",
        "xG prediction model."
      )
    )
  }

  # ----------------------------------------------------------
  # AGGREGATE PLAYER SHOTS
  # ----------------------------------------------------------

  shots %>%
    group_by(
      player_id,
      player,
      team
    ) %>%
    summarise(
      S = n(),
      GF = sum(
        goal,
        na.rm = TRUE
      ),
      xGF = sum(
        xg,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}


# ============================================================
# 11. TEAM GAME TOTALS
# ============================================================

build_team_game_stats <- function(
    pbp
) {

  shots <- build_shot_context(
    pbp
  )

  if (nrow(shots) == 0) {
    return(tibble())
  }

  shots %>%
    group_by(team) %>%
    summarise(
      S = sum(S),
      GF = sum(GF),
      xGF = sum(xGF),
      .groups = "drop"
    )
}


# ============================================================
# 12. BUILD PLAYER CONTEXT
# ============================================================
#
# NHL is not soccer, so there isn't an Understat-style roster
# position/time schema to reproduce exactly.
#
# We use NHL player box/roster information plus PBP shooting.
#
# ============================================================

build_player_context <- function(
    game_id,
    pbp
) {

  # ----------------------------------------------------------
  # GAME FEED
  # ----------------------------------------------------------

  feed <- get_raw_game_context(
    game_id
  )

  # ----------------------------------------------------------
  # PLAYER SHOOTING
  # ----------------------------------------------------------

  shots <- build_shot_context(
    pbp
  )

  # ----------------------------------------------------------
  # PLAYER BOX
  # ----------------------------------------------------------

  player_box <- tryCatch({

    # Current fastRhockey releases expose the game-level
    # player/skater data through the NHL loaders/API.
    #
    # We use game_feed as the raw source where possible.
    #
    # If the feed exposes player stats, normalize them below.

    NULL

  }, error = function(e) {

    message(
      "Could not retrieve separate player box: ",
      e$message
    )

    NULL
  })

  # ----------------------------------------------------------
  # MINIMUM PLAYER CONTEXT
  # ----------------------------------------------------------

  if (
    nrow(shots) == 0
  ) {

    return(tibble(
      game_id = as.character(game_id)
    ))
  }

  shots %>%
    mutate(
      game_id = as.character(game_id)
    ) %>%
    select(
      game_id,
      team,
      player_id,
      player,
      S,
      GF,
      xGF
    )
}


# ============================================================
# 13. BUILD TEAM GAME BASELINE
# ============================================================

build_team_game_baseline <- function(
    game_id,
    team,
    pbp
) {

  team_stats <- build_team_game_stats(
    pbp
  )

  if (nrow(team_stats) == 0) {
    return(tibble())
  }

  # ----------------------------------------------------------
  # MAIN TEAM
  # ----------------------------------------------------------

  main <- team_stats %>%
    filter(
      team == !!team
    )

  # ----------------------------------------------------------
  # OPPONENT
  # ----------------------------------------------------------

  opponent <- team_stats %>%
    filter(
      team != !!team
    ) %>%
    rename(
      opponent = team,
      SA = S,
      GA = GF,
      xGA = xGF
    )

  main <- main %>%
    rename(
      team = team,
      S = S,
      GF = GF,
      xGF = xGF
    )

  # ----------------------------------------------------------
  # COMBINE
  # ----------------------------------------------------------

  baseline <- main %>%
    mutate(
      game_id = as.character(game_id)
    ) %>%
    left_join(
      opponent %>%
        select(
          opponent,
          SA,
          GA,
          xGA
        ),
      by = character()
    )

  if (nrow(baseline) == 0) {
    return(tibble())
  }

  # ----------------------------------------------------------
  # HOME / AWAY
  # ----------------------------------------------------------

  game_id_num <- suppressWarnings(
    as.numeric(game_id)
  )

  # NHL game IDs encode season/game type/game number, but
  # home/away should be obtained from the schedule/feed rather
  # than inferred from the ID.
  #
  # Caller adds h_a.
  #
  baseline
}


# ============================================================
# 14. PROCESS ONE GAME
# ============================================================

process_game <- function(
    match
) {

  game_id <- match$game_id

  message(
    "\nProcessing ",
    game_id,
    " (",
    match$date,
    ")..."
  )

  pbp <- get_game_pbp(
    game_id
  )

  if (nrow(pbp) == 0) {
    stop(
      "No PBP available for game ",
      game_id
    )
  }

  # ----------------------------------------------------------
  # TEAM STATS
  # ----------------------------------------------------------

  team_stats <- build_team_game_stats(
    pbp
  )

  # ----------------------------------------------------------
  # MAIN TEAM
  # ----------------------------------------------------------

  main <- team_stats %>%
    filter(
      team == match$team
    )

  # ----------------------------------------------------------
  # OPPONENT
  # ----------------------------------------------------------

  opponent <- team_stats %>%
    filter(
      team == match$opponent
    )

  if (
    nrow(main) == 0 ||
    nrow(opponent) == 0
  ) {

    stop(
      "Could not identify both teams in game ",
      game_id
    )
  }

  # ----------------------------------------------------------
  # BASELINE
  # ----------------------------------------------------------

  baseline <- tibble(

    game_id = as.character(game_id),

    date = match$date,

    season = match$season,

    team = match$team,

    opponent = match$opponent,

    h_a = match$h_a,

    S = main$S,

    GF = main$GF,

    xGF = main$xGF,

    SA = opponent$S,

    GA = opponent$GF,

    xGA = opponent$xGF

  )

  # ----------------------------------------------------------
  # EXPECTED POINTS
  # ----------------------------------------------------------

  baseline$xPts <- expected_points_nhl(
    baseline$xGF,
    baseline$xGA
  )

  # ----------------------------------------------------------
  # ADDITIONAL HOCKEY METRICS
  # ----------------------------------------------------------

  baseline <- baseline %>%
    mutate(

      xG_diff = xGF - xGA,

      goal_diff = GF - GA,

      shot_diff = S - SA,

      shooting_pct = if_else(
        S > 0,
        GF / S,
        NA_real_
      ),

      save_pct_proxy = if_else(
        SA > 0,
        1 - GA / SA,
        NA_real_
      )

    )

  baseline
}


# ============================================================
# 15. PROCESS PREVIOUS OPPONENT GAMES
# ============================================================

build_opponent_baseline <- function(
    matches,
    main_team,
    n
) {

  output <- list()

  for (
    i in seq_len(nrow(matches))
  ) {

    target <- matches[i, ]

    opponent <- target$opponent

    previous <- get_previous_matches(
      team = opponent,
      target_date = target$date,
      current_season = target$season,
      n = n
    )

    if (
      nrow(previous) == 0
    ) {

      message(
        "Skipping ",
        opponent,
        " - no history."
      )

      next
    }

    for (
      j in seq_len(nrow(previous))
    ) {

      previous_match <- previous[j, ]

      message(
        "  Processing opponent game ",
        previous_match$game_id,
        " (",
        previous_match$date,
        ")..."
      )

      tryCatch({

        pbp <- get_game_pbp(
          previous_match$game_id
        )

        if (nrow(pbp) == 0) {
          next
        }

        team_stats <- build_team_game_stats(
          pbp
        )

        own <- team_stats %>%
          filter(
            team == opponent
          )

        opp <- team_stats %>%
          filter(
            team != opponent
          )

        if (
          nrow(own) == 0 ||
          nrow(opp) == 0
        ) {
          next
        }

        row <- tibble(

          game_id =
            as.character(
              previous_match$game_id
            ),

          date =
            previous_match$date,

          season =
            previous_match$season,

          team =
            opponent,

          opponent =
            opp$team,

          h_a =
            previous_match$h_a,

          S =
            own$S,

          GF =
            own$GF,

          xGF =
            own$xGF,

          SA =
            opp$S,

          GA =
            opp$GF,

          xGA =
            opp$xGF,

          xPts =
            expected_points_nhl(
              own$xGF,
              opp$xGF
            ),

          target_game_id =
            target$game_id,

          target_date =
            target$date,

          target_team =
            main_team,

          target_opponent =
            opponent

        )

        output[[length(output) + 1]] <- row

      }, error = function(e) {

        message(
          "    Could not process ",
          previous_match$game_id,
          ": ",
          e$message
        )
      })
    }
  }

  if (length(output) == 0) {
    return(tibble())
  }

  bind_rows(output) %>%
    arrange(
      target_date,
      target_game_id,
      date
    )
}


# ============================================================
# 16. MAIN BATCH FUNCTION
# ============================================================

get_last_n_match_baselines <- function(
    team,
    season,
    n = DEFAULT_N_MATCHES,
    opponent_n_matches =
      DEFAULT_OPPONENT_N_MATCHES
) {

  # ----------------------------------------------------------
  # GET LAST N GAMES
  # ----------------------------------------------------------

  message(
    "Getting last ",
    n,
    " games for ",
    team,
    "..."
  )

  matches <- get_last_n_matches(
    team = team,
    season = season,
    n = n
  )

  # ----------------------------------------------------------
  # MAIN GAME BASELINES
  # ----------------------------------------------------------

  baseline_list <- list()

  player_context_list <- list()

  for (
    i in seq_len(nrow(matches))
  ) {

    match <- matches[i, ]

    tryCatch({

      # ------------------------------------------------------
      # PBP
      # ------------------------------------------------------

      pbp <- get_game_pbp(
        match$game_id
      )

      if (nrow(pbp) == 0) {
        next
      }

      # ------------------------------------------------------
      # TEAM BASELINE
      # ------------------------------------------------------

      baseline <- process_game(
        match
      )

      baseline_list[
        [length(baseline_list) + 1]
      ] <- list(baseline)

      # ------------------------------------------------------
      # PLAYER CONTEXT
      # ------------------------------------------------------

      player_context <- build_player_context(
        match$game_id,
        pbp
      ) %>%
        mutate(
          date = match$date,
          season = match$season,
          h_a = match$h_a,
          opponent = match$opponent
        ) %>%
        select(
          game_id,
          date,
          season,
          team,
          opponent,
          player_id,
          player,
          S,
          GF,
          xGF,
          everything()
        )

      player_context_list[
        [length(player_context_list) + 1]
      ] <- list(player_context)

    }, error = function(e) {

      message(
        "Could not process ",
        match$game_id,
        ": ",
        e$message
      )
    })
  }

  # ----------------------------------------------------------
  # COMBINE MAIN BASELINES
  # ----------------------------------------------------------

  baseline_df <- bind_rows(
    baseline_list
  ) %>%
    arrange(date)

  # ----------------------------------------------------------
  # COMBINE PLAYER CONTEXT
  # ----------------------------------------------------------

  player_df <- bind_rows(
    player_context_list
  )

  # ----------------------------------------------------------
  # OPPONENT BASELINES
  # ----------------------------------------------------------

  opponent_df <- build_opponent_baseline(
    matches = matches,
    main_team = team,
    n = opponent_n_matches
  )

  # ----------------------------------------------------------
  # RETURN
  # ----------------------------------------------------------

  list(
    baseline = baseline_df,
    context = player_df,
    opponents = opponent_df
  )
}


# ============================================================
# 17. COMMAND-LINE ARGUMENTS
# ============================================================

args <- commandArgs(
  trailingOnly = TRUE
)

get_arg <- function(
    name,
    default
) {

  prefix <- paste0(
    "--",
    name,
    "="
  )

  value <- args[
    startsWith(
      args,
      prefix
    )
  ]

  if (length(value) == 0) {
    return(default)
  }

  sub(
    prefix,
    "",
    value[1]
  )
}


season <- as.integer(
  get_arg(
    "season",
    DEFAULT_SEASON
  )
)

team <- get_arg(
  "team",
  DEFAULT_TEAM
)

n_matches <- as.integer(
  get_arg(
    "n-matches",
    DEFAULT_N_MATCHES
  )
)

opponent_n_matches <- as.integer(
  get_arg(
    "opponent-n-matches",
    DEFAULT_OPPONENT_N_MATCHES
  )
)


# ============================================================
# 18. RUN
# ============================================================

dir.create(
  "output",
  showWarnings = FALSE,
  recursive = TRUE
)

result <- get_last_n_match_baselines(
  team = team,
  season = season,
  n = n_matches,
  opponent_n_matches = opponent_n_matches
)


# ============================================================
# 19. OUTPUT
# ============================================================

write_csv(
  result$baseline,
  "output/baseline.csv"
)

write_csv(
  result$context,
  "output/context.csv"
)

write_csv(
  result$opponents,
  "output/opponents.csv"
)

message(
  "\nFinished."
)

message(
  "  output/baseline.csv"
)

message(
  "  output/context.csv"
)

message(
  "  output/opponents.csv"
)
