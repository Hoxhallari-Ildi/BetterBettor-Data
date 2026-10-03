import io
import requests
import pandas as pd


# ============================================================
# CONFIG
# ============================================================

DEFAULT_SEASON = 2026
DEFAULT_TEAM = "BOS"
DEFAULT_N_MATCHES = 3
DEFAULT_OPPONENT_N_MATCHES = 10

# ============================================================
# MONEYPUCK CLIENT
# ============================================================

def load_data(season=DEFAULT_SEASON, team=DEFAULT_TEAM):

    data_types = ["skaters", "goalies", "lines"]
    game_types = ["regular", "playoffs"]
    seasons = [season, season - 1]

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/140.0.0.0 Safari/537.36"
        )
    }

    results = {}

    for data_type in data_types:
        dfs = []

        for s in seasons:
            for game_type in game_types:

                url = (
                    f"https://moneypuck.com/moneypuck/playerData/"
                    f"teamPlayerGameByGame/{s}/{game_type}/"
                    f"{data_type}/{team}.csv"
                )

                response = requests.get(
                    url,
                    headers=headers,
                    timeout=30
                )

                if response.status_code == 404:
                    continue

                response.raise_for_status()

                df = pd.read_csv(io.BytesIO(response.content))
                df = pd.concat(
                    [
                        df,
                        pd.DataFrame({
                            "season": s,
                            "game_type": game_type,
                        }, index=df.index)
                    ],
                    axis=1
                )

                dfs.append(df)

        results[data_type] = (
            pd.concat(dfs, ignore_index=True)
            if dfs
            else pd.DataFrame()
        )

    return (
        results["skaters"],
        results["goalies"],
        results["lines"]
    )

# ============================================================
# 1. GET LAST N MATCHES
# ============================================================

def get_last_n_matches(team, n):

    skaters, goalies, lines = load_data()

    # ============================================================
    # Skaters
    # ============================================================

    skaters["gameDate"] = pd.to_datetime(
        skaters["gameDate"],
        format="%Y%m%d"
    ).dt.date

    # Keep only games involving the requested team
    skaters = skaters[
        skaters["playerTeam"] == team
    ]

    # Only use all-situations rows
    skaters = skaters[
        skaters["situation"] == "all"
    ]

    # ------------------------------------------------------------
    # Get the team's most recent N games
    # ------------------------------------------------------------

    recent_game_ids = (
        skaters[["gameId", "gameDate"]]
        .drop_duplicates("gameId")
        .sort_values("gameDate", ascending=False)
        .head(n)["gameId"]
    )

    skaters = skaters[
        skaters["gameId"].isin(recent_game_ids)
    ]

    # ------------------------------------------------------------
    # Game-level columns
    # ------------------------------------------------------------

    first_cols = [
        "gameDate",
        "season",
        "game_type",
        "playerTeam",
        "opposingTeam",
        "home_or_away",
    ]

    # ------------------------------------------------------------
    # Skater statistics
    #
    # These are additive team-level "FOR" statistics.
    #
    # The shot-outcome fields are intentionally kept together:
    #
    #   shot
    #       -> goal
    #       -> rebound
    #       -> freeze
    #       -> play stopped
    #       -> continued in zone
    #       -> continued outside zone
    #
    # Keeping both actual and expected versions lets us later
    # engineer rates such as rebound generation and conversion.
    # ------------------------------------------------------------

    skater_sum_cols = [

        # --------------------------------------------------------
        # Shot generation
        # --------------------------------------------------------

        "I_F_xOnGoal",
        "I_F_xGoals",
        "I_F_shotsOnGoal",
        "I_F_missedShots",
        "I_F_shotAttempts",
        "I_F_unblockedShotAttempts",

        # --------------------------------------------------------
        # Shot outcomes
        # --------------------------------------------------------

        "I_F_xRebounds",
        "I_F_rebounds",
        "I_F_reboundGoals",

        "I_F_xFreeze",
        "I_F_freeze",

        "I_F_xPlayStopped",
        "I_F_playStopped",

        "I_F_xPlayContinuedInZone",
        "I_F_playContinuedInZone",

        "I_F_xPlayContinuedOutsideZone",
        "I_F_playContinuedOutsideZone",

        # --------------------------------------------------------
        # Scoring
        # --------------------------------------------------------

        "I_F_goals",

        # --------------------------------------------------------
        # Possession / team activity
        # --------------------------------------------------------

        "I_F_faceOffsWon",
        "faceoffsLost",

        "I_F_hits",
        "I_F_takeaways",
        "I_F_giveaways",
        "I_F_dZoneGiveaways",

        "shotsBlockedByPlayer",

        # --------------------------------------------------------
        # Penalties
        # --------------------------------------------------------

        "penalties",
        "I_F_penalityMinutes",
        "penalityMinutesDrawn",
        "penaltiesDrawn",
    ]

    # ------------------------------------------------------------
    # Aggregate skaters to one row per game
    # ------------------------------------------------------------

    skater_agg = {
        col: "first"
        for col in first_cols
    }

    skater_agg.update({
        col: "sum"
        for col in skater_sum_cols
    })

    skaters = (
        skaters
        .groupby("gameId")
        .agg(skater_agg)
        .reset_index()
        .sort_values("gameDate", ascending=False)
        .reset_index(drop=True)
    )

    # ------------------------------------------------------------
    # Rename skater columns
    #
    # These describe what the team produced / generated.
    # ------------------------------------------------------------

    skater_rename_cols = {

        # Shot generation
        "I_F_xOnGoal": "x_on_goal_for",
        "I_F_xGoals": "x_goals_for",
        "I_F_shotsOnGoal": "shots_on_goal_for",
        "I_F_missedShots": "missed_shots_for",
        "I_F_shotAttempts": "shot_attempts_for",
        "I_F_unblockedShotAttempts":
            "unblocked_shot_attempts_for",

        # Shot outcomes
        "I_F_xRebounds": "x_rebounds_for",
        "I_F_rebounds": "rebounds_for",
        "I_F_reboundGoals": "rebound_goals_for",

        "I_F_xFreeze": "x_freeze_for",
        "I_F_freeze": "freeze_for",

        "I_F_xPlayStopped": "x_play_stopped_for",
        "I_F_playStopped": "play_stopped_for",

        "I_F_xPlayContinuedInZone":
            "play_continued_in_zone_for",
        "I_F_playContinuedInZone":
            "play_continued_in_zone_for",

        "I_F_xPlayContinuedOutsideZone":
            "x_play_continued_outside_zone_for",
        "I_F_playContinuedOutsideZone":
            "play_continued_outside_zone_for",

        # Scoring
        "I_F_goals": "goals_for",

        # Team activity
        "I_F_faceOffsWon": "faceoffs_won_for",
        "faceoffsLost": "faceoffs_lost_for",
        "I_F_hits": "hits_for",
        "I_F_takeaways": "takeaways_for",
        "I_F_giveaways": "giveaways_for",
        "I_F_dZoneGiveaways": "d_zone_giveaways_for",
        "shotsBlockedByPlayer": "shots_blocked_for",

        # Penalties
        "penalties": "penalties_for",
        "I_F_penalityMinutes": "penalty_minutes_for",
        "penalityMinutesDrawn": "penalty_minutes_drawn_for",
        "penaltiesDrawn": "penalties_drawn_for",
    }

    skaters = skaters.rename(
        columns=skater_rename_cols
    )

    # ============================================================
    # Goalies
    # ============================================================

    goalies["gameDate"] = pd.to_datetime(
        goalies["gameDate"],
        format="%Y%m%d"
    ).dt.date

    # Keep only games involving the requested team
    goalies = goalies[
        goalies["playerTeam"] == team
    ]

    # Only use all-situations rows
    goalies = goalies[
        goalies["situation"] == "all"
    ]

    # ------------------------------------------------------------
    # Use the EXACT same games selected from the skater data.
    # ------------------------------------------------------------

    goalies = goalies[
        goalies["gameId"].isin(recent_game_ids)
    ]

    # ------------------------------------------------------------
    # Goalie statistics to aggregate
    #
    # These describe what the team faced / allowed.
    #
    # If multiple goalies played in a game, their event totals are
    # summed so the final row represents the complete team-game.
    # ------------------------------------------------------------

    goalie_sum_cols = [

        # --------------------------------------------------------
        # Shot generation faced
        # --------------------------------------------------------

        "xOnGoal",
        "ongoal",
        "unblocked_shot_attempts",

        # --------------------------------------------------------
        # Expected / actual goals
        # --------------------------------------------------------

        "xGoals",
        "goals",

        # --------------------------------------------------------
        # Shot outcomes faced
        # --------------------------------------------------------

        "xRebounds",
        "rebounds",

        "xFreeze",
        "freeze",

        "xPlayStopped",
        "playStopped",

        "xPlayContinuedInZone",
        "playContinuedInZone",

        "xPlayContinuedOutsideZone",
        "playContinuedOutsideZone",

        # --------------------------------------------------------
        # Blocked shots
        # --------------------------------------------------------

        "blocked_shot_attempts",
    ]

    # ------------------------------------------------------------
    # Aggregate to one row per game
    # ------------------------------------------------------------

    goalies = (
        goalies
        .groupby("gameId")[goalie_sum_cols]
        .sum()
        .reset_index()
    )

    # ------------------------------------------------------------
    # Rename goalie columns
    #
    # These describe what the team faced / allowed.
    # ------------------------------------------------------------

    goalie_rename_cols = {

        # Shot generation faced
        "xOnGoal":
            "x_on_goal_against",

        "ongoal":
            "shots_on_goal_against",

        "unblocked_shot_attempts":
            "unblocked_shot_attempts_against",

        # Expected / actual goals
        "xGoals":
            "x_goals_against",

        "goals":
            "goals_against",

        # Shot outcomes faced
        "xRebounds":
            "x_rebounds_against",

        "rebounds":
            "rebounds_against",

        "xFreeze":
            "x_freeze_against",

        "freeze":
            "freeze_against",

        "xPlayStopped":
            "x_play_stopped_against",

        "playStopped":
            "play_stopped_against",

        "xPlayContinuedInZone":
            "x_play_continued_in_zone_against",

        "playContinuedInZone":
            "play_continued_in_zone_against",

        "xPlayContinuedOutsideZone":
            "x_play_continued_outside_zone_against",

        "playContinuedOutsideZone":
            "play_continued_outside_zone_against",

        # Blocked shots
        "blocked_shot_attempts":
            "blocked_shot_attempts_against",
    }

    goalies = goalies.rename(
        columns=goalie_rename_cols
    )

    # ============================================================
    # Merge skater + goalie data
    # ============================================================

    games = skaters.merge(
        goalies,
        on="gameId",
        how="inner",
        suffixes=("", "_goalie")
    )

    # ------------------------------------------------------------
    # Final ordering
    # ------------------------------------------------------------

    games = (
        games
        .sort_values("gameDate", ascending=False)
        .reset_index(drop=True)
    )

    return games

print(get_last_n_matches('BOS', 3))
