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

    skaters, goalies, lines = load_data(team=team)

    skaters["gameDate"] = pd.to_datetime(
        skaters["gameDate"],
        format="%Y%m%d"
    ).dt.date

    skaters = skaters[
        skaters["playerTeam"] == team
    ]

    recent_game_ids = (
        skaters[["gameId", "gameDate"]]
        .drop_duplicates("gameId")
        .sort_values("gameDate", ascending=False)
        .head(n)["gameId"]
    )

    skaters = skaters[
        skaters["gameId"].isin(recent_game_ids)
    ]

    first_cols = [
        "gameDate",
        "season",
        "game_type",
        "playerTeam",
        "opposingTeam",
        "home_or_away",
    ]

    sum_cols = [
        "I_F_xOnGoal",
        "I_F_xGoals",
        "I_F_xRebounds",
        "I_F_primaryAssists",
        "I_F_secondaryAssists",
        "I_F_shotsOnGoal",
        "I_F_missedShots",
        "I_F_blockedShotAttempts",
        "I_F_shotAttempts",
        "I_F_points",
        "I_F_goals",
        "I_F_rebounds",
        "I_F_reboundGoals",
        "I_F_freeze",
        "I_F_playStopped",
        "I_F_playContinuedInZone",
        "I_F_playContinuedOutsideZone",
        "I_F_savedShotsOnGoal",
        "I_F_savedUnblockedShotAttempts",
        "penalties",
        "I_F_penalityMinutes",
        "I_F_faceOffsWon",
        "I_F_hits",
        "I_F_takeaways",
        "I_F_giveaways",
        "I_F_lowDangerShots",
        "I_F_mediumDangerShots",
        "I_F_highDangerShots",
        "I_F_lowDangerxGoals",
        "I_F_mediumDangerxGoals",
        "I_F_highDangerxGoals",
        "I_F_lowDangerGoals",
        "I_F_mediumDangerGoals",
        "I_F_highDangerGoals",
        "I_F_scoreAdjustedShotsAttempts",
        "I_F_unblockedShotAttempts",
        "I_F_scoreAdjustedUnblockedShotAttempts",
        "I_F_dZoneGiveaways",
        "faceoffsLost",
        "penalityMinutesDrawn",
        "penaltiesDrawn",
        "shotsBlockedByPlayer",
    ]

    agg = {col: "first" for col in first_cols}
    agg.update({col: "sum" for col in sum_cols})

    skaters = (
        skaters
        .groupby("gameId")
        .agg(agg)
        .reset_index()
        .sort_values("gameDate", ascending=False)
        .reset_index(drop=True)
    )

    rename_cols = {
        "I_F_xOnGoal": "x_on_goal",
        "I_F_xGoals": "x_goals",
        "I_F_xRebounds": "x_rebounds",
        "I_F_primaryAssists": "primary_assists",
        "I_F_secondaryAssists": "secondary_assists",
        "I_F_shotsOnGoal": "shots_on_goal",
        "I_F_missedShots": "missed_shots",
        "I_F_blockedShotAttempts": "blocked_shot_attempts",
        "I_F_shotAttempts": "shot_attempts",
        "I_F_points": "points",
        "I_F_goals": "goals",
        "I_F_rebounds": "rebounds",
        "I_F_reboundGoals": "rebound_goals",
        "I_F_freeze": "freeze",
        "I_F_playStopped": "play_stopped",
        "I_F_playContinuedInZone": "play_continued_in_zone",
        "I_F_playContinuedOutsideZone": "play_continued_outside_zone",
        "I_F_savedShotsOnGoal": "saved_shots_on_goal",
        "I_F_savedUnblockedShotAttempts": "saved_unblocked_shot_attempts",
        "penalties": "penalties",
        "I_F_penalityMinutes": "penalty_minutes",
        "I_F_faceOffsWon": "faceoffs_won",
        "I_F_hits": "hits",
        "I_F_takeaways": "takeaways",
        "I_F_giveaways": "giveaways",
        "I_F_lowDangerShots": "low_danger_shots",
        "I_F_mediumDangerShots": "medium_danger_shots",
        "I_F_highDangerShots": "high_danger_shots",
        "I_F_lowDangerxGoals": "low_danger_x_goals",
        "I_F_mediumDangerxGoals": "medium_danger_x_goals",
        "I_F_highDangerxGoals": "high_danger_x_goals",
        "I_F_lowDangerGoals": "low_danger_goals",
        "I_F_mediumDangerGoals": "medium_danger_goals",
        "I_F_highDangerGoals": "high_danger_goals",
        "I_F_scoreAdjustedShotsAttempts": "score_adjusted_shot_attempts",
        "I_F_unblockedShotAttempts": "unblocked_shot_attempts",
        "I_F_scoreAdjustedUnblockedShotAttempts": "score_adjusted_unblocked_shot_attempts",
        "I_F_dZoneGiveaways": "d_zone_giveaways",
        "faceoffsLost": "faceoffs_lost",
        "penalityMinutesDrawn": "penalty_minutes_drawn",
        "penaltiesDrawn": "penalties_drawn",
        "shotsBlockedByPlayer": "shots_blocked",
    }

    skaters = skaters.rename(columns=rename_cols)

    return skaters

print(get_last_n_matches('BOS', 3))
