import io
import time
import requests
import pandas as pd

from functools import lru_cache
import argparse
import os


# ============================================================
# DEFAULT SETTINGS
# ============================================================

DEFAULT_SEASON = 2026
DEFAULT_TEAM = "BOS"
DEFAULT_N_MATCHES = 3
DEFAULT_OPPONENT_N_MATCHES = 10


# ============================================================
# LOAD MONEYPUCK DATA
# ============================================================

@lru_cache(maxsize=None)
def _load_data_cached(season, team):
    """
    Download MoneyPuck data once per (season, team).

    The lru_cache prevents repeatedly downloading the same
    team/season files.
    """

    data_types = [
        "skaters",
        "goalies",
        "lines"
    ]

    game_types = [
        "regular",
        "playoffs"
    ]

    seasons = [
        season,
        season - 1
    ]

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/140.0.0.0 Safari/537.36"
        )
    }

    session = requests.Session()
    session.headers.update(headers)

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

                # ------------------------------------------------
                # Retry if MoneyPuck rate-limits us
                # ------------------------------------------------

                for attempt in range(4):

                    response = session.get(
                        url,
                        timeout=30
                    )

                    if response.status_code == 429:

                        wait_time = 5

                        print(
                            f"MoneyPuck rate limit for {team} "
                            f"({data_type}, {s}, {game_type}). "
                            f"Waiting {wait_time}s..."
                        )

                        time.sleep(wait_time)

                        continue

                    break

                # ------------------------------------------------
                # File doesn't exist
                # ------------------------------------------------

                if response.status_code == 404:
                    continue

                response.raise_for_status()

                # ------------------------------------------------
                # Read CSV
                # ------------------------------------------------

                df = pd.read_csv(
                    io.BytesIO(response.content)
                )

                df["season"] = s
                df["game_type"] = game_type

                dfs.append(df)

        # --------------------------------------------------------
        # Store empty DataFrame if nothing was found
        # --------------------------------------------------------

        results[data_type] = (
            pd.concat(
                dfs,
                ignore_index=True
            )
            if dfs
            else pd.DataFrame()
        )

    return (
        results["skaters"],
        results["goalies"],
        results["lines"]
    )


def load_data(
    season=DEFAULT_SEASON,
    team=DEFAULT_TEAM
):
    """
    Public wrapper around cached data.

    Return copies so that get_last_n_matches() can safely modify
    columns without modifying the cached DataFrames.
    """

    skaters, goalies, lines = _load_data_cached(
        season,
        team
    )

    return (
        skaters.copy(),
        goalies.copy(),
        lines.copy()
    )


# ============================================================
# GET LAST N MATCHES
# ============================================================

def get_last_n_matches(
    team,
    n,
    season=DEFAULT_SEASON
):
    """
    Return the most recent N games for a team.

    Returns:
        games  - one row per game
        lines  - line-level data for those games
    """

    skaters, goalies, lines = load_data(
        season=season,
        team=team
    )

    # ============================================================
    # SKATERS
    # ============================================================

    if skaters.empty:
        return pd.DataFrame(), pd.DataFrame()

    skaters["gameDate"] = pd.to_datetime(
        skaters["gameDate"],
        format="%Y%m%d"
    ).dt.date

    # ------------------------------------------------------------
    # Keep only games involving requested team
    # ------------------------------------------------------------

    skaters = skaters[
        skaters["playerTeam"] == team
    ]

    # ------------------------------------------------------------
    # Only all-situations rows
    # ------------------------------------------------------------

    skaters = skaters[
        skaters["situation"] == "all"
    ]

    # ------------------------------------------------------------
    # Get team's most recent N games
    # ------------------------------------------------------------

    recent_game_ids = (
        skaters[
            ["gameId", "gameDate"]
        ]
        .drop_duplicates("gameId")
        .sort_values(
            "gameDate",
            ascending=False
        )
        .head(n)["gameId"]
    )

    skaters = skaters[
        skaters["gameId"].isin(recent_game_ids)
    ]

    # ============================================================
    # GAME-LEVEL COLUMNS
    # ============================================================

    first_cols = [
        "gameDate",
        "season",
        "game_type",
        "playerTeam",
        "opposingTeam",
        "home_or_away",
    ]

    # ============================================================
    # SKATER STATISTICS
    # ============================================================

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

    # ============================================================
    # AGGREGATE SKATERS TO ONE ROW PER GAME
    # ============================================================

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
        .sort_values(
            "gameDate",
            ascending=False
        )
        .reset_index(drop=True)
    )

    # ============================================================
    # RENAME SKATER COLUMNS
    # ============================================================

    skater_rename_cols = {

        # Shot generation

        "I_F_xOnGoal":
            "x_on_goal_for",

        "I_F_xGoals":
            "x_goals_for",

        "I_F_shotsOnGoal":
            "shots_on_goal_for",

        "I_F_missedShots":
            "missed_shots_for",

        "I_F_shotAttempts":
            "shot_attempts_for",

        "I_F_unblockedShotAttempts":
            "unblocked_shot_attempts_for",

        # Shot outcomes

        "I_F_xRebounds":
            "x_rebounds_for",

        "I_F_rebounds":
            "rebounds_for",

        "I_F_reboundGoals":
            "rebound_goals_for",

        "I_F_xFreeze":
            "x_freeze_for",

        "I_F_freeze":
            "freeze_for",

        "I_F_xPlayStopped":
            "x_play_stopped_for",

        "I_F_playStopped":
            "play_stopped_for",

        "I_F_xPlayContinuedInZone":
            "x_play_continued_in_zone_for",

        "I_F_playContinuedInZone":
            "play_continued_in_zone_for",

        "I_F_xPlayContinuedOutsideZone":
            "x_play_continued_outside_zone_for",

        "I_F_playContinuedOutsideZone":
            "play_continued_outside_zone_for",

        # Scoring

        "I_F_goals":
            "goals_for",

        # Team activity

        "I_F_faceOffsWon":
            "faceoffs_won_for",

        "faceoffsLost":
            "faceoffs_lost_for",

        "I_F_hits":
            "hits_for",

        "I_F_takeaways":
            "takeaways_for",

        "I_F_giveaways":
            "giveaways_for",

        "I_F_dZoneGiveaways":
            "d_zone_giveaways_for",

        "shotsBlockedByPlayer":
            "shots_blocked_for",

        # Penalties

        "penalties":
            "penalties_for",

        "I_F_penalityMinutes":
            "penalty_minutes_for",

        "penalityMinutesDrawn":
            "penalty_minutes_drawn_for",

        "penaltiesDrawn":
            "penalties_drawn_for",
    }

    skaters = skaters.rename(
        columns=skater_rename_cols
    )

    # ============================================================
    # GOALIES
    # ============================================================

    if goalies.empty:
        return pd.DataFrame(), pd.DataFrame()

    goalies["gameDate"] = pd.to_datetime(
        goalies["gameDate"],
        format="%Y%m%d"
    ).dt.date

    # ------------------------------------------------------------
    # Keep only games involving requested team
    # ------------------------------------------------------------

    goalies = goalies[
        goalies["playerTeam"] == team
    ]

    # ------------------------------------------------------------
    # Only all-situations rows
    # ------------------------------------------------------------

    goalies = goalies[
        goalies["situation"] == "all"
    ]

    # ------------------------------------------------------------
    # Use EXACT same games selected from skater data
    # ------------------------------------------------------------

    goalies = goalies[
        goalies["gameId"].isin(recent_game_ids)
    ]

    # ============================================================
    # GOALIE STATISTICS
    # ============================================================

    goalie_sum_cols = [

        # Shot generation faced

        "xOnGoal",
        "ongoal",
        "unblocked_shot_attempts",

        # Expected / actual goals

        "xGoals",
        "goals",

        # Shot outcomes faced

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

        # Blocked shots

        "blocked_shot_attempts",
    ]

    # ============================================================
    # AGGREGATE GOALIES TO ONE ROW PER GAME
    # ============================================================

    goalies = (
        goalies
        .groupby("gameId")[goalie_sum_cols]
        .sum()
        .reset_index()
    )

    # ============================================================
    # RENAME GOALIE COLUMNS
    # ============================================================

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
    # MERGE SKATER + GOALIE DATA
    # ============================================================

    games = skaters.merge(
        goalies,
        on="gameId",
        how="inner",
        suffixes=("", "_goalie")
    )

    # ============================================================
    # FINAL GAME ORDERING
    # ============================================================

    games = (
        games
        .sort_values(
            "gameDate",
            ascending=False
        )
        .reset_index(drop=True)
    )

    # ============================================================
    # LINES
    # ============================================================

    if not lines.empty:

        lines["gameDate"] = pd.to_datetime(
            lines["gameDate"],
            format="%Y%m%d"
        ).dt.date

        lines = lines[
            (lines["playerTeam"] == team) &
            (lines["gameId"].isin(recent_game_ids))
        ].copy()

    return games, lines


# ============================================================
# MAIN PROGRAM
# ============================================================

def main():

    # ========================================================
    # COMMAND-LINE ARGUMENTS
    # ========================================================

    parser = argparse.ArgumentParser(
        description=(
            "Download and process MoneyPuck NHL game data."
        )
    )

    parser.add_argument(
        "--season",
        type=int,
        default=DEFAULT_SEASON,
        help=(
            f"Season to analyze "
            f"(default: {DEFAULT_SEASON})"
        )
    )

    parser.add_argument(
        "--team",
        type=str,
        default=DEFAULT_TEAM,
        help=(
            f"NHL team abbreviation "
            f"(default: {DEFAULT_TEAM})"
        )
    )

    parser.add_argument(
        "--matches",
        type=int,
        default=DEFAULT_N_MATCHES,
        help=(
            f"Number of recent team matches "
            f"(default: {DEFAULT_N_MATCHES})"
        )
    )

    parser.add_argument(
        "--opponent-matches",
        type=int,
        default=DEFAULT_OPPONENT_N_MATCHES,
        help=(
            f"Number of recent matches per opponent "
            f"(default: {DEFAULT_OPPONENT_N_MATCHES})"
        )
    )

    parser.add_argument(
        "--output-dir",
        type=str,
        default="output",
        help=(
            "Directory for output CSV files "
            "(default: output)"
        )
    )

    args = parser.parse_args()

    # ========================================================
    # CREATE OUTPUT DIRECTORY
    # ========================================================

    os.makedirs(
        args.output_dir,
        exist_ok=True
    )

    # ========================================================
    # LOAD ORIGINAL TEAM
    # ========================================================

    print()
    print("=" * 60)
    print(
        f"Loading last {args.matches} games for "
        f"{args.team}"
    )
    print("=" * 60)
    print()

    games, lines = get_last_n_matches(
        team=args.team,
        n=args.matches,
        season=args.season
    )

    if games.empty:

        print(
            f"No games found for {args.team}."
        )

        return

    # ========================================================
    # SAVE BASELINE
    # ========================================================

    baseline_path = os.path.join(
        args.output_dir,
        "baseline.csv"
    )

    games.to_csv(
        baseline_path,
        index=False
    )

    print(
        f"Saved {len(games)} games to "
        f"{baseline_path}"
    )

    # ========================================================
    # SAVE LINE CONTEXT
    # ========================================================

    context_path = os.path.join(
        args.output_dir,
        "context.csv"
    )

    lines = (
        lines
        .sort_values("gameId")
        .reset_index(drop=True)
    )

    lines.to_csv(
        context_path,
        index=False
    )

    print(
        f"Saved {len(lines)} line rows to "
        f"{context_path}"
    )

    # ========================================================
    # FIND OPPONENTS
    # ========================================================

    opponents = (
        games["opposingTeam"]
        .dropna()
        .unique()
        .tolist()
    )

    print()
    print("Opponents:")
    print(opponents)
    print()

    # ========================================================
    # LOAD OPPONENT GAMES
    # ========================================================

    opponent_games_list = []

    for opponent in opponents:

        print(
            f"Loading last "
            f"{args.opponent_matches} "
            f"games for {opponent}..."
        )

        opponent_games, _ = get_last_n_matches(
            team=opponent,
            n=args.opponent_matches,
            season=args.season
        )

        if not opponent_games.empty:

            opponent_games_list.append(
                opponent_games
            )

    # ========================================================
    # COMBINE OPPONENT DATA
    # ========================================================

    if opponent_games_list:

        opponents_games = pd.concat(
            opponent_games_list,
            ignore_index=True
        )

    else:

        opponents_games = pd.DataFrame()

    # ========================================================
    # REMOVE GAMES WHERE OPPONENT PLAYED ORIGINAL TEAM
    # ========================================================

    if not opponents_games.empty:

        opponents_games = opponents_games[
            opponents_games["opposingTeam"] != args.team
        ].reset_index(drop=True)

    # ========================================================
    # SAVE OPPONENT DATA
    # ========================================================

    opponents_path = os.path.join(
        args.output_dir,
        "opponents.csv"
    )

    opponents_games.to_csv(
        opponents_path,
        index=False
    )

    print(
        f"Saved {len(opponents_games)} opponent games "
        f"to {opponents_path}"
    )

    # ========================================================
    # FINISHED
    # ========================================================

    print()
    print("=" * 60)
    print("DONE")
    print("=" * 60)
    print()
    print(f"Output directory: {args.output_dir}")
    print()
    print("Files created:")

    print(
        f"  - {baseline_path}"
    )

    print(
        f"  - {context_path}"
    )

    print(
        f"  - {opponents_path}"
    )

    print()


# ============================================================
# ENTRY POINT
# ============================================================

if __name__ == "__main__":
    main()
