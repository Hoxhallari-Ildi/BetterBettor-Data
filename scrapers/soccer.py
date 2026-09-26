import math
import argparse
import numpy as np
import pandas as pd
from understatapi import UnderstatClient


# ============================================================
# CONFIG
# ============================================================

DEFAULT_SEASON = 2026
DEFAULT_TEAM = "Racing Santander"
DEFAULT_LEAGUE = 'La_Liga'
DEFAULT_N_MATCHES = 3
DEFAULT_OPPONENT_N_MATCHES = 10

POSITION_MAP = {

    # Goalkeeper
    "GK": "GK",

    # Defenders
    "DL": "DEF",
    "DC": "DEF",
    "DR": "DEF",

    # Defensive midfielders
    "DML": "MID",
    "DMC": "MID",
    "DMR": "MID",

    # Midfielders
    "ML": "MID",
    "MC": "MID",
    "MR": "MID",

    # Attacking midfielders
    "AML": "AM",
    "AMC": "AM",
    "AMR": "AM",

    # Forwards
    "FWL": "FWD",
    "FW": "FWD",
    "FWR": "FWD",

    # Substitutes
    "Sub": "SUB",
}


def expected_points(
    xgf,
    xga,
    max_goals=10,
):
    """
    Calculate expected points from expected goals.

    Uses independent Poisson distributions for
    goals scored and goals conceded.

    xPts = 3 * P(win) + 1 * P(draw)
    """

    xgf_probs = [
        np.exp(-xgf)
        * (xgf ** k)
        / math.factorial(k)
        for k in range(max_goals + 1)
    ]

    xga_probs = [
        np.exp(-xga)
        * (xga ** k)
        / math.factorial(k)
        for k in range(max_goals + 1)
    ]

    win_prob = 0.0
    draw_prob = 0.0

    for goals_for in range(max_goals + 1):

        for goals_against in range(max_goals + 1):

            probability = (
                xgf_probs[goals_for]
                * xga_probs[goals_against]
            )

            if goals_for > goals_against:
                win_prob += probability

            elif goals_for == goals_against:
                draw_prob += probability

    return (
        3 * win_prob
        + draw_prob
    )

# ============================================================
# UNDERSTAT CLIENT
# ============================================================

understat = UnderstatClient()


# ============================================================
# 1. GET LAST N MATCHES
# ============================================================

def get_last_n_matches(
    team,
    season,
    n,
):

    matches_data = []
    current_season = int(season)
    min_season = current_season - 2 

    while len(matches_data) < n and current_season >= min_season:

        print(
            f"Getting match list for season "
            f"{current_season}..."
        )

        matches = understat.team(team).get_match_data(
            season=str(current_season)
        )

        matches = [
            match
            for match in matches
            if match["isResult"]
        ]

        matches.sort(
            key=lambda x: x["datetime"],
            reverse=True,
        )

        for match in matches:

            opponent = (
                match["a"]["title"]
                if match["side"] == "h"
                else match["h"]["title"]
            )

            matches_data.append({
                "match_id": str(match["id"]),
                "team": team.replace("_", " "),
                "opponent": opponent,
                "season": current_season,
                "date": match["datetime"].split(" ")[0],
                "h_a": match["side"],
            })

        current_season -= 1

    return (
        pd.DataFrame(
            matches_data[:n]
        )
        .sort_values("date")
        .reset_index(drop=True)
    )


# ============================================================
# 2. GET TEAM MATCHES FOR ONE SEASON
# ============================================================

def get_team_season_matches(
    team,
    season,
):
    """
    Get completed Understat matches for a team in one season.

    Returns an empty DataFrame if Understat has no history
    for that team in that season.
    """

    print(
        f"Getting match list for {team} "
        f" season {season}..."
    )

    try:

        matches = understat.team(team).get_match_data(
            season=str(season)
        )

    except Exception as e:

        print(
            f"  Could not retrieve {team} "
            f"for season {season}: {e}"
        )

        return pd.DataFrame()

    matches = [
        match
        for match in matches
        if match["isResult"]
    ]

    rows = []

    for match in matches:

        opponent = (
            match["a"]["title"]
            if match["side"] == "h"
            else match["h"]["title"]
        )

        rows.append({
            "match_id": str(match["id"]),
            "team": team.replace("_", " "),
            "opponent": opponent,
            "season": int(season),
            "date": match["datetime"].split(" ")[0],
            "h_a": match["side"],
        })

    if not rows:
        return pd.DataFrame(
            columns=[
                "match_id",
                "team",
                "opponent",
                "season",
                "date",
                "h_a",
            ]
        )

    return (
        pd.DataFrame(rows)
        .sort_values("date")
        .reset_index(drop=True)
    )


# ============================================================
# 3. GET PREVIOUS N MATCHES FOR AN OPPONENT
# ============================================================

def get_previous_matches(
    team,
    target_date,
    current_season,
    n,
):
    """
    Get the previous N matches for a team before target_date.

    Search strategy:

        1. Target/current season
        2. Immediately previous season

    This means teams such as Chelsea can carry their history
    from 2025 into 2026 if they need more matches.

    Teams such as Coventry, which have no Understat EPL
    history in 2025, simply return whatever current-season
    history exists.

    We deliberately DO NOT continue searching indefinitely
    into older seasons.
    """

    target_date = pd.to_datetime(
        target_date
    )

    current_season = int(
        current_season
    )

    previous_season = (
        current_season - 1
    )

    print(
        f"\nGetting previous {n} matches for "
        f"{team} before {target_date.strftime('%Y-%m-%d')}..."
    )

    season_frames = []

    # --------------------------------------------------------
    # CURRENT SEASON
    # --------------------------------------------------------

    current_matches = get_team_season_matches(
        team,
        current_season,
    )

    if not current_matches.empty:

        current_matches["date"] = (
            pd.to_datetime(
                current_matches["date"]
            )
        )

        current_matches = current_matches[
            current_matches["date"] < target_date
        ].copy()

        if not current_matches.empty:
            season_frames.append(
                current_matches
            )

    # --------------------------------------------------------
    # CHECK WHETHER WE NEED PREVIOUS SEASON
    # --------------------------------------------------------

    current_count = sum(
        len(frame)
        for frame in season_frames
    )

    if current_count >= n:

        previous_matches = pd.DataFrame()

    else:

        previous_matches = get_team_season_matches(
            team,
            previous_season,
        )

        if not previous_matches.empty:

            previous_matches["date"] = (
                pd.to_datetime(
                    previous_matches["date"]
                )
            )

            previous_matches = previous_matches[
                previous_matches["date"] < target_date
            ].copy()

            if not previous_matches.empty:
                season_frames.append(
                    previous_matches
                )

    # --------------------------------------------------------
    # NO HISTORY
    # --------------------------------------------------------

    if not season_frames:

        print(
            f"  No previous matches found for "
            f"{team} in available seasons."
        )

        return pd.DataFrame()

    # --------------------------------------------------------
    # COMBINE
    # --------------------------------------------------------

    matches = (
        pd.concat(
            season_frames,
            ignore_index=True,
        )
        .sort_values("date")
        .reset_index(drop=True)
    )

    # --------------------------------------------------------
    # KEEP MOST RECENT N
    # --------------------------------------------------------

    matches = (
        matches
        .tail(n)
        .sort_values("date")
        .reset_index(drop=True)
    )

    # --------------------------------------------------------
    # STATUS MESSAGE
    # --------------------------------------------------------

    if len(matches) < n:

        print(
            f"  Only {len(matches)} previous matches "
            f"available for {team}."
        )

    else:

        print(
            f"  Found {len(matches)} previous matches "
            f"for {team}."
        )

    return matches


# ============================================================
# 4. GET LEAGUE HISTORY FOR ONE SEASON
# ============================================================

def get_league_history_for_season(
    season,
    team,
    league_name,
):
    """
    Get PPDA history for a team for one Understat season.
    """

    print(
        f"Getting league history for season "
        f"{season} ({team})..."
    )

    league = understat.league(league_name)

    try:

        team_data = league.get_team_data(
            int(season)
        )

    except Exception as e:

        print(
            f"  Could not retrieve league history: {e}"
        )

        return pd.DataFrame()

    rows = []

    for team_id, team_info in team_data.items():

        if team_info["title"] != team:
            continue

        for match in team_info["history"]:

            rows.append({
                "team_id": team_id,
                "team": team_info["title"],
                "understat_season": season,
                **match,
            })

    if not rows:
        return pd.DataFrame()

    history = pd.DataFrame(rows)

    history["date"] = (
        pd.to_datetime(
            history["date"],
            errors="coerce",
        )
        .dt.strftime("%Y-%m-%d")
    )

    history["h_a"] = (
        history["h_a"]
        .astype(str)
        .str.lower()
        .str.strip()
    )

    return history


# ============================================================
# 5. GET ALL RELEVANT LEAGUE HISTORY
# ============================================================

def get_league_history(
    matches,
    team,
    league_name,
):
    """
    Get league history for every Understat season represented
    by the requested matches.
    """

    seasons = sorted(
        matches["season"]
        .dropna()
        .astype(int)
        .unique()
    )

    histories = []

    for season in seasons:

        history = get_league_history_for_season(
            season,
            team,
            league_name,
        )

        if not history.empty:
            histories.append(
                history
            )

    if not histories:

        raise ValueError(
            f"No league history found for {team} "
            f"for seasons {seasons}"
        )

    return pd.concat(
        histories,
        ignore_index=True,
    )


# ============================================================
# 6. MATCH PPDA TO MATCH LIST
# ============================================================

def merge_ppda(
    matches,
    league_history,
):
    """
    Merge PPDA onto a match list.

    Uses:
        date + h_a

    because the league-history endpoint doesn't expose the
    Understat match ID.
    """

    matches = matches.copy()
    history = league_history.copy()

    # --------------------------------------------------------
    # NORMALIZE DATES
    # --------------------------------------------------------

    matches["date"] = (
        pd.to_datetime(
            matches["date"],
            errors="coerce",
        )
        .dt.strftime("%Y-%m-%d")
    )

    history["date"] = (
        pd.to_datetime(
            history["date"],
            errors="coerce",
        )
        .dt.strftime("%Y-%m-%d")
    )

    # --------------------------------------------------------
    # NORMALIZE HOME/AWAY
    # --------------------------------------------------------

    matches["h_a"] = (
        matches["h_a"]
        .astype(str)
        .str.lower()
        .str.strip()
    )

    history["h_a"] = (
        history["h_a"]
        .astype(str)
        .str.lower()
        .str.strip()
    )

    # --------------------------------------------------------
    # PPDA DATA
    # --------------------------------------------------------

    ppda_data = history[
        [
            "date",
            "h_a",
            "ppda",
            "ppda_allowed",
        ]
    ].copy()

    ppda_data = (
        ppda_data
        .drop_duplicates(
            subset=[
                "date",
                "h_a",
            ]
        )
    )

    # --------------------------------------------------------
    # MERGE
    # --------------------------------------------------------

    matches = matches.merge(
        ppda_data,
        on=[
            "date",
            "h_a",
        ],
        how="left",
    )

    # --------------------------------------------------------
    # CHECK MISSING PPDA
    # --------------------------------------------------------

    missing = matches[
        matches["ppda"].isna()
    ]

    if not missing.empty:

        print(
            "\nWARNING: PPDA missing for:"
        )

        print(
            missing[
                [
                    "match_id",
                    "date",
                    "h_a",
                    "season",
                ]
            ].to_string(
                index=False
            )
        )

        raise ValueError(
            "Some matches could not be matched "
            "to league-history PPDA data."
        )

    return matches


# ============================================================
# 7. GET RAW MATCH CONTEXT
# ============================================================

def get_raw_match_context(
    match_id,
):

    match = understat.match(
        str(match_id)
    )

    # --------------------------------------------------------
    # ROSTER
    # --------------------------------------------------------

    roster_data = (
        match.get_roster_data()
    )

    roster_rows = [
        player.copy()
        for players in roster_data.values()
        for player in players.values()
    ]

    roster_df = pd.DataFrame(
        roster_rows
    )

    roster_cols = [
        "player_id",
        "player",
        "position",
        "h_a",
        "time",
        "own_goals",
        "yellow_card",
        "red_card",
        "key_passes",
        "assists",
        "xA",
        "xGChain",
        "xGBuildup",
    ]

    roster_df = roster_df[
        roster_cols
    ]

    numeric_cols = [
        "player_id",
        "time",
        "own_goals",
        "yellow_card",
        "red_card",
        "key_passes",
        "assists",
        "xA",
        "xGChain",
        "xGBuildup",
    ]

    roster_df[numeric_cols] = (
        roster_df[numeric_cols]
        .apply(
            pd.to_numeric,
            errors="coerce",
        )
    )

    # --------------------------------------------------------
    # SHOTS
    # --------------------------------------------------------

    shot_data = (
        match.get_shot_data()
    )

    shot_rows = [
        {
            **shot,
            "side": side,
        }
        for side, team_shots in shot_data.items()
        for shot in team_shots
    ]

    shots_df = pd.DataFrame(
        shot_rows
    )

    shots_df["goal"] = (
        shots_df["result"] == "Goal"
    ).astype(int)

    shots_df["npxGF"] = (
        shots_df["xG"]
        .where(
            shots_df["situation"] != "Penalty",
            0,
        )
        .astype(float)
    )

    shots_df["team"] = (
        shots_df["h_team"]
        .where(
            shots_df["h_a"] == "h",
            shots_df["a_team"],
        )
    )

    shots_df["opponent"] = (
        shots_df["h_team"]
        .where(
            shots_df["h_a"] == "a",
            shots_df["a_team"],
        )
    )

    shots_df = shots_df[
        [
            "match_id",
            "player_id",
            "player",
            "team",
            "opponent",
            "h_a",
            "xG",
            "npxGF",
            "goal",
        ]
    ]

    shots_df[
        [
            "match_id",
            "player_id",
            "xG",
        ]
    ] = (
        shots_df[
            [
                "match_id",
                "player_id",
                "xG",
            ]
        ]
        .apply(
            pd.to_numeric,
            errors="coerce",
        )
    )

    # --------------------------------------------------------
    # PLAYER SHOT TOTALS
    # --------------------------------------------------------

    shots_df = (
        shots_df
        .groupby(
            [
                "match_id",
                "player_id",
                "player",
                "h_a",
                "team",
                "opponent",
            ],
            as_index=False,
        )
        .agg(
            S=("xG", "count"),
            GF=("goal", "sum"),
            xGF=("xG", "sum"),
            npxGF=("npxGF", "sum"),
        )
    )

    return roster_df, shots_df


# ============================================================
# 8. BUILD MATCH CONTEXT
# ============================================================

def build_match_context(
    roster_df,
    shots_df,
):

    match_data = roster_df.merge(
        shots_df,
        on=[
            "player_id",
            "player",
            "h_a",
        ],
        how="left",
    )

    # --------------------------------------------------------
    # MATCH ID
    # --------------------------------------------------------

    if match_data["match_id"].isna().any():

        match_ids = (
            match_data["match_id"]
            .dropna()
        )

        if not match_ids.empty:

            match_data["match_id"] = (
                match_data["match_id"]
                .fillna(
                    match_ids.iloc[0]
                )
            )

    # --------------------------------------------------------
    # TEAM / OPPONENT
    # --------------------------------------------------------

    match_data[
        [
            "team",
            "opponent",
        ]
    ] = (
        match_data
        .groupby(
            [
                "match_id",
                "h_a",
            ]
        )[
            [
                "team",
                "opponent",
            ]
        ]
        .transform("first")
    )

    # --------------------------------------------------------
    # NO SHOTS = ZERO
    # --------------------------------------------------------

    match_data[
        [
            "S",
            "GF",
            "xGF",
            "npxGF",
        ]
    ] = (
        match_data[
            [
                "S",
                "GF",
                "xGF",
                "npxGF",
            ]
        ]
        .fillna(0)
    )

    return match_data


# ============================================================
# 9. BUILD TEAM MATCH BASELINE
# ============================================================

def build_team_match_baseline(
    match_data,
    team,
):

    # --------------------------------------------------------
    # FORMATION
    # --------------------------------------------------------

    starters = match_data[
        match_data["position"] != "Sub"
    ].copy()

    starters["position"] = (
        starters["position"]
        .map(POSITION_MAP)
    )

    formation_counts = (
        starters
        .groupby(
            [
                "match_id",
                "team",
                "position",
            ]
        )
        .size()
        .unstack(
            fill_value=0
        )
    )

    for position in [
        "DEF",
        "MID",
        "AM",
        "FWD",
    ]:

        if position not in formation_counts.columns:
            formation_counts[position] = 0

    def format_formation(row):

        counts = []

        for position in [
            "DEF",
            "MID",
            "AM",
            "FWD",
        ]:

            count = int(
                row[position]
            )

            if count > 0:
                counts.append(
                    str(count)
                )

        return "-".join(counts)

    formations = (
        formation_counts[
            [
                "DEF",
                "MID",
                "AM",
                "FWD",
            ]
        ]
        .apply(
            format_formation,
            axis=1,
        )
        .rename("formation")
        .reset_index()
    )

    # --------------------------------------------------------
    # TEAM STATS
    # --------------------------------------------------------

    stat_cols = [
        "own_goals",
        "yellow_card",
        "red_card",
        "key_passes",
        "assists",
        "xA",
        "xGChain",
        "xGBuildup",
        "S",
        "GF",
        "xGF",
        "npxGF",
    ]

    team_stats = (
        match_data
        .groupby(
            [
                "match_id",
                "team",
                "opponent",
                "h_a",
            ],
            as_index=False,
        )[stat_cols]
        .sum()
    )

    team_stats = team_stats.merge(
        formations,
        on=[
            "match_id",
            "team",
        ],
        how="left",
    )

    columns = team_stats.columns.tolist()

    columns.insert(
        columns.index("h_a") + 1,
        columns.pop(
            columns.index("formation")
        ),
    )

    team_stats = team_stats[
        columns
    ]

    # --------------------------------------------------------
    # TEAM
    # --------------------------------------------------------

    stats = team_stats[
        team_stats["team"] == team
    ].copy()

    # --------------------------------------------------------
    # OPPONENT
    # --------------------------------------------------------

    opponent = team_stats[
        team_stats["team"] != team
    ].copy()

    opponent = opponent.rename(
        columns={
            "formation": "opponent_formation",
            "own_goals": "opponent_own_goals",
            "yellow_card": "opponent_yellow_card",
            "red_card": "opponent_red_card",
            "key_passes": "opponent_key_passes",
            "assists": "opponent_assists",
            "xA": "xA_against",
            "xGChain": "xGChain_against",
            "xGBuildup": "xGBuildup_against",
            "S": "SA",
            "GF": "GA",
            "xGF": "xGA",
            "npxGF": "npxGA",
        }
    )

    opponent = opponent[
        [
            "match_id",
            "opponent_formation",
            "opponent_own_goals",
            "opponent_yellow_card",
            "opponent_red_card",
            "opponent_key_passes",
            "opponent_assists",
            "xA_against",
            "xGChain_against",
            "xGBuildup_against",
            "SA",
            "GA",
            "xGA",
            "npxGA",
        ]
    ]

    # --------------------------------------------------------
    # MERGE
    # --------------------------------------------------------

    baseline = stats.merge(
        opponent,
        on="match_id",
        how="left",
    )

    return baseline


# ============================================================
# 10. BUILD OPPONENT BASELINES
# ============================================================

def get_opponent_baselines(
    matches,
    main_team,
    league_name,
    n,
):
    """
    For every opponent faced by main_team, get that opponent's
    previous N matches before the relevant main-team match.

    Previous-season history is used when available.

    If Understat has no previous-season history for the team,
    the team is simply skipped if it has no current-season
    history.
    """

    opponent_baselines = []

    # --------------------------------------------------------
    # PROCESS EACH MAIN-TEAM MATCH
    # --------------------------------------------------------

    for _, target_match in matches.iterrows():

        opponent = target_match["opponent"]
        target_date = target_match["date"]
        target_match_id = target_match["match_id"]

        # ----------------------------------------------------
        # GET PREVIOUS MATCHES
        # ----------------------------------------------------

        previous_matches = get_previous_matches(
            team=opponent,
            target_date=target_date,
            current_season=target_match["season"],
            n=n,
        )

        if previous_matches.empty:

            print(
                f"  Skipping {opponent} - "
                f"no history available."
            )

            continue

        # ----------------------------------------------------
        # GET LEAGUE HISTORY ONLY FOR SEASONS USED
        # ----------------------------------------------------

        league_history_frames = []

        for season in (
            previous_matches["season"]
            .dropna()
            .astype(int)
            .unique()
        ):

            history = get_league_history_for_season(
                season,
                opponent,
                league_name,
            )

            if not history.empty:

                league_history_frames.append(
                    history
                )

        if not league_history_frames:

            print(
                f"  Skipping {opponent} - "
                f"no league history available."
            )

            continue

        league_history = pd.concat(
            league_history_frames,
            ignore_index=True,
        )

        # ----------------------------------------------------
        # MERGE PPDA
        # ----------------------------------------------------

        try:

            previous_matches = merge_ppda(
                previous_matches,
                league_history,
            )

        except ValueError:

            print(
                f"  Skipping {opponent} - "
                f"PPDA could not be matched."
            )

            continue

        # ----------------------------------------------------
        # PROCESS EACH PREVIOUS MATCH
        # ----------------------------------------------------

        for _, previous_match in (
            previous_matches.iterrows()
        ):

            previous_match_id = (
                previous_match["match_id"]
            )

            previous_match_date = (
                previous_match["date"]
            )

            print(
                f"  Processing opponent match "
                f"{previous_match_id} "
                f"({previous_match_date})..."
            )

            # =================================================
            # API
            # =================================================

            roster_df, shots_df = (
                get_raw_match_context(
                    previous_match_id
                )
            )

            # =================================================
            # LOCAL PROCESSING
            # =================================================

            match_data = build_match_context(
                roster_df,
                shots_df,
            )

            # =================================================
            # BASELINE
            # =================================================

            baseline = (
                build_team_match_baseline(
                    match_data,
                    opponent,
                )
            )

            # =================================================
            # PPDA
            # =================================================

            ppda = (
                previous_match["ppda"]
            )

            ppda_allowed = (
                previous_match["ppda_allowed"]
            )

            baseline["ppda"] = (
                ppda["att"] /
                ppda["def"]
            )

            baseline["ppda_allowed"] = (
                ppda_allowed["att"] /
                ppda_allowed["def"]
            )          

            # =================================================
            # MATCH METADATA
            # =================================================

            baseline["date"] = (
                previous_match_date
            )

            baseline["season"] = (
                previous_match["season"]
            )

            # =================================================
            # TARGET MATCH METADATA
            # =================================================

            baseline["target_match_id"] = (
                target_match_id
            )

            baseline["target_date"] = (
                target_date
            )

            baseline["target_team"] = (
                main_team
            )

            baseline["target_opponent"] = (
                opponent
            )

            opponent_baselines.append(
                baseline
            )

    # --------------------------------------------------------
    # NOTHING FOUND
    # --------------------------------------------------------

    if not opponent_baselines:

        return pd.DataFrame()

    # --------------------------------------------------------
    # COMBINE
    # --------------------------------------------------------

    result = (
        pd.concat(
            opponent_baselines,
            ignore_index=True,
        )
        .sort_values(
            [
                "target_date",
                "target_match_id",
                "date",
            ]
        )
        .reset_index(drop=True)
    )

    # --------------------------------------------------------
    # EXPECTED POINTS
    # --------------------------------------------------------

    result["xPts"] = result.apply(
        lambda row: expected_points(
            row["xGF"],
            row["xGA"],
        ),
        axis=1,
    )

    # --------------------------------------------------------
    # KEEP ONLY BASELINE COLUMNS
    # --------------------------------------------------------

    baseline_columns = [
        "match_id",
        "date",
        "season",
        "team",
        "opponent",
        "h_a",
        "formation",
        "own_goals",
        "yellow_card",
        "red_card",
        "key_passes",
        "assists",
        "xA",
        "xGChain",
        "xGBuildup",
        "S",
        "GF",
        "xGF",
        "npxGF",
        "opponent_formation",
        "opponent_own_goals",
        "opponent_yellow_card",
        "opponent_red_card",
        "opponent_key_passes",
        "opponent_assists",
        "xA_against",
        "xGChain_against",
        "xGBuildup_against",
        "SA",
        "GA",
        "xGA",
        "xPts",
        "npxGA",
        "ppda",
        "ppda_allowed",
    ]

    result = result[baseline_columns]

    # --------------------------------------------------------
    # MATCH ID TYPES
    # --------------------------------------------------------

    result["match_id"] = (
        result["match_id"]
        .astype(int)
    )

    return result



# ============================================================
# 11. MAIN BATCH FUNCTION
# ============================================================

def get_last_n_match_baselines(
    team,
    season,
    n,
    opponent_n_matches,
    league_name,
):

    # --------------------------------------------------------
    # GET MATCHES
    # --------------------------------------------------------

    print(
        f"Getting last {n} matches for {team}..."
    )

    matches = get_last_n_matches(
        team,
        season,
        n,
    )

    # --------------------------------------------------------
    # GET LEAGUE HISTORY
    # --------------------------------------------------------

    print(
        "Getting league history..."
    )

    league_history = get_league_history(
        matches,
        team,
        league_name,
    )

    # --------------------------------------------------------
    # MERGE PPDA
    # --------------------------------------------------------

    matches = merge_ppda(
        matches,
        league_history,
    )

    # --------------------------------------------------------
    # PROCESS EACH MAIN-TEAM MATCH
    # --------------------------------------------------------

    baselines = []
    player_contexts = []

    for _, match in matches.iterrows():

        match_id = match["match_id"]
        match_date = match["date"]

        print(
            f"Processing {match_id} "
            f"({match_date})..."
        )

        # ====================================================
        # API
        # ====================================================

        roster_df, shots_df = (
            get_raw_match_context(
                match_id
            )
        )

        # ====================================================
        # LOCAL PROCESSING
        # ====================================================

        match_data = build_match_context(
            roster_df,
            shots_df,
        )

        # ----------------------------------------------------
        # PRESERVE PLAYER-LEVEL DATA
        # ----------------------------------------------------

        match_data["date"] = (
            match_date
        )

        match_data["season"] = (
            match["season"]
        )

        player_contexts.append(
            match_data.copy()
        )

        # ----------------------------------------------------
        # BUILD MATCH-LEVEL BASELINE
        # ----------------------------------------------------

        baseline = (
            build_team_match_baseline(
                match_data,
                team,
            )
        )

        # ====================================================
        # PPDA
        # ====================================================

        ppda = match["ppda"]
        ppda_allowed = match["ppda_allowed"]

        baseline["ppda"] = (
            ppda["att"] /
            ppda["def"]
        )

        baseline["ppda_allowed"] = (
            ppda_allowed["att"] /
            ppda_allowed["def"]
        )

        # ====================================================
        # EXPECTED POINTS
        # ====================================================

        baseline["xPts"] = expected_points(
            baseline["xGF"].iloc[0],
            baseline["xGA"].iloc[0],
        )  

        # ====================================================
        # MATCH METADATA
        # ====================================================

        baseline["date"] = (
            match_date
        )

        baseline["season"] = (
            match["season"]
        )

        baselines.append(
            baseline
        )

    # --------------------------------------------------------
    # COMBINE MAIN-TEAM BASELINES
    # --------------------------------------------------------

    result = (
        pd.concat(
            baselines,
            ignore_index=True,
        )
        .sort_values("date")
        .reset_index(drop=True)
    )

    result["match_id"] = (
        result["match_id"]
        .astype(int)
    )

    # Put date and season after match_id.
    columns = result.columns.tolist()

    columns.remove("date")
    columns.remove("season")

    match_id_index = columns.index(
        "match_id"
    )

    columns.insert(
        match_id_index + 1,
        "date",
    )

    columns.insert(
        match_id_index + 2,
        "season",
    )

    result = result[columns]

    # --------------------------------------------------------
    # COMBINE PLAYER CONTEXT
    # --------------------------------------------------------

    player_df = (
        pd.concat(
            player_contexts,
            ignore_index=True,
        )
        .sort_values(
            [
                "date",
                "match_id",
                "team",
                "player",
            ]
        )
        .reset_index(drop=True)
    )

    player_df["match_id"] = (
        player_df["match_id"]
        .astype(int)
    )

        # --------------------------------------------------------
    # FINAL COLUMN ORDER
    # --------------------------------------------------------

    player_columns = [
        "match_id",
        "date",
        "season",
        "team",
        "opponent",
        "player_id",
        "player",
        "position",
        "h_a",
        "time",
        "own_goals",
        "yellow_card",
        "red_card",
        "key_passes",
        "assists",
        "xA",
        "xGChain",
        "xGBuildup",
        "S",
        "GF",
        "xGF",
        "npxGF",
    ]

    player_df = player_df[player_columns]


    # --------------------------------------------------------
    # OPPONENT BASELINES
    # --------------------------------------------------------

    opponent_df = (
        get_opponent_baselines(
            matches=matches,
            main_team=team,
            n=opponent_n_matches,
            league_name=league_name,
        )
    )

    # --------------------------------------------------------
    # RETURN
    # --------------------------------------------------------

    return (
        result,
        player_df,
        opponent_df,
    )


def parse_args():
    parser = argparse.ArgumentParser(
        description="Load data."
    )

    parser.add_argument(
        "--season",
        type=int,
        default=DEFAULT_SEASON,
    )

    parser.add_argument(
        "--team",
        type=str,
        default=DEFAULT_TEAM,
    )

    parser.add_argument(
        "--league",
        type=str,
        default=DEFAULT_LEAGUE,
    )

    parser.add_argument(
        "--n-matches",
        type=int,
        default=DEFAULT_N_MATCHES,
    )

    parser.add_argument(
        "--opponent-n-matches",
        type=int,
        default=DEFAULT_OPPONENT_N_MATCHES,
    )

    return parser.parse_args()


# ============================================================
# RUN
# ============================================================

args = parse_args()

df, player_df, opponent_df = (
    get_last_n_match_baselines(
        team=args.team,
        season=args.season,
        league_name=args.league,
        n=args.n_matches,
        opponent_n_matches=args.opponent_n_matches,
    )
)

# ============================================================
# OUTPUT
# ============================================================

df.to_csv('output/baseline.csv', index=False)
player_df.to_csv('output/context.csv', index=False)
opponent_df.to_csv('output/opponents.csv', index=False)
