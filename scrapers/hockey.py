import io
import zipfile
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

def load_data(season=DEFAULT_SEASON):

    datas = ["skaters", "goalies", "lines"]

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/140.0.0.0 Safari/537.36"
        )
    }

    results = {}

    for data in datas:
        season_dfs = []

        # Load season and season - 1
        for s in [season, season - 1]:

            url = (
                f"https://peter-tanner.com/moneypuck/downloads/"
                f"seasonPlayersSummary/{data}/{s}.zip"
            )

            response = requests.get(url, headers=headers)
            response.raise_for_status()

            with zipfile.ZipFile(io.BytesIO(response.content)) as z:
                csv_file = next(
                    name for name in z.namelist()
                    if name.endswith(".csv")
                )

                df = pd.read_csv(z.open(csv_file))

            # Make sure the season is explicitly available
            df["season"] = s

            season_dfs.append(df)

        # Combine the two seasons
        results[data] = pd.concat(
            season_dfs,
            ignore_index=True
        )

        print(f"{data}: {results[data].shape}")

    return (
        results["skaters"],
        results["goalies"],
        results["lines"]
    )


# ============================================================
# LOAD
# ============================================================

skaters, goalies, lines = load_data()

print(skaters.head())
print(goalies.head())
print(lines.head())
