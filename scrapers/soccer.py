"""
Season: '2026' (2026/27)

EPL
Aston Villa
Everton
Bournemouth
Sunderland
Crystal Palace
Chelsea
Tottenham
Arsenal
Newcastle United
Liverpool
Manchester City
Manchester United
Hull
Brighton
Fulham
Brentford
Leeds
Nottingham Forest
Ipswich
Coventry

La_Liga
Malaga
Sevilla
Deportivo La Coruna
Real Sociedad
Espanyol
Getafe
Atletico Madrid
Rayo Vallecano
Valencia
Athletic Club
Barcelona
Real Madrid
Levante
Celta Vigo
Real Betis
Villarreal
Osasuna
Alaves
Elche
Racing Santander

Bundesliga
Bayern Munich
Hamburger SV
Bayer Leverkusen
Hoffenheim
Augsburg
Werder Bremen
Schalke 04
Mainz 05
Borussia Dortmund
Borussia M.Gladbach
Eintracht Frankfurt
VfB Stuttgart
FC Cologne
Freiburg
RasenBallsport Leipzig
Paderborn
Union Berlin
Elversberg

Serie_A
Roma
Lazio
Bologna
Juventus
Udinese
Genoa
Sassuolo
Napoli
Inter
Atalanta
Fiorentina
AC Milan
Frosinone
Torino
Cagliari
Parma Calcio 1913
Lecce
Venezia
Monza
Como

Ligue_1
Lille
Paris Saint Germain
Rennes
Marseille
Angers
Nice
Monaco
Troyes
Toulouse
Lyon
Lorient
Lens
Strasbourg
Brest
Auxerre
Le Havre
Paris FC
Le Mans

RFPL
Spartak Moscow
CSKA Moscow
Rubin Kazan
FC Rostov
FK Akhmat
Zenit St. Petersburg
Dinamo Moscow
Lokomotiv Moscow
Krylya Sovetov Samara
FC Krasnodar
FC Orenburg
Fakel
Baltika
Akron
Dynamo Makhachkala
Rodina
"""

import pandas as pd
from understatapi import UnderstatClient

season = 2026
team = "Arsenal"


def get_last_n_matches(team, season, n):

    matches_data = []
    with UnderstatClient() as understat:
        current_season = int(season)

        while len(matches_data) < n: 

            matches = understat.team(team).get_match_data(season=str(current_season))
            matches = [match for match in matches if match["isResult"]]
            matches = sorted(matches, key=lambda x: x["datetime"], reverse=True)

            for match in matches:
                opponent = (match["a"]["title"] if match["side"] == "h" else match["h"]["title"])

                matches_data.append({
                    "matchid": match["id"],
                    "team": team.replace("_", " "),
                    "opp": opponent,
                    "season": current_season,
                    "date": match["datetime"].split(" ")[0]
                })

            current_season -= 1

    return pd.DataFrame(matches_data[:n]).iloc[::-1].reset_index(drop=True)

df = get_last_n_matches(team=team, season=season, n=10)

##############################################################################################

with UnderstatClient() as understat:
    shots = understat.match("31216").get_shot_data()

shot_rows = []
for side, team_shots in shots.items():
    for shot in team_shots:
        row = shot.copy()
        row["side"] = side
        shot_rows.append(row)

shots_df = pd.DataFrame(shot_rows)
shots_df['goal'] = (shots_df['result'] == 'Goal').astype(int)
shots_df["npxGF"] = shots_df["xG"].where(shots_df["situation"] != "Penalty", 0).astype(float)
shots_df["team"] = shots_df["h_team"].where(shots_df["h_a"] == "h", shots_df["a_team"])
shots_df["opponent"] = shots_df["h_team"].where(shots_df["h_a"] == "a", shots_df["a_team"])
shots_df = shots_df[
    [
        'match_id',
        'player_id',
        'player',
        'team', 
        'opponent',
        'h_a',
        'situation',
        'xG',
        'npxGF',
        'result',
        'goal'
    ]
]

shots_df[["xG", "player_id", "match_id"]] = shots_df[["xG", "player_id", "match_id"]].apply(
    pd.to_numeric,
    errors="coerce"
)

result = (shots_df.groupby(["match_id", "player_id", "player", "h_a", 'team', 'opponent'], as_index=False)
    .agg(
        shots=("xG", "count"),
        GF=("goal", "sum"),
        xGF=("xG", "sum"),
        npxGF=('npxGF', 'sum')
    )
)

##############################################################################################

with UnderstatClient() as understat:
    match = understat.match("31216")
    player_stats = match.get_roster_data()

    player_rows = []

    for side, players in player_stats.items():
        for roster_id, player in players.items():
            row = player.copy()
            player_rows.append(row)

    player_df = pd.DataFrame(player_rows)

player_df = player_df[
    ['player_id', 
     'player', 
     'position',
      'h_a',
      'time', 
      'own_goals',  
      'yellow_card', 
      'red_card',
      'key_passes', 
      'assists', 
      'xA', 
      'xGChain',
      'xGBuildup']
]

player_df[["player_id", "time", 'own_goals', 'yellow_card', 'red_card', 'key_passes', 'assists', 'xA', 
    'xGChain', 'xGBuildup']] = player_df[["player_id", "time", 'own_goals', 'yellow_card', 'red_card', 
    'key_passes', 'assists', 'xA', 
    'xGChain', 'xGBuildup']].apply(
    pd.to_numeric,
    errors="coerce"
)

##############################################################################################

merged_df = player_df.merge(result,
    on=["player_id", "player", "h_a"],
    how="left"
)

merged_df = merged_df[['match_id', 'player_id', 'player', 'position', 'team', 'opponent', 'h_a', 'time',
                        'own_goals', 'yellow_card', 'red_card',	'key_passes', 'assists', 'xA',
                        'xGChain', 'xGBuildup', 'shots', 'GF', 'xGF', 'npxGF']]

merged_df["match_id"] = merged_df["match_id"].fillna(merged_df["match_id"].dropna().iloc[0])
merged_df[["team", "opponent"]] = (merged_df.groupby(["match_id", "h_a"])[["team", "opponent"]]
                                   .transform("first"))
merged_df[["shots", "GF", "xGF", "npxGF"]] = merged_df[["shots", "GF", "xGF", "npxGF"]].fillna(0)

print(merged_df)
