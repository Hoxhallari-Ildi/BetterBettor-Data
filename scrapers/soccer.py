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

df = get_last_n_matches(team="Manchester_United", season="2026", n=11)

##############################################################################################

with UnderstatClient() as understat:

    shots = understat.match("29100").get_shot_data()

shot_rows = []
for side, team_shots in shots.items():
    for shot in team_shots:
        row = shot.copy()
        row["side"] = side
        shot_rows.append(row)

shots_df = pd.DataFrame(shot_rows)
shots_df = shots_df[
    [
        'match_id',
        'player_id',
        'player',
        'h_a',
        'h_team',
        'a_team',
        'situation',
        'xG',
        'result'
    ]
]

print(shots_df)

with UnderstatClient() as understat:
    match = understat.match("29100")
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
