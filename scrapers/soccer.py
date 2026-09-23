from understatapi import UnderstatClient

team = "Manchester_United"
season = "2026"

with UnderstatClient() as understat:
    matches = understat.team(team).get_match_data(season=season)

games_played = len([match for match in matches if match["isResult"] is True])
print(f"{team} played {games_played} games in the {season} season.")
