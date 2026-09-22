import asyncio
from understat import Understat
import aiohttp

async def main():
    async with aiohttp.ClientSession() as session:
        understat = Understat(session)

        data = await understat.get_player_stats(
            player_id=2097,
        )

        print(data)

asyncio.run(main())