from progress.bar import ChargingBar
from player import PlayerProfile

BASE_URL = "https://www.transfermarkt.co.uk"


class Team:
    def __init__(self, url, name, scraper, teamName):
        self.LeagueName = name
        self.TeamName = teamName
        soup = scraper(url)
        # reading player table and filtering out goalkeepers
        playerTable = soup.find("table", class_="items")
        players = playerTable.find_all("a", class_="spielprofil_tooltip")[::2]
        players = filter(Team.isNotGoalkeeper, players)
        playersUrls = [BASE_URL + player["href"]
                       for player in players]
        #self.PlayerData = [PlayerProfile( playerUrl, scraper) for playerUrl in playersUrls]
        self.PlayersData = []
        bar = ChargingBar("Extracting "+self.TeamName +
                          " players", max=len(playersUrls))
        for playerUrl in playersUrls:
            # print("playerUrl", playerUrl)
            try:
                NewPlayerProfile = PlayerProfile(playerUrl, scraper)
                NewPlayerProfile.PlayerData["current league"] = self.LeagueName
                self.PlayersData.append(NewPlayerProfile)
                bar.next()
            except:
                continue

        bar.finish()

    @staticmethod
    def isStrikerOrWinger(player):
        position = player.find_next("tr").text.strip().lower()
        # print(position)
        return "wing" in position or "centre-forward" in position

    @staticmethod
    def isNotGoalkeeper(player):
        position = player.find_next("tr").text.strip().lower()
        # print(position)
        return not("goalkeeper" in position)
