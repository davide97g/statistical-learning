import re
from team import Team

BASE_URL = "https://www.transfermarkt.co.uk"


class League:
    def __init__(self, name, url, scrapper):
        self.LeagueName = name
        soup = scrapper(url)
        teamsTable = soup.find("table", class_="items")
        teamUrls = teamsTable.find_all(
            "a", class_="vereinprofil_tooltip", id=re.compile("\d+"))[::2]
        teamNames = [teamUrl['href'].split(
            "/")[1].replace("-", " ") for teamUrl in teamUrls]
        teamUrls = [BASE_URL + teamUrl["href"] for teamUrl in teamUrls]
        self.TeamsData = []
        print("\nExtracting", self.LeagueName, "teams\n")
        for i in range(len(teamUrls)):
            self.TeamsData.append(
                Team(teamUrls[i], self.LeagueName, scrapper, teamNames[i]))
