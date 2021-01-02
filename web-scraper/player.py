from bs4 import BeautifulSoup as bs
import re
import pandas as pd

CURRENT_YEAR = 21
N_SEASON_HISTORY = 4


class PlayerProfile:
    def __init__(self, playerUrl, pageScraper):
        urlPerfPage = playerUrl.replace("profil", "leistungsdatendetails")
        soup = pageScraper(playerUrl)

        playerAttributes = {}

        # scraping profile page information
        playerAttributes["name"] = soup.find(
            "div", class_="dataMain").find("h1").text
        StoredAttributes = ["Age:", "Height:", "Nationality:",
                            "Position:", "Foot:", "Current club:", "Contract expires:"]
        for entry in soup.find("table", class_="auflistung").find_all("th"):
            key = entry.text.strip()
            val = entry.find_next_sibling().text
            if key in StoredAttributes:
                playerAttributes[key[:-1].lower()] = val.strip()

        # find market value
        marketValue = soup.find("div", class_="zeile-oben").find("div",
                                                                 class_="right-td").text
        playerAttributes['market value'] = marketValue.replace("£", "").strip()
        # print(playerAttributes)

        # cleaning some entries
        if "height" in playerAttributes:
            # converting height in meters str to cm int
            meter, centimeters = re.search(
                "(\d),(\d+)", playerAttributes["height"]).groups()
            playerAttributes["height"] = int(meter)*100 + int(centimeters)
        if "nationality" in playerAttributes:
            if "\xa0" in playerAttributes["nationality"]:
                playerAttributes["nationality"] = playerAttributes["nationality"].replace(
                    "\xa0", " ")
        if "position" in playerAttributes:
            playerAttributes["position"] = PlayerProfile.mapPosition(
                playerAttributes["position"])
        if "age" in playerAttributes:
            playerAttributes["age"] = int(playerAttributes["age"])
        if "market value" in playerAttributes:
            # search for the right multiplier
            if "m" in playerAttributes["market value"]:
                playerAttributes["market value"] = float(
                    playerAttributes["market value"].replace("m", ""))*1000000
            elif "Th." in playerAttributes["market value"]:
                playerAttributes["market value"] = float(
                    playerAttributes["market value"].replace("Th.", ""))*1000

        # scraping performance page information
        soup = pageScraper(urlPerfPage)
        performanceColumns = ("season", "games", "goals", "assists",
                              "yellows", "second yellows", "reds", "minutes")
        performanceRows = pd.DataFrame({col: [] for col in performanceColumns})
        for row in soup.find("div", class_="responsive-table").find("tbody").find_all("tr"):
            # try:
            rowContents = PlayerProfile.readRow(row)
            # except:
            #	 print( row)
            # else:
            #	 pass
            year = rowContents[0]
            if re.match("\d{4}", year):
                year = int(year[2:])
                year = "%02d/%02d" % (year-1, year)
                rowContents[0] = year
            if not re.match("\d{2}\/\d{2}", year):
                raise ValueError("Wrong format for played year")
            if int(year[:2]) < CURRENT_YEAR - N_SEASON_HISTORY:
                break
            # print( rowContents)
            performanceRows = performanceRows.append({col: val for col, val in zip(
                performanceColumns, rowContents)}, ignore_index=True)
        performanceDF = performanceRows.groupby("season").sum()
        # converting to serie
        performanceSeries = pd.Series({"%s %s" % (
            row, col): performanceDF[col][row] for row in performanceDF.index for col in performanceDF.columns})

        self.PlayerData = pd.Series(playerAttributes).append(performanceSeries)
        # print("\t%s done" % self.PlayerData["name"])
        # print(self.PlayerData)

    def __str__(self):
        return "Performance profile for %s" % self.PlayerData["name"]

    def __repr__(self):
        return "< profile of %s >" % self.PlayerData["name"]

    @ staticmethod
    def readRow(row):
        cells = row.find_all("td")
        cells = list(map(lambda x: x.text.strip(), cells))
        # print(cells)
        year = cells[0]
        games_played = cells[4]
        goals_scored = cells[6]
        assists = cells[7]

        cards = cells[8].replace("\xa0", "").split("/")
        yellows = int(cards[0].replace("-", "0"))
        second_yellows = int(cards[1].replace("-", "0"))
        reds = int(cards[2].replace("-", "0"))

        minutes_played = cells[-1]
        games_played = int(games_played) if games_played != "-" else 0
        goals_scored = int(goals_scored) if goals_scored != "-" else 0
        assists = int(assists) if assists != "-" else 0
        minutes_played = int(
            minutes_played[:-1].replace(".", "")) if minutes_played != "-" else 0
        return [year, games_played, goals_scored, assists, yellows, second_yellows, reds, minutes_played]

    @ staticmethod
    def mapPosition(position):
        if 'attack - Right Winger' in position:
            return "A-RW"
        elif 'attack - Left Winger' in position:
            return "A-LW"
        elif "Centre-Forward" in position:
            return "CF"
        elif "Defender - Centre-Back" in position:
            return "D-CB"
        elif "Defender - Left-Back" in position:
            return "D-LB"
        elif "Defender - Right-Back" in position:
            return "D-RB"
        elif "midfield - Defensive Midfield" in position:
            return "M-DM"
        elif "midfield - Central Midfield" in position:
            return "M-CM"
        elif "midfield - Attacking Midfield" in position:
            return "M-AM"
        elif "midfield - Right Midfield" in position:
            return "M-RM"
        elif "midfield - Left Midfield" in position:
            return "M-LM"
        elif "attack - Second Striker" in position:
            return "A-SS"
        elif "attack" in position:
            return "A"
        elif "Defender" in position:
            return "D"
        else:
            print("\n\tPosition", position, "not mapped")
