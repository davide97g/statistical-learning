import pandas as pd

features = ["Commenti", "IscrittiConquistati", "IscrittiPersi",
            "MiPiace", "NonMiPiace", "TempoDiVisualizzazione", "Visualizzazioni"]

map = {}
for feature in features:
    df = pd.read_csv("../../data/merged/"+feature+"_total.csv")
    colname = df.columns[2]

    for i in range(len(df)):
        row = df.iloc[i]
        id = row['Video']
        value = row[colname]

        # never inserted this video before
        if map.get(id) is None:
            map[id] = {}  # create sub map empty

        # test if this feature is present
        if map[id].get(colname) is None:
            # if not, create new array in the key "colname"
            map[id][colname] = [value]
        else:
            # already present, just append value
            map[id][colname].append(value)


print(map.get("83BnzNllyv8"))
