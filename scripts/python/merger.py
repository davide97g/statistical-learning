import pandas as pd
import glob


features = ["Commenti", "IscrittiConquistati", "IscrittiPersi",
            "MiPiace", "NonMiPiace", "TempoDiVisualizzazione", "Visualizzazioni"]

columns_to_drop = ["Titolo video",
                   "Ora pubblicazione video"]

path_to_save = "../../data/merged/"

for feature in features:

    print("\n### FEATURE "+feature+"\n")

    # path for file in feature folder
    path = '..\\..\\data\\features\\'+feature+'\\'

    # find all matching file inside every feature folder
    files = [f for f in glob.glob(
        path + "**/Dati del grafico.csv", recursive=True)]

    # store all the dataframes for one feature
    feature_dfs = []
    total_df = pd.DataFrame()

    for f in files:
        # print("\tFile: "+f)

        # read csv
        df = pd.read_csv(f)

        # drop columns
        df = df.drop(columns=columns_to_drop)

        # append to list
        feature_dfs.append(df)

        if len(total_df) == 0:
            total_df = df
        else:
            total_df.append(df)
    total_df.to_csv(path_to_save+feature+"_total.csv", index=False)
