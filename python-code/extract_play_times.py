import json
import pathlib
import gzip
import pandas as pd


def extract_play_times(data_dir, yr):
    datapath = pathlib.Path(data_dir).joinpath(str(yr))
    all_times = []
    for file in datapath.glob('*.json.gz'):
        with gzip.open(file, 'r') as in_gz:
            jdat = json.load(in_gz)
        game_id = jdat['data']['viewer']['gameDetail']['id'][0]
        plays = jdat['data']['viewer']['gameDetail']['plays']
        times = [
            {'play_id': p['playId'],
             'game_id': game_id,
             'real_time': p['timeOfDay']
             }
            for p in plays
        ]
        all_times.extend(times)
    return all_times


def play_times_to_df(data_dir, startyr, endyr):
    playtimes = []
    for y in range(startyr, endyr+1):
        t = extract_play_times(data_dir, y)
        playtimes.extend(t)
    return pd.DataFrame(playtimes)
