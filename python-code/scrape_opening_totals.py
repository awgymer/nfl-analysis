import requests
import pandas as pd


def scrape_tables(wk):
    r = requests.get(f'https://thefootballlines.com/nfl-over-under/{wk}')
    tabs = pd.read_html(r.content)
    return tabs


YEAR_MAP = {
    0: 2019,
    1: 2018,
    2: 2017,
    3: 2016,
    4: 2015,
    5: 2014,
    6: 2013,
    7: 2012,
    8: 2011,
    9: 2010,
    10: 2009,
    11: 2008,
    12: 2007
}


def scrape_reg_season():
    all_tabs = []
    for i in range(1, 18):
        tabs = scrape_tables(f'week-{i}')
        for y, t in enumerate(tabs):
            t['game_id'] = t.apply(
                lambda x: parse_home_team_id(
                    x['Home Team ID'],
                    YEAR_MAP[y],
                    i
                ), axis=1
            )
        all_tabs.extend(tabs)
    return all_tabs


def scrape_post_season():
    poff_dict = {
        'wildcard': 18, 'division': 19,
        'conference': 20, 'superbowl': 21
    }
    all_tabs = []
    for k in poff_dict.keys():
        tabs = scrape_tables(k)
        for y, t in enumerate(tabs):
            t['game_id'] = t.apply(
                lambda x: parse_home_team_id(
                    x['Home Team ID'],
                    YEAR_MAP[y],
                    poff_dict[k]
                ), axis=1
            )
        all_tabs.extend(tabs)
    return all_tabs


TEAM_MAP = {
    'LAR': 'LA',
    'JAC': 'JAX',
}


def parse_home_team_id(hid, szn, wk):
    tids = [
        s.strip().split(' ')[0]
        for s in hid.split('@')
    ]
    return f'{szn}_{wk:02}_{"_".join([TEAM_MAP.get(t, t) for t in tids])}'


COLUMN_MAP = {
    'Home Team ID': 'game_detail',
    'Date': 'date',
    'Opening Total': 'opening_total_line',
    'Opening Result': 'opening_result',
    'Closing Total': 'closing_total_line',
    'Closing Result': 'closing_result',
    'Game Total': 'game_total'
}


def do_scrape(outfile):
    regtabs = scrape_reg_season()
    posttabs = scrape_post_season()
    all_tabs = []
    all_tabs.extend(regtabs)
    all_tabs.extend(posttabs)
    df = pd.concat(all_tabs)
    df.rename(columns=COLUMN_MAP, inplace=True)
    df.to_csv(outfile, index=False)
