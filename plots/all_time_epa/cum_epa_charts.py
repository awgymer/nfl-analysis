import pandas as pd
import plotly.io


def read_and_prep(incsv):
    df = pd.read_csv(incsv)
    df.label = df.label.str.replace('\n', '<br>')
    return df


def get_traces(df, xkey):
    all_traces = []
    for g, grp in df.groupby(['commname', 'career_stop']):
        trace = dict(
            type="scattergl",
            mode="lines",
            line=dict(shape='vh', color=grp.col1.iloc[0]),
            x=grp[xkey],
            y=grp.cum_epa,
            text=grp.label,
            hovertemplate="%{text}<br>EPA: %{y}<extra></extra>",
            legendgroup=g[0],
            name=g[0],
            showlegend=bool(g[1] == 1)
        )
        all_traces.append(trace)
    return all_traces


def title_axes_all_time(df):
    seasons = sorted(df.season.unique())
    tickvals = list(range(1, max(df.gkey), 21))
    xaxis = dict(
        title={'text': 'NFL Seasons -->'},
        tickvals=tickvals,
        ticktext=seasons
    )
    yaxis = dict(
        title={'text': 'Cumulative QB EPA'}
    )
    title = dict(
        text='All-Time Cumulative QB EPA (incl. playoffs)',
        font={'size': 30},
        xref='paper',
        xanchor='left',
        x=0
    )
    return dict(title=title, xaxis=xaxis, yaxis=yaxis)


def title_axes_player_start():
    xaxis = dict(
        title={'text': 'NFL Gameweeks Since First Game'},
    )
    yaxis = dict(
        title={'text': 'Cumulative QB EPA'}
    )
    title = dict(
        text='Career Cumulative QB EPA (incl. playoffs)',
        font={'size': 30},
        xref='paper',
        xanchor='left',
        x=0
    )
    return dict(title=title, xaxis=xaxis, yaxis=yaxis)


def get_credits():
    credits_text = 'Chart by @awgymer | Data from @nflfastR'
    credits = dict(
        showarrow=False,
        text=credits_text,
        xref='paper',
        yref='paper',
        xanchor='right',
        yanchor='top',
        x=1, y=-0.05,
    )
    return credits


def get_subtitle(text):
    subtitle = dict(
            showarrow=False,
            text=text,
            xref='paper',
            yref='paper',
            xanchor='left',
            yanchor='bottom',
            x=0, y=1,
            align='left'
        )
    return subtitle


def make_plot(traces, layout, annotations=None):
    credits = get_credits()
    _annotations = [credits]
    if annotations is not None:
        _annotations.extend(annotations)
    _layout = dict(
        template='plotly_white',
        annotations=_annotations
    )
    _layout.update(layout)
    fig = dict(data=traces, layout=_layout)
    return fig


def save_plot(plt, outfile, plotlyjs='cdn'):
    plotly.io.write_html(
        plt, outfile, include_plotlyjs=plotlyjs
    )


def all_time_plot(df):
    traces = get_traces(df, 'gkey')
    layout = title_axes_all_time(df)
    plt = make_plot(traces, layout)
    return plt


def games_played_plot(df):
    dfsub = df[~pd.isna(df.player_game_played)]
    traces = get_traces(dfsub, 'player_game_played')
    layout = title_axes_player_start()
    layout['xaxis']['title']['text'] = 'Games Played'
    subtitle = get_subtitle(
        'Excludes games in which player made no plays')
    plt = make_plot(traces, layout, [subtitle])
    return plt


def career_span_plot(df):
    traces = get_traces(df, 'player_game')
    layout = title_axes_player_start()
    subtitle = get_subtitle(
        'Includes games in which player made no plays e.g. was injured')
    plt = make_plot(traces, layout, [subtitle])
    return plt
