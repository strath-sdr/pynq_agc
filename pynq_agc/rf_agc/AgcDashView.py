import dash_core_components as dcc
import dash_html_components as html
import dash_bootstrap_components as dbc
import numpy as np

_in_control_panel = lambda state : dbc.Container([
    dbc.Row(
        dbc.Col([
            dbc.ListGroup([
                dbc.ListGroupItem("Input Signal", color='secondary'),
                dbc.ListGroupItem([
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Type", addon_type="prepend"),
                        dbc.Select(id='in-sig-type',options=[
                            {"label": "Sinusoid", "value": 'sin'},
                            {"label": "Amplitude Modulated", "value": 'am'},
                            {"label": "Frequency Modulated", "value": 'fm'},
                            {"label": "QPSK Baseband", "value": 'qpsk_bb'},
                            {"label": "QPSK IF", "value": 'qpsk_if'},
                        ], value=state['signal_mode']),
                ], className='mt-1'),
                     dbc.InputGroup([
                        dbc.InputGroupAddon("Data", addon_type="prepend"),
                        dcc.Slider(id='in-f-data', min=400,max=4000, value=state['fm']/1000, className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['fm']/1000)+" kHz",id='in-f-data-label', addon_type="append"),
                ], className='mt-1'),
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Carrier", addon_type="prepend"),
                        dcc.Slider(id='in-f-carrier',min=4,max=400,step=10, value=(state['fc']/1000000), className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['fc']/1000000)+" MHz", id='in-f-carrier-label', addon_type="append")
                ], className='mt-1'),
                ]),
             ])
        ])
    ),

    dbc.Row(
        dbc.Col([
            dbc.ListGroup([
                dbc.ListGroupItem("Envelope handles", color='secondary'),
                dbc.ListGroupItem(
                    dbc.ButtonGroup([
                        dbc.Button(html.Span('',className='fa fa-plus'), id='btn-add-handle',color="primary", outline=True, className='w-100'),
                        dbc.Button(html.Span('',className='fa fa-minus'), id='btn-rm-handle', color="danger", outline=True, className='w-100')
                    ], className='d-flex')
                )
             ])
        ])
    ,className='mt-3')
])

_agc_control_panel = lambda state: dbc.Container([
    dbc.Row(
        dbc.Col([


            dbc.Tabs([

            # Thresholds tab
                dbc.Tab(
                dbc.ListGroup([
                dbc.ListGroupItem([

                    dbc.InputGroup([
                        dbc.InputGroupAddon("Range", addon_type="prepend"),
                        dcc.RangeSlider(id='thres-range',min=0,max=1,step=0.01, value=(state['thres_low'], state['thres_high']), className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['thres_low']*100)+"-"+ str(state['thres_high']*100) + " %", id='thres-range-label', addon_type="append")
                ], className='mt-1'),
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Hysteresis", addon_type="prepend"),
                        dcc.Slider(id='thres-hyst',min=0.01,max=10,step=0.01, value=(state['thres_hyst']*1e6), className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['thres_hyst']*1e6)+" µs", id='thres-hyst-label', addon_type="append")
                ], className='mt-1'),

                dbc.InputGroup([
                        dbc.InputGroupAddon("Bypass AGC", addon_type="prepend"),
                        dbc.Checklist(
                            options=[
                                {"label": "", "value": 1},
                            ],
                            value=[0],
                            id="agc-bypass",
                            switch=True,
                            className = "form-control"
                        ),
                ], className='mt-1'),

                ])
                ])
                , label = "Thresholds"),

            # AGC tab
            dbc.Tab(
            dbc.ListGroup([
                dbc.ListGroupItem([
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Atk period", addon_type="prepend"),
                        dcc.Slider(id='agc-atk-t',min=0.01,max=2,step=0.01, value=(state['agc_atk_t']*1e6), className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['agc_atk_t']*1e6)+" µs", id='agc-atk-t-label', addon_type="append")
                ], className='mt-1'),
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Atk step", addon_type="prepend"),
                        dcc.Slider(id='agc-atk-step',min=-18,max=-1,step=1, value=(int(np.log2(state['agc_atk_step']))), className="form-control dbc-slider"),
                        dbc.InputGroupAddon('2^' + str(int(np.log2(state['agc_atk_step']))), id='agc-atk-step-label', addon_type="append")
                ], className='mt-1'),
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Dec period", addon_type="prepend"),
                        dcc.Slider(id='agc-dec-t',min=0.01,max=2,step=0.01, value=(state['agc_dec_t']*1e6), className="form-control dbc-slider"),
                        dbc.InputGroupAddon(str(state['agc_dec_t']*1e6)+" µs", id='agc-dec-t-label', addon_type="append")
                ], className='mt-1'),
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Dec step", addon_type="prepend"),
                        dcc.Slider(id='agc-dec-step',min=-18,max=-1,step=1, value=(int(np.log2(state['agc_dec_step']))), className="form-control dbc-slider"),
                        dbc.InputGroupAddon('2^' + str(int(np.log2(state['agc_dec_step']))), id='agc-dec-step-label', addon_type="append")
                ], className='mt-1'),

                ]),
            ])

                , label = "AGC Parameters"),

            ])
        ])
        ,className='mt-3'),
    
    dbc.Row(
        dbc.Col([
            dbc.ListGroup([
                dbc.ListGroupItem("Visualisation", color='secondary'),
                dbc.ListGroupItem([
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Domain", addon_type="prepend"),
                        dbc.Select(id='agc-graph-mode',options=[
                            {"label": "Time", "value": 'time'},
                            {"label": "Frequency", "value": 'freq'}
                        ], value=state['agc_graph_mode']),
                ], className='mt-1'),
                ])
            ])
        ])
    ,className='mt-3')
])


_presets_control_panel = lambda state : dbc.Container([
    dbc.Row(
        dbc.Col([
            #dbc.ListGroup([
                #dbc.ListGroupItem("Preset Scenarios", color='secondary'),
                #dbc.ListGroupItem([
                    dbc.InputGroup([
                        dbc.InputGroupAddon("Preset Scenarios", addon_type="prepend"),
                        dbc.Select(id='preset-option',options=[
                            {"label": k, "value": k}
                            for k in state['presets'].keys()
                        ], value='Default'),
                    ], className='mt-1'),
                #]),
             #])
        ])
    )
])


_in_graph = lambda state: html.Div(className="loader-wrapper", children=[ dcc.Loading( type='dot', className='loading-anim align-self-center',  children=[
    dcc.Graph(
    id='graph-inputs',
    className='six columns',
    figure={
        'data': [{
            'x': state['t'],
            'y': state['tx'],
            'name': 'TX',
            'mode': 'lines'
        }],
        'layout': {
            'title': 'Input Signal',
            'xaxis': {'title' : 'Time (s)'},
            'yaxis': {'title' : 'Normalised Amplitude'},
            'height': 350,
            'margin': dict(l=50,
                           r=0,
                           b=50,
                           t=50,
                           pad=4),
            'legend':dict(
                        yanchor="bottom",
                        y=1,
                        xanchor="right",
                        x=1,
            ),
            'shapes': [{
                'type': 'circle',
                'name': 'envelope_'+str(i),
                'x0': -5,
                'x1': 5,
                'xref': 'x',
                'xanchor': x,
                'xsizemode':'pixel',

                'y0': -5,
                'y1': 5,
                'yref': 'y',
                'yanchor': y,
                'ysizemode':'pixel',
                'fillcolor': '#001EB4',
                'opacity': 0.9,
                'line': {
                    'width': 5,
                    'color': '#001EB4',
                    'opacity': 0.9,
                }
            } for (i,(x,y)) in enumerate(state['handle_pos'])]
        }
    },
    config={
        'editable': True,
        'edits': {
            'shapePosition': True
        }
    }
)])])

_agc_graph = lambda state: html.Div(className="loader-wrapper", children=[ dcc.Loading( type='dot', className='loading-anim align-self-center', children=[
    dcc.Graph(
    id='graph-agc',
    figure={
        'data': [{
            'x': state['t'],
            'y': state['rx'],
            'name': 'RX',
            'mode': 'lines'
        }],
        'layout': {
            'title': 'Output Signal',
            'xaxis': {'title' : 'Time (s)'},
            'yaxis': {'title' : 'Normalised Amplitude'},
            'height': 350,
            'margin': dict(l=50,
                           r=0,
                           b=50,
                           t=50,
                           pad=4),
            'legend':dict(
                        yanchor="bottom",
                        y=1,
                        xanchor="right",
                        x=1,
            ),
        }
    })
    ])
])

view_template = lambda state: html.Div(style={'font-size':'12px'},className='row', children=[
    dbc.Container([
        dbc.ListGroup(dbc.ListGroupItem(
        dbc.Row([
            dbc.Col(_in_graph(state)        , width=8),
            dbc.Col(_in_control_panel(state), width=4),
        ], className='align-items-center')),  className='mt-3'),
        dbc.ListGroup(dbc.ListGroupItem(dbc.Row([
            dbc.Col(_agc_graph(state)        , width=8),
            dbc.Col(_agc_control_panel(state), width=4),
        ], className='align-items-center')),  className='mt-3'),
        dbc.ListGroup(dbc.ListGroupItem(dbc.Row([
            dbc.Col(_presets_control_panel(state), width=12),
        ], className='align-items-center')),  className='mt-3'),
    ]),
    html.Div(id='new-input-signal', style={'display':'none'}),
    dcc.Input(id='new-env-preset', style={'display':'none'}),
])
