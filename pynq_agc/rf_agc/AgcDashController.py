from .AgcDashView import view_template
import dash
import dash_bootstrap_components as dbc
import numpy as np
import ast
from dash.dependencies import Input, Output, State
from jupyter_dash import JupyterDash
from netifaces import AF_INET, AF_INET6, AF_LINK, AF_PACKET, AF_BRIDGE, ifaddresses


# Pure helper functions

def get_envelope_handles(shapes):
    dots = []
    for s in shapes:
        name = s['name'] or ''
        if name.startswith('envelope_'):
            dots.append((s['xanchor'], s['yanchor']))
    return dots

def get_num_envelope_handles(shapes):
    max_index = -1
    for s in shapes:
        name = s['name'] or ''
        if name.startswith('envelope_'):
            cur_index = int(name.strip('envelope_'))
            max_index = max(cur_index,max_index)
    return max_index + 1

def get_envelope_handle(shapes,index):
    for s in shapes:
        if s['name'] == 'envelope_' + str(index):
            return s
    return None

def add_envelope_handle(shapes, xmax):
    # Add a new envelope handle
    # Use same y value and halfway between the furthest x handle and a maximum
    num_handles = get_num_envelope_handles(shapes)
    shape_template = get_envelope_handle(shapes,num_handles-1).copy()
    shape_template.update(
        {   'name'   : 'envelope_'+str(num_handles),
            'xanchor':(shape_template['xanchor']+xmax)/2
        })
    shapes.append(shape_template)

def rm_envelope_handle(shapes, index=None):
    # Remove envelope_handle with supplied index (of with highest index)
    if index==None:
        index = get_num_envelope_handles(shapes)-1
        # Never implicitly delete the last remaining handle
        if index == 0:
            return

    for (i,s) in enumerate(shapes):
        if s['name'] == 'envelope_'+str(index):
            shapes.pop(i)


class AgcDashController():

    def __init__(self, model, N_handles=2):
        self.model = model

        # Generate an initial state
        init_state = dict(
            signal_mode = 'sin',    # sin, am, fm, qpsk_bb, or qpsk_if
            fc = 50e6,
            fm =  1e6,
            agc_atk_t = 50 / 100e6,
            agc_dec_t = 50 / 100e6,
            agc_atk_step = 2**(-9),
            agc_dec_step = 2**(-8),
            agc_bypass  = False,
            agc_max_gain = 0.99,
            thres_low = 0.5,
            thres_high = 0.7,
            thres_hyst = 1376 / 1.024e9,
            agc_graph_mode = 'time', # time, const, freq
            t = self.model.t,
            N_handles = N_handles,
            handle_pos = [((i+1)*self.model.N/self.model.fs/(N_handles+1),0.3) for i in range(N_handles)],
        )

        init_state['tx'] = model.test_input(
         model.envelope(init_state['handle_pos']),
         model.ref_signal(
            init_state['signal_mode'], init_state['fc'], init_state['fm']
        ))

        model.agc_cfg(not init_state['agc_bypass'], init_state['agc_atk_step'],
                      init_state['agc_atk_t']     , init_state['agc_dec_step'],
                      init_state['agc_dec_t']     , init_state['agc_max_gain']
        )

        model.threshold_cfg(
                      init_state['thres_low']     , init_state['thres_high'],
                      init_state['thres_hyst']
        )

        init_state['rx'] = model.agc_loopback(init_state['tx'])

        # Set preset configurations
        init_state['presets'] = {
            'Default' : {
                'signal_mode'   : 'sin',
                'fm'            : 1e6,
                'fc'            : 50e6,
                'agc_atk_t'       : 50/100e6,
                'agc_atk_step'     : 2**(-9),
                'agc_dec_t'       : 50/100e6,
                'agc_dec_step'     : 2**(-8),
                'thres_low'    : 0.5,
                'thres_high'    : 0.7,
                'thres_hyst'    : 1.35e-6,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.33, 0.3), (0.66, 0.3)]
                                  ],
            },
            'Static compensation' : {
                'signal_mode'   : 'sin',
                'fm'            : 1e6,
                'fc'            : 50e6,
                'agc_atk_t'       : 4/100e6,
                'agc_atk_step'     : 2**(-13),
                'agc_dec_t'       : 4/100e6,
                'agc_dec_step'     : 2**(-13),
                'thres_low'    : 0.7,
                'thres_high'    : 0.95,
                'thres_hyst'    : 64/1.024e9,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.5, 0.3)]
                                  ],
            },
            'Step up' : {
                'signal_mode'   : 'sin',
                'fm'            : 1e6,
                'fc'            : 50e6,
                'agc_atk_t'       : 1e-6,
                'agc_atk_step'     : 2**(-10),
                'agc_dec_t'       : 1e-6,
                'agc_dec_step'     : 2**(-7),
                'thres_low'    : 0.7,
                'thres_high'    : 0.95,
                'thres_hyst'    : 62.5e-9,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.49, 0.26), (0.51, 0.56)]
                                  ],
            },
            'Step down' : {
                'signal_mode'   : 'sin',
                'fm'            : 1e6,
                'fc'            : 50e6,
                'agc_atk_t'       : 0.32e-6,
                'agc_atk_step'     : 2**(-10),
                'agc_dec_t'       : 1e-6,
                'agc_dec_step'     : 2**(-7),
                'thres_low'    : 0.7,
                'thres_high'    : 0.95,
                'thres_hyst'    : 1.41e-9,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.49, 0.56), (0.51, 0.26)]
                                  ],
            },
            'Pass-through' : {
                'signal_mode'   : 'sin',
                'fm'            : 1e6,
                'fc'            : 50e6,
                'agc_atk_t'       : 4/100e6,
                'agc_atk_step'     : 2**(-13),
                'agc_dec_t'       : 4/100e6,
                'agc_dec_step'     : 2**(-13),
                'thres_low'    : 0.5,
                'thres_high'    : 0.7,
                'thres_hyst'    : 64/1.024e9,
                'agc_bypass'    : True,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.33, 1.0), (0.66, 1.0)]
                                  ],
            },
        }
        self.init_state =  init_state
        view = view_template(init_state)

        # Make Dash app
        app = JupyterDash(__name__, external_stylesheets=[
            dbc.themes.BOOTSTRAP,
            'https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css'
        ])
        app.layout = view
        self.app = app

        @app.callback(
            Output('agc-atk-t-label', 'children'),
            [Input('agc-atk-t', 'value')])
        def update_agc_atk_t_label(t):
            return f'{t} µs'

        @app.callback(
            Output('agc-atk-step-label', 'children'),
            [Input('agc-atk-step', 'value')])
        def update_agc_atk_step_label(step):
            return f'2^{step}'

        @app.callback(
            Output('agc-dec-t-label', 'children'),
            [Input('agc-dec-t', 'value')])
        def update_agc_dec_t_label(t):
            return f'{t} µs'

        @app.callback(
            Output('agc-dec-step-label', 'children'),
            [Input('agc-dec-step', 'value')])
        def update_agc_dec_step_label(step):
            return f'2^{step}'

        @app.callback(
            Output('thres-range-label', 'children'),
            [Input('thres-range', 'value')])
        def update_thresh_low_label(percs):
            return f'{int(100*percs[0])}-{int(100*percs[1])}%'

        @app.callback(
            Output('thres-hyst-label', 'children'),
            [Input('thres-hyst', 'value')])
        def update_thresh_hyst_label(t):
            return f'{t} µs'

        @app.callback(
            Output('in-f-data-label', 'children'),
            [Input('in-f-data', 'value')])
        def update_data_rate_label(f):
            return f'{int(f)} kHz'

        @app.callback(
            Output('in-f-carrier-label', 'children'),
            [Input('in-f-carrier', 'value')])
        def update_carrier_label(f):
            return f'{int(f)} MHz'

        @app.callback(
            [
             Output('in-sig-type', 'value'),
             Output('in-f-carrier', 'value'),
             Output('in-f-data', 'value'),
             Output('agc-atk-t', 'value'),
             Output('agc-atk-step', 'value'),
             Output('agc-dec-t', 'value'),
             Output('agc-dec-step', 'value'),
             Output('thres-range', 'value'),
             Output('thres-hyst', 'value'),
             Output('agc-bypass', 'value'),
             Output('agc-graph-mode', 'value'),
             Output('new-env-preset', 'value'),
            ],
            [Input('preset-option', 'value')]
        )
        def set_to_preset(preset_name):

            preset = self.init_state['presets'][preset_name]

            # Return new GUI values
            return (preset['signal_mode'],
                    preset['fc'] / 1e6,
                    preset['fm'] / 1e3,
                    preset['agc_atk_t'] * 1e6,
                    int(np.log2(preset['agc_atk_step'])),
                    preset['agc_dec_t'] * 1e6,
                    int(np.log2(preset['agc_dec_step'])),
                    [preset['thres_low'], preset['thres_high']],
                    preset['thres_hyst'] * 1e6,
                    [1] if preset['agc_bypass'] else [],
                    preset['agc_graph_mode'],
                    str(preset['handle_pos']))


        @app.callback(
            [Output('graph-inputs', 'figure'), Output('new-input-signal', 'children')],
            [Input('graph-inputs', 'relayoutData'),
             Input('btn-add-handle', 'n_clicks'),
             Input('btn-rm-handle', 'n_clicks'),
             Input('in-sig-type', 'value'),
             Input('in-f-carrier', 'value'),
             Input('in-f-data', 'value'),
             Input('new-env-preset', 'value'),
             ],
            [State('graph-inputs', 'figure')])
        def input_stage_callback(_,_btnadd,_btnrm,in_sig_type,in_f_carrier,in_data_rate,env_preset, fig_in):

            changed_ids = [p['prop_id'] for p in dash.callback_context.triggered]
            if 'btn-add-handle' in changed_ids[0]:
                add_envelope_handle(fig_in['layout']['shapes'], max(self.model.t))
            if 'btn-rm-handle' in changed_ids[0]:
                rm_envelope_handle(fig_in['layout']['shapes'])
            if any(map(lambda x: 'new-env-preset' in x, changed_ids)):
                h_template = get_envelope_handle(fig_in['layout']['shapes'],0)
                points = ast.literal_eval(env_preset)
                if points:
                    new_shapes = []
                    for (i,(x,y)) in enumerate(points):
                        p = dict(h_template)
                        p.update({
                            'name': 'envelope_'+str(i),
                            'xanchor': x,
                            'yanchor': y,
                        })
                        new_shapes.append(p)
                    fig_in['layout']['shapes'] = new_shapes

            in_f_carrier = in_f_carrier*1000000
            in_data_rate = in_data_rate*1000

            ref_tx = self.model.ref_signal(in_sig_type, in_f_carrier, in_data_rate)
            handles = get_envelope_handles(fig_in['layout']['shapes'])
            env = self.model.envelope(handles)
            tx = self.model.test_input(ref_tx,env)
            trace_t = self.model.t

            fig_in['data'][0]['x'] = trace_t
            fig_in['data'][0]['y'] = tx
            return fig_in, [f'{in_sig_type} {in_f_carrier} {in_data_rate}, {handles}']

        @app.callback(
            Output('graph-agc', 'figure'),
            [Input('new-input-signal', 'children'),
             Input('agc-atk-step', 'value'),
             Input('agc-atk-t', 'value'),
             Input('agc-dec-step', 'value'),
             Input('agc-dec-t', 'value'),
             Input('agc-bypass', 'value'),
             Input('thres-range', 'value'),
             Input('thres-hyst', 'value'),
             Input('agc-graph-mode', 'value')
             ],
            [State('graph-inputs', 'figure'), State('graph-agc', 'figure')])
        def agc_stage_callback(_,agc_atk_step,agc_atk_t,agc_dec_step,agc_dec_t,agc_bypass,thres_range,thres_hyst,graph_mode,fig_in,fig_agc):
            # TODO Check if State uses client... if so, remove the graph-inputs state arg to avoid round trip for no reason
            agc_en = 0 if agc_bypass else 1
            self.model.agc_cfg(
                agc_en,
                2**agc_atk_step, agc_atk_t*1e-6,
                2**agc_dec_step, agc_dec_t*1e-6,
                self.init_state['agc_max_gain']
            )

            self.model.threshold_cfg(
                thres_range[0], thres_range[1], thres_hyst * 1e-6
            )

            tx = np.array(fig_in['data'][0]['y'])
            rx = self.model.agc_loopback(tx)
            trace_t = self.model.t

            if graph_mode == 'time':
                fig_agc['data'][0]['x'] = trace_t
                fig_agc['data'][0]['y'] = rx
                fig_agc['layout']['xaxis']['title'] = "Time (s)"
                fig_agc['layout']['yaxis']['title'] = "Normalised Amplitude"
                fig_agc['layout']['shapes'] = [
                    dict(
                        visible = True,
                        type = 'rect',
                        editable = False,
                        layer = 'below',
                        opacity = 0.6,
                        fillcolor = '#DCD8EA',
                        xref = 'x',
                        x0 = self.model.t[-1] - (thres_hyst * 1e-6),
                        x1 = self.model.t[-1],
                        yref = 'y',
                        y0 = -1,
                        y1 = 1,
                        line = {'width':-1},
                    )
                ]
            if graph_mode == 'freq':
                (freq_x, freq_y) = self.model.calc_fft(rx)
                fig_agc['data'][0]['x'] = freq_x
                fig_agc['data'][0]['y'] = freq_y
                fig_agc['data'][1]['x'] = []
                fig_agc['data'][1]['y'] = []
                fig_agc['data'][2]['x'] = []
                fig_agc['data'][2]['y'] = []
                fig_agc['layout']['xaxis']['title'] = "Frequency (Hz)"
                fig_agc['layout']['yaxis']['title'] = "Power dB"
                fig_agc['layout']['shapes']=[]

            return fig_agc


    def show(self,debug=False):
        mode = 'external' if debug else 'inline'
        return self.app.run_server(mode=mode, host=ifaddresses('eth0')[AF_INET][0]['addr'], port=8051, height=950)
