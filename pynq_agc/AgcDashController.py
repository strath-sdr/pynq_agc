from .AgcDashView import view_template
import numpy as np
import ast
from netifaces import AF_INET, AF_INET6, AF_LINK, AF_PACKET, AF_BRIDGE, ifaddresses

# catch dash deprecation warnings
import warnings
with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import dash
    from dash.dependencies import Input, Output, State
    from jupyter_dash import JupyterDash
    import dash_bootstrap_components as dbc

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

    def __init__(self, model, N_handles=4):
        self.model = model

        # Generate an initial state
        init_state = dict(
            signal_mode = 'sin',    # sin, am, fm, qpsk_bb, or qpsk_if
            fc = 20000,
            fm =  2000,
            agc_ref = 0.7,
            agc_alpha = 0.6,
            agc_window = 6,
            agc_graph_mode = 'time', # time, const, freq
            t = self.model.t,
            N_handles = N_handles,
            handle_pos = [((i+1)*self.model.N/self.model.fs/(N_handles+1),1) for i in range(N_handles)],
        )

        (init_state['i'], init_state['q']) = model.ref_signal(
            init_state['signal_mode'], init_state['fc'], init_state['fm']
        )

        model.agc_cfg(1, init_state['agc_window'], init_state['agc_ref'], init_state['agc_alpha'])

        (init_state['agc_i'], init_state['agc_q'], init_state['agc_g']) = model.agc_loopback(
            init_state['i'], init_state['q']
        )

        # Set preset configurations
        init_state['presets'] = {
            'Default' : {
                'signal_mode'   : 'sin',
                'fm'            : 2000,
                'fc'            : 20000,
                'agc_ref'       : 0.7,
                'agc_alpha'     : 0.7,
                'agc_window'    : 64,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.2, 1.0), (0.4, 1.0), (0.6, 1.0), (0.8, 1.0)]
                                  ],
            },
            'Slow fading' : {
                'signal_mode'   : 'sin',
                'fm'            : 2000,
                'fc'            : 40000,
                'agc_ref'       : 0.7,
                'agc_alpha'     : 1.0,
                'agc_window'    : 64,
                'agc_bypass'    : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.1, 1.0), (0.4, 0.2), (0.5, 0.2), (0.9,1.0)]
                                  ],
            },
            'AM envelope preservation' : {
                'signal_mode'  : 'am',
                'fm'           : 8000,
                'fc'           : 80000,
                'agc_ref'      : 0.4,
                'agc_alpha'    : 0.9,
                'agc_window'   : 256,
                'agc_bypass'   : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.1, 1.0), (0.105, 0.1), (0.6, 0.1), (0.62,1.0)]
                                  ],
            },
            'Packet preambles with QPSK' : {
                'signal_mode'  : 'qpsk_bb',
                'fm'           : 8000,
                'fc'           : 80000,
                'agc_ref'      : 0.7,
                'agc_alpha'    : 0.5,
                'agc_window'   : 64,
                'agc_bypass'   : False,
                'agc_graph_mode': 'time',
                'handle_pos'    : [(x*self.model.N/self.model.fs, y)
                                   for (x,y) in [(0.18, 0), (0.181, 1.0), (0.68, 1.0), (0.681,0)]
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
            Output('agc-ref-label', 'children'),
            [Input('agc-ref', 'value')])
        def update_data_rate_label(f):
            return f'{int(f*100)} %'

        @app.callback(
            Output('agc-window-label', 'children'),
            [Input('agc-window', 'value')])
        def update_data_rate_label(f):
            return f'{int(2**f)} Samples'

        @app.callback(
            Output('agc-alpha-label', 'children'),
            [Input('agc-alpha', 'value')])
        def update_data_rate_label(f):
            return str(f)

        @app.callback(
            Output('in-f-data-label', 'children'),
            [Input('in-f-data', 'value')])
        def update_data_rate_label(f):
            return f'{int(f)} Hz'

        @app.callback(
            Output('in-f-carrier-label', 'children'),
            [Input('in-f-carrier', 'value')])
        def update_carrier_label(f):
            return f'{int(f)} kHz'

        @app.callback(
            [
             Output('in-sig-type', 'value'),
             Output('in-f-carrier', 'value'),
             Output('in-f-data', 'value'),
             Output('agc-ref', 'value'),
             Output('agc-alpha', 'value'),
             Output('agc-window', 'value'),
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
                    preset['fc'] / 1000,
                    preset['fm'],
                    preset['agc_ref'],
                    preset['agc_alpha'],
                    int(np.log2(preset['agc_window'])),
                    preset['agc_bypass'],
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

            in_f_carrier = in_f_carrier*1000
            (ref_i,ref_q) = self.model.ref_signal(in_sig_type, in_f_carrier,in_data_rate)

            handles = get_envelope_handles(fig_in['layout']['shapes'])
            env = self.model.envelope(handles)
            (trace_i, trace_q) = self.model.test_input((ref_i,ref_q),env)
            trace_t = self.model.t

            fig_in['data'][0]['x'] = trace_t
            fig_in['data'][0]['y'] = trace_i
            fig_in['data'][1]['x'] = trace_t
            fig_in['data'][1]['y'] = trace_q
            return fig_in, [f'{in_sig_type} {in_f_carrier} {in_data_rate}, {handles}']

        @app.callback(
            Output('graph-agc', 'figure'),
            [Input('new-input-signal', 'children'),
             Input('agc-ref', 'value'),
             Input('agc-alpha', 'value'),
             Input('agc-window', 'value'),
             Input('agc-bypass', 'value'),
             Input('agc-graph-mode', 'value')
             ],
            [State('graph-inputs', 'figure'), State('graph-agc', 'figure')])
        def agc_stage_callback(_,agc_ref,agc_alpha,agc_window,agc_bypass,graph_mode,fig_in,fig_agc):
            # TODO Check if State uses client... if so, remove the graph-inputs state arg to avoid round trip for no reason
            agc_en = 0 if agc_bypass else 1
            self.model.agc_cfg(agc_en,agc_window,agc_ref,agc_alpha)

            (trace_i, trace_q) = (fig_in['data'][0]['y'],fig_in['data'][1]['y'])
            (agc_i, agc_q, agc_g) = self.model.agc_loopback(trace_i,trace_q)
            trace_t = self.model.t

            if graph_mode == 'time':
                fig_agc['data'][0]['x'] = trace_t
                fig_agc['data'][0]['y'] = agc_i
                fig_agc['data'][1]['x'] = trace_t
                fig_agc['data'][1]['y'] = agc_q
                fig_agc['data'][2]['x'] = trace_t
                fig_agc['data'][2]['y'] = agc_g
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
                        x0 = self.model.t[-1] - (2**agc_window / self.model.fs),
                        x1 = self.model.t[-1],
                        yref = 'y',
                        y0 = -1,
                        y1 = 1,
                        line = {'width':-1},
                    )
                ]
            if graph_mode == 'const':
                fig_agc['data'][0]['x'] = agc_i
                fig_agc['data'][0]['y'] = agc_q
                fig_agc['data'][1]['x'] = []
                fig_agc['data'][1]['y'] = []
                fig_agc['data'][2]['x'] = []
                fig_agc['data'][2]['y'] = []
                fig_agc['layout']['xaxis']['title'] = "I Amplitude"
                fig_agc['layout']['yaxis']['title'] = "Q Amplitude"
                fig_agc['layout']['shapes']=[]
            if graph_mode == 'freq':
                (freq_x, freq_y) = self.model.calc_fft(agc_i, agc_q)
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
        return self.app.run_server(mode=mode, host=ifaddresses('eth0')[AF_INET][0]['addr'], height=930)
