from AgcDashView import view_template
import dash
import dash_bootstrap_components as dbc
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

    def __init__(self, model, N_handles=4):
        self.model = model

        # Generate an initial state
        init_state = dict(
            signal_mode = 'sin',
            fc = 20000,
            fm =  2000,
            agc_ref = 0.7,
            agc_alpha = 1.0,
            agc_window = 6,
            agc_graph_mode = 'time',
            t = self.model.t,
            N_handles = N_handles,
            handle_pos = [((i+1)*self.model.N/self.model.fs/(N_handles+1),1) for i in range(N_handles)],
        )
        (init_state['i'], init_state['q']) = model.ref_signal(init_state['signal_mode'], init_state['fc'], init_state['fm'])
        (init_state['agc_i'], init_state['agc_q']) = model.ref_signal(init_state['signal_mode'], init_state['fc'], init_state['fm'])
        model.agc_cfg(1, init_state['agc_window'], init_state['agc_ref'], init_state['agc_alpha'])
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
            [Output('graph-inputs', 'figure'), Output('new-input-signal', 'children')],
            [Input('graph-inputs', 'relayoutData'),
             Input('btn-add-handle', 'n_clicks'),
             Input('btn-rm-handle', 'n_clicks'),
             Input('in-sig-type', 'value'),
             Input('in-f-carrier', 'value'),
             Input('in-f-data', 'value'),
             ],
            [State('graph-inputs', 'figure')])
        def input_stage_callback(_,_btnadd,_btnrm,in_sig_type,in_f_carrier,in_data_rate,fig_in):

            changed_id = [p['prop_id'] for p in dash.callback_context.triggered][0]
            if 'btn-add-handle' in changed_id:
                add_envelope_handle(fig_in['layout']['shapes'], max(trace_t))
            if 'btn-rm-handle' in changed_id:
                rm_envelope_handle(fig_in['layout']['shapes'])

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
             Input('agc-window', 'value')
             ],
            [State('graph-inputs', 'figure'), State('graph-agc', 'figure')])
        def agc_stage_callback(_,agc_ref,agc_alpha,agc_window,fig_in,fig_agc):

            self.model.agc_cfg(1,agc_window,agc_ref,agc_alpha)

            (trace_i, trace_q) = (fig_in['data'][0]['y'],fig_in['data'][1]['y'])
            (agc_i, agc_q) = self.model.agc_loopback(trace_i,trace_q)
            trace_t = self.model.t

            fig_agc['data'][0]['x'] = trace_t
            fig_agc['data'][0]['y'] = agc_i
            fig_agc['data'][1]['x'] = trace_t
            fig_agc['data'][1]['y'] = agc_q
            return fig_agc


    def show(self,debug=False):
        mode = 'external' if debug else 'inline'
        return self.app.run_server(mode=mode, host=ifaddresses('eth0')[AF_INET][0]['addr'], height=810)
