-module(mud_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-include("object.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %%% Verify mnesia is running, abort if not
    case mnesia:wait_for_tables([schema], 1000) of
        {error, {node_not_running, _Node}} ->
            {error, mnesia_not_running};
        ok ->
            %%% Add the schema to disc if it's not already there
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            %%% Add the objects table if it's not already there
            mnesia:create_table(object, 
                                       [
                                        {attributes, record_info(fields, object)}, 
                                        {index, [#object.location_x, #object.location_y, #object.location_z]}
                                       ]),
            mnesia:change_table_copy_type(object, node(), disc_copies),
            %%% Return the childspec!
            return_child_spec()
    end.
return_child_spec() ->
    {ok, { 
            {one_for_all, 1, 10},
            [
%                {
%                 location_port,
%                 {location_port, start_link, []},
%                 permanent,
%                 brutal_kill,
%                 worker,
%                 [location_port]
%                },
                {
                 object_sup,
                 {object_sup, start_link, []},
                 permanent,
                 120,
                 supervisor,
                 [object_sup]
                },
                {
                 loader,
                 {loader, start_link, []},
                 transient,
                 brutal_kill,
                 worker,
                 [loader]
                }
            ]
    }}.
