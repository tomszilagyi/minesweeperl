-module(gui).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(wx_object).

%% Client API
-export([ screensaver/0
        , start_link/3
        , stop/1
        ]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
         handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-include("../include/minesweeperl.hrl").

-define(MOVE_INTERVAL, 1000).

-record(state,
        { frame
        , panel
        , mode = interactive
        , bitmap
        , config
        , board
        , solver_state = flag
        }).

screensaver() ->
    {ok, _EPid} = eprof:start(),
    Server = wx:new(),
    {_, _, _, Pid} = wx_object:start_link(?MODULE, [Server, screensaver], []),
    {ok, Pid}.

start_link(Rows, Cols, Mines) ->
    Server = wx:new(),
    GameOpts = {Rows, Cols, Mines},
    {_, _, _, Pid} = wx_object:start_link(?MODULE, [Server, GameOpts], []),
    {ok, Pid}.

stop(Pid) ->
    gen_server:call(Pid, shutdown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init([Server, screensaver]) ->
    profiling = eprof:start_profiling([self()], {?MODULE, '_', '_'}),
    DC = wxScreenDC:new(),
    {ScreenX, ScreenY} = wxDC:getSize(DC),
    wxScreenDC:destroy(DC),
    Rows = ScreenY div 16,
    Cols = ScreenX div 16,
    Mines = Rows * Cols div 8,
    Config = {Rows, Cols, Mines},
    io:format("config: ~p~n", [Config]),
    Bitmap = wxBitmap:new(16 * Cols, 16 * Rows),
    Board = create_and_draw_board(Config, Bitmap),
    State = #state{config=Config, bitmap=Bitmap, board=Board, mode=screensaver},
    Frame = wxFrame:new(Server, ?wxID_ANY, "MinesweepErl", []),
    refresh(State),
    schedule_move(State),
    {Frame, State#state{frame=Frame}};
do_init([Server, {Rows, Cols, _Mines}=Config]) ->
    Frame = wxFrame:new(Server, ?wxID_ANY, "MinesweepErl",
                        [{style,
                          ?wxCAPTION bor
                          ?wxCLIP_CHILDREN bor
                          ?wxCLOSE_BOX bor
                          ?wxFRAME_FLOAT_ON_PARENT bor
                          %%?wxFRAME_NO_TASKBAR bor
                          ?wxMAXIMIZE_BOX bor
                          ?wxMINIMIZE_BOX bor
                          ?wxRESIZE_BORDER bor
                          %%?wxSTAY_ON_TOP bor
                          ?wxSYSTEM_MENU
                         }]),

    Panel = wxPanel:new(Frame, []),
    Bitmap = wxBitmap:new(16 * Cols, 16 * Rows),
    Board = create_and_draw_board(Config, Bitmap),

    wxPanel:connect(Panel, left_up, [{userData, dummy}]),
    wxPanel:connect(Panel, middle_up, [{userData, dummy}]),
    wxPanel:connect(Panel, right_up, [{userData, dummy}]),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, key_up),

    wxFrame:centerOnScreen(Frame),
    wxFrame:show(Frame),
    {Frame, #state{frame=Frame, panel=Panel, config=Config,
                   bitmap=Bitmap, board=Board}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
                  #state{panel=Panel, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Panel),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxMouse{x = X, y = Y}},
             #state{board=#board{dims={Rows, Cols}}}=State)
  when X < 0; Y < 0; Rows < Y div 16; Cols < X div 16 ->
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = MouseEventType, x = X, y = Y}},
             State) ->
    Fun = case MouseEventType of
              left_up   -> step;
              right_up  -> flag;
              middle_up -> question
          end,
    {noreply, board_event(Fun, {Y div 16, X div 16}, State)};
handle_event(#wx{event=#wxKey{keyCode=$ }}, State) ->
    {noreply, do_move(State)};
handle_event(#wx{event=#wxKey{keyCode=$F}}, State) ->
    {noreply, solver_event(flag_apparent_mines, State)};
handle_event(#wx{event=#wxKey{keyCode=$S}}, State) ->
    {noreply, solver_event(uncover_safe_areas, State)};
handle_event(#wx{event=#wxKey{keyCode=$R}}, State) ->
    {noreply, solver_event(uncover_unsafe, State)};
handle_event(#wx{event=#wxKey{keyCode=$N}}, State) ->
    {noreply, newgame(State)};
handle_event(#wx{event=#wxKey{keyCode=_KC}}, State) ->
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(move, State) ->
    {noreply, do_move(State)};
handle_info(shutdown, State) ->
    profiling_stopped = eprof:stop_profiling(),
    ok = eprof:log("eprof.txt"),
    ok = eprof:analyze(total, [{sort, time}]),
    {stop, normal, State};
handle_info(Msg, State) ->
    io:format("Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{frame=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_and_draw_board({Rows, Cols, Mines}, Bitmap) ->
    Board = board:new(Rows, Cols, Mines),
    draw_grid(Bitmap, Rows, Cols),
    Board.

newgame(#state{bitmap=Bitmap, config=Config} = State) ->
    Board = create_and_draw_board(Config, Bitmap),
    refresh(State),
    State#state{board=Board, solver_state=flag}.

schedule_move(#state{mode=screensaver, solver_state=newgame}=State) ->
    timer:send_after(5 * ?MOVE_INTERVAL, move),
    State;
schedule_move(#state{mode=screensaver}=State) ->
    timer:send_after(?MOVE_INTERVAL, move),
    State;
schedule_move(#state{mode=interactive}=State) ->
    State.

do_move(#state{solver_state=newgame}=State) ->
    schedule_move(newgame(State));
do_move(#state{solver_state=SState, board=Board}=State) ->
    sink_events(solver:step(SState, Board), State).

board_event(Fun, Arg, #state{board=Board}=State) ->
    sink_events(board:Fun(Arg, Board), State).

solver_event(Fun, #state{board=Board}=State) ->
    sink_events(solver:Fun(Board), State).

sink_events({[{game_over, _} | _] = Events, Board}, State) ->
    sink_events({newgame, Events, Board}, State);
sink_events({Events, Board}, State) ->
    sink_events({flag, Events, Board}, State);
sink_events({SolverState, Events, Board}, State) ->
    draw_events(Events, State),
    refresh(State),
    schedule_move(State#state{solver_state=SolverState, board=Board}).

draw_events(Events, #state{board=#board{dims=Dims}, bitmap=Bitmap}) ->
    DestDC = wxMemoryDC:new(Bitmap),
    draw_events(Events, Dims, DestDC).

draw_events([E|Events], Dims, DestDC) ->
    draw_event(E, Dims, DestDC),
    draw_events(Events, Dims, DestDC);
draw_events([], _Dims, DestDC) ->
    wxMemoryDC:destroy(DestDC).

draw_event({game_over, Result}, _Dims, _DestDC) ->
    io:format("Game over: you ~p~n", [Result]);
draw_event({Event, Seq}, Dims, DestDC) ->
    draw_icon(DestDC, Event, board:seq2pos(Seq, Dims));
draw_event({uncovered, Seq, Type}, Dims, DestDC) ->
    draw_icon(DestDC, Type, board:seq2pos(Seq, Dims));
draw_event(Event, _Dims, _DestDC) ->
    io:format("Event: ~p~n", [Event]).

draw_grid(Bitmap, Rows, Cols) ->
    %% Draw a row and copy that over to other rows for speed.
    DC = wxMemoryDC:new(Bitmap),
    [draw_icon(DC, covered, {0, Py}) || Py <- lists:seq(0, Cols-1)],
    [wxDC:blit(DC, {0, 16*Px}, {16*Cols, 16*(Px+1)}, DC, {0,0}) ||
        Px <- lists:seq(1, Rows-1)],
    wxMemoryDC:destroy(DC).

draw_icon(DestDC, FieldState, {Px, Py}) ->
    SrcDC = memoize(fun() -> icon_dc(FieldState) end),
    wxDC:blit(DestDC, {16*Py ,16*Px}, {16, 16}, SrcDC, {0,0}).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
              {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
              MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).

refresh(#state{mode=screensaver, bitmap=Bitmap}) ->
    DC = wxScreenDC:new(),
    redraw(DC, Bitmap),
    wxScreenDC:destroy(DC);
refresh(#state{mode=interactive, frame=Frame}) ->
    wxPanel:refresh(Frame).

icon_dc(FieldState) ->
    Image = wxImage:new("priv/images/" ++ icon_filename(FieldState)),
    Bmp = wxBitmap:new(Image),
    wxMemoryDC:new(Bmp).

memoize(Fun) ->
    case get(Fun) of
        undefined ->
            Value = Fun(),
            put(Fun, Value),
            Value;
        Value ->
            Value
    end.

icon_filename({empty, 0}) -> "0.gif";
icon_filename({empty, 1}) -> "1.gif";
icon_filename({empty, 2}) -> "2.gif";
icon_filename({empty, 3}) -> "3.gif";
icon_filename({empty, 4}) -> "4.gif";
icon_filename({empty, 5}) -> "5.gif";
icon_filename({empty, 6}) -> "6.gif";
icon_filename({empty, 7}) -> "7.gif";
icon_filename({empty, 8}) -> "8.gif";
icon_filename(covered)    -> "empty.gif";
icon_filename(flagged)    -> "flag.gif";
icon_filename(questioned) -> "question.gif";
icon_filename(mine)       -> "mine.gif";
icon_filename(exploded)   -> "mineexpl.gif";
icon_filename(mistaken)   -> "error.gif".
