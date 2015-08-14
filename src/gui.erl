-module(gui).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(wx_object).

%% Client API
-export([ start_link/3
        , stop/1
        ]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
         handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-include("../include/minesweeperl.hrl").

-record(state,
        { frame
        , panel
        , bitmap
        , config
        , board
        }).

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

do_init([Server, {Rows, Cols, Mines}]) ->
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

    Board = board:new(Rows, Cols, Mines),
    Panel = wxPanel:new(Frame, []),
    Bitmap = wxBitmap:new(16 * Cols, 16 * Rows),

    wxPanel:connect(Panel, left_up, [{userData, dummy}]),
    wxPanel:connect(Panel, middle_up, [{userData, dummy}]),
    wxPanel:connect(Panel, right_up, [{userData, dummy}]),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, key_up),

    [draw_icon(Bitmap, covered, {Px, Py}) || Px <- lists:seq(0, Rows-1),
                                             Py <- lists:seq(0, Cols-1)],

    wxFrame:show(Frame),
    {Frame, #state{frame=Frame, panel=Panel, bitmap=Bitmap, board=Board}}.

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
             #state{frame=Frame, board=Board}=State) ->
    Fun = case MouseEventType of
              left_up   -> step;
              right_up  -> flag;
              middle_up -> question
          end,
    {Events, NewBoard} = board:Fun({Y div 16, X div 16}, Board),
    #board{n_hidden = NHidden} = NewBoard,
    io:format("N hidden: ~p~n", [NHidden]),
    lists:foreach(fun(E) -> draw_event(E, State) end, Events),
    wxPanel:refresh(Frame),
    {noreply, State#state{board=NewBoard}};
handle_event(#wx{event=#wxKey{keyCode=$F}}, State) ->
    solver_event(flag_apparent_mines, State);
handle_event(#wx{event=#wxKey{keyCode=$S}}, State) ->
    solver_event(uncover_safe_areas, State);
handle_event(#wx{event=#wxKey{keyCode=$R}}, State) ->
    solver_event(uncover_unsafe, State);
handle_event(#wx{event=#wxKey{keyCode=KC}}, State) ->
    io:format("keyCode ~p\n", [KC]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(flag, #state{frame=Frame, board=Board}=State) ->
    {Events, NewBoard} = solver:flag_apparent_mines(Board),
    lists:foreach(fun(E) -> draw_event(E, State) end, Events),
    wxPanel:refresh(Frame),
    io:format("Events: ~p~n", [Events]),
    {noreply, State#state{board=NewBoard}};
handle_info(safe, #state{frame=Frame, board=Board}=State) ->
    {Events, NewBoard} = solver:uncover_safe_areas(Board),
    lists:foreach(fun(E) -> draw_event(E, State) end, Events),
    wxPanel:refresh(Frame),
    io:format("Events: ~p~n", [Events]),
    {noreply, State#state{board=NewBoard}};
handle_info(risk, #state{frame=Frame, board=Board}=State) ->
    {Events, NewBoard} = solver:uncover_unsafe(Board),
    lists:foreach(fun(E) -> draw_event(E, State) end, Events),
    wxPanel:refresh(Frame),
    io:format("Events: ~p~n", [Events]),
    {noreply, State#state{board=NewBoard}};
handle_info(Msg, State) ->
    io:format("Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{frame=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    io:format(State#state.config, "Got Call ~p\n", [Msg]),
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

solver_event(Fun, #state{frame=Frame, board=Board}=State) ->
    {Events, NewBoard} = solver:Fun(Board),
    lists:foreach(fun(E) -> draw_event(E, State) end, Events),
    wxPanel:refresh(Frame),
    io:format("Events: ~p~n", [Events]),
    {noreply, State#state{board=NewBoard}}.

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

draw_event({game_over, Result}, #state{}) ->
    io:format("Game over: you ~p~n", [Result]);
draw_event({Event, Seq}, #state{board=#board{dims=Dims},
                                bitmap=Bitmap}) ->
    draw_icon(Bitmap, Event, board:seq2pos(Seq, Dims));
draw_event({uncovered, Seq, Type}, #state{board=#board{dims=Dims},
                                          bitmap=Bitmap}) ->
    draw_icon(Bitmap, Type, board:seq2pos(Seq, Dims));
draw_event(Event, _Panel) ->
    io:format("Event: ~p~n", [Event]).

draw_icon(Bitmap, FieldState, {Px, Py}) ->
    Image = wxImage:new("priv/images/" ++ icon_filename(FieldState)),
    Bmp = wxBitmap:new(Image),
    SrcDC = wxMemoryDC:new(Bmp),
    DestDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DestDC, {16*Py ,16*Px}, {16, 16}, SrcDC, {0,0}),
    wxMemoryDC:destroy(SrcDC),
    wxMemoryDC:destroy(DestDC),
    wxBitmap:destroy(Bmp).

%% Buffered makes it all appear on the screen at the same time
redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
              {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
              MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).
