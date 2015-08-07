-module(gui).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(wx_object).

%% Client API
-export([ start_link/3
        , stop/0
        ]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-include("../include/minesweeperl.hrl").

-record(state,
        { frame
        , panel
        , canvas
        , bitmap
        , config
        , board
        }).

start_link(Rows, Cols, Mines) ->
    Server = wx:new(),
    {_, _, _, Pid} = wx_object:start_link({local, minesweeperl_gui}, ?MODULE,
                                          [Server, {Rows, Cols, Mines}], []),
    {ok, Pid}.

stop() ->
    wx:destroy(),
    ok.

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

    Board = minesweeperl:new_game(Rows, Cols, Mines),

    Panel = wxPanel:new(Frame, []),
    %Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    %wxPanel:connect(Canvas, paint, [callback]),
    %{W,H} = wxPanel:getSize(Canvas),
    %Bitmap = wxBitmap:new(erlang:max(W, 30), erlang:max(30, H)),

    wxPanel:connect(Panel, motion, [{userData, dummy}]),
    wxPanel:connect(Panel, left_up, [{userData, dummy}]),
    wxPanel:connect(Panel, middle_up, [{userData, dummy}]),
    wxPanel:connect(Panel, right_up, [{userData, dummy}]),
    wxFrame:show(Frame),

    self() ! init_board,

    {Frame, #state{frame=Frame, panel=Panel, board=Board}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.

%% handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
%%                   #state{canvas=Canvas, bitmap=Bitmap}) ->
%%     DC = wxPaintDC:new(Canvas),
%%     redraw(DC, Bitmap),
%%     wxPaintDC:destroy(DC),
%%     ok.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}},
             State = #state{}) ->
    case Id of
        ?wxID_NEW -> io:format(State#state.config, "New\n", []);
        ?wxID_OPEN -> io:format(State#state.config, "Open\n", []);
        ?wxID_COPY -> io:format(State#state.config, "Copy\n", []);
        ?wxID_PASTE -> io:format(State#state.config, "Paste\n", []);
        ?wxID_HELP ->
            wx_misc:launchDefaultBrowser("http://erlang.org/doc/apps/wx/part_frame.html");
        _ -> ignore
    end,
    {noreply, State};
handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}},
             State = #state{}) ->
    {noreply, State};
handle_event(#wx{userData = UD, event = #wxMouse{type = motion, x = X, y = Y}},
             State) ->
    {noreply, State};

handle_event(#wx{event = #wxMouse{x = X, y = Y}},
             #state{board=#board{dims={Rows, Cols}}}=State)
  when Rows < Y div 16; Cols < X div 16 ->
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = MouseEventType, x = X, y = Y}},
             #state{panel=Panel, board=Board}=State) ->
    io:format("~p {~p,~p}~n", [MouseEventType, X, Y]),
    Fun = case MouseEventType of
              left_up -> step;
              middle_up -> question;
              right_up -> flag
          end,
    {Events, NewBoard} = minesweeperl:Fun({Y div 16, X div 16}, Board),
    lists:foreach(fun(E) -> draw_event(E, Panel) end, Events),
    {noreply, State#state{board=NewBoard}}.

%% Callbacks handled as normal gen_server callbacks
handle_info(init_board, #state{panel=Panel, board=#board{dims={X, Y}}} = State) ->
    [draw_icon(Panel, covered, {Px, Py}) || Px <- lists:seq(0, X-1),
                                            Py <- lists:seq(0, Y-1)],
    {noreply, State};
handle_info(Msg, State) ->
    io:format("Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(shutdown, _From, State=#state{panel=Panel}) ->
    wxPanel:destroy(Panel),
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
icon_filename(error)      -> "error.gif".

draw_event(Event, _Panel) ->
    io:format("Event: ~p~n", [Event]).

draw_icon(Panel, FieldState, {Px, Py}) ->
    Image = wxImage:new("priv/images/" ++ icon_filename(FieldState)),
    Bmp = wxBitmap:new(Image),
    MemoryDC = wxMemoryDC:new(Bmp),
    CDC = wxWindowDC:new(Panel),
    wxDC:blit(CDC, {16*Py ,16*Px}, {16, 16}, MemoryDC, {0, 0}),
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC),
    wxBitmap:destroy(Bmp).

%% Buffered makes it all appear on the screen at the same time
draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    Fun(MemoryDC),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).
