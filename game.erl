-module(game).
-export([start/0]).

start() ->
  rand:seed(exs64, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
  io:format("*************************~n"),
  io:format("* Welcome to No Thanks! *~n"),
  io:format("*************************~n"),

  Deck = shuffle(lists:seq(3, 35)),
  {Playing_Deck, _Discards} = lists:split(24, Deck),

  Player_Count = prompt_for_player_count(),
  Players = [ {Seat, [], 11} || Seat <- lists:seq(1, Player_Count) ],

  game_loop(Playing_Deck, 0, Players, []).


game_loop([], _Chips, Yet_To_Play, Played) ->
  who_won(lists:append(Yet_To_Play, Played));

game_loop(Deck, Chips, [], Played) ->
  game_loop(Deck, Chips, lists:reverse(Played), []);

game_loop([Offer|Deck], Chips, [{Seat, Cards, Stash}|Yet_To_Play], Played) ->
  io:format("~nPlayer ~p, you're up.~n", [Seat]),
  io:format("Card on Offer: ~p with ~p Chips~n", [Offer, Chips]),
  io:format("Your cards: "),
  lists:foreach(fun(X) -> io:format("~p ", [X]) end, Cards),
  io:format("~nYour stash of chips: ~p~n", [Stash]),

  case Stash of
    0 ->
      io:format("/////////////////////////////~n"),
      io:format("Looks like ye had to take it!~n"),
      io:format("/////////////////////////////~n"),
      game_loop(Deck, 0, [{Seat, lists:sort([Offer|Cards]), Stash+Chips}|Yet_To_Play], Played);

    _ ->
      Action = prompt_for_action(),

      case Action of
        $T ->
          game_loop(Deck, 0, [{Seat, lists:sort([Offer|Cards]), Stash+Chips}|Yet_To_Play], Played);
        $D ->
          game_loop([Offer|Deck], Chips+1, Yet_To_Play, [{Seat, Cards, Stash-1}|Played])
      end
  end.


%% Input helpers
prompt_for_player_count() ->
  case io:fread("How many players? ", "~d") of
    {ok, [Player_Count]} ->
      case Player_Count =< 5 andalso Player_Count >= 3 of
        true ->
          Player_Count;
        false ->
          io:format("No Thanks!  Plays 3 to 5 players...~n"),
          prompt_for_player_count()
      end;
    _ -> prompt_for_player_count()
  end.


prompt_for_action() ->
  [Action|_] = io:get_line("(T)ake or (D)ecline? "),

  case Action of
    $t -> $T;
    $T -> $T;
    $d -> $D;
    $D -> $D;
    $q -> exit(quit);
    $Q -> exit(quit);
    _ -> prompt_for_action()
  end.


%% Show me the winners!
who_won(Players) ->
  Scores = lists:map(fun({Seat, Cards, Stash}) -> {Seat, score(Cards, Stash)} end, Players),
  Places = lists:zip(lists:seq(1, length(Scores)), lists:sort(fun({_, X}, {_, Y}) -> X < Y end, Scores)),
  io:format("**************************~n"),
  io:format("*      FINAL SCORES      *~n"),
  io:format("**************************~n"),
  lists:foreach(fun({Place, {Seat, Score}}) -> io:format("~p. Player ~p with ~p points~n", [Place, Seat, Score]) end, Places).


%% Give me the score
score(Cards, Stash) ->
  score(Cards, Stash, [], 0).

score([], Stash, Sequence, Acc) ->
  [N|_] = lists:reverse(Sequence),
  Acc + N - Stash;
score([C|Cards], Stash, [], Acc) ->
  score(Cards, Stash, [C], Acc);
score([C|Cards], Stash, [S|Seq], Acc) when C == S + 1 ->
  score(Cards, Stash, [C,S|Seq], Acc);
score([C|Cards], Stash, Seq, Acc) ->
  [N|_] = lists:reverse(Seq),
  score(Cards, Stash, [C], Acc+N).


%% Helper Shuffler
shuffle(L) ->
  [ X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- L]) ].
