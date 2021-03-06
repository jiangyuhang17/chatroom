-ifndef(LIB_H).
-define(LIB_H, lib_h).

%% lager
-define(DEBUG(Fmt, Args),       catch lager:debug(Fmt, Args)).
-define(INFO(Fmt, Args),        catch lager:debug(Fmt, Args)).
-define(WARNING(Fmt, Args),     catch lager:warning(Fmt, Args)).
-define(ERROR(Fmt, Args),       catch lager:error(Fmt, Args)).

-define(DEBUG(Arg),          catch lager:debug("~p",   [Arg])).
-define(INFO(Arg),           catch lager:info("~p",    [Arg])).
-define(WARNING(Arg),        catch lager:warning("~p", [Arg])).
-define(ERROR(Arg),          catch lager:error("~p",   [Arg])).

-define(NOW, time_util:datetime()).

%% supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD(N, I, Args, Type), {N, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(V_CHILD(Mod, Name), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).

%% record
-record(chat, {id, uname, ts, content}).

-endif.