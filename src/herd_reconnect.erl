-module(herd_reconnect).

-export([exp_backoff/3, exp_backoff/4]).


-spec exp_backoff(integer(), integer(), integer()) -> integer().
exp_backoff(Attempt, BaseTimeout, MaxTimeout) ->
    exp_backoff(Attempt, 10, BaseTimeout, MaxTimeout).


-spec exp_backoff(integer(), integer(), integer(), integer()) -> integer().
exp_backoff(Attempt, MaxAttempt, _BaseTimeout, MaxTimeout) when Attempt >= MaxAttempt ->
    Half = MaxTimeout div 2,
    Half + random:uniform(Half);

exp_backoff(Attempt, _MaxAttempt, BaseTimeout, MaxTimeout) ->
    Timeout = min(erlang:round(math:pow(2, Attempt) * BaseTimeout), MaxTimeout),
    Half = Timeout div 2,
    Half + random:uniform(Half).
