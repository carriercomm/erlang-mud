-module(trie_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    trie:new().

cleanup(Trie) ->
    trie:delete(Trie).

-define(trietest(F), {setup, fun setup/0, fun cleanup/1, F}).

trie_test_() ->
    {inparallel,
      [
        {"Simple add/retrieve with full string", ?trietest(fun simple_add_full_retrieve/1)},
        {"Simple add/retrieve with partial string", ?trietest(fun simple_add_partial_retrieve/1)},
        {"Multiple add/retrieve with full string", ?trietest(fun multi_add_full_retrieve/1)},
        {"Multiple add/retrieve with partial string", ?trietest(fun multi_add_partial_retrieve/1)},
        {"Multiple add/retrieve with ambiguous partial string", ?trietest(fun ambiguous_partial_retrieve/1)}
      ]
    }.

simple_add_full_retrieve(Trie) ->
    trie:add_term(Trie, "example", value),
    ?_assertEqual(value, trie:lookup_term(Trie, "example")).

simple_add_partial_retrieve(Trie) ->
    trie:add_term(Trie, "example", value),
    ?_assertEqual(value, trie:lookup_term(Trie, "e")).

multi_add_full_retrieve(Trie) ->
    trie:add_term(Trie, "example1", value1),
    trie:add_term(Trie, "example2", value2),
    [ ?_assertEqual(value1, trie:lookup_term(Trie, "example1")),
      ?_assertEqual(value2, trie:lookup_term(Trie, "example2")) ].

multi_add_partial_retrieve(Trie) ->
    trie:add_term(Trie, "example", value),
    trie:add_term(Trie, "foo", bar),
    [ ?_assertEqual(value, trie:lookup_term(Trie, "e")),
      ?_assertEqual(bar, trie:lookup_term(Trie, "f")) ].

ambiguous_partial_retrieve(Trie) ->
    trie:add_term(Trie, "example", value),
    trie:add_term(Trie, "exemplary", val_hue),
    trie:add_term(Trie, "foo", bar),
    trie:add_term(Trie, "fool", hardy),
    [ ?_assertEqual(undefined, trie:lookup_term(Trie, "ex")),
      ?_assertEqual(undefined, trie:lookup_term(Trie, "f")),
      ?_assertEqual(value, trie:lookup_term(Trie, "exa")),
      ?_assertEqual(bar, trie:lookup_term(Trie, "foo")) ].
