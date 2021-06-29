-module(kv_util).

-export([get/2, set/3]).

get(DBName, Key) ->
  DBHandle = bitcask:open(DBName, [read_write]),
  Value    = bitcask:get(DBHandle, term_to_binary(Key)),
  bitcask:close(DBHandle),
  Value.

set(DBName, Key, Value) ->
  DBHandle = bitcask:open(DBName, [read_write]),
  bitcask:put(DBHandle, term_to_binary(Key), term_to_binary(Value)),
  bitcask:close(DBHandle).
