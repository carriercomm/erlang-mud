-module(mnesia_test).
-compile(export_all).

-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).

main() ->
    record_info(fields, shop).
