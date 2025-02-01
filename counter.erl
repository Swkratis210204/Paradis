-module(counter).
-compile(export_all).

loop() ->
    receive
        ok -> loop()
    after 1000 ->
        io:format("Got noting"),
        loop()
    end.
