%% Copyright (c) 2012-2016, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(crypt_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
        all/0
    ]).

-export([
        crypt/1,
        crypt_to_string/1
    ]).

all() ->
    [crypt, crypt_to_string].

crypt(_Config) ->
    <<"aaqPiZY5xR5l.">> = crypt:crypt("test", "aa"),
    <<"aaqPiZY5xR5l.">> = crypt:crypt([<<"t">>, ["e", [<<"s">>], <<"t">>]],
        [<<"a">>, "a"]),

    case os:type() of
        {unix, OS} when OS =:= linux; OS =:= freebsd ->
            <<"$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0">>
                = crypt:crypt("test", "$1$aaaaaaaa"),
            <<"$6$aaaaaaaa$HREHv6TuSmUS/7spCDO5Js3ssSZ6."
              "iwVkUoVtatJUhJDKVmERrRKBTolrPMub2s5dX6IEjZg6d6wZzFRlidV41">>
                = crypt:crypt("test", "$6$aaaaaaaa");
        {unix, openbsd} ->
            <<"$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0">>
                = crypt:crypt("test", "$1$aaaaaaaa");
        _ -> ok
    end.

crypt_to_string(_Config) ->
    "aaqPiZY5xR5l." = crypt:crypt_to_string(<<"test">>,"aa").
