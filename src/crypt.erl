%%% @copyright 2010-2022 Michael Santos <michael.santos@gmail.com>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice,
%%% this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(crypt).

-export([crypt/2, crypt_to_string/2]).

-on_load(on_load/0).

on_load() ->
    Lib =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                filename:join([
                    filename:dirname(code:which(?MODULE)),
                    "..",
                    "priv",
                    ?MODULE
                ]);
            Dir ->
                filename:join([Dir, ?MODULE])
        end,
    erlang:load_nif(Lib, []).

%% @doc crypt(3): passphrase hashing
%%
%% Calls the system crypt(3) function with the provided arguments.
%%
%% If crypt(3) is not supported by the OS, the crypt module will
%% fail to load.
%%
%% Depending on your system crypt(3) library, errors may or may not
%% be returned. Some implementations return NULL. If this occurs,
%% crypt/2 will throw a bad arg exception. Other implementations
%% may choose to return a fixed string (if this is a concern,
%% the caller will need to test for this condition).
%%
%% The NetBSD man page for crypt(3) summarizes the situation as:
%%
%%     The behavior of crypt() on errors isn't well standardized.
%%     Some implementations simply can't fail (unless the process
%%     dies, in which case they obviously can't return), others
%%     return NULL or a fixed string.  Most implementations
%%     don't set errno, but some do.  Version 2 of the Single
%%     UNIX Specification (``SUSv2'') specifies only returning
%%     NULL and setting errno as a valid behavior, and defines
%%     only one possible error (ENOSYS, ``The functionality is
%%     not supported on this implementation.'') Unfortunately,
%%     most existing applications aren't prepared to handle NULL
%%     returns from crypt().  The description below corresponds
%%     to this implementation of crypt() only.  The behavior may
%%     change to match standards, other implementations or existing
%%     applications.
%%
%%     crypt() may only fail (and return) when passed an invalid
%%     or unsupported setting, in which case it returns a pointer
%%     to a magic string that is shorter than 13 characters and is
%%     guaranteed to differ from setting.  This behavior is safe
%%     for older applications which assume that crypt() can't fail,
%%     when both setting new passwords and authenticating against
%%     existing password hashes.
%%
%% == Examples ==
%%
%% ```
%% 1> crypt:crypt("test","aa").
%% <<"aaqPiZY5xR5l.">>
%% 2> crypt:crypt("test","$1$aaaaaaaa").
%% <<"$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0">>
%% 3> crypt:crypt("test","$6$aaaaaaaa").
%% <<"$6$aaaaaaaa$HREHv6TuSmUS/7spCDO5Js3ssSZ6.iwVkUoVtatJUhJDKVmERrRKBTolrPMub2s5dX6IEjZg6d6wZzFRlidV41">>
%% '''
-spec crypt(iodata(), iodata()) -> binary().
crypt(_, _) ->
    erlang:nif_error(not_implemented).

%% @doc crypt(3): passphrase hashing returning a string
%%
%% == Examples ==
%%
%% ```
%% 1> crypt:crypt_to_string(<<"test">>,"aa").
%% "aaqPiZY5xR5l."
%% '''
-spec crypt_to_string(iodata(), iodata()) -> [byte()].
crypt_to_string(Key, Salt) ->
    binary_to_list(crypt(Key, Salt)).
