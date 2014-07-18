# crypt library for Erlang

Wrapper around the system `crypt(3)` library for Erlang.

## WARNING

The algorithms supported by `crypt` are dependent on the system `crypt(3)`
library. For example, Mac OS X only supports DES (booooooo!!!!).

The NIF library is only safe for multi-threaded environments where the
system `crypt_r(3)` function is available. For example, Mac OS X doesn't
support it (booooooo!!!! again).


## USAGE

    crypt(Key, Salt) -> Crypted

        Types   Key = string() | binary()
                Salt = string() | binary()
                Crypted = string() | binary()

        Calls the system crypt(3) function with the provided arguments.
        If the Key is passed as a string, then the Salt will have to be
        passed as a string aswell and the Crypted result will also be a
        string. The same constraints apply when passing a binary as the
        Key.

        If crypt(3) is not supported by the OS, the crypt module will
        fail to load.

        Depending on your system crypt(3) library, errors may or may not
        be returned. Some implementations return NULL. If this occurs,
        crypt/2 will throw a bad arg exception. Other implementations
        may choose to return a fixed string (if this is a concern,
        the caller will need to test for this condition).

        The NetBSD man page for crypt(3) summarizes the situation as:

            The behavior of crypt() on errors isn't well standardized.
            Some implementations simply can't fail (unless the process
            dies, in which case they obviously can't return), others
            return NULL or a fixed string.  Most implementations
            don't set errno, but some do.  Version 2 of the Single
            UNIX Specification (``SUSv2'') specifies only returning
            NULL and setting errno as a valid behavior, and defines
            only one possible error (ENOSYS, ``The functionality is
            not supported on this implementation.'') Unfortunately,
            most existing applications aren't prepared to handle NULL
            returns from crypt().  The description below corresponds
            to this implementation of crypt() only.  The behavior may
            change to match standards, other implementations or existing
            applications.

            crypt() may only fail (and return) when passed an invalid
            or unsupported setting, in which case it returns a pointer
            to a magic string that is shorter than 13 characters and is
            guaranteed to differ from setting.  This behavior is safe
            for older applications which assume that crypt() can't fail,
            when both setting new passwords and authenticating against
            existing password hashes.


## EXAMPLES

    1> crypt:crypt("test","aa").
    "aaqPiZY5xR5l."
    2> crypt:crypt("test","$1$aaaaaaaa").
    "$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0"
    3> crypt:crypt("test","$6$aaaaaaaa").
    "$6$aaaaaaaa$HREHv6TuSmUS/7spCDO5Js3ssSZ6.iwVkUoVtatJUhJDKVmERrRKBTolrPMub2s5dX6IEjZg6d6wZzFRlidV41"
    4> crypt:crypt(<<"test">>,<<"aa">>).
    <<"aaqPiZY5xR5l.">>
    5> crypt:crypt(<<"test">>,<<"$1$aaaaaaaa">>).
    <<"$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0">>
    6> crypt:crypt(<<"test">>,<<"$6$aaaaaaaa">>).
    <<"$6$aaaaaaaa$HREHv6TuSmUS/7spCDO5Js3ssSZ6.iwVkUoVtatJUhJDKVmERrRKBTolrPMub2s5dX6IEjZg6d6wZzFRlidV41">>
