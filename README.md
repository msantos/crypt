Wrapper around the system crypt(3) library for Erlang.

## WARNING

Which algorithms are supported by crypt are dependent on the system
crypt(3) library. For example, Mac OS X only supports DES (booooooo!!!!).


## USAGE

    crypt(Password, Salt) -> Crypted

        Types   Password = string()
                Salt = string()
                Crypted = string()
    
        Calls the system crypt(3) function with the provided arguments.

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


## EXAMPLE

    1> crypt:crypt("test","aa").
    "aaqPiZY5xR5l."
    2> crypt:crypt("test","$1$aaaaaaaa").
    "$1$aaaaaaaa$lWxWtPmiNjS/cwJnGm6fe0"
    3> crypt:crypt("test","$6$aaaaaaaa").
    "$6$aaaaaaaa$HREHv6TuSmUS/7spCDO5Js3ssSZ6.iwVkUoVtatJUhJDKVmERrRKBTolrPMub2s5dX6IEjZg6d6wZzFRlidV41"



## TODO

1. What is the maximum password and salt length?

   There doesn't appear to be a limit. DES has an 8 character limit and
   MD5 has a 256 character limit, but for the other types, no idea.

   sysconf(\_SC\_PASS\_MAX) seems to return the limit on Solaris, but isn't
   available on Ubuntu.
