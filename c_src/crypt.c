/* Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include "erl_nif.h"
#include "crypt.h"

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf, size_t buflen);
static ERL_NIF_TERM error_message(ErlNifEnv *env, char *atom, char *err, char *msg);


    static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    return (0);
}   


    static int
reload(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    return load(env, priv, load_info);
}


    static ERL_NIF_TERM
nif_crypt(ErlNifEnv *env, ERL_NIF_TERM _key, ERL_NIF_TERM _salt)
{
    char key[MAXBUFLEN];
    char salt[MAXBUFLEN];
    char *result = NULL;
    int rerrno = 0;

    (void)memset(&key, '\0', sizeof(key));
    (void)memset(&salt, '\0', sizeof(salt));

    if (!my_enif_get_string(env, _key, key, sizeof(key)))
        return enif_make_badarg(env);

    if (!my_enif_get_string(env, _salt, salt, sizeof(salt)))
        return enif_make_badarg(env);

    errno = 0;
    result = crypt(key, salt);
    rerrno = errno;

    (void)memset(&key, '\0', sizeof(key));
    (void)memset(&salt, '\0', sizeof(salt));

    if (result == NULL)
        return error_message(env, "error", "crypt", strerror(rerrno));

    return enif_make_string(env, result);
}


    static ERL_NIF_TERM
error_message(ErlNifEnv *env, char *atom, char *err, char *msg)
{
    return enif_make_tuple(env, 2,
            enif_make_atom(env, atom),
            enif_make_tuple(env, 2,
            enif_make_atom(env, err),
            enif_make_string(env, msg)));
}


/* from:
 * http://d.hatena.ne.jp/vostok92/20091201/1259680319
 *
 * Copies at most one less than buflen from buf and null
 * terminates the string.
 *
 * Should probably indicate that a truncation has taken place, but
 * the convention of the enif_get_* interfaces seems to be to return
 * true/false.
 *
 * The alternative is to return failure if a string is too large
 * for the buffer. This seems to allow for more predictable 
 * behaviour.
 *
 */
    static int
my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf, size_t buflen)
{
    ERL_NIF_TERM head, tail;
    int val;
    int n = 1;


    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (!enif_get_int(env, head, &val))
            return (0);

        if (n++ >= buflen)
            return (0);

        *buf = (char)val;
        buf++;
        list = tail; 
    }
    *buf = '\0';

    return (1);
}

static ErlNifFunc nif_funcs[] = {
    {"crypt", 2, nif_crypt}
};

ERL_NIF_INIT(crypt, nif_funcs, load, reload, NULL, NULL)


