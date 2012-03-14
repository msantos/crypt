/* Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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
#include "erl_driver.h"
#include "crypt.h"


    static int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return crypt("Test crypt() support", "xx") == NULL;
}

    static ERL_NIF_TERM
nif_crypt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char key[MAXBUFLEN] = {0};
    char salt[MAXBUFLEN] = {0};
    char *result = NULL;


    if (enif_get_string(env, argv[KEY], key, sizeof(key), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[SALT], salt, sizeof(salt), ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    result = crypt(key, salt);

    (void)memset(&key, '\0', sizeof(key));

    if (result == NULL)
        return enif_make_badarg(env);

    return enif_make_string(env, result, ERL_NIF_LATIN1);
}


static ErlNifFunc nif_funcs[] = {
    {"crypt", 2, nif_crypt}
};

ERL_NIF_INIT(crypt, nif_funcs, load, NULL, NULL, NULL)
