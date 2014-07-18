/* Copyright (c) 2010-2013, Michael Santos <michael.santos@gmail.com>
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
#ifdef __GNUC__
#define _GNU_SOURCE
#include <crypt.h>
#else
#define _XOPEN_SOURCE
#define __USE_XOPEN
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <alloca.h>
#include <sys/errno.h>
#include <erl_nif.h>
#include <erl_driver.h>

#define CRYPT_VERSION   "0.3.0"
#define MAXBUFLEN       1024    /* maximum values for passwd length and salt */
#define KEY             0       /* Position of key in argv */
#define SALT            1       /* Position of salt in argv */

static ERL_NIF_TERM nif_crypt_bin(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_crypt_str(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifndef HAVE_CRYPT_R
#warning "No support for crypt_r(); NIF library NOT safe for multi-threaded environments"
#endif

static int load_nif(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
#ifdef HAVE_CRYPT_R
    struct crypt_data data;
    data.initialized = 0;
    return crypt_r("Test crypt() support", "xx", &data) == NULL;
#else
    return crypt("Test crypt() support", "xx") == NULL;
#endif
}

static ERL_NIF_TERM nif_crypt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2)
        return enif_make_badarg(env);

    if (enif_is_binary(env, argv[KEY]) && enif_is_binary(env, argv[SALT])) {
        return nif_crypt_bin(env, argc, argv);
    } else if (enif_is_list(env, argv[KEY]) && enif_is_list(env, argv[SALT])) {
        return nif_crypt_str(env, argc, argv);
    } else {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM nif_crypt_bin(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_CRYPT_R
    struct crypt_data data;
#endif
    char *key_buf;
    ErlNifBinary key_bin;
    char *salt_buf;
    ErlNifBinary salt_bin;
    char *result;
    size_t result_len;
    ErlNifBinary result_bin;

    if (!enif_inspect_binary(env, argv[KEY], &key_bin) ||
        !enif_inspect_binary(env, argv[SALT], &salt_bin))
        return enif_make_badarg(env);

    /* Copy the binary 'key' to a NULL-terminated buffer */
    key_buf = alloca(key_bin.size + 1);
    memcpy(key_buf, key_bin.data, key_bin.size);
    key_buf[key_bin.size] = '\0';
    /* Copy the binary 'salt' to a NULL-terminated buffer */
    salt_buf = alloca(salt_bin.size + 1);
    memcpy(salt_buf, salt_bin.data, salt_bin.size);
    salt_buf[salt_bin.size] = '\0';

#ifdef HAVE_CRYPT_R
    data.initialized = 0;
    result = crypt_r(key_buf, salt_buf, &data);
#else
    result = crypt(key_buf, salt_buf);
#endif
    /* Clean up the copy of the key in our stack */
    memset(key_buf, '\0', key_bin.size);
    if (result == NULL)
        return enif_make_badarg(env);

    result_len = strlen(result);
    if (!enif_alloc_binary(result_len, &result_bin))
        return enif_make_atom(env, "enomem");

    /* Copy the encrypted result to the binary used as return value */
    result_bin.size = result_len;
    memcpy(result_bin.data, result, result_len);
    return enif_make_binary(env, &result_bin);
}

static ERL_NIF_TERM nif_crypt_str(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_CRYPT_R
    struct crypt_data data;
#endif
    char *key;
    unsigned key_len = 0;
    char *salt;
    unsigned salt_len = 0;
    char *result;

    if (!enif_get_list_length(env, argv[KEY], &key_len))
        return enif_make_badarg(env);

    key = alloca(key_len + 1);
    if (enif_get_string(env, argv[KEY], key, key_len + 1, ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

    if (!enif_get_list_length(env, argv[SALT], &salt_len))
        return enif_make_badarg(env);

    salt = alloca(salt_len + 1);
    if (enif_get_string(env, argv[SALT], salt, salt_len + 1, ERL_NIF_LATIN1) < 1)
        return enif_make_badarg(env);

#ifdef HAVE_CRYPT_R
    data.initialized = 0;
    result = crypt_r(key, salt, &data);
#else
    result = crypt(key, salt);
#endif

    /* Clean up the copy of the key in our stack */
    memset(key, '\0', key_len);
    if (result == NULL)
        return enif_make_badarg(env);

    return enif_make_string(env, result, ERL_NIF_LATIN1);
}


static ErlNifFunc nif_funcs[] = {
    {"crypt", 2, nif_crypt}
};


ERL_NIF_INIT(crypt, nif_funcs, load_nif, NULL, NULL, NULL)
