/* Copyright (c) 2010-2025, Michael Santos <michael.santos@gmail.com>
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
#if defined(__GNUC__) && defined(__linux__)
#define _GNU_SOURCE
#endif
#if defined(__linux__) || (defined(__SVR4) && defined(__sun))
#include <crypt.h>
#else
#define _XOPEN_SOURCE
#define __USE_XOPEN
#include <unistd.h>
#endif
#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

#include <erl_nif.h>

#include <erl_driver.h>

#include "explicit_bzero.h"

#ifndef HAVE_CRYPT_R
struct PrivData {
  ErlNifMutex *mutex;
};
#endif

static int load_nif(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
#ifdef HAVE_CRYPT_R
  struct crypt_data data;
  data.initialized = 0;
  return crypt_r("Test crypt() support", "xx", &data) == NULL;
#else
  struct PrivData *p = enif_alloc(sizeof(struct PrivData));
  ErlNifMutex *mutex = enif_mutex_create("msantos_crypto");
  if (!p || !mutex) {
    if (mutex)
      enif_mutex_destroy(mutex);
    if (p)
      enif_free(p);
    return 1;
  };
  p->mutex = mutex;
  *priv_data = p;

  return crypt("Test crypt() support", "xx") == NULL;
#endif
}

static ERL_NIF_TERM nif_crypt(ErlNifEnv *env, int argc,
                              const ERL_NIF_TERM argv[]) {
#ifdef HAVE_CRYPT_R
  struct crypt_data data;
#else
  struct PrivData *p = (struct PrivData *)enif_priv_data(env);
#endif

  ErlNifBinary key_bin;
  ErlNifBinary salt_bin;
  char *result;
  ErlNifBinary result_bin;

  if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin))
    return enif_make_badarg(env);

  if (!enif_inspect_iolist_as_binary(env, argv[1], &salt_bin))
    return enif_make_badarg(env);

  /* NULL-terminate the key and salt binaries.
   *
   * Empty binaries (<<>>) are converted to a NULL terminated string (<<0>>).
   *
   */
  if (!enif_realloc_binary(&key_bin, key_bin.size + 1))
    return enif_make_badarg(env);

  key_bin.data[key_bin.size - 1] = '\0';

  if (!enif_realloc_binary(&salt_bin, salt_bin.size + 1)) {
    enif_release_binary(&key_bin);
    return enif_make_badarg(env);
  }

  salt_bin.data[salt_bin.size - 1] = '\0';

#ifdef HAVE_CRYPT_R
  data.initialized = 0;
  result =
      crypt_r((const char *)key_bin.data, (const char *)salt_bin.data, &data);
#else
  enif_mutex_lock(p->mutex);
  result = crypt((const char *)key_bin.data, (const char *)salt_bin.data);
  enif_mutex_unlock(p->mutex);
#endif
  /* Clean up the copy of the key */
  explicit_bzero(key_bin.data, key_bin.size);
  /* these realloc:ed binaries need to be released */
  enif_release_binary(&key_bin);
  enif_release_binary(&salt_bin);

  if (result == NULL)
    return enif_make_badarg(env);

  if (!enif_alloc_binary(strlen(result), &result_bin))
    return enif_make_badarg(env);

  memcpy(result_bin.data, result, result_bin.size);
  return enif_make_binary(env, &result_bin);
}

static void unload_nif(ErlNifEnv *env, void *priv) {
#ifndef HAVE_CRYPT_R
  struct PrivData *p = (struct PrivData *)priv;
  enif_mutex_destroy(p->mutex);
  enif_free(priv);
#endif
}

static ErlNifFunc nif_funcs[] = {{"crypt", 2, nif_crypt}};

ERL_NIF_INIT(crypt, nif_funcs, load_nif, NULL, NULL, unload_nif)
