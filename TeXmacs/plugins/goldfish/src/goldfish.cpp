//
// COPYRIGHT: (C) 2025  Liii Network Inc
// All rights reverved.
//

#include "goldfish.hpp"
#include "s7.h"
#include <string>
#include <iostream>
#include <cpr/cpr.h>

using namespace goldfish;
using namespace std;

static s7_pointer
response2hashtable (s7_scheme* sc, cpr::Response r) {
  s7_pointer ht= s7_make_hash_table (sc, 8);
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "status-code"), s7_make_integer (sc, r.status_code));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "url"), s7_make_string (sc, r.url.c_str()));
  s7_hash_table_set (sc, ht, s7_make_symbol(sc, "elapsed"), s7_make_real (sc, r.elapsed));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "text"), s7_make_string (sc, r.text.c_str ()));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "reason"), s7_make_string (sc, r.reason.c_str ()));
  s7_pointer headers= s7_make_hash_table(sc, 8);
  for (const auto &header : r.header) {
    const auto key= header.first.c_str ();
    const auto value= header.second.c_str ();
    s7_hash_table_set (sc, headers, s7_make_string (sc, key), s7_make_string (sc, value));
  }
  s7_hash_table_set (sc, ht, s7_make_symbol(sc, "headers"), headers);

  return ht;
}

inline cpr::Parameters
to_cpr_parameters (s7_scheme* sc, s7_pointer args) {
  cpr::Parameters params= cpr::Parameters{};
  if (s7_is_list(sc, args)) {
    s7_pointer iter= args;
    while (!s7_is_null (sc, iter)) {
      s7_pointer pair= s7_car (iter);
      if (s7_is_pair (pair)) {
        const char* key= s7_string (s7_car (pair));
        const char* value= s7_string (s7_cdr (pair));
        params.Add (cpr::Parameter (string (key), string (value)));
      }
      iter= s7_cdr (iter);
    }
  }
  return params;
}

inline cpr::Header
to_cpr_headers (s7_scheme* sc, s7_pointer args) {
  cpr::Header headers= cpr::Header{};
  if (s7_is_list(sc, args)) {
    s7_pointer iter= args;
    while (!s7_is_null (sc, iter)) {
      s7_pointer pair= s7_car (iter);
      if (s7_is_pair (pair)) {
        const char* key= s7_string (s7_car (pair));
        const char* value= s7_string (s7_cdr (pair));
        headers.insert (std::make_pair (key, value));
      }
      iter= s7_cdr (iter);
    }
  }
  return headers;
}

inline cpr::Proxies
to_cpr_proxies (s7_scheme* sc, s7_pointer args) {
  std::map<std::string, std::string> proxy_map;
  if (s7_is_list(sc, args)) {
    s7_pointer iter= args;
    while (!s7_is_null (sc, iter)) {
      s7_pointer pair= s7_car (iter);
      if (s7_is_pair (pair)) {
        const char* key= s7_string (s7_car (pair));
        const char* value= s7_string (s7_cdr (pair));
        proxy_map[key] = value;
      }
      iter= s7_cdr (iter);
    }
  }
  return cpr::Proxies(proxy_map);
}
static s7_pointer
f_http_head (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  cpr::Response r= session.Head ();
  return response2hashtable (sc, r);
}

inline void
glue_http_head (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* s_http_head = "g_http-head";
  const char* d_http_head = "(g_http-head url ...) => hash-table?";
  auto func_http_head= s7_make_typed_function (sc, s_http_head, f_http_head, 1, 0, false, d_http_head, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_http_head), func_http_head);
}

static s7_pointer
f_http_get (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  s7_pointer params= s7_cadr (args);
  cpr::Parameters cpr_params= to_cpr_parameters(sc, params);
  s7_pointer proxy= s7_caddr (args);
  cpr::Proxies cpr_proxies= to_cpr_proxies(sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  if (s7_is_list(sc, proxy) && !s7_is_null(sc, proxy)) {
    session.SetProxies(cpr_proxies);
  }

  cpr::Response r= session.Get ();
  return response2hashtable (sc, r);
}

inline void
glue_http_get (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* s_http_get= "g_http-get";
  const char* d_http_get= "(g_http-get url params proxy) => hash-table?";
  auto func_http_get= s7_make_typed_function (sc, s_http_get, f_http_get, 3, 0, false, d_http_get, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_http_get), func_http_get);
}

static s7_pointer
f_http_post (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  s7_pointer params= s7_cadr (args);
  cpr::Parameters cpr_params= to_cpr_parameters(sc, params);
  const char* body= s7_string (s7_caddr (args));
  cpr::Body cpr_body= cpr::Body (body);
  s7_pointer headers= s7_cadddr (args);
  cpr::Header cpr_headers= to_cpr_headers (sc, headers);
  s7_pointer proxy= s7_car (s7_cddddr (args));
  cpr::Proxies cpr_proxies= to_cpr_proxies (sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  session.SetBody (cpr_body);
  session.SetHeader (cpr_headers);
  if (s7_is_list(sc, proxy) && !s7_is_null(sc, proxy)) {
    session.SetProxies(cpr_proxies);
  }

  cpr::Response r= session.Post ();
  return response2hashtable (sc, r);
}

inline void
glue_http_post (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* name= "g_http-post";
  const char* doc= "(g_http-post url params body headers proxy) => hash-table?";
  auto func_http_post= s7_make_typed_function (sc, name, f_http_post, 5, 0, false, doc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func_http_post);
}

static s7_pointer
f_http_stream_get (s7_scheme* sc, s7_pointer args) {
  const char* url = s7_string (s7_car (args));
  s7_pointer params = s7_cadr (args);
  s7_pointer proxy = s7_caddr (args);
  s7_pointer userdata = s7_cadddr (args);
  s7_pointer callback = s7_car(s7_cddddr(args));

  cpr::Parameters cpr_params = to_cpr_parameters(sc, params);
  cpr::Proxies cpr_proxies = to_cpr_proxies(sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  if (s7_is_list(sc, proxy) && !s7_is_null(sc, proxy)) {
    session.SetProxies (cpr_proxies);
  }

  session.SetWriteCallback(cpr::WriteCallback{[sc, callback](const std::string_view& data, intptr_t cpr_userdata) -> bool {
    // Retrieve userdata from intptr_t
    s7_pointer userdata_ptr = (s7_pointer)cpr_userdata;

    // Call the scheme callback inline
    s7_pointer data_str = s7_make_string_with_length(sc, data.data(), data.length());
    s7_pointer args = s7_cons(sc, data_str, s7_cons(sc, userdata_ptr, s7_nil(sc)));

    s7_pointer ret = s7_call(sc, callback, args);
    if (s7_is_boolean(ret)) {
      return s7_boolean(sc, ret);
    }

    return true; // Continue receiving
  }, reinterpret_cast<intptr_t>(userdata)});

  try {
    cpr::Response response = session.Get();
  } catch (const std::exception& e) {
    return s7_make_integer(sc, 500); // Error case
  }
  return s7_undefined(sc);
}

inline void
glue_http_stream_get (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);
  const char* s_stream_get = "g_http-stream-get";
  const char* d_stream_get = "(g_http-stream-get url params proxy userdata callback) => undefined";
  auto func_stream_get = s7_make_typed_function (sc, s_stream_get, f_http_stream_get, 5, 0, false, d_stream_get, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_stream_get), func_stream_get);
}

static s7_pointer
f_http_stream_post (s7_scheme* sc, s7_pointer args) {
  s7_pointer url_arg = s7_car (args);
  s7_pointer params = s7_cadr (args);
  s7_pointer body_arg = s7_caddr (args);
  s7_pointer headers = s7_cadddr (args);
  s7_pointer proxy = s7_car(s7_cddddr(args));
  s7_pointer userdata = s7_cadr(s7_cddddr(args));
  s7_pointer callback = s7_list_ref(sc, args, 6);

  const char* url = s7_string(url_arg);
  const char* body = s7_string(body_arg);

  cpr::Parameters cpr_params = to_cpr_parameters(sc, params);
  cpr::Header cpr_headers = to_cpr_headers(sc, headers);
  cpr::Proxies cpr_proxies = to_cpr_proxies(sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  session.SetBody (cpr::Body (body));
  session.SetHeader (cpr_headers);
  if (s7_is_list(sc, proxy) && !s7_is_null(sc, proxy)) {
    session.SetProxies (cpr_proxies);
  }


  // Store userdata in s7 managed memory to prevent GC
  s7_pointer userdata_loc = s7_make_c_pointer(sc, (void*)userdata);

  session.SetWriteCallback(cpr::WriteCallback{[sc, callback](const std::string_view& data, intptr_t cpr_userdata) -> bool {
    // Retrieve userdata from intptr_t
    s7_pointer userdata_ptr = (s7_pointer)cpr_userdata;

    // Call the scheme callback inline
    s7_pointer data_str = s7_make_string_with_length(sc, data.data(), data.length());
    s7_pointer args = s7_cons(sc, data_str, s7_cons(sc, userdata_ptr, s7_nil(sc)));

    s7_pointer ret = s7_call(sc, callback, args);
    if (s7_is_boolean(ret)) {
      return s7_boolean(sc, ret);
    }

    return true; // Continue receiving
  }, reinterpret_cast<intptr_t>(userdata)});

  try {
    cpr::Response response = session.Post();
  } catch (const std::exception& e) {
    return s7_make_integer(sc, 500); // Error case
  }
  return s7_undefined(sc);
}

inline void
glue_http_stream_post (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_stream_post = "g_http-stream-post";
  const char* d_stream_post = "(g_http-stream-post url params body headers proxy userdata callback) => undefined";
  auto func_stream_post = s7_make_typed_function (sc, s_stream_post, f_http_stream_post, 7, 0, false, d_stream_post, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, s_stream_post), func_stream_post);
}

inline void
glue_http (s7_scheme* sc) {
  glue_http_head (sc);
  glue_http_get (sc);
  glue_http_post (sc);
}

inline void
glue_http_stream (s7_scheme* sc) {
  glue_http_stream_get(sc);
  glue_http_stream_post(sc);
}

int
main (int argc, char** argv) {
#ifdef TB_CONFIG_OS_WINDOWS
  SetConsoleOutputCP (65001);
#endif
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  s7_scheme* sc= init_goldfish_scheme (gf_lib);
  glue_http (sc);
  glue_http_stream (sc);
  return repl_for_community_edition (sc, argc, argv);
}

