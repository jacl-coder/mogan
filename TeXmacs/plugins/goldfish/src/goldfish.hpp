//
// Copyright (C) 2024 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include <algorithm>
#include <argh.h>
#include <chrono>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <s7.h>
#include <string>
#include <vector>

#include <tbox/platform/file.h>
#include <tbox/platform/path.h>
#include <tbox/tbox.h>

#ifdef TB_CONFIG_OS_WINDOWS
#include <io.h>
#include <windows.h>
#elif TB_CONFIG_OS_MACOSX
#include <limits.h>
#include <mach-o/dyld.h>
#elif defined(__EMSCRIPTEN__)
#include <limits.h>
#else
#include <linux/limits.h>
#endif

#if !defined(TB_CONFIG_OS_WINDOWS)
#include <errno.h>
#include <pwd.h>
#include <unistd.h>
#if !defined(__EMSCRIPTEN__)
#include <wordexp.h>
#endif
#endif

#ifdef GOLDFISH_WITH_REPL
#include <functional>
#include <isocline.h>
#endif

#define GOLDFISH_VERSION "17.11.19"

#define GOLDFISH_PATH_MAXN TB_PATH_MAXN

static std::vector<std::string> command_args= std::vector<std::string> ();

namespace goldfish {
using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;

inline s7_pointer
string_vector_to_s7_vector (s7_scheme* sc, vector<string> v) {
  int        N  = v.size ();
  s7_pointer ret= s7_make_vector (sc, N);
  for (int i= 0; i < N; i++) {
    s7_vector_set (sc, ret, i, s7_make_string (sc, v[i].c_str ()));
  }
  return ret;
}

inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static s7_pointer
f_version (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, GOLDFISH_VERSION);
}

static s7_pointer
f_delete_file (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_file_remove (path_c));
}

inline void
glue_goldfish (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_version    = "version";
  const char* d_version    = "(version) => string";
  const char* s_delete_file= "g_delete-file";
  const char* d_delete_file= "(g_delete-file string) => boolean";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_version),
             s7_make_typed_function (sc, s_version, f_version, 0, 0, false, d_version, NULL));

  s7_define (sc, cur_env, s7_make_symbol (sc, s_delete_file),
             s7_make_typed_function (sc, s_delete_file, f_delete_file, 1, 0, false, d_delete_file, NULL));
}

static s7_pointer
f_current_second (s7_scheme* sc, s7_pointer args) {
  // TODO: use std::chrono::tai_clock::now() when using C++ 20
  tb_timeval_t tp= {0};
  tb_gettimeofday (&tp, tb_null);
  s7_double res= (time_t) tp.tv_sec + (tp.tv_usec / 1000000.0);
  return s7_make_real (sc, res);
}

inline void
glue_scheme_time (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_current_second= "g_current-second";
  const char* d_current_second= "(g_current-second): () => double, return the "
                                "current unix timestamp in double";
  s7_define (sc, cur_env, s7_make_symbol (sc, s_current_second),
             s7_make_typed_function (sc, s_current_second, f_current_second, 0, 0, false, d_current_second, NULL));
}

static s7_pointer
f_get_environment_variable (s7_scheme* sc, s7_pointer args) {
#ifdef _MSC_VER
  std::string path_sep= ";";
#else
  std::string path_sep= ":";
#endif
  std::string          ret;
  tb_size_t            size       = 0;
  const char*          key        = s7_string (s7_car (args));
  tb_environment_ref_t environment= tb_environment_init ();
  if (environment) {
    size= tb_environment_load (environment, key);
    if (size >= 1) {
      tb_for_all_if (tb_char_t const*, value, environment, value) { ret.append (value).append (path_sep); }
    }
  }
  tb_environment_exit (environment);
  if (size == 0) { // env key not found
    return s7_make_boolean (sc, false);
  }
  else {
    return s7_make_string (sc, ret.substr (0, ret.size () - 1).c_str ());
  }
}

static s7_pointer
f_command_line (s7_scheme* sc, s7_pointer args) {
  s7_pointer ret = s7_nil (sc);
  int        size= command_args.size ();
  for (int i= size - 1; i >= 0; i--) {
    ret= s7_cons (sc, s7_make_string (sc, command_args[i].c_str ()), ret);
  }
  return ret;
}

static s7_pointer
f_unset_environment_variable (s7_scheme* sc, s7_pointer args) {
  const char* env_name= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_environment_remove (env_name));
}

inline void
glue_scheme_process_context (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_get_environment_variable= "g_get-environment-variable";
  const char* d_get_environment_variable= "(g_get-environemt-variable string) => string";
  const char* s_command_line            = "g_command-line";
  const char* d_command_line            = "(g_command-line) => string";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_get_environment_variable),
             s7_make_typed_function (sc, s_get_environment_variable, f_get_environment_variable, 1, 0, false,
                                     d_get_environment_variable, NULL));
  s7_define (sc, cur_env, s7_make_symbol (sc, s_command_line),
             s7_make_typed_function (sc, s_command_line, f_command_line, 0, 0, false, d_command_line, NULL));
}

string
goldfish_exe () {
#ifdef TB_CONFIG_OS_WINDOWS
  char buffer[GOLDFISH_PATH_MAXN];
  GetModuleFileName (NULL, buffer, GOLDFISH_PATH_MAXN);
  return string (buffer);
#elif TB_CONFIG_OS_MACOSX
  char        buffer[PATH_MAX];
  uint32_t    size= sizeof (buffer);
  if (_NSGetExecutablePath (buffer, &size) == 0) {
    char real_path[GOLDFISH_PATH_MAXN];
    if (realpath (buffer, real_path) != NULL) {
      return string (real_path);
    }
  }
#elif TB_CONFIG_OS_LINUX
  char    buffer[GOLDFISH_PATH_MAXN];
  ssize_t len= readlink ("/proc/self/exe", buffer, sizeof (buffer) - 1);
  if (len != -1) {
    buffer[len]= '\0';
    return std::string (buffer);
  }
#endif
  return "";
}

static s7_pointer
f_executable (s7_scheme* sc, s7_pointer args) {
  string exe_path= goldfish_exe ();
  return s7_make_string (sc, exe_path.c_str ());
}

inline void
glue_executable (s7_scheme* sc) {
  const char* name= "g_executable";
  const char* desc= "(g_executable) => string";
  glue_define (sc, name, desc, f_executable, 0, 0);
}

inline void
glue_liii_sys (s7_scheme* sc) {
  glue_executable (sc);
}

static s7_pointer
f_os_arch (s7_scheme* sc, s7_pointer args) {
  return s7_make_string (sc, TB_ARCH_STRING);
}

inline void
glue_os_arch (s7_scheme* sc) {
  const char* name= "g_os-arch";
  const char* desc= "(g_os-arch) => string";
  glue_define (sc, name, desc, f_os_arch, 0, 0);
}

static s7_pointer
f_os_type (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_LINUX
  return s7_make_string (sc, "Linux");
#endif
#ifdef TB_CONFIG_OS_MACOSX
  return s7_make_string (sc, "Darwin");
#endif
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_string (sc, "Windows");
#endif
  return s7_make_boolean (sc, false);
}

inline void
glue_os_type (s7_scheme* sc) {
  const char* name= "g_os-type";
  const char* desc= "(g_os-type) => string";
  glue_define (sc, name, desc, f_os_type, 0, 0);
}

static s7_pointer
f_os_call (s7_scheme* sc, s7_pointer args) {
  const char*       cmd_c= s7_string (s7_car (args));
  tb_process_attr_t attr = {tb_null};
  attr.flags             = TB_PROCESS_FLAG_NO_WINDOW;
  int ret;

#if (defined(_MSC_VER) || defined(__MINGW32__))
  ret= (int) std::system (cmd_c);
#elif defined(__EMSCRIPTEN__)
  tb_char_t* argv[]= {(tb_char_t*) cmd_c, tb_null};
  ret              = (int) tb_process_run (argv[0], (tb_char_t const**) argv, &attr);
#else
  wordexp_t p;
  ret= wordexp (cmd_c, &p, 0);
  if (ret != 0) {
    // failed after calling wordexp
  }
  else if (p.we_wordc == 0) {
    wordfree (&p);
    ret= EINVAL;
  }
  else {
    ret= (int) tb_process_run (p.we_wordv[0], (tb_char_t const**) p.we_wordv, &attr);
    wordfree (&p);
  }
#endif
  return s7_make_integer (sc, ret);
}

inline void
glue_os_call (s7_scheme* sc) {
  const char* name= "g_os-call";
  const char* desc= "(g_os-call string) => int, execute a shell command and return the exit code";
  glue_define (sc, name, desc, f_os_call, 1, 0);
}

static s7_pointer
f_system (s7_scheme* sc, s7_pointer args) {
  const char* cmd_c= s7_string (s7_car (args));
  int         ret  = (int) std::system (cmd_c);
  return s7_make_integer (sc, ret);
}

inline void
glue_system (s7_scheme* sc) {
  const char* name= "g_system";
  const char* desc= "(g_system string) => int, execute a shell command and return the exit code";
  glue_define (sc, name, desc, f_system, 1, 0);
}

static s7_pointer
f_access (s7_scheme* sc, s7_pointer args) {
  const char* path_c= s7_string (s7_car (args));
  int         mode  = s7_integer ((s7_cadr (args)));
  bool        ret   = false;
  if (mode == 0) {
    tb_file_info_t info;
    ret= tb_file_info (path_c, &info);
  }
  else {
    ret= tb_file_access (path_c, mode);
  }

  return s7_make_boolean (sc, ret);
}

inline void
glue_access (s7_scheme* sc) {
  const char* name= "g_access";
  const char* desc= "(g_access string integer) => boolean, check file access permissions";
  glue_define (sc, name, desc, f_access, 2, 0);
}

// 实现 putenv 功能
static s7_pointer
f_set_environment_variable (s7_scheme* sc, s7_pointer args) {
  const char* key  = s7_string (s7_car (args));
  const char* value= s7_string (s7_cadr (args));
  return s7_make_boolean (sc, tb_environment_set (key, value));
}

inline void
glue_setenv (s7_scheme* sc) {
  const char* name= "g_setenv";
  const char* desc= "(g_setenv key value) => boolean, set an environment variable";
  glue_define (sc, name, desc, f_set_environment_variable, 2, 0);
}

inline void
glue_unsetenv (s7_scheme* sc) {
  const char* name= "g_unsetenv";
  const char* desc= "(g_unsetenv string): string => boolean";
  glue_define (sc, name, desc, f_unset_environment_variable, 1, 0);
}

static s7_pointer
f_os_temp_dir (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_temporary (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

inline void
glue_os_temp_dir (s7_scheme* sc) {
  const char* name= "g_os-temp-dir";
  const char* desc= "(g_os-temp-dir) => string, get the temporary directory path";
  glue_define (sc, name, desc, f_os_temp_dir, 0, 0);
}

static s7_pointer
f_mkdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_create (dir_c));
}

inline void
glue_mkdir (s7_scheme* sc) {
  const char* name= "g_mkdir";
  const char* desc= "(g_mkdir string) => boolean, create a directory";
  glue_define (sc, name, desc, f_mkdir, 1, 0);
}

static s7_pointer
f_rmdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_remove (dir_c));
}

inline void
glue_rmdir (s7_scheme* sc) {
  const char* name= "g_rmdir";
  const char* desc= "(g_rmdir string) => boolean, remove a directory";
  glue_define (sc, name, desc, f_rmdir, 1, 0);
}

static s7_pointer
f_remove_file (s7_scheme* sc, s7_pointer args) {
  const char* path   = s7_string (s7_car (args));
  bool        success= tb_file_remove (path); // 直接调用 TBOX 删除文件
  return s7_make_boolean (sc, success);
}

inline void
glue_remove_file (s7_scheme* sc) {
  const char* name= "g_remove-file";
  const char* desc= "(g_remove-file path) => boolean, delete a file";
  glue_define (sc, name, desc, f_remove_file, 1, 0);
}

static s7_pointer
f_chdir (s7_scheme* sc, s7_pointer args) {
  const char* dir_c= s7_string (s7_car (args));
  return s7_make_boolean (sc, tb_directory_current_set (dir_c));
}

inline void
glue_chdir (s7_scheme* sc) {
  const char* name= "g_chdir";
  const char* desc= "(g_chdir string) => boolean, change the current working directory";
  glue_define (sc, name, desc, f_chdir, 1, 0);
}

static tb_long_t
tb_directory_walk_func (tb_char_t const* path, tb_file_info_t const* info, tb_cpointer_t priv) {
  // check
  tb_assert_and_check_return_val (path && info, TB_DIRECTORY_WALK_CODE_END);

  vector<string>* p_v_result= (vector<string>*) priv;
  p_v_result->push_back (string (path));
  return TB_DIRECTORY_WALK_CODE_CONTINUE;
}

static s7_pointer
f_listdir (s7_scheme* sc, s7_pointer args) {
  const char*    path_c= s7_string (s7_car (args));
  vector<string> entries;
  s7_pointer     ret= s7_make_vector (sc, 0);
  tb_directory_walk (path_c, 0, tb_false, tb_directory_walk_func, &entries);

  int    entries_N   = entries.size ();
  string path_s      = string (path_c);
  int    path_N      = path_s.size ();
  int    path_slash_N= path_N;
  char   last_ch     = path_s[path_N - 1];
#if defined(TB_CONFIG_OS_WINDOWS)
  if (last_ch != '/' && last_ch != '\\') {
    path_slash_N= path_slash_N + 1;
  }
#else
  if (last_ch != '/') {
    path_slash_N= path_slash_N + 1;
  }
#endif
  for (int i= 0; i < entries_N; i++) {
    entries[i]= entries[i].substr (path_slash_N);
  }
  return string_vector_to_s7_vector (sc, entries);
}

inline void
glue_listdir (s7_scheme* sc) {
  const char* name= "g_listdir";
  const char* desc= "(g_listdir string) => vector, list the contents of a directory";
  glue_define (sc, name, desc, f_listdir, 1, 0);
}

static s7_pointer
f_getcwd (s7_scheme* sc, s7_pointer args) {
  tb_char_t path[GOLDFISH_PATH_MAXN];
  tb_directory_current (path, GOLDFISH_PATH_MAXN);
  return s7_make_string (sc, path);
}

inline void
glue_getcwd (s7_scheme* sc) {
  const char* name= "g_getcwd";
  const char* desc= "(g_getcwd) => string, get the current working directory";
  glue_define (sc, name, desc, f_getcwd, 0, 0);
}

static s7_pointer
f_getlogin (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_boolean (sc, false);
#else
  uid_t          uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  return s7_make_string (sc, pwd->pw_name);
#endif
}

inline void
glue_getlogin (s7_scheme* sc) {
  const char* name= "g_getlogin";
  const char* desc= "(g_getlogin) => string, get the current user's login name";
  glue_define (sc, name, desc, f_getlogin, 0, 0);
}

static s7_pointer
f_getpid (s7_scheme* sc, s7_pointer args) {
#ifdef TB_CONFIG_OS_WINDOWS
  return s7_make_integer (sc, (int) GetCurrentProcessId ());
#else
  return s7_make_integer (sc, getpid ());
#endif
}

inline void
glue_getpid (s7_scheme* sc) {
  const char* name= "g_getpid";
  const char* desc= "(g_getpid) => integer";
  glue_define (sc, name, desc, f_getpid, 0, 0);
}

inline void
glue_liii_os (s7_scheme* sc) {
  glue_os_arch (sc);
  glue_os_type (sc);
  glue_os_call (sc);
  glue_system (sc);
  glue_access (sc);
  glue_setenv (sc);
  glue_unsetenv (sc);
  glue_getcwd (sc);
  glue_os_temp_dir (sc);
  glue_mkdir (sc);
  glue_rmdir (sc);
  glue_remove_file (sc);
  glue_chdir (sc);
  glue_listdir (sc);
  glue_getlogin (sc);
  glue_getpid (sc);
}

static s7_pointer
f_uuid4 (s7_scheme* sc, s7_pointer args) {
  tb_char_t        uuid[37];
  const tb_char_t* ret= tb_uuid4_make_cstr (uuid, tb_null);
  return s7_make_string (sc, ret);
}

inline void
glue_uuid4 (s7_scheme* sc) {
  const char* name= "g_uuid4";
  const char* desc= "(g_uuid4) => string";
  glue_define (sc, name, desc, f_uuid4, 0, 0);
}

inline void
glue_liii_uuid (s7_scheme* sc) {
  glue_uuid4 (sc);
}

static s7_pointer
f_isdir (s7_scheme* sc, s7_pointer args) {
  const char*    dir_c= s7_string (s7_car (args));
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_DIRECTORY:
    case TB_FILE_TYPE_DOT:
    case TB_FILE_TYPE_DOT2:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

inline void
glue_isdir (s7_scheme* sc) {
  const char* name= "g_isdir";
  const char* desc= "(g_isdir string) => boolean";
  glue_define (sc, name, desc, f_isdir, 1, 0);
}

static s7_pointer
f_isfile (s7_scheme* sc, s7_pointer args) {
  const char*    dir_c= s7_string (s7_car (args));
  tb_file_info_t info;
  bool           ret= false;
  if (tb_file_info (dir_c, &info)) {
    switch (info.type) {
    case TB_FILE_TYPE_FILE:
      ret= true;
    }
  }
  return s7_make_boolean (sc, ret);
}

inline void
glue_isfile (s7_scheme* sc) {
  const char* name= "g_isfile";
  const char* desc= "(g_isfile string) => boolean";
  glue_define (sc, name, desc, f_isfile, 1, 0);
}

static s7_pointer
f_path_getsize (s7_scheme* sc, s7_pointer args) {
  const char*    path_c= s7_string (s7_car (args));
  tb_file_info_t info;
  if (tb_file_info (path_c, &info)) {
    return s7_make_integer (sc, (int) info.size);
  }
  else {
    return s7_make_integer (sc, (int) -1);
  }
}

inline void
glue_path_getsize (s7_scheme* sc) {
  const char* name= "g_path-getsize";
  const char* desc= "(g_path_getsize string): string => integer";
  glue_define (sc, name, desc, f_path_getsize, 1, 0);
}

static s7_pointer
f_path_read_text (s7_scheme* sc, s7_pointer args) {
  const char* path= s7_string (s7_car (args));
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_RO);
  if (file == tb_null) {
    // TODO: warning on the tb_file_init failure
    return s7_make_boolean (sc, false);
  }

  tb_file_sync (file);

  tb_size_t size= tb_file_size (file);
  if (size == 0) {
    tb_file_exit (file);
    return s7_make_string (sc, "");
  }

  tb_byte_t* buffer   = new tb_byte_t[size + 1];
  tb_size_t  real_size= tb_file_read (file, buffer, size);
  buffer[real_size]   = '\0';

  tb_file_exit (file);
  std::string content (reinterpret_cast<char*> (buffer), real_size);
  delete[] buffer;

  return s7_make_string (sc, content.c_str ());
}

inline void
glue_path_read_text (s7_scheme* sc) {
  const char* name= "g_path-read-text";
  const char* desc= "(g_path-read-text path) => string, read the content of the file at the given path";
  s7_define_function (sc, name, f_path_read_text, 1, 0, false, desc);
}

static s7_pointer
f_path_read_bytes (s7_scheme* sc, s7_pointer args) {
  const char* path= s7_string (s7_car (args));
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_RO);
  if (file == tb_null) {
    return s7_make_boolean (sc, false);
  }

  tb_file_sync (file);
  tb_size_t size= tb_file_size (file);

  if (size == 0) {
    tb_file_exit (file);
    // Create an empty bytevector with correct parameters
    return s7_make_byte_vector (sc, 0, 1, NULL); // 1 dimension, no dimension info
  }

  // Allocate buffer similar to f_path_read_text
  tb_byte_t* buffer   = new tb_byte_t[size];
  tb_size_t  real_size= tb_file_read (file, buffer, size);
  tb_file_exit (file);

  if (real_size != size) {
    delete[] buffer;
    return s7_make_boolean (sc, false); // Read failed
  }

  // Create a Scheme bytevector and copy data
  s7_pointer bytevector     = s7_make_byte_vector (sc, real_size, 1, NULL); // 1 dimension, no dimension info
  tb_byte_t* bytevector_data= s7_byte_vector_elements (bytevector);
  memcpy (bytevector_data, buffer, real_size);

  delete[] buffer;
  return bytevector; // Return the bytevector
}

inline void
glue_path_read_bytes (s7_scheme* sc) {
  const char* name= "g_path-read-bytes";
  const char* desc= "(g_path-read-bytes path) => bytevector, read the binary content of the file at the given path";
  s7_define_function (sc, name, f_path_read_bytes, 1, 0, false, desc);
}

static s7_pointer
f_path_write_text (s7_scheme* sc, s7_pointer args) {
  const char* path= s7_string (s7_car (args));
  if (!path) {
    return s7_make_integer (sc, -1);
  }

  const char* content= s7_string (s7_cadr (args));
  if (!content) {
    return s7_make_integer (sc, -1);
  }

  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_TRUNC);
  if (file == tb_null) {
    return s7_make_integer (sc, -1);
  }

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return s7_make_integer (sc, -1);
  }

  tb_size_t content_size= strlen (content);
  tb_size_t written_size= tb_file_writ (file, reinterpret_cast<const tb_byte_t*> (content), content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer (sc, written_size);
  }
  else {
    return s7_make_integer (sc, -1);
  }
}

inline void
glue_path_write_text (s7_scheme* sc) {
  const char* name= "g_path-write-text";
  const char* desc= "(g_path-write-text path content) => integer,\
write content to the file at the given path and return the number of bytes written, or -1 on failure";
  s7_define_function (sc, name, f_path_write_text, 2, 0, false, desc);
}

static s7_pointer
f_path_append_text (s7_scheme* sc, s7_pointer args) {
  const char* path= s7_string (s7_car (args));
  if (!path) {
    return s7_make_integer (sc, -1);
  }

  const char* content= s7_string (s7_cadr (args));
  if (!content) {
    return s7_make_integer (sc, -1);
  }

  // 以追加模式打开文件
  tb_file_ref_t file= tb_file_init (path, TB_FILE_MODE_WO | TB_FILE_MODE_CREAT | TB_FILE_MODE_APPEND);
  if (file == tb_null) {
    return s7_make_integer (sc, -1);
  }

  tb_filelock_ref_t lock= tb_filelock_init (file);
  if (tb_filelock_enter (lock, TB_FILELOCK_MODE_EX) == tb_false) {
    tb_filelock_exit (lock);
    tb_file_exit (file);
    return s7_make_integer (sc, -1);
  }

  tb_size_t content_size= strlen (content);
  tb_size_t written_size= tb_file_writ (file, reinterpret_cast<const tb_byte_t*> (content), content_size);

  bool release_success= tb_filelock_leave (lock);
  tb_filelock_exit (lock);
  bool exit_success= tb_file_exit (file);

  if (written_size == content_size && release_success && exit_success) {
    return s7_make_integer (sc, written_size);
  }
  else {
    return s7_make_integer (sc, -1);
  }
}

inline void
glue_path_append_text (s7_scheme* sc) {
  const char* name= "g_path-append-text";
  const char* desc= "(g_path-append-text path content) => integer,\
append content to the file at the given path and return the number of bytes written, or -1 on failure";
  s7_define_function (sc, name, f_path_append_text, 2, 0, false, desc);
}

static s7_pointer
f_path_touch (s7_scheme* sc, s7_pointer args) {
  const char* path= s7_string (s7_car (args));
  if (!path) {
    return s7_make_boolean (sc, false);
  }

  tb_bool_t success= tb_file_touch (path, 0, 0);

  if (success == tb_true) {
    return s7_make_boolean (sc, true);
  }
  else {
    return s7_make_boolean (sc, false);
  }
}

inline void
glue_path_touch (s7_scheme* sc) {
  const char* name= "g_path-touch";
  const char* desc= "(g_path-touch path) => boolean, create empty file or update modification time";
  s7_define_function (sc, name, f_path_touch, 1, 0, false, desc);
}

inline void
glue_liii_path (s7_scheme* sc) {
  glue_isfile (sc);
  glue_isdir (sc);
  glue_path_getsize (sc);
  glue_path_read_text (sc);
  glue_path_read_bytes (sc);
  glue_path_write_text (sc);
  glue_path_append_text (sc);
  glue_path_touch (sc);
}

static s7_pointer
f_datetime_now (s7_scheme* sc, s7_pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return s7_f (sc);
  }

  // Use C++ chrono to get microseconds
  std::uint64_t micros= 0;
#ifdef TB_CONFIG_OS_WINDOWS
  // On Windows, ensure we properly handle chrono
  FILETIME       ft;
  ULARGE_INTEGER uli;
  GetSystemTimeAsFileTime (&ft);
  uli.LowPart = ft.dwLowDateTime;
  uli.HighPart= ft.dwHighDateTime;
  // Convert to microseconds and get modulo
  micros= (uli.QuadPart / 10) % 1000000; // Convert from 100-nanosecond intervals to microseconds
#else
  // Standard approach for other platforms
  auto now_chrono= std::chrono::system_clock::now ();
  auto duration  = now_chrono.time_since_epoch ();
  micros         = std::chrono::duration_cast<std::chrono::microseconds> (duration).count () % 1000000;
#endif

  // Create a vector with the time components - vector is easier to index than list in Scheme
  s7_pointer time_vec= s7_make_vector (sc, 7);

  // Fill the vector with values
  s7_vector_set (sc, time_vec, 0, s7_make_integer (sc, lt.year));   // year
  s7_vector_set (sc, time_vec, 1, s7_make_integer (sc, lt.month));  // month
  s7_vector_set (sc, time_vec, 2, s7_make_integer (sc, lt.mday));   // day
  s7_vector_set (sc, time_vec, 3, s7_make_integer (sc, lt.hour));   // hour
  s7_vector_set (sc, time_vec, 4, s7_make_integer (sc, lt.minute)); // minute
  s7_vector_set (sc, time_vec, 5, s7_make_integer (sc, lt.second)); // second
  s7_vector_set (sc, time_vec, 6, s7_make_integer (sc, micros));    // micro-second

  return time_vec;
}

inline void
glue_datetime_now (s7_scheme* sc) {
  const char* name= "g_datetime-now";
  const char* desc= "(g_datetime-now) => datetime, create a datetime object with current time";
  s7_define_function (sc, name, f_datetime_now, 0, 0, false, desc);
}

static s7_pointer
f_date_now (s7_scheme* sc, s7_pointer args) {
  // Get current time using tbox for year, month, day, etc.
  tb_time_t now= tb_time ();

  // Get local time
  tb_tm_t lt= {0};
  if (!tb_localtime (now, &lt)) {
    return s7_f (sc);
  }

  // Create a vector with the time components - vector is easier to index than list in Scheme
  s7_pointer time_vec= s7_make_vector (sc, 3);

  // Fill the vector with values
  s7_vector_set (sc, time_vec, 0, s7_make_integer (sc, lt.year));  // year
  s7_vector_set (sc, time_vec, 1, s7_make_integer (sc, lt.month)); // month
  s7_vector_set (sc, time_vec, 2, s7_make_integer (sc, lt.mday));  // day

  return time_vec;
}

inline void
glue_date_now (s7_scheme* sc) {
  const char* name= "g_date-now";
  const char* desc= "(g_date-now) => date, create a date object with current date";
  s7_define_function (sc, name, f_date_now, 0, 0, false, desc);
}

inline void
glue_liii_datetime (s7_scheme* sc) {
  glue_datetime_now (sc);
  glue_date_now (sc);
}

// -------------------------------- iota --------------------------------
static inline s7_pointer
iota_list (s7_scheme* sc, s7_int count, s7_pointer start, s7_int step) {
  s7_pointer res= s7_nil (sc);
  s7_int     val;
  for (val= s7_integer (start) + step * (count - 1); count > 0; count--) {
    res= s7_cons (sc, s7_make_integer (sc, val), res);
    val-= step;
  }
  return res;
}

static s7_pointer
iota_list_p_ppp (s7_scheme* sc, s7_pointer count, s7_pointer start, s7_pointer step) {
  if (!s7_is_integer (count)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: count must be an integer"), count));
  }
  if (!s7_is_integer (start)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: start must be an integer"), start));
  }
  if (!s7_is_integer (step)) {
    return s7_error (sc, s7_make_symbol (sc, "type-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: step must be an integer"), step));
  }
  s7_int cnt= s7_integer (count);
  if (cnt < 0) {
    return s7_error (sc, s7_make_symbol (sc, "value-error"),
                     s7_list (sc, 2, s7_make_string (sc, "iota: count is negative"), count));
  }
  s7_int st = s7_integer (start);
  s7_int stp= s7_integer (step);
  return iota_list (sc, cnt, start, stp);
}

static s7_pointer
g_iota_list (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg1 = s7_car (args); // count
  s7_pointer rest1= s7_cdr (args);
  s7_pointer arg2 = (s7_is_pair (rest1)) ? s7_car (rest1) : s7_make_integer (sc, 0); // start value, default 0
  s7_pointer rest2= s7_cdr (rest1);
  s7_pointer arg3 = (s7_is_pair (rest2)) ? s7_car (rest2) : s7_make_integer (sc, 1); // step size, default 1
  return iota_list_p_ppp (sc, arg1, arg2, arg3);
}

inline void
glue_iota_list (s7_scheme* sc) {
  const char* name= "iota";
  const char* desc= "(iota count [start [step]]) => list, returns a list of count elements starting from start "
                    "(default 0) with step (default 1)";
  s7_define_function (sc, name, g_iota_list, 1, 2, false, desc);
}

inline void
glue_liii_list (s7_scheme* sc) {
  glue_iota_list (sc);
}

void
glue_for_community_edition (s7_scheme* sc) {
  glue_goldfish (sc);
  glue_scheme_time (sc);
  glue_scheme_process_context (sc);
  glue_liii_sys (sc);
  glue_liii_os (sc);
  glue_liii_path (sc);
  glue_liii_list (sc);
  glue_liii_datetime (sc);
  glue_liii_uuid (sc);
}

static void
display_help () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "--help, -h    \tDisplay this help message" << endl;
  cout << "--version, -v \tDisplay version" << endl;
  cout << "--mode, -m    \tAllowed mode: default, liii, sicp, r7rs, s7" << endl;
  cout << "--eval, -e    \tLoad and evaluate Scheme code from the command line" << endl
       << "\t\t  e.g. -e '(begin (display `Hello) (+ 1 2))'" << endl;
  cout << "--load, -l FILE\tLoad Scheme code from FILE" << endl;
#ifdef GOLDFISH_WITH_REPL
  cout << "--repl, -i    \tEnter interactive REPL mode" << endl;
#else
  cout << "--repl, -i    \t*Interactive REPL is not available in this build*" << endl;
#endif
  cout << "FILE [FILE...]\tLoad and evaluate Scheme code from one or more files" << endl
       << "\t\t  (all non-option arguments are treated as files to load)" << endl;
  cout << endl << "If no FILE is specified, REPL is entered by default (if available)." << endl;
}

static void
display_version () {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "based on S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")" << endl;
}

static void
display_for_invalid_options (const std::vector<std::string>& invalid_opts) {
  for (const auto& opt : invalid_opts) {
    std::cerr << "Invalid option: " << opt << "\n";
  }
  std::cerr << "\n";
  display_help ();
}

static void
goldfish_eval_file (s7_scheme* sc, string path, bool quiet) {
  s7_pointer result= s7_load (sc, path.c_str ());
  if (!result) {
    cerr << "Failed to load " << path << endl;
    exit (-1);
  }
  if (!quiet) {
    cout << path << " => " << s7_object_to_c_string (sc, result) << endl;
  }
}

static void
goldfish_eval_code (s7_scheme* sc, string code) {
  s7_pointer x= s7_eval_c_string (sc, code.c_str ());
  cout << s7_object_to_c_string (sc, x) << endl;
}

s7_scheme*
init_goldfish_scheme (const char* gf_lib) {
  s7_scheme* sc= s7_init ();
  s7_add_to_load_path (sc, gf_lib);

  if (!tb_init (tb_null, tb_null)) exit (-1);

  glue_for_community_edition (sc);
  return sc;
}

void
customize_goldfish_by_mode (s7_scheme* sc, string mode, const char* boot_file_path) {
  if (mode != "s7") {
    s7_load (sc, boot_file_path);
  }

  if (mode == "default" || mode == "liii") {
    s7_eval_c_string (sc, "(import (liii base) (liii error) (liii oop))");
  }
  else if (mode == "scheme") {
    s7_eval_c_string (sc, "(import (liii base) (liii error))");
  }
  else if (mode == "sicp") {
    s7_eval_c_string (sc, "(import (scheme base) (srfi sicp))");
  }
  else if (mode == "r7rs") {
    s7_eval_c_string (sc, "(import (scheme base))");
  }
  else if (mode == "s7") {
  }
  else {
    cerr << "No such mode: " << mode << endl;
    exit (-1);
  }
}

string
find_goldfish_library () {
  string exe_path= goldfish_exe ();

  tb_char_t        data_bin[TB_PATH_MAXN]= {0};
  tb_char_t const* ret_bin               = tb_path_directory (exe_path.c_str (), data_bin, sizeof (data_bin));

  tb_char_t        data_root[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_root                = tb_path_directory (ret_bin, data_root, sizeof (data_root));

  tb_char_t        data_lib[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_lib                = tb_path_absolute_to (gf_root, "share/goldfish", data_lib, sizeof (data_lib));
#ifdef TB_CONFIG_OS_LINUX
  if (strcmp (gf_root, "/") == 0) {
    gf_lib= "/usr/share/goldfish";
  }
#endif

  if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
    gf_lib= tb_path_absolute_to (gf_root, "goldfish", data_lib, sizeof (data_lib));
    if (!tb_file_access (gf_lib, TB_FILE_MODE_RO)) {
      cerr << "The load path for Goldfish standard library does not exist" << endl;
      exit (-1);
    }
  }

  return string (gf_lib);
}

string
find_goldfish_boot (const char* gf_lib) {
  tb_char_t        data_boot[TB_PATH_MAXN]= {0};
  tb_char_t const* gf_boot= tb_path_absolute_to (gf_lib, "scheme/boot.scm", data_boot, sizeof (data_boot));

  if (!tb_file_access (gf_boot, TB_FILE_MODE_RO)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
    exit (-1);
  }
  return string (gf_boot);
}

#ifdef GOLDFISH_WITH_REPL
struct SymbolInfo {
  std::string name;
  std::string doc;
};
static std::vector<SymbolInfo> cached_symbols;

// UNLIMITED history
// TODO(jinser): 1. programatic value-history procedure api in scheme
//               2. `,option value-history` meta command
static std::vector<s7_pointer> history_values;

inline void
update_symbol_cache (s7_scheme* sc) {
  cached_symbols.clear ();
  s7_pointer cur_env = s7_curlet (sc);
  s7_pointer sym_list= s7_let_to_list (sc, cur_env);
  int        n       = s7_list_length (sc, sym_list);
  for (int i= 0; i < n; ++i) {
    s7_pointer  pair= s7_list_ref (sc, sym_list, i);
    s7_pointer  sym = s7_car (pair);
    s7_pointer  val = s7_cdr (pair);
    const char* name= s7_symbol_name (sym);
    const char* doc = s7_documentation (sc, val);
    cached_symbols.push_back ({name, doc ? doc : ""});
  }
}

inline void
ic_goldfish_eval (s7_scheme* sc, const char* code) {
  int        err_gc_loc= -1, out_gc_loc= -1;
  s7_pointer old_err_port= s7_set_current_error_port (sc, s7_open_output_string (sc));
  if (old_err_port != s7_nil (sc)) err_gc_loc= s7_gc_protect (sc, old_err_port);

  s7_pointer out_port    = s7_open_output_string (sc);
  s7_pointer old_out_port= s7_set_current_output_port (sc, out_port);
  if (old_err_port != s7_nil (sc)) out_gc_loc= s7_gc_protect (sc, old_out_port);

  s7_pointer result= s7_eval_c_string (sc, code);

  const char* display_out= s7_get_output_string (sc, out_port);
  if (display_out && *display_out) {
    std::string out_str= display_out;
    if (!out_str.empty () && out_str.back () == '\n') {
      ic_printf ("%s", display_out);
    }
    else {
      // 用以表示换行符由 REPL 添加
      ic_printf ("%s↩\n", display_out);
    }
  }

  const char* errmsg= s7_get_output_string (sc, s7_current_error_port (sc));

  if (errmsg && *errmsg) {
    ic_printf ("[error]%s[/]\n", errmsg); // 美化输出
  }
  if (result) {
    history_values.push_back (result);
    s7_gc_protect (sc, result);
    std::string name   = "$" + std::to_string (history_values.size ());
    s7_pointer  cur_env= s7_curlet (sc);
    s7_define (sc, cur_env, s7_make_symbol (sc, name.c_str ()), result);

    char* result_str= s7_object_to_c_string (sc, result);
    if (result_str) {
      ic_printf ("%s [gray]=[/] %s\n", name.c_str (), result_str);
      free (result_str);
    }
  }

  s7_close_output_port (sc, s7_current_error_port (sc));
  s7_set_current_error_port (sc, old_err_port);

  if (err_gc_loc != -1) s7_gc_unprotect_at (sc, err_gc_loc);
  if (out_gc_loc != -1) s7_gc_unprotect_at (sc, out_gc_loc);

  update_symbol_cache (sc);
}

inline std::string
get_history_path () {
#ifdef TB_CONFIG_OS_WINDOWS
  const char* appdata= getenv ("APPDATA");
  std::string dir    = appdata ? std::string (appdata) + "\\goldfish" : ".";
  tb_directory_create (dir.c_str ());
  std::string path= dir + "\\history";
#else
  const char* xdg_state= getenv ("XDG_STATE_HOME");
  const char* xdg_data = getenv ("XDG_DATA_HOME");
  const char* home     = getenv ("HOME");
  std::string dir;
  if (xdg_data) {
    dir= std::string (xdg_data) + "/goldfish";
  }
  else if (home) {
    dir= std::string (home) + "/.local/share/goldfish";
  }
  else {
    dir= ".";
  }
  // 可选：创建目录
  tb_directory_create (dir.c_str ());
  std::string path= dir + "/history";
#endif
  return path;
}

inline bool
is_symbol_char (const char* s, long len) {
  int c= (unsigned char) *s;
  return isalnum (c) || strchr ("!$%&*/:<=>?^_~+-.", c);
}

inline void
symbol_completer (ic_completion_env_t* cenv, const char* symbol) {
  constexpr size_t MAXLEN   = 79;
  size_t           input_len= strlen (symbol);
  for (const auto& info : cached_symbols) {
    if (strncmp (info.name.c_str (), symbol, input_len) == 0) {
      const char* doc= nullptr;
      std::string short_doc;
      if (!info.doc.empty ()) {
        if (info.doc.length () > MAXLEN) {
          short_doc= info.doc.substr (0, MAXLEN) + "...";
          doc      = short_doc.c_str ();
        }
        else {
          doc= info.doc.c_str ();
        }
      }
      ic_add_completion_ex (cenv, info.name.c_str (), info.name.c_str (), doc);
    }
  }
}

inline void
goldfish_completer (ic_completion_env_t* cenv, const char* input) {
  ic_complete_word (cenv, input, &symbol_completer, is_symbol_char);
}

inline void
goldfish_highlighter (ic_highlight_env_t* henv, const char* input, void* arg) {
  static const char* keywords[]= {"define",
                                  "lambda",
                                  "if",
                                  "else",
                                  "let",
                                  "let*",
                                  "letrec",
                                  "begin",
                                  "quote",
                                  "set!",
                                  "cond",
                                  "case",
                                  "and",
                                  "or",
                                  "do",
                                  "delay",
                                  "quasiquote",
                                  "unquote",
                                  "unquote-splicing",
                                  NULL};
  long               len       = (long) strlen (input);
  for (long i= 0; i < len;) {
    long tlen;
    if ((tlen= ic_match_any_token (input, i, &ic_char_is_idletter, keywords)) > 0) {
      // 关键字
      ic_highlight (henv, i, tlen, "keyword");
      i+= tlen;
    }
    else if ((tlen= ic_is_token (input, i, &is_symbol_char)) > 0) {
      // 已定义符号

      std::string token (input + i, tlen);
      if (std::any_of (cached_symbols.begin (), cached_symbols.end (),
                       [&] (const SymbolInfo& info) { return info.name == token; })) {
        ic_highlight (henv, i, tlen, "symbol");
      }
      else {
        ic_highlight (henv, i, tlen, nullptr);
      }
      i+= tlen;
    }
    else if ((tlen= ic_is_token (input, i, &ic_char_is_digit)) > 0) {
      // 数字
      ic_highlight (henv, i, tlen, "number");
      i+= tlen;
    }
    else if (input[i] == '#' && (input[i + 1] == 't' || input[i + 1] == 'f')) {
      // 布尔值
      ic_highlight (henv, i, 2, "constant");
      i+= 2;
    }
    else if (input[i] == '"') {
      long start= i;
      i++;
      while (i < len && input[i] != '"') {
        if (input[i] == '\\' && i + 1 < len) i++; // 跳过转义
        i++;
      }
      if (i < len) i++; // 包含结尾引号
      ic_highlight (henv, start, i - start, "string");
    }
    else if (input[i] == ';') {
      // 注释
      long start= i;
      while (i < len && input[i] != '\n')
        i++;
      ic_highlight (henv, start, i - start, "comment");
    }
    else {
      // 其它
      ic_highlight (henv, i, 1, nullptr);
      i++;
    }
  }
}

struct MetaCommand {
  const char* name;
  const char* help;
  bool        exact;

  std::function<bool (const char* input, s7_scheme* sc, const char* arg)> handler;
};

inline bool meta_quit (const char*, s7_scheme*, const char*);
inline bool meta_help (const char*, s7_scheme*, const char*);
inline bool meta_import (const char*, s7_scheme*, const char*);
inline bool meta_apropos (const char*, s7_scheme* sc, const char* arg);
inline bool meta_describe (const char*, s7_scheme* sc, const char* arg);

const MetaCommand commands[]= {
    {",quit", "exit REPL", true, meta_quit},
    {",q", "exit REPL", true, meta_quit},
    {",help", "show this help", true, meta_help},
    {",?", "show this help", true, meta_help},
    {",import", "import Scheme module", false, meta_import},
    {",apropos", "search symbols by substring", false, meta_apropos},
    {",a", "search symbols by substring", false, meta_apropos},
    {",describe", "describe symbol", false, meta_describe},
    {",d", "describe symbol", false, meta_describe},
};
const size_t commands_count= sizeof (commands) / sizeof (commands[0]);

inline bool
meta_quit (const char*, s7_scheme*, const char*) {
  return true;
}

// TODO: ,help <command>
inline bool
meta_help (const char*, s7_scheme*, const char*) {
  ic_printf ("[b]Meta commands:[/]\n");
  for (const auto& cmd : commands) {
    ic_printf ("[b]%-16s[/] %s\n", cmd.name, cmd.help);
  }
  return false;
}

inline bool
meta_import (const char*, s7_scheme* sc, const char* arg) {
  if (!arg || *arg == 0) {
    ic_printf ("[red]Usage:[/] ,import <module>\n");
    return false;
  }
  std::string mod = arg;
  std::string code= "(import " + mod + ")";

  ic_goldfish_eval (sc, code.c_str ());

  return false;
}

inline bool
meta_apropos (const char*, s7_scheme*, const char* arg) {
  if (!arg || !*arg) {
    ic_printf ("[b]Usage:[/] ,apropos <substring>\n");
    return false;
  }
  int found= false;
  for (const auto& info : cached_symbols) {
    if (strstr (info.name.c_str (), arg)) {
      ic_printf ("[b cyan]%s[/] [dim](procedure)[/] %s\n", info.name.c_str (),
                 info.doc.empty () ? "" : info.doc.c_str ());
      found= true;
    }
  }
  if (!found) ic_printf ("[dim]No symbol matches '%s'[/]\n", arg);
  return false;
}

inline bool
meta_describe (const char*, s7_scheme* sc, const char* arg) {
  if (!arg || !*arg) {
    ic_printf ("[b]Usage:[/] ,describe <symbol>\n");
    return false;
  }
  // 查找符号
  s7_pointer sym= s7_make_symbol (sc, arg);

  // 检查是否已定义
  if (!s7_is_defined (sc, s7_symbol_name (sym))) {
    ic_printf ("[dim]Symbol not defined: %s[/]\n", arg);
    return false;
  }
  s7_pointer  val = s7_symbol_value (sc, sym);
  const char* type= s7_object_to_c_string (sc, s7_type_of (sc, val));
  ic_printf ("[b]%s[/] [dim](%s)[/]\n", arg, type);

  if (s7_is_procedure (val)) {
    // 参数信息
    s7_pointer arity   = s7_arity (sc, val);
    s7_int     min_args= s7_integer (s7_car (arity));
    s7_int     max_args= s7_integer (s7_cdr (arity));

    std::string max_str= (max_args >= 0x20000000) ? "any" : std::to_string (max_args);
    ic_printf ("  [gray]Arity:[/] min [number]%d[/], max [number]%s[/]\n", min_args, max_str.c_str ());

    s7_pointer sig= s7_signature (sc, val);
    if (sig && !s7_is_null (sc, sig)) {
      char* sig_str= s7_object_to_c_string (sc, sig);
      if (sig_str) {
        ic_printf ("  [gray]Signature:[/] %s\n", sig_str);
        free (sig_str);
      }
    }

    // 文档
    const char* doc= s7_documentation (sc, val);
    if (doc && *doc) {
      ic_printf ("  [gray]Doc:[/] %s\n", doc);
    }
  }
  else {
    char*       val_str= s7_object_to_c_string (sc, val);
    std::string preview;
    if (val_str) {
      preview= std::string (val_str).substr (0, 80);
      if (strlen (val_str) > 80) preview+= "...";
    }
    else {
      preview= "";
    }
    ic_printf ("  [gray]Value:[/] %s\n", preview.c_str ());
    if (val_str) free (val_str);
  }
  return false;
}

inline bool
handle_meta_command (const char* input, s7_scheme* sc) {
  for (const auto& cmd : commands) {
    size_t len= strlen (cmd.name);
    if (cmd.exact) {
      if (strcmp (input, cmd.name) == 0) return cmd.handler (input, sc, nullptr);
    }
    else {
      if (strncmp (input, cmd.name, len) == 0) {
        // 跳过空格
        const char* arg= input + len + 1;
        while (*arg == ' ')
          ++arg;
        return cmd.handler (input, sc, input + len + 1);
      }
    }
  }
  ic_printf ("[red]Unknown meta command:[/] %s\n", input);
  return false;
}

inline void
goldfish_repl (s7_scheme* sc, const string& mode) {
  setlocale (LC_ALL, "C.UTF-8");
  ic_style_def ("kbd", "gray underline");
  ic_style_def ("ic-prompt", "gold");

  // 自定义样式
  ic_style_def ("error", "red");
  ic_style_def ("symbol", "cyan");

  ic_printf ("[b gold]Goldfish Scheme[/] [b plum]%s[/] by LiiiLabs\n"
             "[i]Based on S7 Scheme %s [dim](%s)[/][/]\n"
             "[b]Mode:[/] [b]%s[/]\n\n",
             GOLDFISH_VERSION, S7_VERSION, S7_DATE, mode.c_str ());
  ic_printf ("- Type ',quit' or ',q' to quit. (or use [kbd]ctrl-d[/]).\n"
             "- Type ',help' for REPL commands help.\n"
             "- Press [kbd]F1[/] for help on editing commands.\n"
             "- Use [kbd]shift-tab[/] for multiline input. (or [kbd]ctrl-enter[/], or [kbd]ctrl-j[/])\n"
             "- Use [kbd]ctrl-r[/] to search the history.\n\n");

  auto history_path= get_history_path ();
  ic_set_history (history_path.c_str (), -1);

  ic_set_default_completer (&goldfish_completer, sc);
  ic_set_default_highlighter (&goldfish_highlighter, nullptr);

  //  prompt_marker, continuation_prompt_marker
  ic_set_prompt_marker ("> ", "... ");
  ic_enable_auto_tab (true);
  // 缓存的符号向量，只需要查表，没有必要延迟
  ic_set_hint_delay (0);

  update_symbol_cache (sc);

  while (true) {
    char* input= ic_readline ("gf");
    if (!input) break;
    if (strlen (input) == 0) {
      free (input);
      continue;
    }
    if (input[0] == ',') {
      bool quit= handle_meta_command (input, sc);
      free (input);
      if (quit) break;
      continue;
    }

    ic_goldfish_eval (sc, input);
  }
}
#endif

int
repl_for_community_edition (s7_scheme* sc, int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  string      gf_boot_path= find_goldfish_boot (gf_lib);
  const char* gf_boot     = gf_boot_path.c_str ();

  // 供 goldfish `g_command-line` procedure 查询
  command_args.assign (argv, argv + argc);

  // params: 如 `--mode r7rs` 或 `-m=r7rs`，使用 operator() 取值
  // flag  : 如 `--mode`      或 `-m`，     使用 operator[] 取存在与否

  const std::vector<std::pair<std::string, std::string>> reg_params_pairs= {
      {"--mode", "-m"}, {"--eval", "-e"}, {"--load", "-l"}};

  // 初始化解析器（使用预定义的向量来注册 params）
  argh::parser cmdl;
  for (const auto& fp : reg_params_pairs) {
    cmdl.add_params ({fp.first.c_str (), fp.second.c_str ()});
  }
  cmdl.parse (argc, argv);

  // 只要存在 help 或 version 参数，忽略其他所有参数，打印相应信息后正常退出
  if (cmdl[{"--help", "-h"}]) {
    display_help ();
    exit (0);
  }
  if (cmdl[{"--version", "-v"}]) {
    display_version ();
    exit (0);
  }

  // --mode / -m: Load the standard library by mode
  std::string mode    = "default";
  auto        mode_arg= cmdl ({"--mode", "-m"});
  auto        eval_arg= cmdl ({"--eval", "-e"});
  auto        load_arg= cmdl ({"--load", "-l"});

  for (const auto& reg_params : reg_params_pairs) {
    // 使用 operator[] 检查是否是 flag，即非 params
    if (cmdl[{reg_params.first.c_str (), reg_params.second.c_str ()}]) {
      std::cerr << "Error: '" << reg_params.first << "' or '" << reg_params.second << "' requires a parameter."
                << std::endl;
      exit (1);
    }
  }

  auto repl_flag=
      cmdl[{"--repl", "-i"}] || (cmdl.get_unaccessed_flags ().empty () && cmdl.get_unaccessed_params ().empty ());

  // 没有注册为 params 的默认都是 flag，因此只需要检查未访问的 flag
  if (!cmdl.get_unaccessed_flags ().empty ()) {
    display_for_invalid_options (cmdl.get_unaccessed_flags ());
    exit (1);
  }

  if (mode_arg) mode= mode_arg.str ();
  customize_goldfish_by_mode (sc, mode, gf_boot);

  // start capture error output
  const char* errmsg  = NULL;
  s7_pointer  old_port= s7_set_current_error_port (sc, s7_open_output_string (sc));
  int         gc_loc  = -1;
  if (old_port != s7_nil (sc)) gc_loc= s7_gc_protect (sc, old_port);

  if (eval_arg) {
    std::string code= eval_arg.str ();
    goldfish_eval_code (sc, code);
  }
  if (load_arg) {
    std::string file= load_arg.str ();
    cout << " * " << file << " evaluated" << endl;
    goldfish_eval_file (sc, file, true);
  }

  // eval only the first file passed as positional argument
  auto& files= cmdl.pos_args ();
  if (files.size () > 1) {
    auto it= files.begin () + 1;
    goldfish_eval_file (sc, *it, true);
    // 如果有指定文件且没有显示传入 --repl, -i 参数，不进入 repl
    if (repl_flag && !cmdl[{"--repl", "-i"}]) repl_flag= false;
  }

  if (repl_flag) {
#ifdef GOLDFISH_WITH_REPL
    errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
    if ((errmsg) && (*errmsg)) ic_printf ("[red]%s[/]\n", errmsg);
    s7_close_output_port (sc, s7_current_error_port (sc));
    s7_set_current_error_port (sc, old_port);
    if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);

    goldfish_repl (sc, mode);
    return 0;
#else
    std::cerr << "Interactive REPL is not available in this build.\n" << std::endl;
    // 如果没有指定 --repl, -i，额外打印 help 消息
    if (!cmdl[{"--repl", "-i"}]) display_help ();
    exit (-1);
#endif
  }

  errmsg= s7_get_output_string (sc, s7_current_error_port (sc));
  if ((errmsg) && (*errmsg)) cout << errmsg;
  s7_close_output_port (sc, s7_current_error_port (sc));
  s7_set_current_error_port (sc, old_port);
  if (gc_loc != -1) s7_gc_unprotect_at (sc, gc_loc);

  if ((errmsg) && (*errmsg)) return -1;
  else return 0;
}

} // namespace goldfish
