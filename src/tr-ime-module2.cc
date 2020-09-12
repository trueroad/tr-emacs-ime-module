// -*- mode: c++; coding: utf-8 -*-

// This file is part of
// Simple IME module for GNU Emacs (tr-emacs-ime-module)
// https://github.com/trueroad/tr-emacs-ime-module
//
// Copyright (C) 2020 Masamichi Hosoda
//
// Simple IME module for GNU Emacs (tr-emacs-ime-module)
// is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Simple IME module for GNU Emacs (tr-emacs-ime-module)
// is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with tr-emacs-ime-module.
// If not, see <https://www.gnu.org/licenses/>.

#define TR_IME_MODULE2_DLL __declspec(dllexport)

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <array>

#include <windows.h>

#include <emacs-module.h>

#include "debug-message.hh"
#include "lisp-in-cc.hh"

int TR_IME_MODULE2_DLL plugin_is_GPL_compatible;

namespace
{
  void
  regist_function (emacs_env *env,
                   const char *name,
                   ptrdiff_t min_arity,
                   ptrdiff_t max_arity,
                   emacs_value (*function) (emacs_env *env,
                                            ptrdiff_t nargs,
                                            emacs_value args[],
                                            void *),
                   const char *documentation,
                   void *data)
  {
    emacs_value defalias = env->intern (env, "defalias");
    emacs_value symbol = env->intern (env, name);
    emacs_value func =
      env->make_function (env, min_arity, max_arity, function,
                          documentation, data);
    std::array<emacs_value, 2> args {symbol, func};

    env->funcall (env, defalias, args.size (), args.data ());
  }

  void
  provide_feature (emacs_env *env, const char *name)
  {
    emacs_value provide = env->intern (env, "provide");
    emacs_value feature = env->intern (env, name);
    std::array<emacs_value, 1> args {feature};

    env->funcall (env, provide, args.size (), args.data ());
  }
};

int TR_IME_MODULE2_DLL
emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  DEBUG_MESSAGE ("enter\n");

  if (ert->size < sizeof (*ert))
    {
      WARNING_MESSAGE ("ert->size < sizeof (*ert)\n");
      return 1;
    }

  emacs_env *env = ert->get_environment (ert);
  if (env->size < sizeof (*env))
    {
      WARNING_MESSAGE ("env->size < sizeof (*env)\n");
      return 2;
    }

  regist_function (env, "w32-tr-ime-install-message-hook-hwnd",
                   1, 1, Fw32_tr_ime_install_message_hook_hwnd,
                   doc_w32_tr_ime_install_message_hook_hwnd,
                   nullptr);
  regist_function (env, "w32-tr-ime-uninstall-message-hook-hwnd",
                   1, 1, Fw32_tr_ime_uninstall_message_hook_hwnd,
                   doc_w32_tr_ime_uninstall_message_hook_hwnd,
                   nullptr);
  regist_function (env, "w32-tr-ime-subclassify-hwnd",
                   1, 2, Fw32_tr_ime_subclassify_hwnd,
                   doc_w32_tr_ime_subclassify_hwnd,
                   nullptr);
  regist_function (env, "w32-tr-ime-unsubclassify-hwnd",
                   1, 2, Fw32_tr_ime_unsubclassify_hwnd,
                   doc_w32_tr_ime_unsubclassify_hwnd,
                   nullptr);
  regist_function (env, "w32-tr-ime-set-dispatch-thread-message",
                   1, 1, Fw32_tr_ime_set_dispatch_thread_message,
                   doc_w32_tr_ime_set_dispatch_thread_message,
                   nullptr);
  regist_function (env, "w32-tr-ime-set-font",
                   15, 15, Fw32_tr_ime_set_font,
                   doc_w32_tr_ime_set_font,
                   nullptr);
  regist_function (env, "w32-tr-ime-set-composition-window",
                   8, 8, Fw32_tr_ime_set_composition_window,
                   doc_w32_tr_ime_set_composition_window,
                   nullptr);
  regist_function (env, "w32-tr-ime-get-dpi",
                   0, 0, Fw32_tr_ime_get_dpi,
                   doc_w32_tr_ime_get_dpi,
                   nullptr);

#ifndef NDEBUG

  regist_function (env, "w32-tr-ime-debug-output",
                   1, 1, Fw32_tr_ime_debug_output,
                   doc_w32_tr_ime_debug_output,
                   nullptr);
  regist_function (env, "w32-tr-ime-debug-rectangle",
                   5, 5, Fw32_tr_ime_debug_rectangle,
                   doc_w32_tr_ime_debug_rectangle,
                   nullptr);

#endif // NDEBUG

  provide_feature (env, "tr-ime-module2");

  return 0;
}

#ifndef NDEBUG
extern "C"
BOOL WINAPI DllMain (HINSTANCE, DWORD dwReason, LPVOID)
{
  switch (dwReason)
    {
    case DLL_PROCESS_ATTACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_PROCESS_ATTACH");
      break;

    case DLL_THREAD_ATTACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_THREAD_ATTACH");
      break;

    case DLL_PROCESS_DETACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_PROCESS_DETACH");
      break;

    case DLL_THREAD_DETACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_THREAD_DETACH");
      break;

    default:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: dwReason is unknown");
      break;
    }

  return TRUE;
}
#endif // NDEBUG
