// -*- mode: c++; coding: utf-8 -*-

// This file is part of
// Emulator of GNU Emacs IME patch for Windows (tr-ime)
// https://github.com/trueroad/tr-emacs-ime-module
//
// Copyright (C) 2020, 2022 Masamichi Hosoda
//
// Emulator of GNU Emacs IME patch for Windows (tr-ime)
// is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Emulator of GNU Emacs IME patch for Windows (tr-ime)
// is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with tr-ime.
// If not, see <https://www.gnu.org/licenses/>.

#define TR_IME_MODADV_DLL __declspec(dllexport)

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <array>
#include <string>

#include <windows.h>

#include <emacs-module.h>

#include "debug-message.hh"
#include "lisp-in-cc.hh"

int TR_IME_MODADV_DLL plugin_is_GPL_compatible;

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
                                            void *) noexcept,
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
  regist_variable (emacs_env *env,
                   const char *name,
                   emacs_value value,
                   const std::string &documentation)
  {
    std::array<emacs_value, 4> args_form
      {
        env->intern (env, "defvar"),
        env->intern (env, name),
        value,
        env->make_string (env, documentation.data (), documentation.size ())
      };
    emacs_value form = env->funcall (env, env->intern (env, "list"),
                                     args_form.size (), args_form.data ());

    std::array<emacs_value, 2> args_eval {form, env->intern (env, "t")};
    env->funcall (env, env->intern (env, "eval"),
                  args_eval.size (), args_eval.data ());
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

int TR_IME_MODADV_DLL
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

  regist_function (env, "tr-ime-modadv--install-message-hook-hwnd",
                   1, 1, Ftr_ime_modadv__install_message_hook_hwnd,
                   doc_tr_ime_modadv__install_message_hook_hwnd,
                   nullptr);
  regist_function (env, "tr-ime-modadv--uninstall-message-hook-hwnd",
                   1, 1, Ftr_ime_modadv__uninstall_message_hook_hwnd,
                   doc_tr_ime_modadv__uninstall_message_hook_hwnd,
                   nullptr);
  regist_function (env, "tr-ime-modadv--subclassify-hwnd",
                   1, 2, Ftr_ime_modadv__subclassify_hwnd,
                   doc_tr_ime_modadv__subclassify_hwnd,
                   nullptr);
  regist_function (env, "tr-ime-modadv--unsubclassify-hwnd",
                   0, 2, Ftr_ime_modadv__unsubclassify_hwnd,
                   doc_tr_ime_modadv__unsubclassify_hwnd,
                   nullptr);
  regist_function (env, "tr-ime-modadv--exists-subclassified",
                   0, 0, Ftr_ime_modadv__exists_subclassified,
                   doc_tr_ime_modadv__exists_subclassified,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-dispatch-thread-message",
                   1, 1, Ftr_ime_modadv__set_dispatch_thread_message,
                   doc_tr_ime_modadv__set_dispatch_thread_message,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-dispatch-thread-wm-timer",
                   1, 1, Ftr_ime_modadv__set_dispatch_thread_wm_timer,
                   doc_tr_ime_modadv__set_dispatch_thread_wm_timer,
                   nullptr);
  regist_function (env, "tr-ime-modadv--setopenstatus",
                   2, 2, Ftr_ime_modadv__setopenstatus,
                   doc_tr_ime_modadv__setopenstatus,
                   nullptr);
  regist_function (env, "tr-ime-modadv--getopenstatus",
                   1, 1, Ftr_ime_modadv__getopenstatus,
                   doc_tr_ime_modadv__getopenstatus,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-font",
                   15, 15, Ftr_ime_modadv__set_font,
                   doc_tr_ime_modadv__set_font,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-composition-window",
                   8, 8, Ftr_ime_modadv__set_composition_window,
                   doc_tr_ime_modadv__set_composition_window,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-startcomposition-defsubclassproc",
                   2, 2, Ftr_ime_modadv__set_startcomposition_defsubclassproc,
                   doc_tr_ime_modadv__set_startcomposition_defsubclassproc,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-prefix-keys",
                   2, 2, Ftr_ime_modadv__set_prefix_keys,
                   doc_tr_ime_modadv__set_prefix_keys,
                   nullptr);
  regist_function (env, "tr-ime-modadv--resume-prefix-key",
                   0, 0, Ftr_ime_modadv__resume_prefix_key,
                   doc_tr_ime_modadv__resume_prefix_key,
                   nullptr);
  regist_variable (env, "tr-ime-modadv--setopenstatus-hook",
                   env->intern (env, "nil"),
                   doc_tr_ime_modadv__setopenstatus_hook);
  regist_variable (env, "tr-ime-modadv--reconvertstring-hook",
                   env->intern (env, "nil"),
                   doc_tr_ime_modadv__reconvertstring_hook);
  regist_variable (env, "tr-ime-modadv--documentfeed-hook",
                   env->intern (env, "nil"),
                   doc_tr_ime_modadv__documentfeed_hook);
  regist_function (env, "tr-ime-modadv--language-change-handler",
                   0, 0, Ftr_ime_modadv__language_change_handler,
                   doc_tr_ime_modadv__language_change_handler,
                   nullptr);
  regist_function (env, "tr-ime-modadv--notify-reconvert-string",
                   3, 3, Ftr_ime_modadv__notify_reconvert_string,
                   doc_tr_ime_modadv__notify_reconvert_string,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-reconversion",
                   2, 2, Ftr_ime_modadv__set_reconversion,
                   doc_tr_ime_modadv__set_reconversion,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-documentfeed",
                   2, 2, Ftr_ime_modadv__set_documentfeed,
                   doc_tr_ime_modadv__set_documentfeed,
                   nullptr);
  regist_function (env, "tr-ime-modadv--get-dpi",
                   0, 0, Ftr_ime_modadv__get_dpi,
                   doc_tr_ime_modadv__get_dpi,
                   nullptr);
  regist_function (env, "tr-ime-modadv--set-verbose-level",
                   1, 1, Ftr_ime_modadv__set_verbose_level,
                   doc_tr_ime_modadv__set_verbose_level,
                   nullptr);
  regist_function (env, "tr-ime-modadv-unload-function",
                   0, 0, Ftr_ime_modadv_unload_function,
                   doc_tr_ime_modadv_unload_function,
                   nullptr);

#ifndef NDEBUG

  regist_function (env, "tr-ime-modadv--debug-output",
                   1, 1, Ftr_ime_modadv__debug_output,
                   doc_tr_ime_modadv__debug_output,
                   nullptr);
  regist_function (env, "tr-ime-modadv--debug-rectangle",
                   5, 5, Ftr_ime_modadv__debug_rectangle,
                   doc_tr_ime_modadv__debug_rectangle,
                   nullptr);

#endif // NDEBUG

  provide_feature (env, "tr-ime-modadv");

  return 0;
}

#ifndef NDEBUG
extern "C"
BOOL WINAPI DllMain (HINSTANCE, DWORD dwReason, LPVOID)
{
  switch (dwReason)
    {
    case DLL_PROCESS_ATTACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_PROCESS_ATTACH\n");
      break;

    case DLL_THREAD_ATTACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_THREAD_ATTACH\n");
      break;

    case DLL_PROCESS_DETACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_PROCESS_DETACH\n");
      break;

    case DLL_THREAD_DETACH:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: DLL_THREAD_DETACH\n");
      break;

    default:
      DEBUG_MESSAGE_STATIC ("debug: DllMain: dwReason is unknown\n");
      break;
    }

  return TRUE;
}
#endif // NDEBUG
