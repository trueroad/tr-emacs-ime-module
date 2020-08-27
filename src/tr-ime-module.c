// -*- mode: c; coding: utf-8 -*-

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

#define TR_IME_MODULE_DLL __declspec(dllexport)

#include "config.h"

#include <emacs-module.h>
#include <windows.h>
#include <imm.h>

// MSDN undocumented
#ifndef IMC_GETOPENSTATUS
#define IMC_GETOPENSTATUS 0x0005
#endif
#ifndef IMC_SETOPENSTATUS
#define IMC_SETOPENSTATUS 0x0006
#endif

int TR_IME_MODULE_DLL plugin_is_GPL_compatible;

static emacs_value
w32_tr_ime_setopenstatus
(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  if (nargs != 2)
    {
      OutputDebugString ("w32_tr_ime_setopenstatus: nargs != 2\n");
      return env->intern (env, "nil");
    }

  HWND hwnd = (HWND)(env->extract_integer (env, args[0]));
  BOOL bopen = env->is_not_nil (env, args[1]);

  if (!IsWindow (hwnd))
    {
      OutputDebugString ("w32_tr_ime_setopenstatus: hwnd is not window\n");
      return env->intern (env, "nil");
    }

  HWND hwnd_ime = ImmGetDefaultIMEWnd (hwnd);
  if (!hwnd_ime)
    {
      OutputDebugString
        ("w32_tr_ime_setopenstatus: ImmGetDefaultIMEWnd failed\n");
      return env->intern (env, "nil");
    }

  // PostMessage does not work.
  LRESULT r = SendMessage (hwnd_ime, WM_IME_CONTROL,
                           IMC_SETOPENSTATUS, (LPARAM)bopen);

  if (r)
    {
      OutputDebugString
        ("w32_tr_ime_setopenstatus: IMC_SETOPENSTATUS failed\n");
      return env->intern (env, "nil");
    }

  return env->intern (env, "t");
}

static emacs_value
w32_tr_ime_getopenstatus
(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  if (nargs != 1)
    {
      OutputDebugString ("w32_tr_ime_getopenstatus: nargs != 1\n");
      return env->intern (env, "nil");
    }

  HWND hwnd = (HWND)(env->extract_integer (env, args[0]));

  if (!IsWindow (hwnd))
    {
      OutputDebugString ("w32_tr_ime_getopenstatus: hwnd is not window\n");
      return env->intern (env, "nil");
    }

  HWND hwnd_ime = ImmGetDefaultIMEWnd (hwnd);
  if (!hwnd_ime)
    {
      OutputDebugString
        ("w32_tr_ime_getopenstatus: ImmGetDefaultIMEWnd failed\n");
      return env->intern (env, "nil");
    }

  LRESULT r = SendMessage (hwnd_ime, WM_IME_CONTROL,
                           IMC_GETOPENSTATUS, 0);

  if (r)
    return env->intern (env, "t"); // IME on

  return env->intern (env, "nil"); // IME off
}

static void
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
  emacs_value args[] = {symbol, func};

  env->funcall (env, defalias, sizeof (args) / sizeof (args[0]), args);
}

static void
provide_feature (emacs_env *env, const char *name)
{
  emacs_value provide = env->intern (env, "provide");
  emacs_value feature = env->intern (env, name);
  emacs_value args[] = {feature};

  env->funcall (env, provide, sizeof (args) / sizeof (args[0]), args);
}

int TR_IME_MODULE_DLL
emacs_module_init (struct emacs_runtime *ert)
{
  if (ert->size < sizeof (*ert))
    {
      OutputDebugString ("tr-ime-module: ert->size < sizeof (*ert)\n");
      return 1;
    }

  emacs_env *env = ert->get_environment (ert);
  if (env->size < sizeof (*env))
    {
      OutputDebugString ("tr-ime-module: env->size < sizeof (*env)\n");
      return 2;
    }

  regist_function (env, "w32-tr-ime-setopenstatus",
                   2, 2, w32_tr_ime_setopenstatus,
"Send WM_IME_CONTROL message with IMC_SETOPENSTATUS (MSDN undocumented)\n\n"
"ARG1 is interpreted as HWND and ARG2 is interpreted as BOOL.\n"
"The message is then sent to the default IME window of the HWND.\n"
"If the BOOL is FALSE, IME is turned off, otherwise, IME is turned on.",
                   NULL);
  regist_function (env, "w32-tr-ime-getopenstatus",
                   1, 1, w32_tr_ime_getopenstatus,
"Send WM_IME_CONTROL message with IMC_GETOPENSTATUS (MSDN undocumented)\n\n"
"ARG1 is interpreted as HWND.\n"
"The message is then sent to the default IME window of the HWND.\n"
"If IME is OFF, nil is returned, otherwise other than nil is returned.",
                   NULL);
  provide_feature (env, "tr-ime-module");

  return 0;
}