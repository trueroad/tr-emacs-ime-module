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

#include "lisp-in-cc.hh"

#include <windows.h>

#include "debug-message.hh"
#include "get_msg_hook.hh"
#include "get_msg_proc.hh"
#include "message.hh"

namespace
{
  get_msg_hook gmh_ (&get_msg_proc::proc, GetModuleHandle (nullptr));
};

const char *doc_w32_tr_ime_subclassify_hwnd =
  "Subclassify a frame to controlling IME\n\n"
  "ARG1 is interpreted as HWND of the frame. If ARG2 is nil or omitted,\n"
  "This function subclasses all frames found in the thread to which the\n"
  "HWND belongs. Otherwise, it subclasses only the specified HWND.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-subclassify-hwnd\n"
  "  (string-to-number (frame-parameter (selected-frame) 'window-id)) nil)";

emacs_value
Fw32_tr_ime_subclassify_hwnd (emacs_env* env, ptrdiff_t nargs,
                              emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs < 1 || nargs > 2)
    {
      WARNING_MESSAGE ("invalid nargs\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  bool b_all = !env->is_not_nil (env, args[1]);

  if (!gmh_.install (GetWindowThreadProcessId (hwnd, nullptr)))
    {
      WARNING_MESSAGE ("hook install failed\n");
      return env->intern (env, "nil");
    }

  PostMessageW (hwnd, u_WM_TR_IME_SUBCLASSIFY_,
                static_cast<WPARAM> (b_all), 0);

  return env->intern (env, "t");
}
