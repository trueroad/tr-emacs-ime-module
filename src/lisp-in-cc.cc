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

#include <string>

#include <windows.h>

#include "debug-message.hh"
#include "get_msg_hook.hh"
#include "get_msg_proc.hh"
#include "message.hh"

namespace
{
  get_msg_hook gmh_ (&get_msg_proc::proc, GetModuleHandle (nullptr));

  std::string
  to_string (emacs_env* env, const emacs_value &str)
  {
    ptrdiff_t len;
    if (!env->copy_string_contents (env, str, nullptr, &len))
    {
      WARNING_MESSAGE ("env->copy_string_contents nullptr error\n");
      return "";
    }

    if (len <= 0)
      {
        WARNING_MESSAGE ("env->copy_string_contents len error\n");
        return "";
      }

    std::string buff (len, '\0');
    if (!env->copy_string_contents (env, str, &buff[0], &len))
    {
      WARNING_MESSAGE ("env->copy_string_contents buff error\n");
      return "";
    }

    if (len > 0)
      buff.resize (len - 1);

    return buff;
  }
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

const char *doc_w32_tr_ime_set_dispatch_thread_message =
  "Set whether or not to dispatch thread messages\n\n"
  "If ARG1 is non-nil, a message hook dispatches the thread messages.\n"
  "Otherwise, it does not.";

emacs_value
Fw32_tr_ime_set_dispatch_thread_message (emacs_env* env, ptrdiff_t nargs,
                                         emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  get_msg_proc::set_b_dispatch_thread_messages
    (env->is_not_nil (env, args[0]));

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_set_font =
  "Set an IME font expressed in the LOGFONTW items to a frame\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2 is interpreted as lfHeight of the LOGFONTW.\n"
  "ARG3 is interpreted as lfWidth of the LOGFONTW.\n"
  "ARG4 is interpreted as lfEscapement of the LOGFONTW.\n"
  "ARG5 is interpreted as lfOrientation of the LOGFONTW.\n"
  "ARG6 is interpreted as lfWeight of the LOGFONTW.\n"
  "ARG7 is interpreted as lfItalic of the LOGFONTW.\n"
  "ARG8 is interpreted as lfUnderline of the LOGFONTW.\n"
  "ARG9 is interpreted as lfStrikeOut of the LOGFONTW.\n"
  "ARG10 is interpreted as lfCharSet of the LOGFONTW.\n"
  "ARG11 is interpreted as lfOutPrecision of the LOGFONTW.\n"
  "ARG12 is interpreted as lfClipPrecision of the LOGFONTW.\n"
  "ARG13 is interpreted as lfQuality of the LOGFONTW.\n"
  "ARG14 is interpreted as lfPitchAndFamily of the LOGFONTW.\n"
  "ARG15 is interpreted as lfFaceName of the LOGFONTW.\n\n"
  "ARG15 is required to be a string and is internally converted to UTF-16.\n"
  "ARG7, ARG8, and ARG9 are required to be nil or non-nil. nil means FALSE.\n"
  "Otherwise means TRUE. Others are required to be integer and are\n"
  "internally converted to the appropriate type. This setting is also\n"
  "applied to other frames in the same thread as the specified frame.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-set-font\n"
  "  (string-to-number (frame-parameter (selected-frame) 'window-id))\n"
  "  50 0 0 0 0 nil nil nil 0 0 0 0 0 \"Harano Aji Mincho Heavy\")";

emacs_value
Fw32_tr_ime_set_font (emacs_env* env, ptrdiff_t nargs,
                      emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 15)
    {
      WARNING_MESSAGE ("nargs != 15\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  LOGFONTW logfont {0};

  logfont.lfHeight =
    static_cast<LONG> (env->extract_integer (env, args[1]));
  logfont.lfWidth =
    static_cast<LONG> (env->extract_integer (env, args[2]));
  logfont.lfEscapement =
    static_cast<LONG> (env->extract_integer (env, args[3]));
  logfont.lfOrientation =
    static_cast<LONG> (env->extract_integer (env, args[4]));
  logfont.lfWeight =
    static_cast<LONG> (env->extract_integer (env, args[5]));

  logfont.lfItalic = env->is_not_nil (env, args[6]);
  logfont.lfUnderline = env->is_not_nil (env, args[7]);
  logfont.lfStrikeOut = env->is_not_nil (env, args[8]);

  logfont.lfCharSet =
    static_cast<BYTE> (env->extract_integer (env, args[9]));
  logfont.lfOutPrecision =
    static_cast<BYTE> (env->extract_integer (env, args[10]));
  logfont.lfClipPrecision =
    static_cast<BYTE> (env->extract_integer (env, args[11]));
  logfont.lfQuality =
    static_cast<BYTE> (env->extract_integer (env, args[12]));
  logfont.lfPitchAndFamily =
    static_cast<BYTE> (env->extract_integer (env, args[13]));

  auto buff = to_string (env, args[14]);
  if(!MultiByteToWideChar (CP_UTF8, 0, buff.data (), buff.size (),
                           logfont.lfFaceName,
                           sizeof (logfont.lfFaceName) /
                           sizeof (logfont.lfFaceName[0])))
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("MultiByteToWideChar failed: " +
                       get_format_message (e) + "\n");
      return env->intern (env, "nil");
    }

  SendMessage (hwnd, u_WM_TR_IME_SET_FONT_,
               reinterpret_cast<WPARAM> (&logfont), 0);

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_get_dpi =
  "Get DPI of the desktop\n\n"
  "The return value is a list containing the DPI in the x and y directions.";

emacs_value
Fw32_tr_ime_get_dpi (emacs_env* env, ptrdiff_t nargs,
                     emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  auto hdc = GetDC (nullptr);
  if (!hdc)
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("GetDC failed: " +
                       get_format_message (e) + "\n");
      return env->intern (env, "nil");
    }

  auto x = GetDeviceCaps (hdc, LOGPIXELSX);
  auto y = GetDeviceCaps (hdc, LOGPIXELSY);

  ReleaseDC (nullptr, hdc);

  auto list = env->intern (env, "list");
  auto ex = env->make_integer (env, x);
  auto ey = env->make_integer (env, y);
  std::array<emacs_value, 2> a {ex, ey};

  return env->funcall (env, list, a.size (), a.data ());
}
