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
#include "subclass_proc.hh"

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

  std::basic_string<WCHAR>
  to_wstring (const std::string &str)
  {
    if (!str.size ())
      return L"";

    auto wsize =
      MultiByteToWideChar (CP_UTF8, 0, str.data (), str.size (), nullptr, 0);
    if (!wsize)
      {
        auto e = GetLastError ();
        WARNING_MESSAGE ("MultiByteToWideChar (zero length) failed: " +
                         get_format_message (e) + "\n");
        return L"";
      }

    if (wsize <= 0)
      {
        WARNING_MESSAGE ("MultiByteToWideChar size error\n");
        return L"";
      }

    std::basic_string<WCHAR> wbuff (wsize, L'\0');
    if(!MultiByteToWideChar (CP_UTF8, 0, str.data (), str.size (),
                             &wbuff[0], wsize))
      {
        auto e = GetLastError ();
        WARNING_MESSAGE ("MultiByteToWideChar (sized) failed: " +
                         get_format_message (e) + "\n");
        return L"";
      }

    return wbuff;
  }
};

const char *doc_w32_tr_ime_install_message_hook_hwnd =
  "Install a message hook into a frame for subclassify\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "Note: The message hook is installed to the thread to which the frame\n"
  "belongs, not the frame. Since most frames belong to a single thread,\n"
  "using this function only once will affect the most frames.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-install-message-hook-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)))";

emacs_value
Fw32_tr_ime_install_message_hook_hwnd (emacs_env* env, ptrdiff_t nargs,
                                       emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  if (!gmh_.install (GetWindowThreadProcessId (hwnd, nullptr)))
    {
      WARNING_MESSAGE ("hook install failed\n");
      return env->intern (env, "nil");
    }

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_uninstall_message_hook_hwnd =
  "Uninstall a message hook into a frame for subclassify\n\n"
  "ARG1 is interpreted as HWND of the frame.\n\n"
  "Note: The message hook is installed to the thread to which the frame\n"
  "belongs, not the frame. Since most frames belong to a single thread,\n"
  "using this function only once will uninstall the message hook for\n"
  "the most frames.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-uninstall-message-hook-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)))";

emacs_value
Fw32_tr_ime_uninstall_message_hook_hwnd (emacs_env* env, ptrdiff_t nargs,
                                         emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  gmh_.uninstall (GetWindowThreadProcessId (hwnd, nullptr));

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_subclassify_hwnd =
  "Subclassify a frame to controlling IME\n\n"
  "ARG1 is interpreted as HWND of the frame. If ARG2 is nil or omitted,\n"
  "this function subclasses all frames found in the thread to which the\n"
  "HWND belongs. Otherwise, it subclasses only the specified HWND.\n"
  "Note: To subclassify, a message hook by\n"
  "w32-tr-ime-install-message-hook-hwnd is required to be installed in\n"
  "the thread to which the HWND belongs.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-subclassify-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)) nil)";

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

  PostMessageW (hwnd, u_WM_TR_IME_SUBCLASSIFY_,
                static_cast<WPARAM> (b_all), 0);

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_unsubclassify_hwnd =
  "Unsubclassify a frame to release controlling IME\n\n"
  "ARG1 is interpreted as HWND of the frame. If ARG2 is nil or omitted,\n"
  "this function unsubclasses all frames in the thread to which the\n"
  "HWND belongs. Otherwise, it unsubclasses only the specified HWND.\n"
  "Note: To unsubclassify, a message hook by\n"
  "w32-tr-ime-install-message-hook-hwnd is required to be installed in\n"
  "the thread to which the HWND belongs.\n"
  "Note: After using this function, w32-tr-ime-subclassify-hwnd's\n"
  "specification to subclassify all frames is disabled and does not\n"
  "subclassify frames found.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-unsubclassify-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)) nil)";

emacs_value
Fw32_tr_ime_unsubclassify_hwnd (emacs_env* env, ptrdiff_t nargs,
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

  PostMessageW (hwnd, u_WM_TR_IME_UNSUBCLASSIFY_,
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
  auto wbuff = to_wstring (buff);
  wbuff.copy (logfont.lfFaceName,
              sizeof (logfont.lfFaceName) / sizeof (logfont.lfFaceName[0]));

  SendMessage (hwnd, u_WM_TR_IME_SET_FONT_,
               reinterpret_cast<WPARAM> (&logfont), 0);

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_set_composition_window =
  "Set a composition window expressed in the COMPOSITIONFORM items to a frame"
  "\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2 is interpreted as dwStyle of the COMPOSITIONFORM.\n"
  "ARG3 is interpreted as ptCurrentPos.x of the COMPOSITIONFORM.\n"
  "ARG4 is interpreted as ptCurrentPos.y of the COMPOSITIONFORM.\n"
  "ARG5 is interpreted as rcArea.left the COMPOSITIONFORM.\n"
  "ARG6 is interpreted as rcArea.top of the COMPOSITIONFORM.\n"
  "ARG7 is interpreted as rcArea.right the COMPOSITIONFORM.\n"
  "ARG8 is interpreted as rcArea.bottom of the COMPOSITIONFORM.\n"
  "ARG2 to 8 are required to be integer and are internally converted to the"
  "appropriate type. If ARG2 is 0, this module does not set the composition"
  "window, and Emacs' settings are enabled. Otherwise, the module sets the"
  "composition window and overrides the Emacs' settings. This setting is"
  "also applied to other frames in the same thread as the specified frame."
  "\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-set-composition-window\n"
  "  (string-to-number (frame-parameter (selected-frame) 'window-id))\n"
  "  1 0 0 0 0 640 32)";

emacs_value
Fw32_tr_ime_set_composition_window (emacs_env* env, ptrdiff_t nargs,
                                    emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 8)
    {
      WARNING_MESSAGE ("nargs != 8\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  COMPOSITIONFORM compform {0};

  compform.dwStyle =
    static_cast<DWORD> (env->extract_integer (env, args[1]));

  compform.ptCurrentPos.x =
    static_cast<LONG> (env->extract_integer (env, args[2]));
  compform.ptCurrentPos.y =
    static_cast<LONG> (env->extract_integer (env, args[3]));

  compform.rcArea.left =
    static_cast<LONG> (env->extract_integer (env, args[4]));
  compform.rcArea.top =
    static_cast<LONG> (env->extract_integer (env, args[5]));
  compform.rcArea.right =
    static_cast<LONG> (env->extract_integer (env, args[6]));
  compform.rcArea.bottom =
    static_cast<LONG> (env->extract_integer (env, args[7]));

  SendMessage (hwnd, u_WM_TR_IME_SET_COMPOSITIONWINDOW_,
               reinterpret_cast<WPARAM> (&compform), 0);

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_set_prefix_keys =
  "Set prefix keys to turn off IME\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2 is interpreted as a list of prefix key codes to be detected.\n"
  "The upper 16 bits of the code are specified the modified key and the\n"
  "lower 16 bits are specified the virtual key code to be modified. The\n"
  "modifier keys are #x10000 for the shift key, #x20000 for the control key,\n"
  " and #x40000 for the ALT key, respectively, and specify by the bitwise OR\n"
  "of them. The virtual key code is specified Windows' code. For example, if\n"
  "you want to specify C-x, specify #x20058 because it is the bitwise OR of\n"
  "#x20000, the modifier of the control key, and #x58, the virtual key code\n"
  "of X key. If you want to specify C-M-x, specify #x60058, including the\n"
  "ALT key modifier. This setting is also applied to other frames in the\n"
  "same thread as the specified frame.\n\n"
  "Sample usage:\n"
  "(w32-tr-ime-set-prefix-keys\n"
  " (string-to-number (frame-parameter nil 'window-id))\n"
  " '(#x20058 #x20048 #x20043 #x1b)";

emacs_value
Fw32_tr_ime_set_prefix_keys (emacs_env* env, ptrdiff_t nargs,
                             emacs_value args[], void*)
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 2)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  std::unordered_set<DWORD> prefix_keys;

  auto car = env->intern (env, "car");
  auto cdr = env->intern (env, "cdr");
  auto remain = args[1];

  while (env->is_not_nil (env, remain))
    {
      std::array<emacs_value, 1> arg {remain};
      auto c = env->funcall (env, car, arg.size (), arg.data ());
      prefix_keys.insert (env->extract_integer (env, c));

      remain = env->funcall (env, cdr, arg.size (), arg.data ());
    }

  SendMessage (hwnd, u_WM_TR_IME_SET_PREFIX_KEYS_,
               reinterpret_cast<WPARAM> (&prefix_keys), 0);

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_resume_prefix_key =
  "Resume IME mode if a prefix key was pressed before\n\n"
  "If IME was automatically turned off previously by the prefix key,\n"
  "IME is resumed to on. This function is for adding to post-command-hook.";

emacs_value
Fw32_tr_ime_resume_prefix_key (emacs_env* env, ptrdiff_t nargs,
                               emacs_value args[], void*)
{
  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  subclass_proc::lisp_resume_prefix_key ();

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

#ifndef NDEBUG

const char *doc_w32_tr_ime_debug_output =
  "Output ARG1 string by OutputDebugStringW ()";

emacs_value
Fw32_tr_ime_debug_output (emacs_env* env, ptrdiff_t nargs,
                          emacs_value args[], void*)
{
  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  auto buff = to_string (env, args[0]);
  auto wbuff = to_wstring (buff);

  OutputDebugStringW (wbuff.c_str ());

  return env->intern (env, "t");
}

const char *doc_w32_tr_ime_debug_rectangle =
  "Draw a rectangle to a frame\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2, ARG3, ARG4, and ARG5 are interpreted as left, top, right, and"
  "bottom of the rectangle coordinates.";

emacs_value
Fw32_tr_ime_debug_rectangle (emacs_env* env, ptrdiff_t nargs,
                             emacs_value args[], void*)
{
  if (nargs != 5)
    {
      WARNING_MESSAGE ("nargs != 5\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  auto left = static_cast<int> (env->extract_integer (env, args[1]));
  auto top = static_cast<int> (env->extract_integer (env, args[2]));
  auto right = static_cast<int> (env->extract_integer (env, args[3]));
  auto bottom = static_cast<int> (env->extract_integer (env, args[4]));

  auto hdc = GetDC (hwnd);
  if (!hdc)
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("GetDC failed: " +
                       get_format_message (e) + "\n");
      return env->intern (env, "nil");
    }

  if (!Rectangle (hdc, left, top, right, bottom))
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("Rectangle failed: " +
                       get_format_message (e) + "\n");
    }

  ReleaseDC (hwnd, hdc);

  return env->intern (env, "t");
}

#endif // NDEBUG
