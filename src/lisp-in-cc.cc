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

#include "lisp-in-cc.hh"

#include <array>
#include <string>
#include <thread>

#include <windows.h>

#include <emacs-module.h>

#include "debug-message.hh"
#include "get_msg_hook.hh"
#include "get_msg_proc.hh"
#include "message.hh"
#include "queue.hh"
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

const char *doc_tr_ime_modadv__install_message_hook_hwnd =
  "Install a message hook into a frame for subclassify.\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "Note: The message hook is installed to the thread to which the frame\n"
  "belongs, not the frame.  Since most frames belong to a single thread,\n"
  "using this function only once will affect the most frames.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--install-message-hook-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)))";

emacs_value
Ftr_ime_modadv__install_message_hook_hwnd (emacs_env* env, ptrdiff_t nargs,
                                           emacs_value args[], void*) noexcept
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

const char *doc_tr_ime_modadv__uninstall_message_hook_hwnd =
  "Uninstall a message hook into a frame for subclassify.\n\n"
  "ARG1 is interpreted as HWND of the frame.\n\n"
  "Note: The message hook is installed to the thread to which the frame\n"
  "belongs, not the frame.  Since most frames belong to a single thread,\n"
  "using this function only once will uninstall the message hook for\n"
  "the most frames.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--uninstall-message-hook-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)))";

emacs_value
Ftr_ime_modadv__uninstall_message_hook_hwnd (emacs_env* env, ptrdiff_t nargs,
                                             emacs_value args[], void*)
  noexcept
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

const char *doc_tr_ime_modadv__subclassify_hwnd =
  "Subclassify a frame to controlling IME.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG2 is nil or omitted,\n"
  "this function subclasses all frames found in the thread to which the\n"
  "HWND belongs.  Otherwise, it subclasses only the specified HWND.\n"
  "Note: To subclassify, a message hook by\n"
  "w32-tr-ime-install-message-hook-hwnd is required to be installed in\n"
  "the thread to which the HWND belongs.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--subclassify-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)) nil)";

emacs_value
Ftr_ime_modadv__subclassify_hwnd (emacs_env* env, ptrdiff_t nargs,
                                  emacs_value args[], void*) noexcept
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

  gmh_.subclassify (hwnd, b_all);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__unsubclassify_hwnd =
  "Unsubclassify a frame to release controlling IME.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG1 is nil or omitted,\n"
  "this function unsbuclasses all frames in the all threads.  If ARG2 is\n"
  "nil or omitted, this function unsubclasses all frames in the thread to\n"
  "which the HWND belongs.  Otherwise, it unsubclasses only the specified\n"
  "HWND.\n"
  "Note: To unsubclassify, a message hook by\n"
  "tr-ime-modadv--install-message-hook-hwnd is required to be installed in\n"
  "the thread to which the HWND belongs.\n"
  "Note: After using this function, tr-ime-modadv--subclassify-hwnd's\n"
  "specification to subclassify all frames is disabled and does not\n"
  "subclassify frames found.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--unsubclassify-hwnd\n"
  " (string-to-number (frame-parameter nil 'window-id)) nil)";

emacs_value
Ftr_ime_modadv__unsubclassify_hwnd (emacs_env* env, ptrdiff_t nargs,
                                    emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs < 1 || nargs > 2)
    {
      WARNING_MESSAGE ("invalid nargs\n");
      return env->intern (env, "nil");
    }

  HWND hwnd = nullptr;
  if (env->is_not_nil (env, args[0]))
    {
      hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
      if (!IsWindow (hwnd))
        {
          WARNING_MESSAGE ("ARG1 is not HWND\n");
          return env->intern (env, "nil");
        }

      bool b_all = !env->is_not_nil (env, args[1]);

      gmh_.unsubclassify (hwnd, b_all);
    }
  else
    gmh_.unsubclassify_all ();

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__exists_subclassified =
  "Return whether or not exists subclassified frames.\n\n"
  "Returns non-nil if there is a subclassified frame.\n"
  "Otherwise, returns nil.";

emacs_value
Ftr_ime_modadv__exists_subclassified (emacs_env* env, ptrdiff_t nargs,
                                      emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  if (gmh_.exists_subclassified_all ())
    return env->intern (env, "t");
  return env->intern (env, "nil");
}

const char *doc_tr_ime_modadv__set_dispatch_thread_message =
  "Set whether or not to dispatch thread messages.\n\n"
  "If ARG1 is non-nil, a message hook dispatches the thread messages.\n"
  "Otherwise, it does not.";

emacs_value
Ftr_ime_modadv__set_dispatch_thread_message (emacs_env* env, ptrdiff_t nargs,
                                             emacs_value args[], void*)
  noexcept
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

const char *doc_tr_ime_modadv__set_dispatch_thread_wm_timer =
  "Set whether or not to dispatch thread WM_TIMER messages.\n\n"
  "If ARG1 is non-nil, a message hook dispatches the WM_TIMER messages\n"
  "and removes them. Otherwise, it does not.";

emacs_value
Ftr_ime_modadv__set_dispatch_thread_wm_timer (emacs_env* env, ptrdiff_t nargs,
                                              emacs_value args[], void*)
  noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  get_msg_proc::set_b_dispatch_thread_wm_timer
    (env->is_not_nil (env, args[0]));

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__setopenstatus =
  "Set IME open status to a frame.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG2 is non-nil, turn on\n"
  "IME.  Otherwise, turn off IME.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--setopenstatus\n"
  " (string-to-number (frame-parameter nil 'window-id)) t)\n";

emacs_value
Ftr_ime_modadv__setopenstatus (emacs_env* env, ptrdiff_t nargs,
                               emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 2)
    {
      WARNING_MESSAGE ("nargs != 2\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  auto r = SendMessageTimeoutW (hwnd, u_WM_TR_IME_SET_OPEN_STATUS_,
                                env->is_not_nil (env, args[1]), 0,
                                SMTO_NORMAL, 1000, nullptr);
  if (r == 0 && GetLastError () == ERROR_TIMEOUT)
    {
      DEBUG_MESSAGE_STATIC ("  SendMessageTimeout: time out\n");
      return env->intern (env, "nil");
    }

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__getopenstatus =
  "Get IME open status from a frame.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If IME is on, it returns\n"
  "non-nil.  Otherwise, it returns nil.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--getopenstatus\n"
  " (string-to-number (frame-parameter nil 'window-id)))\n";

emacs_value
Ftr_ime_modadv__getopenstatus (emacs_env* env, ptrdiff_t nargs,
                               emacs_value args[], void*) noexcept
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

  DWORD_PTR result = 0;
  auto r = SendMessageTimeoutW (hwnd, u_WM_TR_IME_GET_OPEN_STATUS_, 0, 0,
                                SMTO_NORMAL, 1000, &result);
  if (r == 0 && GetLastError () == ERROR_TIMEOUT)
    {
      DEBUG_MESSAGE_STATIC ("  SendMessageTimeout: time out\n");
      return env->intern (env, "nil");
    }

  if (result)
    return env->intern (env, "t");

  return env->intern (env, "nil");
}

const char *doc_tr_ime_modadv__set_font =
  "Set an IME font expressed in the LOGFONTW items to a frame.\n\n"
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
  "ARG7, ARG8, and ARG9 are required to be nil or non-nil.  nil means FALSE.\n"
  "Otherwise means TRUE.  Others are required to be integer and are\n"
  "internally converted to the appropriate type.  This setting is also\n"
  "applied to other frames in the same thread as the specified frame.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-font\n"
  " (string-to-number (frame-parameter nil 'window-id))\n"
  " 50 0 0 0 0 nil nil nil 0 0 0 0 0 \"Harano Aji Mincho Heavy\")";

emacs_value
Ftr_ime_modadv__set_font (emacs_env* env, ptrdiff_t nargs,
                          emacs_value args[], void*) noexcept
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

  SendMessageW (hwnd, u_WM_TR_IME_SET_FONT_,
               reinterpret_cast<WPARAM> (&logfont), 0);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__set_composition_window =
  "Set a composition window expressed in the COMPOSITIONFORM items to a frame."
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
  "appropriate type.  If ARG2 is 0, this module does not set the composition"
  "window, and Emacs' settings are enabled.  Otherwise, the module sets the"
  "composition window and overrides the Emacs' settings.  This setting is"
  "also applied to other frames in the same thread as the specified frame."
  "\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-composition-window\n"
  " (string-to-number (frame-parameter nil 'window-id))\n"
  " 1 0 0 0 0 640 32)";

emacs_value
Ftr_ime_modadv__set_composition_window (emacs_env* env, ptrdiff_t nargs,
                                        emacs_value args[], void*) noexcept
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

  SendMessageW (hwnd, u_WM_TR_IME_SET_COMPOSITIONWINDOW_,
                reinterpret_cast<WPARAM> (&compform), 0);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__set_startcomposition_defsubclassproc =
  "Set whether or not the WM_IME_STARTCOMPOSITION always call DefSubclassProc."
  "\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG2 is nil or omitted,\n"
  "WM_IME_STARTCOMPOSITION does not call DefSubclassProc when the \n"
  "composition window is set.  Otherwise, it always calls DefSubclassProc."
  "\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-startcomposition-defsubclassproc\n"
  " (string-to-number (frame-parameter nil 'window-id)) nil)";

emacs_value
Ftr_ime_modadv__set_startcomposition_defsubclassproc (emacs_env* env,
                                                      ptrdiff_t nargs,
                                                      emacs_value args[],
                                                      void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 2)
    {
      WARNING_MESSAGE ("nargs != 2\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  bool b_set = env->is_not_nil (env, args[1]);
  subclass_proc::lisp_set_startcomposition_defsubclassproc (b_set);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__set_prefix_keys =
  "Set prefix keys to turn off IME.\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2 is interpreted as a list of prefix key codes to be detected.\n"
  "The upper 16 bits of the code are specified the modified key and the\n"
  "lower 16 bits are specified the virtual key code to be modified.  The\n"
  "modifier keys are #x10000 for the shift key, #x20000 for the control key,\n"
  " and #x40000 for the ALT key, respectively, and specify by the bitwise OR\n"
  "of them.  The virtual key code is specified Windows' code.  For example,\n"
  "if you want to specify C-x, specify #x20058 because it is the bitwise OR\n"
  "of #x20000, the modifier of the control key, and #x58, the virtual key\n"
  "code of X key.  If you want to specify C-M-x, specify #x60058, including\n"
  "the ALT key modifier.  This setting is also applied to other frames in\n"
  "the same thread as the specified frame.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-prefix-keys\n"
  " (string-to-number (frame-parameter nil 'window-id))\n"
  " '(#x20058 #x20048 #x20043 #x1b)";

emacs_value
Ftr_ime_modadv__set_prefix_keys (emacs_env* env, ptrdiff_t nargs,
                                 emacs_value args[], void*) noexcept
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

  SendMessageW (hwnd, u_WM_TR_IME_SET_PREFIX_KEYS_,
                reinterpret_cast<WPARAM> (&prefix_keys), 0);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__resume_prefix_key =
  "Resume IME mode if a prefix key was pressed before.\n\n"
  "If IME was automatically turned off previously by the prefix key,\n"
  "IME is resumed to on.  This function is for adding to pre-command-hook.";

emacs_value
Ftr_ime_modadv__resume_prefix_key (emacs_env* env, ptrdiff_t nargs,
                                   emacs_value args[], void*) noexcept
{
  // DEBUG_MESSAGE ("enter\n");

  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  subclass_proc::lisp_resume_prefix_key ();

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__setopenstatus_hook =
  "Hook to be called when IMN_SETOPENSTATUS notification has been received.";

const char *doc_tr_ime_modadv__reconvertstring_hook =
  "Hook to be called when IMR_RECONVERTSTING notification has been received.";

const char *doc_tr_ime_modadv__documentfeed_hook =
  "Hook to be called when IMR_DOCUMENTFEED notification has been received.";

const char *doc_tr_ime_modadv__language_change_handler =
  "WM_INPUTLANGCHANGE special-event-map language-change handler.\n\n"
  "Check the queue from the UI thread.  This function does nothing\n"
  "if it's empty.  This function is called from language-change in the\n"
  "special-event-map.\n\n"
  "Sample usage:\n"
  "(define-key special-event-map [language-change]\n"
  "  (lambda ()\n"
  "    (interactive)\n"
  "    (tr-ime-modadv--language-change-handler)))";

emacs_value
Ftr_ime_modadv__language_change_handler (emacs_env* env, ptrdiff_t nargs,
                                         emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  while (!ui_to_lisp_queue::empty ())
    {
      auto msg = ui_to_lisp_queue::dequeue ();
      if (!msg)
        return env->intern (env, "nil");

      switch (msg->get_message ())
        {
        case queue_message::message::setopenstatus:
          DEBUG_MESSAGE_STATIC ("  setopenstatus\n");
          {
            emacs_value run_hooks = env->intern (env, "run-hooks");
            emacs_value hook_symbol
              = env->intern (env, "tr-ime-modadv--setopenstatus-hook");
            std::array<emacs_value, 1> arg {hook_symbol};

            env->funcall (env, run_hooks, arg.size (), arg.data ());
          }
          break;
        case queue_message::message::reconvertstring:
          DEBUG_MESSAGE_STATIC ("  reconvertstring\n");
          {
            emacs_value run_hooks = env->intern (env, "run-hooks");
            emacs_value hook_symbol
              = env->intern (env, "tr-ime-modadv--reconvertstring-hook");
            std::array<emacs_value, 1> arg {hook_symbol};

            env->funcall (env, run_hooks, arg.size (), arg.data ());
          }
          break;
        case queue_message::message::documentfeed:
          DEBUG_MESSAGE_STATIC ("  documentfeed\n");
          {
            emacs_value run_hooks = env->intern (env, "run-hooks");
            emacs_value hook_symbol
              = env->intern (env, "tr-ime-modadv--documentfeed-hook");
            std::array<emacs_value, 1> arg {hook_symbol};

            env->funcall (env, run_hooks, arg.size (), arg.data ());
          }
          break;
        case queue_message::message::backward_char:
          DEBUG_MESSAGE_STATIC ("  backward_char\n");
          {
            emacs_value backward_char = env->intern (env, "backward-char");
            emacs_value chars = env->make_integer (env, msg->get_parameter ());
            std::array<emacs_value, 1> arg {chars};

            env->funcall (env, backward_char, arg.size (), arg.data ());

            auto r = SendMessageTimeoutW
              (msg->get_hwnd (), u_WM_TR_IME_NOTIFY_BACKWARD_COMPLETE_, 0, 0,
               SMTO_NORMAL, 1000, nullptr);

            if (r == 0 && GetLastError () == ERROR_TIMEOUT)
              {
                DEBUG_MESSAGE_STATIC ("  SendMessageTimeout: time out\n");
              }
          }
          break;
        case queue_message::message::delete_char:
          DEBUG_MESSAGE_STATIC ("  delete_char\n");
          {
            emacs_value delete_char = env->intern (env, "delete-char");
            emacs_value chars = env->make_integer (env, msg->get_parameter ());
            std::array<emacs_value, 1> arg {chars};

            env->funcall (env, delete_char, arg.size (), arg.data ());
          }
          break;
        default:
          WARNING_MESSAGE ("unknown message\n");
          break;
        }
    }

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__notify_reconvert_string =
  "Notify reconvert string to UI thread.\n\n"
  "ARG1 is interpreted as HWND of the frame.  ARG2 is a string of the line\n"
  "where the point exists.  ARG3 is the position of the point counting from\n"
  "the beginning of the string.  If the point is at the beginning of the\n"
  "string, it is zero.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--notify-reconvert-string\n"
  " (string-to-number (frame-parameter nil 'window-id))\n"
  " (buffer-substring-no-properties\n"
  "  (line-beginning-position) (line-end-position))\n"
  " (- (point) (line-beginning-position)))";

emacs_value
Ftr_ime_modadv__notify_reconvert_string (emacs_env* env, ptrdiff_t nargs,
                                         emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 3)
    {
      WARNING_MESSAGE ("nargs != 3\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  auto buff = to_string (env, args[1]);
  auto wbuff = to_wstring (buff);

  auto point =
    static_cast<LPARAM> (env->extract_integer (env, args[2]));

  SendMessage (hwnd, u_WM_TR_IME_NOTIFY_RECONVERT_STRING_,
               reinterpret_cast<WPARAM> (wbuff.c_str ()), point);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__set_reconversion =
  "Set whether to use reconversion or not.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG2 is nil, the module\n"
  "does not perform reconversion.  Otherwise, it performs reconversion.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-reconversion\n"
  " (string-to-number (frame-parameter nil 'window-id)) t)";

emacs_value
Ftr_ime_modadv__set_reconversion (emacs_env* env, ptrdiff_t nargs,
                                  emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 2)
    {
      WARNING_MESSAGE ("nargs != 2\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  bool b_set = env->is_not_nil (env, args[1]);
  subclass_proc::lisp_set_reconversion (b_set);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__set_documentfeed =
  "Set whether to use documentfeed or not.\n\n"
  "ARG1 is interpreted as HWND of the frame.  If ARG2 is nil, the module\n"
  "does not perform documentfeed.  Otherwise, it performs documentfeed.\n\n"
  "Sample usage:\n"
  "(tr-ime-modadv--set-documentfeed\n"
  " (string-to-number (frame-parameter nil 'window-id)) t)";

emacs_value
Ftr_ime_modadv__set_documentfeed (emacs_env* env, ptrdiff_t nargs,
                                  emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 2)
    {
      WARNING_MESSAGE ("nargs != 2\n");
      return env->intern (env, "nil");
    }

  auto hwnd = reinterpret_cast<HWND> (env->extract_integer (env, args[0]));
  if (!IsWindow (hwnd))
    {
      WARNING_MESSAGE ("ARG1 is not HWND\n");
      return env->intern (env, "nil");
    }

  bool b_set = env->is_not_nil (env, args[1]);
  subclass_proc::lisp_set_documentfeed (b_set);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv__get_dpi =
  "Get DPI of the desktop.\n\n"
  "The return value is a cons containing the DPI in the x and y directions.";

emacs_value
Ftr_ime_modadv__get_dpi (emacs_env* env, ptrdiff_t nargs,
                         emacs_value args[], void*) noexcept
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

  auto list = env->intern (env, "cons");
  auto ex = env->make_integer (env, x);
  auto ey = env->make_integer (env, y);
  std::array<emacs_value, 2> a {ex, ey};

  return env->funcall (env, list, a.size (), a.data ());
}

const char *doc_tr_ime_modadv__set_verbose_level =
  "Set verbose level for module's OutputDebugStringA/W () output.\n\n"
  "ARG is verbose level in integer.\n"
  "0: none\n"
  "1: fatal\n"
  "2: error\n"
  "3: warn\n"
  "4: info\n"
  "5: debug\n"
  "6: trace";

emacs_value
Ftr_ime_modadv__set_verbose_level (emacs_env* env, ptrdiff_t nargs,
                                   emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 1)
    {
      WARNING_MESSAGE ("nargs != 1\n");
      return env->intern (env, "nil");
    }

  verbose_level = env->extract_integer (env, args[0]);

  return env->intern (env, "t");
}

const char *doc_tr_ime_modadv_unload_function =
  "Prepare to unload.\n\n"
  "Unsubclassify all frames and uninstall message hook for all threads.";

emacs_value
Ftr_ime_modadv_unload_function (emacs_env* env, ptrdiff_t nargs,
                                emacs_value args[], void*) noexcept
{
  DEBUG_MESSAGE ("enter\n");

  if (nargs != 0)
    {
      WARNING_MESSAGE ("nargs != 0\n");
      return env->intern (env, "nil");
    }

  gmh_.unsubclassify_all ();
  for (int i = 0; gmh_.exists_subclassified_all () && i < 10; ++i)
    std::this_thread::yield ();

  gmh_.uninstall_all ();

  return env->intern (env, "t");
}

#ifndef NDEBUG

const char *doc_tr_ime_modadv__debug_output =
  "Output ARG1 string by OutputDebugStringW ().";

emacs_value
Ftr_ime_modadv__debug_output (emacs_env* env, ptrdiff_t nargs,
                              emacs_value args[], void*) noexcept
{
  // DEBUG_MESSAGE ("enter\n");

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

const char *doc_tr_ime_modadv__debug_rectangle =
  "Draw a rectangle to a frame.\n\n"
  "ARG1 is interpreted as HWND of the frame.\n"
  "ARG2, ARG3, ARG4, and ARG5 are interpreted as left, top, right, and"
  "bottom of the rectangle coordinates.";

emacs_value
Ftr_ime_modadv__debug_rectangle (emacs_env* env, ptrdiff_t nargs,
                                 emacs_value args[], void*) noexcept
{
  // DEBUG_MESSAGE ("enter\n");

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
