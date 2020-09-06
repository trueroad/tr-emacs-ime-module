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

#include "get_msg_proc.hh"

#include <windows.h>
#include <commctrl.h>

#include "debug-message.hh"
#include "message.hh"
#include "subclass_proc.hh"

LRESULT
get_msg_proc::wm_tr_ime_subclassify (int code, WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("enter\n");

  auto msg = reinterpret_cast<MSG*> (lparam);

  if (SetWindowSubclass (msg->hwnd, &subclass_proc::proc,
                         subclass_proc::get_subclass_id (), 0))
    {
      DEBUG_MESSAGE ("SetWindowSubclass succeeded");
    }
  else
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("SetWindowSubclass failed: " +
                       get_format_message (e) + "\n");
    }

  return CallNextHookEx (nullptr, code, wparam, lparam);
}

LRESULT
get_msg_proc::proc (int code, WPARAM wparam, LPARAM lparam)
{
  if (code < 0)
    return CallNextHookEx (nullptr, code, wparam, lparam);

  auto msg = reinterpret_cast<MSG*> (lparam);
  if (!msg)
    {
      WARNING_MESSAGE ("msg broken\n");
      return CallNextHookEx (nullptr, code, wparam, lparam);
    }

  if (msg->message == u_WM_TR_IME_SUBCLASSIFY_)
    return wm_tr_ime_subclassify (code, wparam, lparam);

  return CallNextHookEx (nullptr, code, wparam, lparam);
}
