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

#include <algorithm>
#include <unordered_set>

#include <windows.h>
#include <commctrl.h>

#include "debug-message.hh"
#include "message.hh"
#include "subclass_proc.hh"

constexpr WCHAR get_msg_proc::target_class_name_[];
thread_local bool get_msg_proc::bsubclassify_all_ {false};
thread_local std::unordered_set<HWND> get_msg_proc::hwnds_;
thread_local std::unordered_set<HWND> get_msg_proc::exclude_hwnds_;

LRESULT
get_msg_proc::wm_tr_ime_subclassify (int code, WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("enter\n");

  auto msg = reinterpret_cast<MSG*> (lparam);
  bsubclassify_all_ = static_cast<bool> (msg->wParam);

  if (!find_hwnd (msg->hwnd))
    {
      if (SetWindowSubclass (msg->hwnd, &subclass_proc::proc,
                             subclass_proc::get_subclass_id (), 0))
        {
          DEBUG_MESSAGE_STATIC ("  SetWindowSubclass succeeded");
          add_hwnd (msg->hwnd);
        }
      else
        {
          auto e = GetLastError ();
          WARNING_MESSAGE ("SetWindowSubclass failed: " +
                           get_format_message (e) + "\n");
        }
    }
  else
    {
      DEBUG_MESSAGE_STATIC ("  already subclassified\n");
    }

  return CallNextHookEx (nullptr, code, wparam, lparam);
}

bool
get_msg_proc::is_target_class (HWND hwnd)
{
  DEBUG_MESSAGE ("enter\n");

  if (GetParent (hwnd))
    {
      DEBUG_MESSAGE_STATIC ("  false (hwnd has parent)\n");
      add_exclude_hwnd (hwnd);

      return false;
    }

  constexpr auto class_size =
    sizeof (target_class_name_) / sizeof (target_class_name_[0]);
  std::array<WCHAR, class_size + 2> buff;
  auto n = GetClassNameW (hwnd, buff.data (), buff.size ());

  if (!std::equal (std::begin (target_class_name_),
                   std::begin (target_class_name_) + class_size,
                   buff.begin (),
                   buff.begin () + n + 1))
    {
      DEBUG_MESSAGE_STATIC ("  false (class is different)\n");
      add_exclude_hwnd (hwnd);

      return false;
    }

  DEBUG_MESSAGE_STATIC ("  true\n");

  return true;
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

  if (bsubclassify_all_ &&
      !find_exclude_hwnd (msg->hwnd) && !find_hwnd (msg->hwnd) &&
      is_target_class (msg->hwnd))
    {
      PostMessage (msg->hwnd, u_WM_TR_IME_SUBCLASSIFY_,
                   static_cast<WPARAM> (true), 0);
    }

  return CallNextHookEx (nullptr, code, wparam, lparam);
}
