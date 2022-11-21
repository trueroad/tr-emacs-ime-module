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

#include "get_msg_proc.hh"

#include <algorithm>
#include <array>
#include <atomic>
#include <future>
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
std::atomic<bool> get_msg_proc::ab_dispatch_thread_messages_ {false};
std::atomic<bool> get_msg_proc::ab_dispatch_thread_wm_timer_ {false};

LRESULT
get_msg_proc::wm_tr_ime_subclassify (int code, WPARAM wparam, LPARAM lparam)
{
  // WM_TR_IME_SUBCLASSIFY does not work with SendMessage ()
  // because only the "post" ed message is passed to GetMsgProc ().

  DEBUG_MESSAGE ("enter\n");

  // This can be called multiple times.
  // So doesn't check whether PM_REMOVE or PM_NOREMOVE.

  auto msg = reinterpret_cast<MSG*> (lparam);
  bsubclassify_all_ = static_cast<bool> (msg->wParam);

  if (!msg->hwnd)
    {
      DEBUG_MESSAGE_STATIC ("  thread message\n");
    }
  else if (!find_hwnd (msg->hwnd))
    {
      if (SetWindowSubclass (msg->hwnd, &subclass_proc::proc,
                             subclass_proc::get_subclass_id (), 0))
        {
          DEBUG_MESSAGE_STATIC ("  SetWindowSubclass succeeded\n");
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

LRESULT
get_msg_proc::wm_tr_ime_unsubclassify (int code, WPARAM wparam, LPARAM lparam)
{
  // WM_TR_IME_UNSUBCLASSIFY does not work with SendMessage ()
  // because only the "post" ed message is passed to GetMsgProc ().

  DEBUG_MESSAGE ("enter\n");

  // This can be called multiple times.
  // So doesn't check whether PM_REMOVE or PM_NOREMOVE.

  auto msg = reinterpret_cast<MSG*> (lparam);
  auto bunsubclassify_all = static_cast<bool> (msg->wParam);
  bsubclassify_all_ = false;

  if (!msg->hwnd)
    {
      DEBUG_MESSAGE_STATIC ("  thread message\n");
    }
  else if (find_hwnd (msg->hwnd))
    {
      if (RemoveWindowSubclass (msg->hwnd, &subclass_proc::proc,
                                subclass_proc::get_subclass_id ()))
        {
          DEBUG_MESSAGE_STATIC ("  RemoveWindowSubclass succeeded\n");
          remove_hwnd (msg->hwnd);
        }
      else
        {
          auto e = GetLastError ();
          WARNING_MESSAGE ("RemoveWindowSubclass failed: " +
                           get_format_message (e) + "\n");
        }
    }
  else
    {
      DEBUG_MESSAGE_STATIC ("  already unsubclassified\n");
    }

  if (bunsubclassify_all)
    {
      for (auto h: hwnds_)
        // SendMessage does not work
        PostMessageW (h, u_WM_TR_IME_UNSUBCLASSIFY_,
                      static_cast<WPARAM> (false), 0);
    }

  return CallNextHookEx (nullptr, code, wparam, lparam);
}

LRESULT
get_msg_proc::wm_tr_ime_exists_subclassified
(int code, WPARAM wparam, LPARAM lparam)
{
  // WM_TR_IME_EXISTS_SUBCLASSIFIED does not work with SendMessage ()
  // because only the "post" ed message is passed to GetMsgProc ().

  DEBUG_MESSAGE ("enter\n");

  if (wparam != PM_REMOVE)
    {
      // Not processed because this may be called again.
      DEBUG_MESSAGE_STATIC ("  wparam != PM_REMOVE\n");
      return CallNextHookEx (nullptr, code, wparam, lparam);
    }

  auto msg = reinterpret_cast<MSG*> (lparam);
  auto p = reinterpret_cast<std::promise<bool>*> (msg->wParam);

  p->set_value (!hwnds_.empty ());

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
    {
      DEBUG_MESSAGE ("code < 0\n");
      return CallNextHookEx (nullptr, code, wparam, lparam);
    }
  if (code != HC_ACTION)
    {
      DEBUG_MESSAGE ("code != HC_ACTION\n");
      return CallNextHookEx (nullptr, code, wparam, lparam);
    }

  auto msg = reinterpret_cast<MSG*> (lparam);
  if (!msg)
    {
      WARNING_MESSAGE ("msg broken\n");
      return CallNextHookEx (nullptr, code, wparam, lparam);
    }

  if (msg->message == u_WM_TR_IME_SUBCLASSIFY_)
    return wm_tr_ime_subclassify (code, wparam, lparam);
  else if (msg->message == u_WM_TR_IME_UNSUBCLASSIFY_)
    return wm_tr_ime_unsubclassify (code, wparam, lparam);
  else if (msg->message == u_WM_TR_IME_EXISTS_SUBCLASSIFIED_)
    return wm_tr_ime_exists_subclassified (code, wparam, lparam);

  if (!msg->hwnd)
    {
      if (wparam != PM_REMOVE)
        return CallNextHookEx (nullptr, code, wparam, lparam);

      if (msg->message == WM_TIMER && get_b_dispatch_thread_wm_timer ())
        {
          DispatchMessageW (msg);
          msg->message = WM_NULL;

          return CallNextHookEx (nullptr, code, wparam, lparam);
        }
      else
        {
          auto r = CallNextHookEx (nullptr, code, wparam, lparam);
          if (get_b_dispatch_thread_messages () && msg->message != WM_QUIT)
            DispatchMessageW (msg);

          return r;
        }
    }

  if (bsubclassify_all_ &&
      !find_exclude_hwnd (msg->hwnd) && !find_hwnd (msg->hwnd) &&
      is_target_class (msg->hwnd))
    {
      // SendMessage does not work
      PostMessageW (msg->hwnd, u_WM_TR_IME_SUBCLASSIFY_,
                    static_cast<WPARAM> (true), 0);
    }

  return CallNextHookEx (nullptr, code, wparam, lparam);
}
