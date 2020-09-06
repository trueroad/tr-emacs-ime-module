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

#ifndef INCLUDE_GUARD_GET_MSG_PROC_HH
#define INCLUDE_GUARD_GET_MSG_PROC_HH

#include <atomic>
#include <unordered_set>

#include <windows.h>

class get_msg_proc final
{
public:
  static LRESULT CALLBACK proc (int, WPARAM, LPARAM);
  static void destroy (HWND hwnd)
  {
    hwnds_.erase (hwnd);
    exclude_hwnds_.erase (hwnd);
  }
  static void set_b_dispatch_thread_messages (bool bset)
  {
    ab_dispatch_thread_messages_.store (bset);
  }

  explicit get_msg_proc () = delete;
  ~get_msg_proc () = delete;
  get_msg_proc (const get_msg_proc &) = delete;
  get_msg_proc (get_msg_proc &&) = delete;
  get_msg_proc& operator = (const get_msg_proc &) = delete;
  get_msg_proc& operator = (get_msg_proc &&) = delete;

private:
  static bool find_hwnd (HWND hwnd)
  {
    return (hwnds_.find (hwnd) != hwnds_.end ());
  }
  static void add_hwnd (HWND hwnd)
  {
    hwnds_.insert (hwnd);
  }
  static bool find_exclude_hwnd (HWND hwnd)
  {
    return (exclude_hwnds_.find (hwnd) != exclude_hwnds_.end ());
  }
  static void add_exclude_hwnd (HWND hwnd)
  {
    exclude_hwnds_.insert (hwnd);
  }
  static bool get_b_dispatch_thread_messages (void)
  {
    return ab_dispatch_thread_messages_.load ();
  }

  static LRESULT wm_tr_ime_subclassify (int, WPARAM, LPARAM);
  static bool is_target_class (HWND);

  static constexpr WCHAR target_class_name_[] {L"Emacs"};

  static thread_local bool bsubclassify_all_;
  static thread_local std::unordered_set<HWND> hwnds_;
  static thread_local std::unordered_set<HWND> exclude_hwnds_;

  static std::atomic<bool> ab_dispatch_thread_messages_;
};

#endif // INCLUDE_GUARD_GET_MSG_PROC_HH
