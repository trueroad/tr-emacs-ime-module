// -*- mode: c++; coding: utf-8 -*-

// This file is part of
// Emulator of GNU Emacs IME patch for Windows (tr-ime)
// https://github.com/trueroad/tr-emacs-ime-module
//
// Copyright (C) 2020 Masamichi Hosoda
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

#ifndef INCLUDE_GUARD_GET_MSG_HOOK_HH
#define INCLUDE_GUARD_GET_MSG_HOOK_HH

#include <mutex>
#include <unordered_map>

#include <windows.h>

class get_msg_hook final
{
public:
  explicit
  get_msg_hook (HOOKPROC get_msg_proc, HMODULE hmodule)
    : get_msg_proc_ (get_msg_proc), hmodule_ (hmodule)
  {
  }
  ~get_msg_hook ()
  {
    uninstall_all ();
  }
  get_msg_hook (const get_msg_hook &) = delete;
  get_msg_hook (get_msg_hook &&) = default;
  get_msg_hook& operator = (const get_msg_hook &) = delete;
  get_msg_hook& operator = (get_msg_hook &&) = default;

  bool install (DWORD thread_id);
  void uninstall (DWORD thread_id);
  void uninstall_all (void);

  void subclassify (HWND hwnd, bool b_all);
  void unsubclassify (HWND hwnd, bool b_all);
  void unsubclassify (DWORD thread_id);
  void unsubclassify_all (void);

private:
  const HOOKPROC get_msg_proc_;
  const HMODULE hmodule_;

  std::mutex mtx_;
  std::unordered_map<DWORD, HHOOK> threads_;
};

#endif // INCLUDE_GUARD_GET_MSG_HOOK_HH
