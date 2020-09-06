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

#include "get_msg_hook.hh"

#include <mutex>
#include <unordered_set>

#include <windows.h>

#include "debug-message.hh"

bool
get_msg_hook::install (DWORD thread_id)
{
  std::lock_guard<std::mutex> lock (mtx_);

  if (threads_.find (thread_id) != threads_.end ())
    {
      DEBUG_MESSAGE ("already installed\n");
      return true;
    }

  auto hhook =
    SetWindowsHookExW (WH_GETMESSAGE, get_msg_proc_, hmodule_, thread_id);
  if (!hhook)
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("SetWindowsHookExW failed: " +
                       get_format_message (e) + "\n");
      return false;
    }

  threads_[thread_id] = hhook;

  return true;
}

void
get_msg_hook::uninstall (DWORD thread_id)
{
  std::lock_guard<std::mutex> lock (mtx_);

  if (threads_.find (thread_id) == threads_.end ())
    {
      WARNING_MESSAGE ("not installed\n");
      return;
    }

  if (!UnhookWindowsHookEx (threads_[thread_id]))
    {
      auto e = GetLastError ();
      WARNING_MESSAGE ("UnhookWindowsHookExW failed" +
                       get_format_message (e) + "\n");
    }

  threads_.erase (thread_id);
}

void
get_msg_hook::uninstall_all (void)
{
  while (true)
    {
      DWORD thread_id;
      {
        std::lock_guard<std::mutex> lock (mtx_);

        if (!threads_.size ())
          break;

        thread_id = threads_.begin ()->first;
      }
      uninstall (thread_id);
    }
}
