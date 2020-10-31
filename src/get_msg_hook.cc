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

#include "get_msg_hook.hh"

#include <chrono>
#include <future>
#include <mutex>
#include <unordered_set>
#include <vector>

#include <windows.h>

#include "debug-message.hh"
#include "message.hh"

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

void
get_msg_hook::subclassify (HWND hwnd, bool b_all)
{
  // SendMessage does not work
  PostMessageW (hwnd, u_WM_TR_IME_SUBCLASSIFY_,
                static_cast<WPARAM> (b_all), 0);
}

void
get_msg_hook::unsubclassify (HWND hwnd, bool b_all)
{
  // SendMessage does not work
  PostMessageW (hwnd, u_WM_TR_IME_UNSUBCLASSIFY_,
                static_cast<WPARAM> (b_all), 0);
}

void
get_msg_hook::unsubclassify (DWORD thread_id)
{
  PostThreadMessageW (thread_id, u_WM_TR_IME_UNSUBCLASSIFY_,
                      static_cast<WPARAM> (true), 0);
}

void
get_msg_hook::unsubclassify_all (void)
{
  std::lock_guard<std::mutex> lock (mtx_);

  for (const auto &t: threads_)
    unsubclassify (t.first);
}

bool
get_msg_hook::exists_subclassified (DWORD thread_id)
{
  std::promise<bool> p;

  PostThreadMessageW (thread_id, u_WM_TR_IME_EXISTS_SUBCLASSIFIED_,
                      reinterpret_cast<WPARAM> (&p), 0);

  auto future = p.get_future ();
  auto results = future.wait_for (std::chrono::seconds (1));
  if (results == std::future_status::ready)
    {
      DEBUG_MESSAGE ("ready\n");
      return future.get ();
    }

  DEBUG_MESSAGE ("timeout\n");

  return false;
}

bool
get_msg_hook::exists_subclassified_all (void)
{
  std::vector<DWORD> v;

  {
    std::lock_guard<std::mutex> lock (mtx_);

    for (const auto &t: threads_)
      v.push_back (t.first);
  }

  for (auto id: v)
    {
      if (exists_subclassified (id))
        return true;
    }

  return false;
}
