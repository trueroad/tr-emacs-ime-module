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

#include "queue.hh"

#include <algorithm>
#include <deque>
#include <memory>
#include <mutex>

std::mutex ui_to_lisp_queue::mtx_;
std::deque<std::unique_ptr<queue_message>> ui_to_lisp_queue::queue_;

bool
ui_to_lisp_queue::empty (void)
{
  std::lock_guard<std::mutex> lock (mtx_);

  return queue_.empty ();
}

void
ui_to_lisp_queue::enqueue (std::unique_ptr<queue_message> &&m)
{
  std::lock_guard<std::mutex> lock (mtx_);

  queue_.push_back (std::move (m));
}

void
ui_to_lisp_queue::enqueue_fast (std::unique_ptr<queue_message> &&m)
{
  std::lock_guard<std::mutex> lock (mtx_);

  queue_.push_front (std::move (m));
}

void
ui_to_lisp_queue::enqueue_one (std::unique_ptr<queue_message> &&m)
{
  std::lock_guard<std::mutex> lock (mtx_);

  if (std::find_if (queue_.begin (), queue_.end (),
                    [&m] (const std::unique_ptr<queue_message> &p)
                    {
                      return p->get_message () == m->get_message ();
                    }
                    ) == queue_.end ())
    queue_.push_back (std::move (m));
}

void
ui_to_lisp_queue::enqueue_fast_one (std::unique_ptr<queue_message> &&m)
{
  std::lock_guard<std::mutex> lock (mtx_);

  while (!queue_.empty ())
    {
      auto it = std::find_if (queue_.begin (), queue_.end (),
                              [&m] (const std::unique_ptr<queue_message> &p)
                              {
                                return p->get_message () == m->get_message ();
                              }
                              );
      if (it == queue_.end ())
        break;
      queue_.erase (it);
    }

  queue_.push_front (std::move (m));
}

std::unique_ptr<queue_message>
ui_to_lisp_queue::dequeue (void)
{
  std::lock_guard<std::mutex> lock (mtx_);

  if (queue_.empty ())
    return nullptr;

  std::unique_ptr<queue_message> m = std::move (queue_.front ());
  if (m)
    {
      queue_.pop_front ();
      return m;
    }

  return nullptr;
}
