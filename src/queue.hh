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

#ifndef INCLUDE_GUARD_QUEUE_HH
#define INCLUDE_GUARD_QUEUE_HH

#include <deque>
#include <memory>
#include <mutex>

class queue_message
{
public:
  enum class message
    { setopenstatus, reconvertstring, documentfeed,
      backward_char
    };

  explicit queue_message (message msg):
    message_ (msg), parameter_ (0)
  {
  }
  explicit queue_message (message msg, int param):
    message_ (msg), parameter_ (param)
  {
  }
  virtual ~queue_message ()
  {
  }

  message get_message (void) const
  {
    return message_;
  }
  int get_parameter (void) const
  {
    return parameter_;
  }

private:
  const message message_;
  const int parameter_;
};

class ui_to_lisp_queue
{
public:
  explicit ui_to_lisp_queue () = delete;
  ~ui_to_lisp_queue () = delete;
  ui_to_lisp_queue (const ui_to_lisp_queue &) = delete;
  ui_to_lisp_queue (ui_to_lisp_queue &&) = delete;
  ui_to_lisp_queue& operator = (const ui_to_lisp_queue &) = delete;
  ui_to_lisp_queue& operator = (ui_to_lisp_queue &&) = delete;

  static bool empty(void);
  static void enqueue (std::unique_ptr<queue_message> &&);
  static void enqueue_fast (std::unique_ptr<queue_message> &&);
  static void enqueue_one (std::unique_ptr<queue_message> &&);
  static void enqueue_fast_one (std::unique_ptr<queue_message> &&);
  static std::unique_ptr<queue_message> dequeue (void);

private:
  static std::mutex mtx_;
  static std::deque<std::unique_ptr<queue_message>> queue_;
};

#endif // INCLUDE_GUARD_QUEUE_HH
