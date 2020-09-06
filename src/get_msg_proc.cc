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

#include "debug-message.hh"

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

  if (msg->message == WM_PAINT)
    DEBUG_MESSAGE ("WM_PAINT\n");

  return CallNextHookEx (nullptr, code, wparam, lparam);
}
