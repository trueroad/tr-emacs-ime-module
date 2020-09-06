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

#ifndef INCLUDE_GUARD_SUBCLASS_PROC_HH
#define INCLUDE_GUARD_SUBCLASS_PROC_HH

#include <windows.h>

class subclass_proc final
{
public:
  static LRESULT CALLBACK
  proc (HWND, UINT, WPARAM, LPARAM, UINT_PTR, DWORD_PTR);

  explicit subclass_proc () = delete;
  ~subclass_proc () = delete;
  subclass_proc (const subclass_proc &) = delete;
  subclass_proc (subclass_proc &&) = delete;
  subclass_proc& operator = (const subclass_proc &) = delete;
  subclass_proc& operator = (subclass_proc &&) = delete;

  static constexpr UINT_PTR get_subclass_id (void)
  {
    return subclass_id_;
  }

private:
  static constexpr UINT_PTR subclass_id_ {0};
};

#endif // INCLUDE_GUARD_SUBCLASS_PROC_HH