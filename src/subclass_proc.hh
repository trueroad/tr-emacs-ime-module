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

#include <mutex>
#include <unordered_set>

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

  static void lisp_resume_prefix_key (void)
  {
    prefix_key::lisp_resume ();
  }

private:
  class prefix_key
  {
  public:
    static void set (HWND);
    static void lisp_resume (void);
  private:
    static std::mutex mtx_;
    static HWND hwnd_;
    static bool b_before_ime_mode_;
  };

  static LRESULT wm_tr_ime_set_open_status (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_get_open_status (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_set_font (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_set_compositionwindow (HWND, UINT, WPARAM, LPARAM);

  static LRESULT wm_keydown (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_startcomposition (HWND, UINT, WPARAM, LPARAM);

#ifndef NDEBUG
  static LRESULT wm_ime_endcomposition (HWND, UINT, WPARAM, LPARAM);
#endif

  static constexpr UINT_PTR subclass_id_ {0};
  static thread_local LOGFONTW lf_imefont_;
  static thread_local COMPOSITIONFORM compform_;
  static thread_local std::unordered_set<DWORD> prefix_keys_;

#ifndef NDEBUG
  static thread_local std::unordered_set<HWND> compositioning_hwnds_;
#endif
};

#endif // INCLUDE_GUARD_SUBCLASS_PROC_HH
