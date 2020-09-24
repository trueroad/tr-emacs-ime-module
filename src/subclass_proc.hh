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

#include <atomic>
#include <functional>
#include <mutex>
#include <string>
#include <unordered_set>
#include <utility>

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
  static void lisp_set_startcomposition_defsubclassproc (bool flag)
  {
    ab_startcomposition_defsubclassproc_.store (flag);
  }
  static void lisp_set_reconversion (bool flag)
  {
    ab_reconversion_.store (flag);
  }
  static void lisp_set_documentfeed (bool flag)
  {
    ab_documentfeed_.store (flag);
  }

  static void set_last_ime_state (bool flag)
  {
    ab_last_ime_state_set_.store (flag);
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

  class reconvert_string
  {
  public:
    // offset is a byte count from the beginning of the string
    // to indicate the cursor position, similar to
    // RECONVERTSTRING dwCompStrOffset.
    static void set (std::basic_string<WCHAR> &&str, DWORD offset)
    {
      str_ = std::move (str);
      offset_ = offset;
      len_ = 0;
      ab_set_.store (true);
    }
    static void add_comp (const std::basic_string<WCHAR> &compstr)
    {
      str_.insert (offset_ / sizeof (WCHAR), compstr);
      len_ = compstr.size ();
    }
    static void clear (void)
    {
      ab_set_.store (false);
    }
    static std::basic_string<WCHAR> &get_str (void)
    {
      return str_;
    }
    // For RECONVERTSTRING dwSize
    // byte count for the struct
    static DWORD get_dwSize (void)
    {
      return sizeof (RECONVERTSTRING) +
        (get_dwStrLen () + 1) * sizeof (WCHAR);
    }
    // For RECONVERTSTRING dwStrLen
    // WCHAR count for the length of the full string
    static DWORD get_dwStrLen (void)
    {
      return str_.size ();
    }
    // For RECONVERTSTRING dwStrOffset
    // byte count from the beginning of the struct to indicate the offset
    // of the full string
    static constexpr DWORD get_dwStrOffset (void)
    {
      return sizeof (RECONVERTSTRING);
    }
    // For RECONVERTSTRING dwComStrOffset
    // byte count from the beginning of the string to indicate the offset
    // of the composition string
    static DWORD get_dwCompStrOffset (void)
    {
      return offset_;
    }
    // For RECONVERTSTRING dwCompStrLen
    // WCHAR count for the length of the composition string
    static DWORD get_dwCompStrLen (void)
    {
      return len_;
    }
    static bool isset (void)
    {
      return ab_set_.load ();
    }
  private:
    static thread_local std::basic_string<WCHAR> str_;
    static thread_local DWORD offset_;
    static thread_local DWORD len_;
    static thread_local std::atomic<bool> ab_set_;
  };

  class backward_complete
  {
  public:
    static void set(void)
    {
      ab_set_.store (true);
    }
    static void clear (void)
    {
      ab_set_.store (false);
    }
    static bool isset (void)
    {
      return ab_set_.load ();
    }
  private:
    static thread_local std::atomic<bool> ab_set_;
  };

  static bool wait_message (HWND, std::function<bool(void)>);
  static bool set_reconvert_string (RECONVERTSTRING*);

  static LRESULT wm_tr_ime_set_open_status (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_get_open_status (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_set_font (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_set_compositionwindow (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_tr_ime_set_prefix_keys (HWND, UINT, WPARAM, LPARAM);
  static LRESULT
  wm_tr_ime_notify_reconvert_string (HWND, UINT, WPARAM, LPARAM);
  static LRESULT
  wm_tr_ime_notify_backward_complete (HWND, UINT, WPARAM, LPARAM);

  static bool get_reconvert_string (HWND);
  static bool add_composition_string (HWND);
  static LRESULT imr_reconvertstring (HWND, UINT, WPARAM, LPARAM);
  static LRESULT imr_documentfeed (HWND, UINT, WPARAM, LPARAM);

  static LRESULT wm_keydown (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_notify (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_request (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_composition (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_startcomposition (HWND, UINT, WPARAM, LPARAM);
  static LRESULT wm_ime_endcomposition (HWND, UINT, WPARAM, LPARAM);

  static constexpr UINT_PTR subclass_id_ {0};
  static thread_local LOGFONTW lf_imefont_;
  static thread_local COMPOSITIONFORM compform_;
  static thread_local std::unordered_set<DWORD> prefix_keys_;
  static std::atomic<bool> ab_startcomposition_defsubclassproc_;
  static std::atomic<bool> ab_last_ime_state_set_;
  static std::atomic<bool> ab_reconversion_;
  static std::atomic<bool> ab_documentfeed_;
  static std::atomic<int> ai_delete_chars_reconversion_complete_;
  static thread_local std::unordered_set<HWND> compositioning_hwnds_;

  static constexpr int i_wait_message_times_ = 100;
  static constexpr DWORD dw_wait_message_single_ = 1000;
  static constexpr DWORD dw_wait_message_total_ = 3000;
};

#endif // INCLUDE_GUARD_SUBCLASS_PROC_HH
