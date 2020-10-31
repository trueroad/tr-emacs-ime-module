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

#include "subclass_proc.hh"

#include <atomic>
#include <functional>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>

#include <windows.h>
#include <commctrl.h>

#include "debug-message.hh"
#include "get_msg_proc.hh"
#include "message.hh"
#include "queue.hh"

std::mutex subclass_proc::prefix_key::mtx_;
HWND subclass_proc::prefix_key::hwnd_ {0};
bool subclass_proc::prefix_key::b_before_ime_mode_;

thread_local std::basic_string<WCHAR> subclass_proc::reconvert_string::str_;
thread_local DWORD subclass_proc::reconvert_string::offset_;
thread_local DWORD subclass_proc::reconvert_string::len_;
thread_local std::atomic<bool>
subclass_proc::reconvert_string::ab_set_ {false};
thread_local std::atomic<bool>
subclass_proc::backward_complete::ab_set_ {false};

thread_local LOGFONTW subclass_proc::lf_imefont_ {0};
thread_local COMPOSITIONFORM subclass_proc::compform_ {0};
thread_local std::unordered_set<DWORD> subclass_proc::prefix_keys_;
std::atomic<bool> subclass_proc::ab_startcomposition_defsubclassproc_ {false};
std::atomic<bool> subclass_proc::ab_last_ime_state_set_ {false};
std::atomic<bool> subclass_proc::ab_reconversion_ {false};
std::atomic<bool> subclass_proc::ab_documentfeed_ {false};
std::atomic<int> subclass_proc::ai_delete_chars_reconversion_complete_ {0};
thread_local std::unordered_set<HWND> subclass_proc::compositioning_hwnds_;

class himc_raii final
{
public:
  explicit himc_raii (HWND hwnd)
    : hwnd_ (hwnd)
  {
    himc_ = ImmGetContext (hwnd_);
    if (!himc_)
      {
        auto e = GetLastError ();
        WARNING_MESSAGE ("ImmGetContext failed: " +
                         get_format_message (e) + "\n");
      }
  }
  ~himc_raii ()
  {
    if (!himc_)
      return;

    if (!ImmReleaseContext (hwnd_, himc_))
      {
        auto e = GetLastError ();
        WARNING_MESSAGE ("ImmReleaseContext failed: " +
                         get_format_message (e) + "\n");
      }
  }
  HIMC get (void) const
  {
    return himc_;
  }
  explicit operator bool () const
  {
    return himc_;
  }

private:
  const HWND hwnd_;
  HIMC himc_;
};

namespace
{
  void ime_set_mode (HWND hwnd, bool bmode)
  {
    subclass_proc::set_last_ime_state (bmode);

    himc_raii himc (hwnd);
    if (himc)
      {
        if (!ImmSetOpenStatus (himc.get (), bmode))
          {
            auto e = GetLastError ();
            WARNING_MESSAGE ("ImmSetOpenStatus failed:" +
                             get_format_message (e) + "\n");
          }
      }
  }

  bool ime_get_mode (HWND hwnd)
  {
    himc_raii himc (hwnd);
    if (himc)
      return ImmGetOpenStatus (himc.get ());
    return false;
  }

  template <class Iterator>
  size_t count_codepoints (Iterator first, Iterator last)
  {
    size_t n = 0;

    for (auto p = first; p < last; ++p, ++n)
      {
        if (0xd800 <= *p && *p <= 0xdbff) // surrogate pair
          ++p;
      }

    return n;
  }

  template <class Iterator>
  DWORD count_offset (Iterator first, Iterator last, int point)
  {
    auto p = first;

    for (int i = 0; i < point && p < last; ++i, ++p)
      {
        if (0xd800 <= *p && *p <= 0xdbff) // surrogate pair
          ++p;
      }

    return (p - first) * sizeof (*first);
  }
};

void
subclass_proc::prefix_key::set (HWND hwnd)
{
  std::lock_guard<std::mutex> lock (mtx_);

  if (!hwnd_)
    {
      b_before_ime_mode_ = ime_get_mode (hwnd);
      if (b_before_ime_mode_)
        {
          hwnd_ = hwnd;
          ime_set_mode (hwnd, false);
        }
    }
}

void
subclass_proc::prefix_key::lisp_resume (void)
{
  // This function is called from lisp threads.
  std::lock_guard<std::mutex> lock (mtx_);

  if (hwnd_)
    {
      DEBUG_MESSAGE ("resume\n");

      PostMessageW (hwnd_, u_WM_TR_IME_SET_OPEN_STATUS_,
                    b_before_ime_mode_, 0);
      hwnd_ = nullptr;
    }
}

bool
subclass_proc::wait_message (HWND hwnd, std::function<bool(void)> f)
{
  DEBUG_MESSAGE ("enter\n");

  if (f ())
    {
      DEBUG_MESSAGE_STATIC ("  recieved\n");
      return true;
    }

  DEBUG_MESSAGE_STATIC ("  wait for message\n");
  auto before_time = GetTickCount ();
  for (int i = 0; i < i_wait_message_times_; ++i)
    {
      auto r = MsgWaitForMultipleObjectsEx
        (0, nullptr, dw_wait_message_single_,
         QS_SENDMESSAGE, MWMO_INPUTAVAILABLE);

      switch (r)
        {
        case WAIT_OBJECT_0:
          DEBUG_MESSAGE_STATIC ("  WAIT_OBJECT_0\n");
          {
            MSG msg;
            PeekMessage (&msg, hwnd, WM_NULL, WM_NULL,
                         PM_NOREMOVE | PM_QS_SENDMESSAGE);
          }
          break;
        case WAIT_TIMEOUT:
          DEBUG_MESSAGE_STATIC ("  WAIT_TIMEOUT\n");
          break;
        default:
          DEBUG_MESSAGE_STATIC ("  unknown\n");
          break;
        }

      if (f ())
        {
          DEBUG_MESSAGE_STATIC ("  recieved\n");
          return true;
        }
      else
        {
          DEBUG_MESSAGE_STATIC ("  not yet\n");
        }

      auto t = GetTickCount ();
      if (t > before_time + dw_wait_message_total_ || t < before_time)
        break;
    }

  WARNING_MESSAGE ("timeout\n");
  return false;
}

bool
subclass_proc::set_reconvert_string (RECONVERTSTRING *rs)
{
  if (!reconvert_string::isset ())
    {
      WARNING_MESSAGE ("no reconvert string\n");
      return false;
    }

  if (rs->dwSize < reconvert_string::get_dwSize ())
    {
      WARNING_MESSAGE ("size over\n");
      return false;
    }

  // Len: WCHAR count, Offset: byte count
  // dwStrOffset: buffer offset from beginning of the struct
  // dw{Comp|Target}Offset: from beginning of the buffer
  rs->dwSize = reconvert_string::get_dwSize ();
  rs->dwVersion = 0;
  rs->dwStrLen = reconvert_string::get_dwStrLen ();
  rs->dwStrOffset = reconvert_string::get_dwStrOffset ();
  rs->dwCompStrLen = reconvert_string::get_dwCompStrLen ();
  rs->dwCompStrOffset = reconvert_string::get_dwCompStrOffset ();
  rs->dwTargetStrLen = rs->dwCompStrLen;
  rs->dwTargetStrOffset = rs->dwCompStrOffset;

  auto *p_str = reinterpret_cast<unsigned char*> (rs) + rs->dwStrOffset;

  std::copy (reconvert_string::get_str ().begin (),
             reconvert_string::get_str ().end (),
             reinterpret_cast<WCHAR*> (p_str));

  return true;
}

LRESULT
subclass_proc::wm_tr_ime_set_open_status (HWND hwnd, UINT umsg,
                                          WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_OPEN_STATUS\n");

  auto bopen = static_cast<bool> (wparam);
  ime_set_mode (hwnd, bopen);

  return 0;
}

LRESULT
subclass_proc::wm_tr_ime_get_open_status (HWND hwnd, UINT umsg,
                                          WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_GET_OPEN_STATUS\n");

  return ime_get_mode (hwnd);
}

LRESULT
subclass_proc::wm_tr_ime_set_font (HWND hwnd, UINT umsg,
                                   WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_FONT\n");

  auto *logfont = reinterpret_cast<LOGFONTW*> (wparam);
  lf_imefont_ = *logfont;

  return 0;
}

LRESULT
subclass_proc::wm_tr_ime_set_compositionwindow (HWND hwnd, UINT umsg,
                                                WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_COMPOSITIONWINDOW\n");

  auto *compform = reinterpret_cast<COMPOSITIONFORM*> (wparam);
  compform_ = *compform;

  return 0;
}

LRESULT
subclass_proc::wm_tr_ime_set_prefix_keys (HWND hwnd, UINT umsg,
                                          WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_PREFIX_KEYS\n");

  auto *prefix_keys = reinterpret_cast<std::unordered_set<DWORD> *> (wparam);
  prefix_keys_ = *prefix_keys;

  return 0;
}

LRESULT
subclass_proc::wm_tr_ime_notify_reconvert_string (HWND hwnd, UINT umsg,
                                                  WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_NOTIFY_RECONVERT_STRING\n");

  std::basic_string<WCHAR> wbuff = reinterpret_cast<WCHAR*> (wparam);
  ReplyMessage (0);

  TRACE_MESSAGE_W (L"  string = \"" << wbuff << L"\"\n");
  DEBUG_MESSAGE_A ("  point = " << static_cast<int> (lparam) << std::endl);

  auto offset = count_offset (wbuff.begin (), wbuff.end (), lparam);
  reconvert_string::set (std::move (wbuff), offset);
  DEBUG_MESSAGE_STATIC ("  set reconvert string\n");

  return 0;
}

LRESULT
subclass_proc::wm_tr_ime_notify_backward_complete (HWND hwnd, UINT umsg,
                                                   WPARAM wparam,
                                                   LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_NOTIFY_BACKWARD_COMPLETE\n");

  ReplyMessage (0);

  backward_complete::set ();

  return 0;
}

bool
subclass_proc::get_reconvert_string (HWND hwnd, bool bdocumentfeed)
{
  reconvert_string::clear ();

  ui_to_lisp_queue::enqueue_one
    (std::make_unique<queue_message>
     (bdocumentfeed ?
      queue_message::message::documentfeed :
      queue_message::message::reconvertstring, hwnd));
  SendMessageW (hwnd, WM_INPUTLANGCHANGE, 0, 0);

  if (!wait_message (hwnd, &reconvert_string::isset))
    return false;

  if (!reconvert_string::isset ())
    {
      WARNING_MESSAGE ("no reconvert string\n");
      return false;
    }

  return true;
}

bool
subclass_proc::add_composition_string (HWND hwnd)
{
  himc_raii himc (hwnd);
  if (himc)
    {
      std::basic_string<WCHAR> buff;
      auto len = ImmGetCompositionStringW (himc.get (),
                                           GCS_COMPSTR,
                                           nullptr, 0);
      if (len > 0)
        {
          buff.resize (len / sizeof (WCHAR));
          if (ImmGetCompositionStringW (himc.get (),
                                        GCS_COMPSTR,
                                        &buff[0], len) > 0)
            {
              TRACE_MESSAGE_W (L"  documentfeed add_comp = \""
                               << buff << "\"\n");
              reconvert_string::add_comp (buff);

              return true;
            }
          else
            {
              auto e = GetLastError ();
              WARNING_MESSAGE ("ImmGetCompositionStringW 2nd failed: " +
                               get_format_message (e) + "\n");
            }
        }
      else
        {
          auto e = GetLastError ();
          WARNING_MESSAGE ("ImmGetCompositionStringW 1st failed: " +
                           get_format_message (e) + "\n");
        }
    }

  return false;
}

void
subclass_proc::process_backward_characters (HWND hwnd,
                                            RECONVERTSTRING *rs, DWORD before)
{
  if (before > rs->dwCompStrOffset)
    {
      DEBUG_MESSAGE_STATIC ("  require backward characters\n");

      auto *p_str = reinterpret_cast <unsigned char*> (rs) + rs->dwStrOffset;
      auto *p_comp = p_str + rs->dwCompStrOffset;
      auto *p_before = p_str + before;
      size_t n = count_codepoints (reinterpret_cast<WCHAR*> (p_comp),
                                   reinterpret_cast<WCHAR*> (p_before));

      DEBUG_MESSAGE_A ("  backward: " << n << std::endl);

      backward_complete::clear ();
      ui_to_lisp_queue::enqueue_one
        (std::make_unique<queue_message>
         (queue_message::message::backward_char, hwnd, n));
      SendMessageW (hwnd, WM_INPUTLANGCHANGE, 0, 0);

      if (!wait_message (hwnd, &backward_complete::isset))
        {
          WARNING_MESSAGE
            ("timeout for WM_TR_IME_NOTIFY_BACKWARD_COMPLETE\n");
        }
    }
}

void
subclass_proc::process_delete_characters (HWND hwnd, RECONVERTSTRING *rs)
{
  if (rs->dwCompStrLen)
    {
      DEBUG_MESSAGE_STATIC ("  require delete characters\n");

      auto *p_str = reinterpret_cast <unsigned char*> (rs) + rs->dwStrOffset;
      auto *p_comp = p_str + rs->dwCompStrOffset;
      auto *p_comp_end = p_comp + rs->dwCompStrLen * sizeof (WCHAR);
      size_t d = count_codepoints (reinterpret_cast<WCHAR*> (p_comp),
                                   reinterpret_cast<WCHAR*> (p_comp_end));

      DEBUG_MESSAGE_A ("  delete: " << d << std::endl);

      ai_delete_chars_reconversion_complete_.store (d);
    }
}

LRESULT
subclass_proc::imr_reconvertstring (HWND hwnd, UINT umsg,
                                    WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("IMR_RECONVERTSTRING\n");

  if (!lparam)
    {
      if (!get_reconvert_string (hwnd, false))
        return 0;

      DEBUG_MESSAGE_STATIC
        ("IMR_RECONVERTSTRING: first attempt succeeded\n");

      return reconvert_string::get_dwSize ();
    }

  DEBUG_MESSAGE_STATIC ("IMR_RECONVERTSTRING: second attempt...\n");

  auto *rs = reinterpret_cast<RECONVERTSTRING*> (lparam);
  if (!set_reconvert_string (rs))
    return 0;

  DEBUG_MESSAGE_RECONVERTSTRING (rs);

  auto before_offset = rs->dwCompStrOffset;

  himc_raii himc (hwnd);
  if (!himc)
    return 0;

  if (!ImmSetCompositionStringW (himc.get (),
                                 SCS_QUERYRECONVERTSTRING,
                                 rs, rs->dwSize, nullptr, 0))
    {
      auto e = GetLastError ();
      WARNING_MESSAGE
        ("ImmSetCompositionStringW: SCS_QUERYRECONVERTSTRING failed: " +
         get_format_message (e) + "\n");
      return 0;
    }

  DEBUG_MESSAGE_STATIC ("  SCS_QUERYRECONVERTSTRING succeeded\n");
  DEBUG_MESSAGE_RECONVERTSTRING (rs);

  process_backward_characters (hwnd, rs, before_offset);
  process_delete_characters (hwnd, rs);

  if (!ImmSetCompositionStringW (himc.get (),
                                 SCS_SETRECONVERTSTRING,
                                 rs, rs->dwSize, nullptr, 0))
    {
      auto e = GetLastError ();
      WARNING_MESSAGE
        ("ImmSetCompositionStringW: SCS_SETRECONVERTSTRING failed: " +
         get_format_message (e) + "\n");
    }
  else
    {
      DEBUG_MESSAGE_STATIC ("  SCS_SETRECONVERTSTRING succeeded\n");
    }

  reconvert_string::clear ();

  return 0;
}

LRESULT
subclass_proc::imr_documentfeed (HWND hwnd, UINT umsg,
                                 WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("IMR_DOCUMENTFEED\n");

  if (!lparam)
    {
      if (!get_reconvert_string (hwnd, true))
        return 0;

      if (!add_composition_string (hwnd))
        return 0;

      DEBUG_MESSAGE_STATIC
        ("IMR_DOCUMENTFEED: first attempt succeeded\n");

      return reconvert_string::get_dwSize ();
    }

  DEBUG_MESSAGE_STATIC ("IMR_DOCUMENTFEED: second attempt...\n");

  auto *rs = reinterpret_cast<RECONVERTSTRING*> (lparam);
  if (!set_reconvert_string (rs))
    return 0;

  DEBUG_MESSAGE_RECONVERTSTRING (rs);

  auto s = reconvert_string::get_dwSize ();
  reconvert_string::clear ();

  return s;
}

LRESULT
subclass_proc::wm_keydown (HWND hwnd, UINT umsg,
                           WPARAM wparam, LPARAM lparam)
{
  if (prefix_keys_.size ())
    {
      DWORD key_code = wparam;
      if (GetAsyncKeyState (VK_SHIFT))
        key_code |= 0x10000;
      if (GetAsyncKeyState (VK_CONTROL))
        key_code |= 0x20000;
      if (GetAsyncKeyState (VK_MENU))
        key_code |= 0x40000;

      if (prefix_keys_.find (key_code) != prefix_keys_.end ())
        {
          DEBUG_MESSAGE ("WM_KEYDOWN: prefix key\n");
          prefix_key::set (hwnd);
        }
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_notify (HWND hwnd, UINT umsg,
                              WPARAM wparam, LPARAM lparam)
{
  switch (wparam)
    {
    case IMN_SETOPENSTATUS:
      DEBUG_MESSAGE ("WM_IME_NOTIFY: IMN_SETOPENSTATUS\n");

      auto bflag = ime_get_mode (hwnd);
      if (ab_last_ime_state_set_.load () != bflag)
        {
          DEBUG_MESSAGE_STATIC ("  IME mode changed\n");
          ab_last_ime_state_set_.store (bflag);

          ui_to_lisp_queue::enqueue_one
            (std::make_unique<queue_message>
             (queue_message::message::setopenstatus, hwnd));
          PostMessageW (hwnd, WM_INPUTLANGCHANGE, 0, 0);
        }
      else
        {
          DEBUG_MESSAGE_STATIC ("  IME mode not changed\n");
        }
      break;
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_request (HWND hwnd, UINT umsg,
                               WPARAM wparam, LPARAM lparam)
{
  switch (wparam)
    {
    case IMR_RECONVERTSTRING:
      if (ab_reconversion_.load ())
        return imr_reconvertstring (hwnd, umsg, wparam, lparam);
      break;

    case IMR_DOCUMENTFEED:
      if (ab_documentfeed_.load ())
        return imr_documentfeed (hwnd, umsg, wparam, lparam);
      break;
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_composition (HWND hwnd, UINT umsg,
                                   WPARAM wparam, LPARAM lparam)
{
  if (lparam & GCS_RESULTSTR)
    {
      auto chars = ai_delete_chars_reconversion_complete_.load ();
      if (chars)
        {
          ui_to_lisp_queue::enqueue_one
            (std::make_unique<queue_message>
             (queue_message::message::delete_char, hwnd, chars));
          PostMessageW (hwnd, WM_INPUTLANGCHANGE, 0, 0);

          ai_delete_chars_reconversion_complete_.store (0);
        }
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_startcomposition (HWND hwnd, UINT umsg,
                                        WPARAM wparam, LPARAM lparam)
{
  bool b_extra_start = true;

  if (compositioning_hwnds_.find (hwnd) == compositioning_hwnds_.end ())
    {
      b_extra_start = false;
      compositioning_hwnds_.insert (hwnd);
    }

  if (!b_extra_start)
    DEBUG_MESSAGE ("WM_IME_STARTCOMPOSITION: initial\n");

  if (lf_imefont_.lfFaceName[0] != 0)
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  LOGFONTW exists, set the font\n");

      himc_raii himc (hwnd);
      if (himc)
        {
          if (!ImmSetCompositionFontW (himc.get (), &lf_imefont_))
            {
              auto e = GetLastError ();
              WARNING_MESSAGE ("ImmSetCompositionFontW failed:" +
                               get_format_message (e) + "\n");
            }
        }
    }
  else
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  LOGFONTW does not exist\n");
    }

  if (compform_.dwStyle)
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  COMPOSITIONFORM exists, set the position\n");

      LRESULT r;
      auto b_defsubclassproc = ab_startcomposition_defsubclassproc_.load ();
      if (b_defsubclassproc)
        {
          // Emacs will set the composition window position.
          r = DefSubclassProc (hwnd, umsg, wparam, lparam);
        }

      himc_raii himc (hwnd);
      if (himc)
        {
          if (!ImmSetCompositionWindow (himc.get (), &compform_))
            {
              auto e = GetLastError ();
              WARNING_MESSAGE ("ImmSetCompositionWindow failed:" +
                               get_format_message (e) + "\n");
            }
        }

      if (b_defsubclassproc)
        {
          // Just return because Emacs called DefWindowProc.
          return r;
        }

      // Not using DefSubclassProc
      // because it overwrites the composition window position.
      return DefWindowProc (hwnd, umsg, wparam, lparam);
    }
  else
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  COMPOSITIONFORM does not exist\n");
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_endcomposition (HWND hwnd, UINT umsg,
                                      WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_IME_ENDCOMPOSITION\n");
  compositioning_hwnds_.erase (hwnd);

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT CALLBACK
subclass_proc::proc (HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam,
                     UINT_PTR, DWORD_PTR)
{
  if (umsg == u_WM_TR_IME_SUBCLASSIFY_)
    DEBUG_MESSAGE ("WM_TR_IME_SUBCLASSIFY\n");
  else if (umsg == u_WM_TR_IME_UNSUBCLASSIFY_)
    {
      WARNING_MESSAGE ("WM_TR_IME_UNSUBCLASSIFY\n");
      WARNING_MESSAGE_STATIC ("  cannot unsubclassify"
                              " because a message hook is not installed\n");
    }
  else if (umsg == u_WM_TR_IME_SET_OPEN_STATUS_)
    return wm_tr_ime_set_open_status (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_GET_OPEN_STATUS_)
    return wm_tr_ime_get_open_status (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_SET_FONT_)
    return wm_tr_ime_set_font (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_SET_COMPOSITIONWINDOW_)
    return wm_tr_ime_set_compositionwindow (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_SET_PREFIX_KEYS_)
    return wm_tr_ime_set_prefix_keys (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_NOTIFY_RECONVERT_STRING_)
    return wm_tr_ime_notify_reconvert_string (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_NOTIFY_BACKWARD_COMPLETE_)
    return wm_tr_ime_notify_backward_complete (hwnd, umsg, wparam, lparam);

  switch (umsg)
    {
    case WM_NCDESTROY:
      DEBUG_MESSAGE ("WM_NCDESTROY\n");
      get_msg_proc::destroy (hwnd);
      break;

    case WM_KEYDOWN:
      return wm_keydown (hwnd, umsg, wparam, lparam);

    case WM_IME_NOTIFY:
      return wm_ime_notify (hwnd, umsg, wparam, lparam);

    case WM_IME_REQUEST:
      return wm_ime_request (hwnd, umsg, wparam, lparam);

    case WM_IME_COMPOSITION:
      return wm_ime_composition (hwnd, umsg, wparam, lparam);

    case WM_IME_STARTCOMPOSITION:
      return wm_ime_startcomposition (hwnd, umsg, wparam, lparam);

    case WM_IME_ENDCOMPOSITION:
      return wm_ime_endcomposition (hwnd, umsg, wparam, lparam);
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}
