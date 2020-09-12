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

#include "subclass_proc.hh"

#include <unordered_set>

#include <windows.h>
#include <commctrl.h>

#include "debug-message.hh"
#include "get_msg_proc.hh"
#include "message.hh"

thread_local LOGFONTW subclass_proc::lf_imefont_ {0};
thread_local COMPOSITIONFORM subclass_proc::compform_ {0};

#ifndef NDEBUG
thread_local std::unordered_set<HWND> subclass_proc::compositioning_hwnds_;
#endif

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
};

LRESULT
subclass_proc::wm_tr_ime_set_font (HWND hwnd, UINT umsg,
                                   WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_FONT\n");

  auto *logfont = reinterpret_cast<LOGFONTW*> (wparam);
  lf_imefont_ = *logfont;

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_tr_ime_set_compositionwindow (HWND hwnd, UINT umsg,
                                                WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_COMPOSITIONWINDOW\n");

  auto *compform = reinterpret_cast<COMPOSITIONFORM*> (wparam);
  compform_ = *compform;

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_startcomposition (HWND hwnd, UINT umsg,
                                        WPARAM wparam, LPARAM lparam)
{
  bool b_extra_start = true;

#ifndef NDEBUG
  if (compositioning_hwnds_.find (hwnd) == compositioning_hwnds_.end ())
    {
      b_extra_start = false;
      compositioning_hwnds_.insert (hwnd);
    }
#endif

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

  auto r = DefSubclassProc (hwnd, umsg, wparam, lparam);

  // The composition window position is set again after Emacs sets it.
  if (compform_.dwStyle)
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  COMPOSITIONFORM exists, set the position\n");
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
    }
  else
    {
      if (!b_extra_start)
        DEBUG_MESSAGE_STATIC ("  COMPOSITIONFORM does not exist\n");
    }

  return r;
}

#ifndef NDEBUG
LRESULT
subclass_proc::wm_ime_endcomposition (HWND hwnd, UINT umsg,
                                      WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_IME_ENDCOMPOSITION\n");
  compositioning_hwnds_.erase (hwnd);

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}
#endif

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
  else if (umsg == u_WM_TR_IME_SET_FONT_)
    return wm_tr_ime_set_font (hwnd, umsg, wparam, lparam);
  else if (umsg == u_WM_TR_IME_SET_COMPOSITIONWINDOW_)
    return wm_tr_ime_set_compositionwindow (hwnd, umsg, wparam, lparam);

  switch (umsg)
    {
    case WM_NCDESTROY:
      DEBUG_MESSAGE ("WM_NCDESTROY\n");
      get_msg_proc::destroy (hwnd);
      break;

    case WM_IME_STARTCOMPOSITION:
      return wm_ime_startcomposition (hwnd, umsg, wparam, lparam);

#ifndef NDEBUG
    case WM_IME_ENDCOMPOSITION:
      return wm_ime_endcomposition (hwnd, umsg, wparam, lparam);
#endif
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}
