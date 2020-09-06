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

#include <windows.h>
#include <commctrl.h>

#include "debug-message.hh"
#include "get_msg_proc.hh"
#include "message.hh"

thread_local LOGFONTW subclass_proc::lf_imefont_ {0};

LRESULT
subclass_proc::wm_tr_ime_set_font (HWND hwnd, UINT umsg,
                                   WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_TR_IME_SET_FONT\n");

  auto *logfont = reinterpret_cast<LOGFONTW*> (wparam);
  if (logfont->lfFaceName[0] != 0)
    {
      lf_imefont_ = *logfont;
      DEBUG_MESSAGE_STATIC ("  ime font set\n");
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT
subclass_proc::wm_ime_startcomposition (HWND hwnd, UINT umsg,
                                        WPARAM wparam, LPARAM lparam)
{
  DEBUG_MESSAGE ("WM_IME_STARTCOMPOSITION\n");

  if (lf_imefont_.lfFaceName[0] != 0)
    {
      DEBUG_MESSAGE_STATIC ("  LOGFONTW exists, set the font\n");

      auto himc = ImmGetContext (hwnd);
      if (himc)
        {
          if (!ImmSetCompositionFontW (himc, &lf_imefont_))
            {
              auto e = GetLastError ();
              WARNING_MESSAGE ("ImmSetCompositionFontW failed:" +
                               get_format_message (e) + "\n");
            }

          if (!ImmReleaseContext (hwnd, himc))
            {
              auto e = GetLastError ();
              WARNING_MESSAGE ("ImmReleaseContext failed: " +
                               get_format_message (e) + "\n");
            }
        }
      else
        {
          auto e = GetLastError ();
          WARNING_MESSAGE ("ImmGetContext failed: " +
                           get_format_message (e) + "\n");
        }
    }
  else
    {
      DEBUG_MESSAGE_STATIC ("  LOGFONTW does not exist\n");
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}

LRESULT CALLBACK
subclass_proc::proc (HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam,
                     UINT_PTR, DWORD_PTR)
{
  if (umsg == u_WM_TR_IME_SUBCLASSIFY_)
    DEBUG_MESSAGE ("WM_TR_IME_SUBCLASSIFY\n");
  else if (umsg == u_WM_TR_IME_SET_FONT_)
    return wm_tr_ime_set_font (hwnd, umsg, wparam, lparam);

  switch (umsg)
    {
    case WM_NCDESTROY:
      DEBUG_MESSAGE ("WM_NCDESTROY\n");
      get_msg_proc::destroy (hwnd);
      break;

    case WM_IME_STARTCOMPOSITION:
      return wm_ime_startcomposition (hwnd, umsg, wparam, lparam);
    }

  return DefSubclassProc (hwnd, umsg, wparam, lparam);
}
