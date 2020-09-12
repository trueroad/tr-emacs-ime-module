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

#include "message.hh"

#include <windows.h>

constexpr WCHAR s_WM_TR_IME_SUBCLASSIFY_[] = L"WM_TR_IME_SUBCLASSIFY";
const UINT u_WM_TR_IME_SUBCLASSIFY_ =
  RegisterWindowMessageW (s_WM_TR_IME_SUBCLASSIFY_);

constexpr WCHAR s_WM_TR_IME_UNSUBCLASSIFY_[] = L"WM_TR_IME_UNSUBCLASSIFY";
const UINT u_WM_TR_IME_UNSUBCLASSIFY_ =
  RegisterWindowMessageW (s_WM_TR_IME_UNSUBCLASSIFY_);

constexpr WCHAR s_WM_TR_IME_SET_FONT_[] = L"WM_TR_IME_SET_FONT";
const UINT u_WM_TR_IME_SET_FONT_ =
  RegisterWindowMessageW (s_WM_TR_IME_SET_FONT_);

constexpr WCHAR s_WM_TR_IME_SET_COMPOSITIONWINDOW_[] =
  L"WM_TR_IME_SET_COMPOSITIONWINDOW";
const UINT u_WM_TR_IME_SET_COMPOSITIONWINDOW_ =
  RegisterWindowMessageW (s_WM_TR_IME_SET_COMPOSITIONWINDOW_);
