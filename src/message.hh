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

#ifndef INCLUDE_GUARD_MESSAGE_HH
#define INCLUDE_GUARD_MESSAGE_HH

#include <windows.h>

extern const UINT u_WM_TR_IME_SUBCLASSIFY_;
extern const UINT u_WM_TR_IME_UNSUBCLASSIFY_;
extern const UINT u_WM_TR_IME_EXISTS_SUBCLASSIFIED_;
extern const UINT u_WM_TR_IME_SET_OPEN_STATUS_;
extern const UINT u_WM_TR_IME_GET_OPEN_STATUS_;
extern const UINT u_WM_TR_IME_SET_FONT_;
extern const UINT u_WM_TR_IME_SET_COMPOSITIONWINDOW_;
extern const UINT u_WM_TR_IME_SET_PREFIX_KEYS_;
extern const UINT u_WM_TR_IME_NOTIFY_RECONVERT_STRING_;
extern const UINT u_WM_TR_IME_NOTIFY_BACKWARD_COMPLETE_;

#endif // INCLUDE_GUARD_MESSAGE_HH
