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

#include "debug-message.hh"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sstream>
#include <string>

#include <windows.h>

// verbose level
// 0: none
// 1: fatal
// 2: error
// 3: warn
// 4: info
// 5: debug
// 6: trace

#ifndef NDEBUG
int verbose_level = 5;
#else
int verbose_level = 3;
#endif

std::string
get_format_message (DWORD dwMessageId)
{
  LPSTR lpbuff;

  if (!FormatMessageA (FORMAT_MESSAGE_ALLOCATE_BUFFER |
                       FORMAT_MESSAGE_FROM_SYSTEM |
                       FORMAT_MESSAGE_IGNORE_INSERTS,
                       nullptr,
                       dwMessageId,
                       MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT),
                       reinterpret_cast<LPSTR> (&lpbuff),
                       0,
                       nullptr))
    return "error";

  std::string ret {lpbuff};
  LocalFree (lpbuff);

  return ret;
}

void
debug_output_reconvert_string (RECONVERTSTRING *rs)
{
  std::stringstream ss;

  ss << "---RECONVERTSTRING---" << std::endl
     << "  dwSize           : " << rs->dwSize << std::endl
     << "  dwVersion        : " << rs->dwVersion << std::endl
     << "  dwStrLen         : " << rs->dwStrLen << std::endl
     << "  dwStrOffset      : " << rs->dwStrOffset << std::endl
     << "  dwCompStrLen     : " << rs->dwCompStrLen << std::endl
     << "  dwCompStrOffset  : " << rs->dwCompStrOffset << std::endl
     << "  dwTargetStrLen   : " << rs->dwTargetStrLen << std::endl
     << "  dwTargetStrOffset: " << rs->dwTargetStrOffset << std::endl;
  DEBUG_MESSAGE_STATIC (ss.str ().c_str ());

  auto *buff = reinterpret_cast<WCHAR*>
    (reinterpret_cast<unsigned char*> (rs) + sizeof (RECONVERTSTRING));
  std::basic_string<WCHAR> str (buff, rs->dwStrLen);
  TRACE_MESSAGE_W (L"  buff = \"" << str << L"\"\n");
}
