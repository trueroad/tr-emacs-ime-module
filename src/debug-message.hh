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

#ifndef INCLUDE_GUARD_DEBUG_MESSAGE_HH
#define INCLUDE_GUARD_DEBUG_MESSAGE_HH

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sstream>
#include <string>

#include <windows.h>

#ifdef HAVE_CXX_PRETTY_FUNCTION

#ifdef NDEBUG
#define DEBUG_MESSAGE(x)
#else
#define DEBUG_MESSAGE(x)                                                \
  do                                                                    \
    {                                                                   \
      std::stringstream ss;                                             \
      ss << "debug: " << __PRETTY_FUNCTION__ << ": " << x;              \
      OutputDebugStringA (ss.str ().c_str ());                          \
    }                                                                   \
  while (false)
#endif

#define WARNING_MESSAGE(x)                                              \
  do                                                                    \
    {                                                                   \
      std::stringstream ss;                                             \
      ss << "warning: " << __PRETTY_FUNCTION__ << ": " << x;            \
      OutputDebugStringA (ss.str ().c_str ());                          \
    }                                                                   \
  while (false)

#else // HAVE_PRETTY_FUNCTION

#ifdef NDEBUG
#define DEBUG_MESSAGE(x)
#else
#define DEBUG_MESSAGE(x)                                                \
  do                                                                    \
    {                                                                   \
      std::stringstream ss;                                             \
      ss << "debug: " << __func__ << ": " << x;                         \
      OutputDebugStringA (ss.str ().c_str ());                          \
    }                                                                   \
  while (false)
#endif

#define WARNING_MESSAGE(x)                                              \
  do                                                                    \
    {                                                                   \
      std::stringstream ss;                                             \
      ss << "warning: " << __func__ << ": " << x;                       \
      OutputDebugStringA (ss.str ().c_str ());                          \
    }                                                                   \
  while (false)

#endif // HAVE_PRETTY_FUNCTION

#ifdef NDEBUG
#define DEBUG_MESSAGE_A(x)
#define DEBUG_MESSAGE_W(x)
#define DEBUG_MESSAGE_STATIC(x)
#define DEBUG_MESSAGE_RECONVERTSTRING(x)
#else
#define DEBUG_MESSAGE_A(x)                      \
  do                                            \
    {                                           \
      std::stringstream ss;                     \
      ss << x;                                  \
      OutputDebugStringA (ss.str ().c_str ());  \
    }                                           \
  while (false)
#define DEBUG_MESSAGE_W(x)                      \
  do                                            \
    {                                           \
      std::basic_stringstream<WCHAR> ss;        \
      ss << x;                                  \
      OutputDebugStringW (ss.str ().c_str ());  \
    }                                           \
  while (false)
#define DEBUG_MESSAGE_STATIC(x) OutputDebugStringA ((x))
#define DEBUG_MESSAGE_RECONVERTSTRING(x) debug_output_reconvert_string ((x))
#endif

#define WARNING_MESSAGE_A(x)                    \
  do                                            \
    {                                           \
      std::stringstream ss;                     \
      ss << x;                                  \
      OutputDebugStringA (ss.str ().c_str ());  \
    }                                           \
  while (false)
#define WARNING_MESSAGE_W(x)                    \
  do                                            \
    {                                           \
      std::basic_stringstream<WCHAR> ss;        \
      ss << x;                                  \
      OutputDebugStringW (ss.str ().c_str ());  \
    }                                           \
  while (false)
#define WARNING_MESSAGE_STATIC(x) OutputDebugStringA ((x))

inline std::string
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

inline void
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
  OutputDebugStringA (ss.str ().c_str ());

  auto *buff = reinterpret_cast<WCHAR*>
    (reinterpret_cast<unsigned char*> (rs) + sizeof (RECONVERTSTRING));
  std::basic_string<WCHAR> str (buff, rs->dwStrLen);
  str = L"  buff = \"" + str + L"\"\n";
  OutputDebugStringW (str.c_str ());
}

#endif // INCLUDE_GUARD_DEBUG_MESSAGE_HH
