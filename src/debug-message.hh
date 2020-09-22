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
      if (verbose_level >= 5)                                           \
        {                                                               \
          std::stringstream ss;                                         \
          ss << "debug: " << __PRETTY_FUNCTION__ << ": " << x;          \
          OutputDebugStringA (ss.str ().c_str ());                      \
        }                                                               \
    }                                                                   \
  while (false)
#endif

#define WARNING_MESSAGE(x)                                              \
  do                                                                    \
    {                                                                   \
      if (verbose_level >= 3)                                           \
        {                                                               \
          std::stringstream ss;                                         \
          ss << "warning: " << __PRETTY_FUNCTION__ << ": " << x;        \
          OutputDebugStringA (ss.str ().c_str ());                      \
        }                                                               \
    }                                                                   \
  while (false)

#else // HAVE_PRETTY_FUNCTION

#ifdef NDEBUG
#define DEBUG_MESSAGE(x)
#else
#define DEBUG_MESSAGE(x)                                                \
  do                                                                    \
    {                                                                   \
      if (verbose_level >= 5)                                           \
        {                                                               \
          std::stringstream ss;                                         \
          ss << "debug: " << __func__ << ": " << x;                     \
          OutputDebugStringA (ss.str ().c_str ());                      \
        }                                                               \
    }                                                                   \
  while (false)
#endif

#define WARNING_MESSAGE(x)                                              \
  do                                                                    \
    {                                                                   \
      if (verbose_level >= 3)                                           \
        {                                                               \
          std::stringstream ss;                                         \
          ss << "warning: " << __func__ << ": " << x;                   \
          OutputDebugStringA (ss.str ().c_str ());                      \
        }                                                               \
    }                                                                   \
  while (false)

#endif // HAVE_PRETTY_FUNCTION

#ifdef NDEBUG
#define DEBUG_MESSAGE_A(x)
#define DEBUG_MESSAGE_W(x)
#define DEBUG_MESSAGE_STATIC(x)
#define DEBUG_MESSAGE_RECONVERTSTRING(x)
#else
#define DEBUG_MESSAGE_A(x)                              \
  do                                                    \
    {                                                   \
      if (verbose_level >= 5)                           \
        {                                               \
          std::stringstream ss;                         \
          ss << x;                                      \
          OutputDebugStringA (ss.str ().c_str ());      \
        }                                               \
    }                                                   \
  while (false)
#define DEBUG_MESSAGE_W(x)                              \
  do                                                    \
    {                                                   \
      if (verbose_level >= 5)                           \
        {                                               \
          std::basic_stringstream<WCHAR> ss;            \
          ss << x;                                      \
          OutputDebugStringW (ss.str ().c_str ());      \
        }                                               \
    }                                                   \
  while (false)
#define DEBUG_MESSAGE_STATIC(x)                 \
  do                                            \
    {                                           \
      if (verbose_level >= 5)                   \
        OutputDebugStringA ((x));               \
    }                                           \
  while (false)
#define DEBUG_MESSAGE_RECONVERTSTRING(x)        \
  do                                            \
    {                                           \
      if (verbose_level >= 5)                   \
        debug_output_reconvert_string ((x));    \
    }                                           \
  while (false)
#endif

#define WARNING_MESSAGE_A(x)                            \
  do                                                    \
    {                                                   \
      if (verbose_level >= 3)                           \
        {                                               \
          std::stringstream ss;                         \
          ss << x;                                      \
          OutputDebugStringA (ss.str ().c_str ());      \
        }                                               \
    }                                                   \
  while (false)
#define WARNING_MESSAGE_W(x)                            \
  do                                                    \
    {                                                   \
      if (verbose_level >= 3)                           \
        {                                               \
          std::basic_stringstream<WCHAR> ss;            \
          ss << x;                                      \
          OutputDebugStringW (ss.str ().c_str ());      \
        }                                               \
    }                                                   \
  while (false)
#define WARNING_MESSAGE_STATIC(x) \
  do                              \
    {                             \
      if (verbose_level >= 3)     \
        OutputDebugStringA ((x)); \
    }                             \
  while (false)

std::string get_format_message (DWORD dwMessageId);
void debug_output_reconvert_string (RECONVERTSTRING *rs);

extern int verbose_level;

#endif // INCLUDE_GUARD_DEBUG_MESSAGE_HH
