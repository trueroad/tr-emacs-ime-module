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

#ifndef INCLUDE_GUARD_LISP_IN_CC_HH
#define INCLUDE_GUARD_LISP_IN_CC_HH

#include <emacs-module.h>

emacs_value
Fw32_tr_ime_install_message_hook_hwnd (emacs_env*, ptrdiff_t,
                                       emacs_value[], void*);
extern const char *doc_w32_tr_ime_install_message_hook_hwnd;

emacs_value
Fw32_tr_ime_uninstall_message_hook_hwnd (emacs_env*, ptrdiff_t,
                                         emacs_value[], void*);
extern const char *doc_w32_tr_ime_uninstall_message_hook_hwnd;

emacs_value
Fw32_tr_ime_subclassify_hwnd (emacs_env*, ptrdiff_t, emacs_value[], void*);
extern const char *doc_w32_tr_ime_subclassify_hwnd;

emacs_value
Fw32_tr_ime_unsubclassify_hwnd (emacs_env*, ptrdiff_t, emacs_value[], void*);
extern const char *doc_w32_tr_ime_unsubclassify_hwnd;

emacs_value
Fw32_tr_ime_set_dispatch_thread_message (emacs_env*, ptrdiff_t,
                                         emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_dispatch_thread_message;

emacs_value
Fw32_tr_ime_setopenstatus2 (emacs_env*, ptrdiff_t,
                            emacs_value[], void*);
extern const char *doc_w32_tr_ime_setopenstatus2;

emacs_value
Fw32_tr_ime_getopenstatus2 (emacs_env*, ptrdiff_t,
                            emacs_value[], void*);
extern const char *doc_w32_tr_ime_getopenstatus2;

emacs_value
Fw32_tr_ime_set_font (emacs_env*, ptrdiff_t,
                      emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_font;

emacs_value
Fw32_tr_ime_set_composition_window (emacs_env*, ptrdiff_t,
                                    emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_composition_window;

emacs_value
Fw32_tr_ime_set_startcomposition_defsubclassproc (emacs_env*, ptrdiff_t,
                                                  emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_startcomposition_defsubclassproc;

emacs_value
Fw32_tr_ime_set_prefix_keys (emacs_env*, ptrdiff_t,
                             emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_prefix_keys;

emacs_value
Fw32_tr_ime_resume_prefix_key (emacs_env*, ptrdiff_t,
                               emacs_value[], void*);
extern const char *doc_w32_tr_ime_resume_prefix_key;

emacs_value
Fw32_tr_ime_language_change_handler (emacs_env*, ptrdiff_t,
                                     emacs_value[], void*);
extern const char *doc_w32_tr_ime_language_change_handler;

emacs_value
Fw32_tr_ime_notify_reconvert_string (emacs_env*, ptrdiff_t,
                                     emacs_value[], void*);
extern const char *doc_w32_tr_ime_notify_reconvert_string;

emacs_value
Fw32_tr_ime_set_reconversion (emacs_env*, ptrdiff_t,
                              emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_reconversion;

emacs_value
Fw32_tr_ime_set_documentfeed (emacs_env*, ptrdiff_t,
                              emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_documentfeed;

emacs_value
Fw32_tr_ime_get_dpi (emacs_env*, ptrdiff_t,
                     emacs_value[], void*);
extern const char *doc_w32_tr_ime_get_dpi;

emacs_value
Fw32_tr_ime_set_verbose_level (emacs_env*, ptrdiff_t,
                               emacs_value[], void*);
extern const char *doc_w32_tr_ime_set_verbose_level;

#ifndef NDEBUG

emacs_value
Fw32_tr_ime_debug_output (emacs_env*, ptrdiff_t,
                          emacs_value[], void*);
extern const char *doc_w32_tr_ime_debug_output;

emacs_value
Fw32_tr_ime_debug_rectangle (emacs_env*, ptrdiff_t,
                             emacs_value[], void*);
extern const char *doc_w32_tr_ime_debug_rectangle;

#endif // NDEBUG

#endif // INCLUDE_GUARD_LISP_IN_CC_HH
