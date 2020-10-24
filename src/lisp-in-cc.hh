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

#ifndef INCLUDE_GUARD_LISP_IN_CC_HH
#define INCLUDE_GUARD_LISP_IN_CC_HH

#include <emacs-module.h>

emacs_value
Ftr_ime_modadv__install_message_hook_hwnd (emacs_env*, ptrdiff_t,
                                           emacs_value[], void*);
extern const char *doc_tr_ime_modadv__install_message_hook_hwnd;

emacs_value
Ftr_ime_modadv__uninstall_message_hook_hwnd (emacs_env*, ptrdiff_t,
                                             emacs_value[], void*);
extern const char *doc_tr_ime_modadv__uninstall_message_hook_hwnd;

emacs_value
Ftr_ime_modadv__subclassify_hwnd (emacs_env*, ptrdiff_t, emacs_value[], void*);
extern const char *doc_tr_ime_modadv__subclassify_hwnd;

emacs_value
Ftr_ime_modadv__unsubclassify_hwnd (emacs_env*, ptrdiff_t,
                                    emacs_value[], void*);
extern const char *doc_tr_ime_modadv__unsubclassify_hwnd;

emacs_value
Ftr_ime_modadv__set_dispatch_thread_message (emacs_env*, ptrdiff_t,
                                             emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_dispatch_thread_message;

emacs_value
Ftr_ime_modadv__setopenstatus (emacs_env*, ptrdiff_t,
                               emacs_value[], void*);
extern const char *doc_tr_ime_modadv__setopenstatus;

emacs_value
Ftr_ime_modadv__getopenstatus (emacs_env*, ptrdiff_t,
                               emacs_value[], void*);
extern const char *doc_tr_ime_modadv__getopenstatus;

emacs_value
Ftr_ime_modadv__set_font (emacs_env*, ptrdiff_t,
                          emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_font;

emacs_value
Ftr_ime_modadv__set_composition_window (emacs_env*, ptrdiff_t,
                                        emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_composition_window;

emacs_value
Ftr_ime_modadv__set_startcomposition_defsubclassproc (emacs_env*, ptrdiff_t,
                                                      emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_startcomposition_defsubclassproc;

emacs_value
Ftr_ime_modadv__set_prefix_keys (emacs_env*, ptrdiff_t,
                                 emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_prefix_keys;

emacs_value
Ftr_ime_modadv__resume_prefix_key (emacs_env*, ptrdiff_t,
                                   emacs_value[], void*);
extern const char *doc_tr_ime_modadv__resume_prefix_key;

emacs_value
Ftr_ime_modadv__language_change_handler (emacs_env*, ptrdiff_t,
                                         emacs_value[], void*);
extern const char *doc_tr_ime_modadv__language_change_handler;

emacs_value
Ftr_ime_modadv__notify_reconvert_string (emacs_env*, ptrdiff_t,
                                         emacs_value[], void*);
extern const char *doc_tr_ime_modadv__notify_reconvert_string;

emacs_value
Ftr_ime_modadv__set_reconversion (emacs_env*, ptrdiff_t,
                                  emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_reconversion;

emacs_value
Ftr_ime_modadv__set_documentfeed (emacs_env*, ptrdiff_t,
                                  emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_documentfeed;

emacs_value
Ftr_ime_modadv__get_dpi (emacs_env*, ptrdiff_t,
                         emacs_value[], void*);
extern const char *doc_tr_ime_modadv__get_dpi;

emacs_value
Ftr_ime_modadv__set_verbose_level (emacs_env*, ptrdiff_t,
                                   emacs_value[], void*);
extern const char *doc_tr_ime_modadv__set_verbose_level;

#ifndef NDEBUG

emacs_value
Ftr_ime_modadv__debug_output (emacs_env*, ptrdiff_t,
                              emacs_value[], void*);
extern const char *doc_tr_ime_modadv__debug_output;

emacs_value
Ftr_ime_modadv__debug_rectangle (emacs_env*, ptrdiff_t,
                                 emacs_value[], void*);
extern const char *doc_tr_ime_modadv__debug_rectangle;

#endif // NDEBUG

#endif // INCLUDE_GUARD_LISP_IN_CC_HH
