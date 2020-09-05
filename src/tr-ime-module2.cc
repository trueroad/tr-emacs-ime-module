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

#define TR_IME_MODULE2_DLL __declspec(dllexport)

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <array>

#include <windows.h>

#include <emacs-module.h>

#include "debug-message.hh"

int TR_IME_MODULE2_DLL plugin_is_GPL_compatible;

namespace
{
  void
  provide_feature (emacs_env *env, const char *name)
  {
    emacs_value provide = env->intern (env, "provide");
    emacs_value feature = env->intern (env, name);
    std::array<emacs_value, 1> args {feature};

    env->funcall (env, provide, args.size (), args.data ());
  }
};

int TR_IME_MODULE2_DLL
emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  DEBUG_MESSAGE ("enter\n");

  if (ert->size < sizeof (*ert))
    {
      WARNING_MESSAGE ("ert->size < sizeof (*ert)\n");
      return 1;
    }

  emacs_env *env = ert->get_environment (ert);
  if (env->size < sizeof (*env))
    {
      WARNING_MESSAGE ("env->size < sizeof (*env)\n");
      return 2;
    }

  provide_feature (env, "tr-ime-module2");

  return 0;
}
