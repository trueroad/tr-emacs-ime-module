#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([Emulator of GNU Emacs IME patch for Windows], [0.3.50],, [tr-ime],
  [https://github.com/trueroad/tr-emacs-ime-module])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AC_CONFIG_SRCDIR([src/tr-ime-mod.c])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIRS([m4])

PACKAGE_FILEVERSION='0,3,50,0'
PACKAGE_PRODUCTVERSION='0,3,50,0'
PACKAGE_COPYRIGHT='Copyright (C) 2020 Masamichi Hosoda.'
PACKAGE_LICENSE='License: GPL3+'

AC_DEFINE_UNQUOTED([PACKAGE_FILEVERSION], [$PACKAGE_FILEVERSION],
  [Define to the FILEVERSION of this package.])
AC_DEFINE_UNQUOTED([PACKAGE_PRODUCTVERSION], [$PACKAGE_PRODUCTVERSION],
  [Define to the PRODUCTVERSION of this package.])
AC_DEFINE_UNQUOTED([PACKAGE_COPYRIGHT], ["$PACKAGE_COPYRIGHT"],
  [Define to the copyright of this package.])
AC_DEFINE_UNQUOTED([PACKAGE_LICENSE], ["$PACKAGE_LICENSE"],
  [Define to the license of this package.])

AC_DEFINE_UNQUOTED([HOST_PLATFORM], ["$host"],
  [Define to the host platform name.])

AC_ARG_ENABLE([module2],
  AS_HELP_STRING([--enable-module2], [enable tr-ime-module2 (experimental)]),
  AS_CASE(["${enableval}"],
    [yes], [module2=true],
    [no], [module2=false],
    [*], AC_MSG_ERROR([bad value ${enableval} for --enable-module2])),
  [module2=false])
AM_CONDITIONAL([ENABLE_MODULE2], [test "x$module2" = xtrue])

# Checks for programs.
AC_PROG_CC_C99
AS_IF([test "x$ac_cv_prog_cc_c99" = xno],
  AC_MSG_ERROR([C99 compiler is not found.]))

AC_PROG_CXX
AX_CXX_COMPILE_STDCXX([14], [ext], [mandatory])

AM_PROG_AR

AS_IF([test "x$target_alias" = x],
  [target_alias=$target])
AC_CHECK_TARGET_TOOL([WINDRES], [windres], [no])
AS_IF([test "x$WINDRES" = xno],
  AC_MSG_ERROR([windres is not found.]))

AM_PATH_LISPDIR
AC_ARG_WITH([pkglispdir],
  AS_HELP_STRING([--with-pkglispdir=DIR],
    [where to install emacs dynamic modules (default lispdir/PACKAGE)]),
  [pkglispdir=${withval}], [pkglispdir='${lispdir}/${PACKAGE}'])
AC_SUBST([pkglispdir])
AC_MSG_NOTICE([where to install emacs dynamic modules is ${pkglispdir}])

# Checks for libraries.
LT_INIT([disable-static shared win32-dll])
LT_LANG([Windows Resource])
SHREXT=${shrext_cmds}
AC_SUBST([SHREXT])

AC_SUBST([objdir])

# Checks for header files.
AC_ARG_WITH([emacs-module-hdir],
  AS_HELP_STRING([--with-emacs-module-hdir=DIR],
    [directory of emacs-module.h]),
  [CPPFLAGS="${CPPFLAGS} -I ${withval}"])
AC_CHECK_HEADERS([emacs-module.h],,
  AC_MSG_ERROR([emacs-module.h is not found.]))

AC_CHECK_HEADERS([windows.h],,
  AC_MSG_ERROR([windows.h is not found.]))

AC_CHECK_HEADERS([imm.h],,
  AC_MSG_ERROR([imm.h is not found.]),
  [#include <windows.h>])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_TYPES([ptrdiff_t])

AC_LANG_PUSH([C++])
AC_MSG_CHECKING([whether the C++ compiler has __PRETTY_FUNCTION__])
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([[]],[[ const char* foo = __PRETTY_FUNCTION__; ]])],
  [AC_MSG_RESULT([yes])
   AC_DEFINE([HAVE_CXX_PRETTY_FUNCTION], [1],
     [Define if C++ compiler has __PRETTY_FUNCTION__])],
  [AC_MSG_RESULT([no])])
AC_LANG_POP([C++])

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 lisp/Makefile
                 src/Makefile
                 w32-ime/Makefile])
AC_OUTPUT
