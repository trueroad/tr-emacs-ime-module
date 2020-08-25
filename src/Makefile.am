noinst_LTLIBRARIES = tr-ime-module.la
tr_ime_module_la_SOURCES = tr-ime-module.c

pkglisp_DATA = $(objdir)/tr-ime-module$(SHREXT)

$(objdir)/tr-ime-module$(SHREXT): tr-ime-module.la

tr_ime_module_la_LIBADD = -limm32

tr_ime_module_la_LDFLAGS = -no-undefined
tr_ime_module_la_LDFLAGS += -module
tr_ime_module_la_LDFLAGS += -avoid-version
tr_ime_module_la_LDFLAGS += -rpath $(pkglispdir)
