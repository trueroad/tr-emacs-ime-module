#
# Targets
#

noinst_LTLIBRARIES = tr-ime-module.la
pkglisp_DATA = tr-ime-module-$(host)$(SHREXT)
CLEANFILES = tr-ime-module-$(host)$(SHREXT)

if ENABLE_MODULE2
noinst_LTLIBRARIES += tr-ime-module2.la
pkglisp_DATA += tr-ime-module2-$(host)$(SHREXT)
CLEANFILES += tr-ime-module2-$(host)$(SHREXT)
endif

#
# For tr-ime-module
#

tr_ime_module_la_SOURCES = tr-ime-module.c tr-ime-module-resource.rc

tr_ime_module_la_LIBADD = -limm32

tr_ime_module_la_LDFLAGS = -no-undefined
tr_ime_module_la_LDFLAGS += -module
tr_ime_module_la_LDFLAGS += -avoid-version
tr_ime_module_la_LDFLAGS += -rpath $(pkglispdir)

$(objdir)/tr-ime-module$(SHREXT): tr-ime-module.la

tr-ime-module-$(host)$(SHREXT): $(objdir)/tr-ime-module$(SHREXT)
	cp -p $< $@

#
# For tr-ime-module2
#

tr_ime_module2_la_SOURCES = tr-ime-module2.cc tr-ime-module2-resource.rc \
	debug-message.hh lisp-in-cc.cc lisp-in-cc.hh \
	get_msg_hook.cc get_msg_hook.hh get_msg_proc.cc get_msg_proc.hh \
	message.cc message.hh subclass_proc.cc subclass_proc.hh

tr_ime_module2_la_LIBADD = -lgdi32 -lcomctl32 -limm32

tr_ime_module2_la_LDFLAGS = -no-undefined
tr_ime_module2_la_LDFLAGS += -module
tr_ime_module2_la_LDFLAGS += -avoid-version
tr_ime_module2_la_LDFLAGS += -rpath $(pkglispdir)

$(objdir)/tr-ime-module2$(SHREXT): tr-ime-module2.la

tr-ime-module2-$(host)$(SHREXT): $(objdir)/tr-ime-module2$(SHREXT)
	cp -p $< $@

#
# For Windows Resource Script (.rc)
#

WINDRESFLAGS = $(DEFAULT_INCLUDES)

.rc.lo:
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
		--tag=RC --mode=compile \
		$(WINDRES) $(WINDRESFLAGS) -i $< -o $@
