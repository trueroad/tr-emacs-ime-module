#
# Targets
#

noinst_LTLIBRARIES = tr-ime-mod.la
pkglisp_DATA = tr-ime-mod-$(MOD_ABI_VERSION)-$(host)$(SHREXT)
CLEANFILES = tr-ime-mod-$(MOD_ABI_VERSION)-$(host)$(SHREXT)

if ENABLE_MODULE2
noinst_LTLIBRARIES += tr-ime-modadv.la
pkglisp_DATA += tr-ime-modadv-$(MODADV_ABI_VERSION)-$(host)$(SHREXT)
CLEANFILES += tr-ime-modadv-$(MODADV_ABI_VERSION)-$(host)$(SHREXT)
endif

#
# For tr-ime-mod
#

tr_ime_mod_la_SOURCES = tr-ime-mod.c tr-ime-mod-resource.rc

tr_ime_mod_la_LIBADD = -limm32

tr_ime_mod_la_LDFLAGS = -no-undefined
tr_ime_mod_la_LDFLAGS += -module
tr_ime_mod_la_LDFLAGS += -avoid-version
tr_ime_mod_la_LDFLAGS += -rpath $(pkglispdir)

$(objdir)/tr-ime-mod$(SHREXT): tr-ime-mod.la

tr-ime-mod-$(MOD_ABI_VERSION)-$(host)$(SHREXT): \
		$(objdir)/tr-ime-mod$(SHREXT)
	cp -p $< $@

#
# For tr-ime-modadv
#

tr_ime_modadv_la_SOURCES = tr-ime-modadv.cc tr-ime-modadv-resource.rc \
	debug-message.cc debug-message.hh lisp-in-cc.cc lisp-in-cc.hh \
	get_msg_hook.cc get_msg_hook.hh get_msg_proc.cc get_msg_proc.hh \
	message.cc message.hh subclass_proc.cc subclass_proc.hh \
	queue.cc queue.hh

tr_ime_modadv_la_LIBADD = -lgdi32 -lcomctl32 -limm32

tr_ime_modadv_la_LDFLAGS = -no-undefined
tr_ime_modadv_la_LDFLAGS += -module
tr_ime_modadv_la_LDFLAGS += -avoid-version
tr_ime_modadv_la_LDFLAGS += -rpath $(pkglispdir)

$(objdir)/tr-ime-modadv$(SHREXT): tr-ime-modadv.la

tr-ime-modadv-$(MODADV_ABI_VERSION)-$(host)$(SHREXT): \
		$(objdir)/tr-ime-modadv$(SHREXT)
	cp -p $< $@

#
# For Windows Resource Script (.rc)
#

WINDRESFLAGS = $(DEFAULT_INCLUDES)

.rc.lo:
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
		--tag=RC --mode=compile \
		$(WINDRES) $(WINDRESFLAGS) -i $< -o $@
