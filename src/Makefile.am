noinst_LTLIBRARIES = tr-ime-module.la
tr_ime_module_la_SOURCES = tr-ime-module.c tr-ime-module-resource.rc

WINDRESFLAGS = $(DEFAULT_INCLUDES)

.rc.lo:
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
		--tag=RC --mode=compile \
		$(WINDRES) $(WINDRESFLAGS) -i $< -o $@

pkglisp_DATA = tr-ime-module-$(host)$(SHREXT)
CLEANFILES = tr-ime-module-$(host)$(SHREXT)

$(objdir)/tr-ime-module$(SHREXT): tr-ime-module.la

tr-ime-module-$(host)$(SHREXT): $(objdir)/tr-ime-module$(SHREXT)
	cp -p $< $@

tr_ime_module_la_LIBADD = -limm32

tr_ime_module_la_LDFLAGS = -no-undefined
tr_ime_module_la_LDFLAGS += -module
tr_ime_module_la_LDFLAGS += -avoid-version
tr_ime_module_la_LDFLAGS += -rpath $(pkglispdir)
