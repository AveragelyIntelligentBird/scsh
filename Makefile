srcdir = .

CC = gcc
LD = ${CC}
DEFS = -DHAVE_CONFIG_H
LIBS = 
CFLAGS = -g -O2 -fPIC
CPPFLAGS =  -I/usr/local/include
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

LDFLAGS =  -rdynamic -shared  -rdynamic


prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
libdir = ${exec_prefix}/lib
incdir = ${prefix}/include
manext = 1
mandir = ${datarootdir}/man/man$(manext)
docdir = ${datarootdir}/doc/${PACKAGE_TARNAME}
datarootdir = ${prefix}/share
datadir = ${datarootdir}

VERSION = 0.7
RUNNABLE = scsh
LIB = $(libdir)/scsh-$(VERSION)
SHARE = $(datadir)/scsh-$(VERSION)
lib_dirs_list = ("${prefix}/lib/scsh/modules" "${prefix}/lib/scsh/modules/${VERSION}")

SCHEME48VERSION =  1.9.3
SCHEME48VM = /usr/local/lib/scheme48-${SCHEME48VERSION}/scheme48vm
SCHEME48 = /usr/local/bin/scheme48

COMPILED_LIBS = c/syscalls.so \
	c/tty.so \
	c/time.so \
	c/time-ticks-sec.so \
	c/tty-baud-rate-flags.so \
	c/tty-control-chars-info.so \
	c/tty-control-flags.so \
	c/tty-input-flags.so \
	c/tty-local-flags.so \
	c/tty-output-flags.so \
	c/tty-tcflow-flags.so \
	c/tty-tcflush-flags.so \
	c/tty-tcsetattr-flags.so

enough: scsh scsh.image go

test: enough
	$(srcdir)/build/test.sh $(srcdir)

.SUFFIXES: .c .so
.c.so:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $< $(LIBS)

SCHEME = scheme/command-line.scm \
	 scheme/condition-handler.scm \
	 scheme/constance.scm \
	 scheme/continuation.scm \
	 scheme/directory.scm \
	 scheme/enumconst.scm \
	 scheme/environment.scm \
	 scheme/event.scm \
	 scheme/ports/fdports.scm \
	 scheme/file.scm \
	 scheme/fileinfo.scm \
	 scheme/filesys.scm \
	 scheme/fname.scm \
	 scheme/fname-system.scm \
	 scheme/fr.scm \
	 scheme/glob.scm \
	 scheme/dot-locking.scm \
	 scheme/here.scm \
	 scheme/lib-dirs.scm \
	 scheme/libscsh.scm \
	 scheme/low-interrupt.scm \
	 scheme/md5.scm \
	 scheme/meta-arg.scm \
	 scheme/ports/fdports.scm \
	 scheme/ports/fdport-internal.scm \
	 scheme/ports/fdport-ops.scm \
	 scheme/ports/port-collect.scm \
	 scheme/ports/bufpol.scm \
	 scheme/process-high-level.scm \
	 scheme/process-state.scm \
	 scheme/process.scm \
	 scheme/procobj.scm \
	 scheme/pty.scm \
	 scheme/rdelim.scm \
	 scheme/resource.scm \
	 scheme/scsh-condition.scm \
	 scheme/scsh-interfaces.scm \
	 scheme/scsh-package.scm \
	 scheme/scsh-read.scm \
	 scheme/scsh-version.scm \
	 scheme/signal.scm \
	 scheme/startup.scm \
	 scheme/stdio.scm \
	 scheme/stringcoll.scm \
	 scheme/syntax-helpers.scm \
	 scheme/syntax.scm \
	 scheme/system.scm \
	 scheme/temp-file.scm \
	 scheme/top.scm \
	 scheme/tty.scm \
	 scheme/tty-consts.scm \
	 scheme/user-group.scm \
	 scheme/utilities.scm \
	 scheme/weaktables.scm \
	 scheme/time.scm \
	 rx/packages.scm \
	 rx/re-match-syntax.scm \
	 rx/rx-lib.scm \
	 rx/parse.scm \
	 rx/re-subst.scm \
	 rx/simp.scm \
	 rx/posixstr.scm \
	 rx/re-syntax.scm \
	 rx/spencer.scm \
	 rx/re-fold.scm \
	 rx/re.scm \
	 rx/re-high.scm \
	 rx/re-low.scm \
	 rx/regress.scm

go: c/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"scsh.image\" \
	$(srcdir)/c/scsh-tramp.c

scsh: c/scsh-tramp.c
	$(CC) -o $@ $(CFLAGS) \
	-DVM=\"$(SCHEME48VM)\" \
	-DIMAGE=\"$(LIB)/scsh.image\" \
	$(srcdir)/c/scsh-tramp.c

LOADS = $(srcdir)/rx/interfaces.scm \
	$(srcdir)/rx/packages.scm \
	$(srcdir)/scheme/scsh-interfaces.scm \
	$(srcdir)/scheme/scsh-package.scm \
	$(srcdir)/scheme/lib/ccp-pack.scm \
	$(srcdir)/scheme/lib/char-package.scm

scsh.image: $(SCHEME) $(COMPILED_LIBS)
	$(srcdir)/build/build-image.sh $(srcdir) \
		"`pwd`/c/" '$@' '$(SCHEME48) -h 0' '$(LOADS)'

dirs:
	for dir in $(bindir) $(LIB) $(SHARE); do\
		{ mkdir -p $(DESTDIR)$$dir && [ -w $(DESTDIR)$$dir ]; } || {	\
			echo "$(DESTDIR)$$dir not a writable directory" >&2;	\
			exit 1;						\
	}								\
	done

install: enough dirs install-scsh-image install-scsh

install-scsh: scsh
        #install runner
	rm -f $(DESTDIR)$(bindir)/$(RUNNABLE)
	$(INSTALL_PROGRAM) scsh $(DESTDIR)$(bindir)/$(RUNNABLE)

	for file in $(patsubst c/%,%,$(COMPILED_LIBS)) ; do \
	$(INSTALL) c/$$file $(DESTDIR)$(LIB)/$$file ; \
	done
        #install scheme source files
	for f in $(srcdir)/scheme/*.scm $(srcdir)/scheme/*/*.scm; \
	    do $(INSTALL_DATA) $$f $(DESTDIR)$(SHARE)/; done

install-scsh-image: install-scsh
	$(srcdir)/build/build-image.sh $(srcdir) \
		"$(LIB)/" '$(DESTDIR)$(LIB)/scsh.image' '$(SCHEME48) -h 0' '$(LOADS)'

refman:
	rm -rf $(srcdir)/doc/man/scsh/ $(srcdir)/doc/man/compiled
	scribble --dest $(srcdir)/doc/man --htmls $(srcdir)/doc/man/scsh.scrbl 

clean:
	rm -rf c/*.o c/*.so c/*.dSYM *.sexpr scsh.image scsh go *.dSYM 
