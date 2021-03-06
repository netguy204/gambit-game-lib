GAMBIT_ROOT?=/usr/local/Gambit-C
GSC=$(GAMBIT_ROOT)/bin/gsc

SCM_VERSION?=1
SCMLIB=$(OUTPUT).o$(SCM_VERSION)
SCM_C=$(patsubst %.scm,%.c,$(SCM_SRC)) $(SCMLIB).c
SCM_O=$(patsubst %.c,%.o,$(SCM_C))

all: $(SCMLIB)

$(SCMLIB): $(SCM_SRC)
	$(GSC) -keep-c -cc-options "-D___DYNAMIC $(CFLAGS)" -ld-options "$(LDFLAGS)" -o $(SCMLIB) $(SCM_SRC)

clean:
	rm -rf $(SCMLIB) $(SCM_C) $(SCM_O)
