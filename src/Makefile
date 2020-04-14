# Emacs invocation
EMACS_COMMAND   := emacs

# Use -q to have /usr/local/share/emacs/site-lisp and subdirs in load-path
EMACS		:= $(EMACS_COMMAND) -Q -q -batch

EVAL   := $(EMACS) --eval
SCRIPT := $(EMACS) --script

ASYNC     := ~/.emacs.d/site-lisp/async-20200113.1745
XELB      := ~/.emacs.d/site-lisp/xelb-20190914.0
EXWM      := ~/.emacs.d/site-lisp/exwm-20200329.0
DASH      := ~/.emacs.d/site-lisp/dash-20200119.2310
DASH-DOCS := ~/.emacs.d/site-lisp/dash-docs-20190516.1702
COMPANY   := ~/.emacs.d/site-lisp/company-20200324.2145
FLYCHECK  := ~/.emacs.d/site-lisp/flycheck-20200405.2310

# Additional emacs loadpath
LOADPATH	:= -L $(DASH) -L $(XELB) -L $(EXWM) -L $(DASH-DOCS) -L $(ASYNC)  -L $(COMPANY) -L $(FLYCHECK)

# Scritps to eval
TANGLE			:= tangle.el

# Files to compile
INIT			:= init.el

# Compiled files
ELC			:= $(EL:.el=.elc)

.PHONY: all

tangle:
	$(SCRIPT) $(TANGLE)

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(INIT)

all: tangle batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)
