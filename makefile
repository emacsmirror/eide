# emacs options :

# -nw     : don't use X interface
# -q      : don't load ~/.emacs
# -l file : load lisp file

# -nw option is not supported in emacs shell

all: clean
	@echo "-------------------------------------------------------------------------------"
	@echo "Compiling Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@emacs -q -l compile-eide.el
	@for file in `ls src/*.el`; do \
	   if [ -e $${file}c ]; then \
	     echo "  [OK]      $${file}" ; \
	   else \
	     echo "  [FAILED]  $${file}" ; \
	   fi ; \
     done
	@if [ -e src/eide.elc ]; then ln -vsf src/eide.elc .emacs; else ln -vsf src/eide.el .emacs; fi

debug: set_debug all

set_debug:
	@touch ".do_not_exit_after_compilation"

clean:
	@echo "-------------------------------------------------------------------------------"
	@echo "Cleaning compilation of Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@rm -vf src/*.elc
	@ln -vsf src/eide.el .emacs

# [ -h ~/.emacs ] is necessary for broken symbolic links
# (in that case [ -e ~/.emacs ] returns false)

install:
	@echo "-------------------------------------------------------------------------------"
	@echo "Installing Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@if [ -e src/eide.elc ]; then ln -vsf src/eide.elc .emacs; else ln -vsf src/eide.el .emacs; fi
	@if [ -e ~/.emacs -o -h ~/.emacs ]; then \
	   echo "WARNING : ~/.emacs already exists." ; \
	   mv -v ~/.emacs ~/.emacs_`date +%F_%T` ; \
	 fi
	@file=$${PWD}/.emacs ; ln -vs $${file} ~
	@if which ctags > /dev/null ; then \
	   if ctags --version | grep -q Exuberant ; then \
	     echo "Checking ctags..... OK." ; \
	   else \
	     echo "Checking ctags..... FAILED : your version of ctags is not \"Exuberant Ctags\"." ; \
	   fi ; \
	 else \
	   echo "Checking ctags..... FAILED : ctags is not installed." ; \
	 fi
	@if which cscope > /dev/null ; then \
	   echo "Checking cscope.... OK." ; \
	 else \
	   echo "Checking cscope.... FAILED : cscope is not installed." ; \
	 fi

uninstall:
	@echo "-------------------------------------------------------------------------------"
	@echo "Uninstalling Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@if [ -e ~/.emacs ]; then \
	   if [ -h ~/.emacs -a `ls -l ~/.emacs | grep -c $${PWD}` = "1" ]; then \
	     rm -vf ~/.emacs ; \
	   else \
	     echo "ERROR : ~/.emacs is not linked to Emacs-IDE." ; \
	   fi ; \
	 else \
	   echo "ERROR : ~/.emacs does not exist." ; \
	 fi
