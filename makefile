# emacs options :
# -nw     : don't use X interface
# -q      : don't load ~/.emacs
# -l file : load lisp file

# -nw option is not supported in emacs shell ($TERM = "dumb")

all: clean
	@echo "-------------------------------------------------------------------------------"
	@echo "Compiling Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@if [ "$${TERM}" = "dumb" ]; then \
	   emacs -q -l compile-eide.el ; \
	 else \
	   emacs -nw -q -l compile-eide.el ; \
	 fi
	@echo "====> These files have been successfully compiled :"
	@for file in `ls *.el`; do if [ -e $${file}c ]; then echo "  $${file} -> $${file}c"; fi; done
	@echo "====> These files have not been compiled - either failed or not requested :"
	@for file in `ls *.el`; do if [ ! -e $${file}c ]; then echo "  $${file}"; fi; done

debug: set_debug all

set_debug:
	@touch ".do_not_exit_after_compilation"

clean:
	@echo "-------------------------------------------------------------------------------"
	@echo "Cleaning compilation of Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@rm -vf *.elc

# [ -h ~/.emacs ] is necessary for broken symbolic links
# (in that case [ -e ~/.emacs ] returns false)

install:
	@echo "-------------------------------------------------------------------------------"
	@echo "Installing Emacs-IDE..."
	@echo "-------------------------------------------------------------------------------"
	@do_install="1" ; \
	 if [ -e ~/.emacs -o -h ~/.emacs ]; then \
	   if [ -h ~/.emacs -a `ls -l ~/.emacs | grep -c $${PWD}` = "1" ]; then \
	     echo "Error : ~/.emacs is already linked to Emacs-IDE." ; \
	     do_install="0" ; \
	   else \
	     echo "Warning : ~/.emacs already exists." ; \
	     mv -v ~/.emacs ~/.emacs_`date +%F_%T` ; \
	   fi ; \
	 fi ; \
	 if [ $${do_install} = "1" ]; then \
	   file=$${PWD}/.emacs ; ln -vs $${file} ~ ; \
	 fi

uninstall:
	@echo "Uninstalling Emacs-IDE..."
	@if [ -e ~/.emacs ]; then \
	   if [ -h ~/.emacs -a `ls -l ~/.emacs | grep -c $${PWD}` = "1" ]; then \
	     rm -vf ~/.emacs ; \
	   else \
	     echo "Error : ~/.emacs is not linked to Emacs-IDE." ; \
	   fi ; \
	 else \
	   echo "Error : ~/.emacs does not exist." ; \
	 fi
