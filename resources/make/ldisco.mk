DISCO_DIR=deps/disco
DISCO_PROJECT=deps/disco-project
DISCO_GIT=git://github.com/discoproject/disco.git
DISCO_VERSION=0.5.3

compile-proj: get-deps
	@echo "Compiling project code and dependencies ..."
	@which rebar.cmd >/dev/null 2>&1 && \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar.cmd compile || \
	PATH=$(SCRIPT_PATH) ERL_LIBS=$(ERL_LIBS) rebar compile

juggle-disco:
	@echo "Extracting Erlang disco library ..."
	@mv $(DISCO_PROJECT)/master deps/disco && \
	rm -rf $(DISCO_PROJECT)

get-disco:
	@echo "Getting disco source ..."
	@git clone $(DISCO_GIT) $(DISCO_PROJECT) && \
	cd $(DISCO_PROJECT) && \
	git checkout tags/$(DISCO_VERSION)

get-disco-deps:
	@echo "Getting disco dependencies ..."
	@cd $(DISCO_DIR) && \
	which rebar.cmd >/dev/null 2>&1 && \
	rebar.cmd get-deps || \
	rebar get-deps

compile-disco-deps:
	@echo "Compiling disco and dependencies ..."
	@cd $(DISCO_DIR) && \
	which rebar.cmd >/dev/null 2>&1 && \
	rebar.cmd compile || \
	rebar compile && \
	mv deps/* ../

$(DISCO_DIR):
	@make get-disco
	@make juggle-disco
	@make get-disco-deps
	@make compile-disco-deps

compile-erl-disco: juggle-erl-disco
	@echo "Compiling Erlang disco library ..."

compile: clean-ebin $(DISCO_DIR) compile-proj
