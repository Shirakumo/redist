.DEFAULT_GOAL := all

LISPC?=sbcl

build:
	$(LISPC) \
		--eval '(pushnew (uiop:getcwd) asdf:*central-registry*)' \
		--eval '(ql:quickload :sqlite)' \
		--eval '(asdf:make :redist)'

all: build
