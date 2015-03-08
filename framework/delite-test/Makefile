TEST_PKG=ppl.tests.scalatest
OBJ_SUFFIX=_O

all:
	cd ${DELITE_HOME} && sbt publish-local

distclean:
	sbt clean

clean:
	rm -rf generated *.deg

%${OBJ_SUFFIX}:
	${DELITE_HOME}/bin/delitec ${ARG} ${TEST_PKG}.$@
