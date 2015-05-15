DELITE_TEST=${DELITE_HOME}/framework/delite-test
CLASSPATH=${DELITE_TEST}/target/scala-2.10/classes/ppl/tests/scalatest
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
	dot -Tpdf deliteIR.dot > foo.pdf
	scp foo.pdf raghu@xenon.stanford.edu:
