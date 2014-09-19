DIFF=../test-xmldiff

TESTS=flat foo foo_foo

KO='\e[1;31m'"*KO\033[1m\033[0m"
OK='\e[1;32m'"OK\033[1m\033[0m"

all:
	@for i in $(TESTS); do \
		($(DIFF) $${i}.xml $${i}2.xml > $${i}.diff && \
		 diff $${i}.ref $${i}.diff && /bin/echo -e $(OK) $$i) \
		 || /bin/echo -e $(KO)" $$i"; \
	done

clean:
	rm -f *.diff