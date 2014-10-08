DIFF=../test-xmldiff

TESTS=flat root foo_foo childof \
	cdata children morechildren \
	multichildren multideep multideepattr multimulti \
	one_two_three

TESTS_LARGE=index index_stog install moult

KO='\e[1;31m'"*KO\033[1m\033[0m"
OK='\e[1;32m'"OK\033[1m\033[0m"

all: $(DIFF)
	@for i in $(TESTS); do \
		($(DIFF) $${i}.xml $${i}2.xml > $${i}.diff && \
		 diff $${i}.ref $${i}.diff && /bin/echo -e $(OK) $$i) \
		 || /bin/echo -e $(KO)" $$i"; \
		($(DIFF) $${i}2.xml $${i}.xml > $${i}-inv.diff && \
		 diff $${i}-inv.ref $${i}-inv.diff && /bin/echo -e $(OK) $${i} \(inv\)) \
		 || /bin/echo -e $(KO)" $${i}" \(inv\); \
	done
	@for i in $(TESTS_LARGE); do \
		($(DIFF) $${i}.xml $${i}2.xml > $${i}.diff && /bin/echo -e $(OK) $$i) \
		 || /bin/echo -e $(KO)" $$i"; \
		($(DIFF) $${i}2.xml $${i}.xml > $${i}-inv.diff && /bin/echo -e $(OK) $${i} \(inv\)) \
		 || /bin/echo -e $(KO)" $${i}" \(inv\); \
	done

$(DIFF):
	cd .. && $(MAKE) `basename $@`

clean:
	rm -f *.diff
