# cob2csv Makefile
# Requires GnuCOBOL: sudo apt install gnucobol

COBC    = cobc
COBFLAGS = -x -free -O2
SRCDIR  = src
BINDIR  = bin

.PHONY: all clean test install

all: $(BINDIR)/cob2csv $(BINDIR)/csv2cob

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINDIR)/cob2csv: $(SRCDIR)/cob2csv.cbl | $(BINDIR)
	$(COBC) $(COBFLAGS) -o $@ $<
	@echo "Built: $@"

$(BINDIR)/csv2cob: $(SRCDIR)/csv2cob.cbl | $(BINDIR)
	$(COBC) $(COBFLAGS) -o $@ $<
	@echo "Built: $@"

test: all
	@chmod +x tests/run_tests.sh
	@bash tests/run_tests.sh

clean:
	rm -rf $(BINDIR)

install: all
	install -m 755 $(BINDIR)/cob2csv /usr/local/bin/
	install -m 755 $(BINDIR)/csv2cob /usr/local/bin/
	@echo "Installed to /usr/local/bin/"
