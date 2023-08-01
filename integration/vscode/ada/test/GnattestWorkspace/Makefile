include ../Makefile.inc

tagged_rec: tagged substitution

tagged:
	$(PREFIX)gnattest -Ptagged_rec.gpr $(QUIET)
	$(PREFIX)gprbuild -Pobj/gnattest/harness/test_driver.gpr $(QUIET)
ifeq ($(RUN),true)
	obj/gnattest/harness/test_runner$(EXE)
endif

substitution:
	$(PREFIX)gnattest -Ptagged_rec.gpr --validate-type-extensions $(QUIET)
	$(PREFIX)gprbuild -f -Pobj/gnattest/harness/test_driver.gpr $(QUIET)
ifeq ($(RUN),true)
	obj/gnattest/harness/test_runner$(EXE)
endif

clean:
	$(PREFIX)gprclean -Ptagged_rec.gpr
	$(RM) -r obj/gnattest/harness
	$(RM) obj/gnattest/tests/speed1-controller_test_data.ad?
	$(RM) obj/gnattest/tests/speed1-controller_test_data-controller_tests.ads
	$(RM) obj/gnattest/tests/speed1-test_data*
	$(RM) obj/gnattest/tests/speed2-auto_controller_test_data-auto_controller_tests.ads
	$(RM) obj/gnattest/tests/speed2-auto_controller_test_data.ad?
	$(RM) obj/gnattest/tests/speed2-test_data*

.PHONY: clean tagged substitution
