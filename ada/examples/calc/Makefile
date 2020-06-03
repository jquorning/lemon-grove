##
##

all:
	lemon calc_ada.y -Lada -Tlempar.adb
	gnatmake calc_ada.adb
	gprbuild calc_ada.gpr -k

clean:
	rm -f *.stdout *.stderr
	rm -f *.o *.ali
	rm -f *.out

cleanall: clean
	rm -f calc_ada.ad*
