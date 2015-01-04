euler_files := $(wildcard euler*.f08)
euler_objects := $(addsuffix .out, $(basename $(euler_files)))

euler : $(euler_objects)

%.out : %.f08
	gfortran -fimplicit-none -std=f2008 $< -o $@
