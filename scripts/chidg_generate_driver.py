#!/bin/python


# Line terminator
ENDLINE = "\n"


# Open new python driver
driver = open('driver.py', 'w')


# Write scripting language
driver.write("#!/bin/python" + ENDLINE)
driver.write("import chidg" + ENDLINE)
driver.write("sim = chidg.sim()" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("toptions = chidg.dict()" + ENDLINE)
driver.write("loptions = chidg.dict()" + ENDLINE)
driver.write("noptions = chidg.dict()" + ENDLINE)
driver.write("poptions = chidg.dict()" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim.init('env')" + ENDLINE)
driver.write("sim.set_accuracy(1)" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim%read_grid('smoothbump.h5', 3)" + ENDLINE)
driver.write("sim%read_boundaryconditions('smoothbump.h5')" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("toptions%set('dt',0.001)" + ENDLINE)
driver.write("toptions%set('nsteps',100)" + ENDLINE)
driver.write("toptions%set('nwrite',100)" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("noptions%set('tol',1.e-6)" + ENDLINE)
driver.write("noptions%set('cfl0',3.0)" + ENDLINE)
driver.write("noptions%set('nsteps',100)" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("loptions%set('tol',1.e-8)" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim%set('time_scheme','steady', toptions)" + ENDLINE)
driver.write("sim%set('nonlinear_solver','quasi_newton', noptions)" + ENDLINE)
driver.write("sim%set('linear_solver','FGMRES', loptions)" + ENDLINE)
driver.write("sim%set('preconditioner','ILU0', poptions)" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim%initialize_solution_domains(1)" + ENDLINE)
driver.write("sim%init('chimera')" + ENDLINE)
driver.write("sim%initialize_solution_solver()" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim.read_solution('smoothbump.h5')" + ENDLINE)
driver.write(" " + ENDLINE)
driver.write(" " + ENDLINE)
driver.write("sim.run()" + ENDLINE)
driver.write("sim.report()" + ENDLINE)







