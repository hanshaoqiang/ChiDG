#!/bin/python
import chidg
sim = chidg.sim()
 
 
toptions = chidg.dict()
loptions = chidg.dict()
noptions = chidg.dict()
poptions = chidg.dict()
 
 
sim.init('env')
sim.set_accuracy(1)
 
 
sim%read_grid('smoothbump.h5', 3)
sim%read_boundaryconditions('smoothbump.h5')
 
 
toptions%set('dt',0.001)
toptions%set('nsteps',100)
toptions%set('nwrite',100)
 
 
noptions%set('tol',1.e-6)
noptions%set('cfl0',3.0)
noptions%set('nsteps',100)
 
 
loptions%set('tol',1.e-8)
 
 
sim%set('time_scheme','steady', toptions)
sim%set('nonlinear_solver','quasi_newton', noptions)
sim%set('linear_solver','FGMRES', loptions)
sim%set('preconditioner','ILU0', poptions)
 
 
sim%initialize_solution_domains(1)
sim%init('chimera')
sim%initialize_solution_solver()
 
 
sim.read_solution('smoothbump.h5')
 
 
sim.run()
sim.report()
