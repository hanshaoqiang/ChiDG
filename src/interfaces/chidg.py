import _chidg
import f90wrap.runtime
import logging

class Type_Chidg(f90wrap.runtime.FortranModule):
    """
    Module type_chidg
    
    
    Defined at type_chidg_python.f90 lines 1-288
    
    """
    class Chidg_T(f90wrap.runtime.FortranDerivedType):
        """
        Type(name=chidg_t)
        
        
        Defined at type_chidg_python.f90 lines 16-18
        
        """
        def init(self, level):
            """
            init(self, level)
            
            
            Defined at type_chidg_python.f90 lines 38-42
            
            Parameters
            ----------
            self : Chidg_T
            level : str
            
            """
            _chidg.f90wrap_init(self=self._handle, level=level)
        
        def set(self, selector, selection, options):
            """
            set(self, selector, selection, options)
            
            
            Defined at type_chidg_python.f90 lines 65-71
            
            Parameters
            ----------
            self : Chidg_T
            selector : str
            selection : str
            options : Dict_T
            
            """
            _chidg.f90wrap_set(self=self._handle, selector=selector, selection=selection, \
                options=options._handle)
        
        def read_grid(self, gridfile, spacedim):
            """
            read_grid(self, gridfile, spacedim)
            
            
            Defined at type_chidg_python.f90 lines 96-101
            
            Parameters
            ----------
            self : Chidg_T
            gridfile : str
            spacedim : int
            
            """
            _chidg.f90wrap_read_grid(self=self._handle, gridfile=gridfile, \
                spacedim=spacedim)
        
        def read_boundaryconditions(self, gridfile):
            """
            read_boundaryconditions(self, gridfile)
            
            
            Defined at type_chidg_python.f90 lines 123-127
            
            Parameters
            ----------
            self : Chidg_T
            gridfile : str
            
            """
            _chidg.f90wrap_read_boundaryconditions(self=self._handle, gridfile=gridfile)
        
        def read_solution(self, solutionfile):
            """
            read_solution(self, solutionfile)
            
            
            Defined at type_chidg_python.f90 lines 144-148
            
            Parameters
            ----------
            self : Chidg_T
            solutionfile : str
            
            """
            _chidg.f90wrap_read_solution(self=self._handle, solutionfile=solutionfile)
        
        def initialize_solution_domains(self, nterms_s):
            """
            initialize_solution_domains(self, nterms_s)
            
            
            Defined at type_chidg_python.f90 lines 164-168
            
            Parameters
            ----------
            self : Chidg_T
            nterms_s : int
            
            """
            _chidg.f90wrap_initialize_solution_domains(self=self._handle, nterms_s=nterms_s)
        
        def initialize_solution_solver(self):
            """
            initialize_solution_solver(self)
            
            
            Defined at type_chidg_python.f90 lines 185-188
            
            Parameters
            ----------
            self : Chidg_T
            
            """
            _chidg.f90wrap_initialize_solution_solver(self=self._handle)
        
        def write_solution(self, solutionfile):
            """
            write_solution(self, solutionfile)
            
            
            Defined at type_chidg_python.f90 lines 204-209
            
            Parameters
            ----------
            self : Chidg_T
            solutionfile : str
            
            """
            _chidg.f90wrap_write_solution(self=self._handle, solutionfile=solutionfile)
        
        def run(self):
            """
            run(self)
            
            
            Defined at type_chidg_python.f90 lines 227-230
            
            Parameters
            ----------
            self : Chidg_T
            
            """
            _chidg.f90wrap_run(self=self._handle)
        
        def report(self):
            """
            report(self)
            
            
            Defined at type_chidg_python.f90 lines 251-254
            
            Parameters
            ----------
            self : Chidg_T
            
            """
            _chidg.f90wrap_report(self=self._handle)
        
        def close(self):
            """
            close(self)
            
            
            Defined at type_chidg_python.f90 lines 273-276
            
            Parameters
            ----------
            self : Chidg_T
            
            """
            _chidg.f90wrap_close(self=self._handle)
        
        def __init__(self, handle=None):
            """
            self = Chidg_T()
            
            
            Defined at type_chidg_python.f90 lines 16-18
            
            
            Returns
            -------
            this : Chidg_T
            	Object to be constructed
            
            
            Automatically generated constructor for chidg_t
            """
            f90wrap.runtime.FortranDerivedType.__init__(self)
            self._handle = _chidg.f90wrap_chidg_t_initialise()
        
        def __del__(self):
            """
            Destructor for class Chidg_T
            
            
            Defined at type_chidg_python.f90 lines 16-18
            
            Parameters
            ----------
            this : Chidg_T
            	Object to be destructed
            
            
            Automatically generated destructor for chidg_t
            """
            if self._alloc:
                _chidg.f90wrap_chidg_t_finalise(this=self._handle)
        
        _dt_array_initialisers = []
        
    _dt_array_initialisers = []
    

type_chidg = Type_Chidg()

class Type_Dict(f90wrap.runtime.FortranModule):
    """
    Module type_dict
    
    
    Defined at type_dict_python.f90 lines 12-75
    
    """
    class Dict_T(f90wrap.runtime.FortranDerivedType):
        """
        Type(name=dict_t)
        
        
        Defined at type_dict_python.f90 lines 27-29
        
        """
        def get_real(self, key, val):
            """
            get_real(self, key, val)
            
            
            Defined at type_dict_python.f90 lines 42-47
            
            Parameters
            ----------
            self : Dict_T
            key : str
            val : float
            
            """
            _chidg.f90wrap_get_real(self=self._handle, key=key, val=val)
        
        def set_real(self, key, val):
            """
            set_real(self, key, val)
            
            
            Defined at type_dict_python.f90 lines 49-58
            
            Parameters
            ----------
            self : Dict_T
            key : str
            val : float
            
            """
            _chidg.f90wrap_set_real(self=self._handle, key=key, val=val)
        
        def get_int(self, key, val):
            """
            get_int(self, key, val)
            
            
            Defined at type_dict_python.f90 lines 60-65
            
            Parameters
            ----------
            self : Dict_T
            key : str
            val : int
            
            """
            _chidg.f90wrap_get_int(self=self._handle, key=key, val=val)
        
        def set_int(self, key, val):
            """
            set_int(self, key, val)
            
            
            Defined at type_dict_python.f90 lines 67-75
            
            Parameters
            ----------
            self : Dict_T
            key : str
            val : int
            
            """
            _chidg.f90wrap_set_int(self=self._handle, key=key, val=val)
        
        def __init__(self, handle=None):
            """
            self = Dict_T()
            
            
            Defined at type_dict_python.f90 lines 27-29
            
            
            Returns
            -------
            this : Dict_T
            	Object to be constructed
            
            
            Automatically generated constructor for dict_t
            """
            f90wrap.runtime.FortranDerivedType.__init__(self)
            self._handle = _chidg.f90wrap_dict_t_initialise()
        
        def __del__(self):
            """
            Destructor for class Dict_T
            
            
            Defined at type_dict_python.f90 lines 27-29
            
            Parameters
            ----------
            this : Dict_T
            	Object to be destructed
            
            
            Automatically generated destructor for dict_t
            """
            if self._alloc:
                _chidg.f90wrap_dict_t_finalise(this=self._handle)
        
        _dt_array_initialisers = []
        
    _dt_array_initialisers = []
    

type_dict = Type_Dict()

