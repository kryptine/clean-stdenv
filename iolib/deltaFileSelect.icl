implementation module deltaFileSelect;

import xfileselect, StdInt, StdFile, deltaEventIO;

    
SelectInputFile :: !*s !(IOState *s)
                         -> (!Bool, !String, !*s, !IOState *s);
SelectInputFile program_state io
   | ok == 1 =  (True, file, program_state, io);
   =  (False, "", program_state, io);
      where {
      (ok, file)=: XSelectInputFile 0;
      };

SelectOutputFile :: !String !String !*s !(IOState *s)
                         -> (!Bool, !String, !*s, !IOState *s);
SelectOutputFile prompt def_file program_state io
   | ok == 1 =  (True, file, program_state, io);
   =  (False, "", program_state, io);
      where {
      (ok, file)=: XSelectOutputFile prompt def_file;
      };
