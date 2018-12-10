let g:ale_fortran_gcc_executable = 'gfortran'
let g:ale_fortran_gcc_options = '-std=f2008ts -pedantic -fall-intrinsics' .
            \ ' -Wall -Wargument-mismatch -Wcharacter-truncation -Wimplicit-procedure -Wextra' .
            \ ' -Wfrontend-loop-interchange'

