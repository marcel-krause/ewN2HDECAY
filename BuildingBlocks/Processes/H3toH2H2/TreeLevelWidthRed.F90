double precision function H3toH2H2Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S1S1f322**INT(2.D0))

 H3toH2H2Tree = totalAmplitude
end function H3toH2H2Tree