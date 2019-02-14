double precision function H3toH1H2Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S1S1f312**INT(2.D0))

 H3toH1H2Tree = totalAmplitude
end function H3toH1H2Tree