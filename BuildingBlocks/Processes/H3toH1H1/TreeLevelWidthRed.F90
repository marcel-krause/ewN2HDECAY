double precision function H3toH1H1Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S1S1f311**INT(2.D0))

 H3toH1H1Tree = totalAmplitude
end function H3toH1H1Tree