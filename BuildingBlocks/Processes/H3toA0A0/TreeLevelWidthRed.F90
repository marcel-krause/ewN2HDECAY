double precision function H3toA0A0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS2S2S1f223**INT(2.D0))

 H3toA0A0Tree = totalAmplitude
end function H3toA0A0Tree