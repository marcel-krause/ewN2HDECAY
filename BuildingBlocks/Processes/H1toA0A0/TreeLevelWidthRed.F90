double precision function H1toA0A0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS2S2S1f221**INT(2.D0))

 H1toA0A0Tree = totalAmplitude
end function H1toA0A0Tree