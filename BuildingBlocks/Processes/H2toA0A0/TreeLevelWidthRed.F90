double precision function H2toA0A0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS2S2S1f222**INT(2.D0))

 H2toA0A0Tree = totalAmplitude
end function H2toA0A0Tree