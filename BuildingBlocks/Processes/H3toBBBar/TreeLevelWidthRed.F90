double precision function H3toBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MB2*(-4.D0*MB2 + MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*SW2)

 H3toBBBarTree = totalAmplitude
end function H3toBBBarTree