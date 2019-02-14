double precision function H1toBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MB2*(-4.D0*MB2 + MH12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*SW2)

 H1toBBBarTree = totalAmplitude
end function H1toBBBarTree