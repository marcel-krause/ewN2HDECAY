double precision function H2toBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MB2*(-4.D0*MB2 + MH22)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*SW2)

 H2toBBBarTree = totalAmplitude
end function H2toBBBarTree