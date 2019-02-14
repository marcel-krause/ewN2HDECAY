double precision function H2toElElBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-4.D0*ME2 + MH22)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*SW2)

 H2toElElBarTree = totalAmplitude
end function H2toElElBarTree