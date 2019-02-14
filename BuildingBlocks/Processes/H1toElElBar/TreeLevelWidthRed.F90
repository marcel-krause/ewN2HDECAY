double precision function H1toElElBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-4.D0*ME2 + MH12)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*SW2)

 H1toElElBarTree = totalAmplitude
end function H1toElElBarTree