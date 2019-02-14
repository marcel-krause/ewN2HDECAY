double precision function H3toElElBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-4.D0*ME2 + MH32)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*SW2)

 H3toElElBarTree = totalAmplitude
end function H3toElElBarTree