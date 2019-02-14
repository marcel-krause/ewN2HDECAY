double precision function H3toZ0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*DBLE(CW**INT(-4.D0))*(3.D0 - (1.D0*MH32)/MZ2 + 0.25D0*DBLE(MH3**INT(4.D0))*DBLE(MZ**INT(-4.D0)))*DBLE(&
  &(CB*RR31 + RR32*SB)**INT(2.D0))* DBLE((CW2 + SW2)**INT(4.D0)))/SW2

 H3toZ0Z0Tree = totalAmplitude
end function H3toZ0Z0Tree