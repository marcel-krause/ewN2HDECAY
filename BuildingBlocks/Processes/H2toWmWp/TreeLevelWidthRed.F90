double precision function H2toWmWpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*(3.D0 - (1.D0*MH22)/MW2 + 0.25D0*DBLE(MH2**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE((CB*RR21 + RR22*SB)**&
  &INT(2.D0)))/SW2

 H2toWmWpTree = totalAmplitude
end function H2toWmWpTree