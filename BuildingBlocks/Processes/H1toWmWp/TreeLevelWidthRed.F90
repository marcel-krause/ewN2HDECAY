double precision function H1toWmWpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE((CB*RR11 + RR12*SB)**&
  &INT(2.D0)))/SW2

 H1toWmWpTree = totalAmplitude
end function H1toWmWpTree