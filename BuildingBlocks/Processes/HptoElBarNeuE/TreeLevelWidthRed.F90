double precision function HptoElBarNeuETree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*ME2*(-1.D0*ME2 + MHp2)*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*SW2)

 HptoElBarNeuETree = totalAmplitude
end function HptoElBarNeuETree