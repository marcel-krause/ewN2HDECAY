double precision function H2toHmHpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S3S3f222**INT(2.D0))

 H2toHmHpTree = totalAmplitude
end function H2toHmHpTree