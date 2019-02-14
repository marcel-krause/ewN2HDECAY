double precision function H1toHmHpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S3S3f122**INT(2.D0))

 H1toHmHpTree = totalAmplitude
end function H1toHmHpTree