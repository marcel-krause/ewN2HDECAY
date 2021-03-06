double complex function DSelfAA(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(21)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = (-0.027777777777777776D0*EL2)/PI2 + (0.125D0*EL2*B0(x, ME2, ME2))/PI2 + (0.08333333333333333D0*EL2*B1(x, ME2, ME&
  &2))/PI2 + (0.16666666666666666D0*EL2*ME2*DB0(x, ME2, ME2))/PI2 + (0.125D0*EL2*x*DB0(x, ME2, ME2))/PI2 + (0.08333333333333333D0&
  &*EL2*x*DB1(x, ME2, ME2))/PI2

 amplitudes(5) = (-0.027777777777777776D0*EL2)/PI2 + (0.125D0*EL2*B0(x, MM2, MM2))/PI2 + (0.08333333333333333D0*EL2*B1(x, MM2, MM&
  &2))/PI2 + (0.16666666666666666D0*EL2*MM2*DB0(x, MM2, MM2))/PI2 + (0.125D0*EL2*x*DB0(x, MM2, MM2))/PI2 + (0.08333333333333333D0&
  &*EL2*x*DB1(x, MM2, MM2))/PI2

 amplitudes(6) = (-0.027777777777777776D0*EL2)/PI2 + (0.125D0*EL2*B0(x, ML2, ML2))/PI2 + (0.08333333333333333D0*EL2*B1(x, ML2, ML&
  &2))/PI2 + (0.16666666666666666D0*EL2*ML2*DB0(x, ML2, ML2))/PI2 + (0.125D0*EL2*x*DB0(x, ML2, ML2))/PI2 + (0.08333333333333333D0&
  &*EL2*x*DB1(x, ML2, ML2))/PI2

 amplitudes(7) = (-0.037037037037037035D0*EL2)/PI2 + (0.16666666666666666D0*EL2*B0(x, MU2, MU2))/PI2 + (0.1111111111111111D0*EL2*&
  &B1(x, MU2, MU2))/PI2 + (0.2222222222222222D0*EL2*MU2*DB0(x, MU2, MU2))/PI2 + (0.16666666666666666D0*EL2*x*DB0(x, MU2, MU2))/PI&
  &2 + (0.1111111111111111D0*EL2*x*DB1(x, MU2, MU2))/PI2

 amplitudes(8) = (-0.037037037037037035D0*EL2)/PI2 + (0.16666666666666666D0*EL2*B0(x, MC2, MC2))/PI2 + (0.1111111111111111D0*EL2*&
  &B1(x, MC2, MC2))/PI2 + (0.2222222222222222D0*EL2*MC2*DB0(x, MC2, MC2))/PI2 + (0.16666666666666666D0*EL2*x*DB0(x, MC2, MC2))/PI&
  &2 + (0.1111111111111111D0*EL2*x*DB1(x, MC2, MC2))/PI2

 amplitudes(9) = (-0.037037037037037035D0*EL2)/PI2 + (0.16666666666666666D0*EL2*B0(x, MT2, MT2))/PI2 + (0.1111111111111111D0*EL2*&
  &B1(x, MT2, MT2))/PI2 + (0.2222222222222222D0*EL2*MT2*DB0(x, MT2, MT2))/PI2 + (0.16666666666666666D0*EL2*x*DB0(x, MT2, MT2))/PI&
  &2 + (0.1111111111111111D0*EL2*x*DB1(x, MT2, MT2))/PI2

 amplitudes(10) = (-0.009259259259259259D0*EL2)/PI2 + (0.041666666666666664D0*EL2*B0(x, MD2, MD2))/PI2 + (0.027777777777777776D0*&
  &EL2*B1(x, MD2, MD2))/PI2 + (0.05555555555555555D0*EL2*MD2*DB0(x, MD2, MD2))/PI2 + (0.041666666666666664D0*EL2*x*DB0(x, MD2, MD&
  &2))/PI2 + (0.027777777777777776D0*EL2*x*DB1(x, MD2, MD2))/PI2

 amplitudes(11) = (-0.009259259259259259D0*EL2)/PI2 + (0.041666666666666664D0*EL2*B0(x, MS2, MS2))/PI2 + (0.027777777777777776D0*&
  &EL2*B1(x, MS2, MS2))/PI2 + (0.05555555555555555D0*EL2*MS2*DB0(x, MS2, MS2))/PI2 + (0.041666666666666664D0*EL2*x*DB0(x, MS2, MS&
  &2))/PI2 + (0.027777777777777776D0*EL2*x*DB1(x, MS2, MS2))/PI2

 amplitudes(12) = (-0.009259259259259259D0*EL2)/PI2 + (0.041666666666666664D0*EL2*B0(x, MB2, MB2))/PI2 + (0.027777777777777776D0*&
  &EL2*B1(x, MB2, MB2))/PI2 + (0.05555555555555555D0*EL2*MB2*DB0(x, MB2, MB2))/PI2 + (0.041666666666666664D0*EL2*x*DB0(x, MB2, MB&
  &2))/PI2 + (0.027777777777777776D0*EL2*x*DB1(x, MB2, MB2))/PI2

 amplitudes(13) = (0.013888888888888888D0*EL2)/PI2 - (0.041666666666666664D0*EL2*B1(x, MW2, MW2))/PI2 - (0.08333333333333333D0*EL&
  &2*MW2*DB0(x, MW2, MW2))/PI2 - (0.041666666666666664D0*EL2*x*DB1(x, MW2, MW2))/PI2

 amplitudes(14) = (0.013888888888888888D0*EL2)/PI2 - (0.041666666666666664D0*EL2*B1(x, MHp2, MHp2))/PI2 - (0.08333333333333333D0*&
  &EL2*MHp2*DB0(x, MHp2, MHp2))/PI2 - (0.041666666666666664D0*EL2*x*DB1(x, MHp2, MHp2))/PI2

 amplitudes(15) = (-0.003472222222222222D0*EL2)/PI2 + (0.010416666666666666D0*EL2*B1(x, MW2, MW2))/PI2 + (0.020833333333333332D0*&
  &EL2*MW2*DB0(x, MW2, MW2))/PI2 + (0.010416666666666666D0*EL2*x*DB1(x, MW2, MW2))/PI2

 amplitudes(16) = (-0.003472222222222222D0*EL2)/PI2 + (0.010416666666666666D0*EL2*B1(x, MW2, MW2))/PI2 + (0.020833333333333332D0*&
  &EL2*MW2*DB0(x, MW2, MW2))/PI2 + (0.010416666666666666D0*EL2*x*DB1(x, MW2, MW2))/PI2

 amplitudes(17) = (-0.006944444444444444D0*EL2)/PI2 - (0.25D0*EL2*B0(x, MW2, MW2))/PI2 - (0.10416666666666667D0*EL2*B1(x, MW2, MW&
  &2))/PI2 - (0.3333333333333333D0*EL2*MW2*DB0(x, MW2, MW2))/PI2 - (0.25D0*EL2*x*DB0(x, MW2, MW2))/PI2 - (0.10416666666666667D0*E&
  &L2*x*DB1(x, MW2, MW2))/PI2

 amplitudes(18) = (0.125D0*CB2*EL2*MW2*SB2*DB0(x, MW2, MW2))/PI2 + (0.0625D0*EL2*MW2*DB0(x, MW2, MW2)*DBLE(CB**INT(4.D0)))/PI2 + &
  &(0.0625D0*EL2*MW2*DB0(x, MW2, MW2)*DBLE(SB**INT(4.D0)))/PI2

 amplitudes(19) = 0.D0

 amplitudes(20) = (0.125D0*CB2*EL2*MW2*SB2*DB0(x, MW2, MW2))/PI2 + (0.0625D0*EL2*MW2*DB0(x, MW2, MW2)*DBLE(CB**INT(4.D0)))/PI2 + &
  &(0.0625D0*EL2*MW2*DB0(x, MW2, MW2)*DBLE(SB**INT(4.D0)))/PI2

 amplitudes(21) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfAA = totalAmplitude
end function DSelfAA

