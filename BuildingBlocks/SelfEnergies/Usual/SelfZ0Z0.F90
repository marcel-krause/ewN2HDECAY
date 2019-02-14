double complex function SelfZ0Z0Usual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(38)

 amplitudes(1) = (0.015625D0*EL2*(CA12*CA22 + CA22*SA12)*A0(MH12)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*A0(MH22)*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)**INT(2.D0)))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*A0(MH32)*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)**INT(2.D0)))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(4) = (0.015625D0*EL2*(CB2 + SB2)*A0(MZ2)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(5) = (0.015625D0*EL2*(CB2 + SB2)*A0(MA02)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(6) = (0.03125D0*EL2*(CB2 + SB2)*A0(MW2)*DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(7) = (0.03125D0*EL2*(CB2 + SB2)*A0(MHp2)*DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(8) = (0.125D0*CW2*EL2*(-2.D0*MW2 + 3.D0*A0(MW2)))/(PI2*SW2)

 amplitudes(9) = (0.003472222222222222D0*EL2*(-1.D0*x + 3.D0*x*B0(x, 0.D0, 0.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(10) = (0.003472222222222222D0*EL2*(-1.D0*x + 3.D0*x*B0(x, 0.D0, 0.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(11) = (0.003472222222222222D0*EL2*(-1.D0*x + 3.D0*x*B0(x, 0.D0, 0.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(12) = (-0.003472222222222222D0*EL2*(-1.D0*(6.D0*ME2 - 1.D0*x)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**IN&
  &T(4.D0))) + 6.D0*A0(ME2)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, ME2, ME2)*(2.D0*CW2*SW2&
  &*(5.D0*ME2 + x) + (ME2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*ME2 + 5.D0*x)*DBLE(SW**INT(4.D0)))))/(CW2*PI2*SW2)

 amplitudes(13) = (-0.003472222222222222D0*EL2*(-1.D0*(6.D0*MM2 - 1.D0*x)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**IN&
  &T(4.D0))) + 6.D0*A0(MM2)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, MM2, MM2)*(2.D0*CW2*SW2&
  &*(5.D0*MM2 + x) + (MM2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*MM2 + 5.D0*x)*DBLE(SW**INT(4.D0)))))/(CW2*PI2*SW2)

 amplitudes(14) = (-0.003472222222222222D0*EL2*(-1.D0*(6.D0*ML2 - 1.D0*x)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**IN&
  &T(4.D0))) + 6.D0*A0(ML2)*(-2.D0*CW2*SW2 + DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, ML2, ML2)*(2.D0*CW2*SW2&
  &*(5.D0*ML2 + x) + (ML2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*ML2 + 5.D0*x)*DBLE(SW**INT(4.D0)))))/(CW2*PI2*SW2)

 amplitudes(15) = (0.0011574074074074073D0*EL2*((6.D0*MU2 - 1.D0*x)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**IN&
  &T(4.D0))) - 6.D0*A0(MU2)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**INT(4.D0))) - 3.D0*B0(x, MU2, MU2)*(6.D0*C&
  &W2*SW2*(11.D0*MU2 + x) + 9.D0*(MU2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*MU2 + 17.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI2*&
  &SW2)

 amplitudes(16) = (0.0011574074074074073D0*EL2*((6.D0*MC2 - 1.D0*x)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**IN&
  &T(4.D0))) - 6.D0*A0(MC2)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**INT(4.D0))) - 3.D0*B0(x, MC2, MC2)*(6.D0*C&
  &W2*SW2*(11.D0*MC2 + x) + 9.D0*(MC2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*MC2 + 17.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI2*&
  &SW2)

 amplitudes(17) = (0.0011574074074074073D0*EL2*((6.D0*MT2 - 1.D0*x)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**IN&
  &T(4.D0))) - 6.D0*A0(MT2)*(-6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 17.D0*DBLE(SW**INT(4.D0))) - 3.D0*B0(x, MT2, MT2)*(6.D0*C&
  &W2*SW2*(11.D0*MT2 + x) + 9.D0*(MT2 - 1.D0*x)*DBLE(CW**INT(4.D0)) - 1.D0*(7.D0*MT2 + 17.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI2*&
  &SW2)

 amplitudes(18) = (-0.0011574074074074073D0*EL2*(-1.D0*(6.D0*MD2 - 1.D0*x)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(S&
  &W**INT(4.D0))) + 6.D0*A0(MD2)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, MD2, MD2)*(6.D&
  &0*CW2*SW2*(7.D0*MD2 - 1.D0*x) + 9.D0*(MD2 - 1.D0*x)*DBLE(CW**INT(4.D0)) + (17.D0*MD2 - 5.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI&
  &2*SW2)

 amplitudes(19) = (-0.0011574074074074073D0*EL2*(-1.D0*(6.D0*MS2 - 1.D0*x)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(S&
  &W**INT(4.D0))) + 6.D0*A0(MS2)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, MS2, MS2)*(6.D&
  &0*CW2*SW2*(7.D0*MS2 - 1.D0*x) + 9.D0*(MS2 - 1.D0*x)*DBLE(CW**INT(4.D0)) + (17.D0*MS2 - 5.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI&
  &2*SW2)

 amplitudes(20) = (-0.0011574074074074073D0*EL2*(-1.D0*(6.D0*MB2 - 1.D0*x)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(S&
  &W**INT(4.D0))) + 6.D0*A0(MB2)*(6.D0*CW2*SW2 + 9.D0*DBLE(CW**INT(4.D0)) + 5.D0*DBLE(SW**INT(4.D0))) + 3.D0*B0(x, MB2, MB2)*(6.D&
  &0*CW2*SW2*(7.D0*MB2 - 1.D0*x) + 9.D0*(MB2 - 1.D0*x)*DBLE(CW**INT(4.D0)) + (17.D0*MB2 - 5.D0*x)*DBLE(SW**INT(4.D0)))))/ (CW2*PI&
  &2*SW2)

 amplitudes(21) = (0.001736111111111111D0*EL2*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0))* (-6.D0*MH1&
  &2*x - 6.D0*MZ2*x + 3.D0*(-1.D0*MH12 + MZ2 - 1.D0*x)*A0(MH12) - 3.D0*(-1.D0*MH12 + MZ2 + x)*A0(MZ2) - 6.D0*MH12*MZ2*B0(x, MH12,&
  & MZ2) - 6.D0*MH12*x*B0(x, MH12, MZ2) - 6.D0*MZ2*x*B0(x, MH12, MZ2) + 3.D0*B0(x, MH12, MZ2)*DBLE(MH1**INT(4.D0)) + 3.D0*B0(x, M&
  &H12, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH12, MZ2)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*x)

 amplitudes(22) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT&
  &(2.D0))*DBLE((CW2 + SW2)**INT(2.D0))* (-6.D0*MH22*x - 6.D0*MZ2*x + 3.D0*(-1.D0*MH22 + MZ2 - 1.D0*x)*A0(MH22) - 3.D0*(-1.D0*MH2&
  &2 + MZ2 + x)*A0(MZ2) - 6.D0*MH22*MZ2*B0(x, MH22, MZ2) - 6.D0*MH22*x*B0(x, MH22, MZ2) - 6.D0*MZ2*x*B0(x, MH22, MZ2) + 3.D0*B0(x&
  &, MH22, MZ2)*DBLE(MH2**INT(4.D0)) + 3.D0*B0(x, MH22, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH22, MZ2&
  &)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*x)

 amplitudes(23) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**IN&
  &T(2.D0))*DBLE((CW2 + SW2)**INT(2.D0))* (-6.D0*MH32*x - 6.D0*MZ2*x + 3.D0*(-1.D0*MH32 + MZ2 - 1.D0*x)*A0(MH32) - 3.D0*(-1.D0*MH&
  &32 + MZ2 + x)*A0(MZ2) - 6.D0*MH32*MZ2*B0(x, MH32, MZ2) - 6.D0*MH32*x*B0(x, MH32, MZ2) - 6.D0*MZ2*x*B0(x, MH32, MZ2) + 3.D0*B0(&
  &x, MH32, MZ2)*DBLE(MH3**INT(4.D0)) + 3.D0*B0(x, MH32, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH32, MZ&
  &2)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*x)

 amplitudes(24) = (0.001736111111111111D0*EL2*DBLE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0))* (-6.D&
  &0*MA02*x - 6.D0*MH12*x - 3.D0*(MA02 - 1.D0*MH12 + x)*A0(MA02) - 3.D0*(-1.D0*MA02 + MH12 + x)*A0(MH12) - 6.D0*MA02*MH12*B0(x, M&
  &A02, MH12) - 6.D0*MA02*x*B0(x, MA02, MH12) - 6.D0*MH12*x*B0(x, MA02, MH12) + 3.D0*B0(x, MA02, MH12)*DBLE(MA0**INT(4.D0)) + 3.D&
  &0*B0(x, MA02, MH12)*DBLE(MH1**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MA02, MH12)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*&
  &x)

 amplitudes(25) = (0.001736111111111111D0*EL2*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)&
  &**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0))*(-6.D0*MA02*x - 6.D0*MH22*x - 3.D0*(MA02 - 1.D0*MH22 + x)*A0(MA02) - 3.D0*(-1.D0*MA&
  &02 + MH22 + x)*A0(MH22) - 6.D0*MA02*MH22*B0(x, MA02, MH22) - 6.D0*MA02*x*B0(x, MA02, MH22) - 6.D0*MH22*x*B0(x, MA02, MH22) + 3&
  &.D0*B0(x, MA02, MH22)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MH22)*DBLE(MH2**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x&
  &, MA02, MH22)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*x)

 amplitudes(26) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB&
  &)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0))*(-6.D0*MA02*x - 6.D0*MH32*x - 3.D0*(MA02 - 1.D0*MH32 + x)*A0(MA02) - 3.D0*(-1.D0*M&
  &A02 + MH32 + x)*A0(MH32) - 6.D0*MA02*MH32*B0(x, MA02, MH32) - 6.D0*MA02*x*B0(x, MA02, MH32) - 6.D0*MH32*x*B0(x, MA02, MH32) + &
  &3.D0*B0(x, MA02, MH32)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MH32)*DBLE(MH3**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(&
  &x, MA02, MH32)*DBLE(x**INT(2.D0))))/(CW2*PI2*SW2*x)

 amplitudes(27) = (-0.001736111111111111D0*EL2*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(x, MW2, MW2))*DBL&
  &E((CW2 - 1.D0*SW2)**INT(2.D0)))/ (CW2*PI2*SW2)

 amplitudes(28) = (0.001736111111111111D0*EL2*(2.D0*(-6.D0*MHp2 + x) - 6.D0*A0(MHp2) + 3.D0*(-4.D0*MHp2 + x)*B0(x, MHp2, MHp2))*D&
  &BLE((CW2 - 1.D0*SW2)**INT(2.D0)))/ (CW2*PI2*SW2)

 amplitudes(29) = (0.001736111111111111D0*CW2*EL2*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(x, MW2, MW2)))&
  &/(PI2*SW2)

 amplitudes(30) = (0.001736111111111111D0*CW2*EL2*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(x, MW2, MW2)))&
  &/(PI2*SW2)

 amplitudes(31) = (-0.003472222222222222D0*CW2*EL2*(2.D0*(-6.D0*MW2 + x) + 66.D0*A0(MW2) + (96.D0*MW2 + 57.D0*x)*B0(x, MW2, MW2))&
  &)/(PI2*SW2)

 amplitudes(32) = (0.015625D0*B0(x, MH12, MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CA1*CA2&
  &*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2

 amplitudes(33) = (0.015625D0*B0(x, MH22, MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(&
  &-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT&
  &(4.D0)))/PI2

 amplitudes(34) = (0.015625D0*B0(x, MH32, MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(&
  &-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**IN&
  &T(4.D0)))/PI2

 amplitudes(35) = (0.015625D0*B0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(&
  &CW2*PI2)

 amplitudes(36) = 0.D0

 amplitudes(37) = (0.015625D0*B0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(&
  &CW2*PI2)

 amplitudes(38) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,38
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfZ0Z0Usual = totalAmplitude
end function SelfZ0Z0Usual

