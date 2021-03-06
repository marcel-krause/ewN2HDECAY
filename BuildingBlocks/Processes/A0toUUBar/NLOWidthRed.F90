double complex function A0toUUBarVC()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(58)

 amplitudes(1) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.0078125D0*CB*B0(MA02, MU2, MU2)*DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**IN&
  &T(-3.D0))*DBLE(PI**INT(-4.D0))* DBLE(RR12**INT(2.D0))*DBLE(SB**INT(-3.D0))*DBLE(SW**INT(-3.D0)) + 0.0078125D0*CB*(-1.D0*MH12 +&
  & 4.D0*MU2)*C0(MA02, MU2, MU2, MU2, MU2, MH12)* DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0&
  &))*DBLE(RR12**INT(2.D0))*DBLE(SB**INT(-3.D0))* DBLE(SW**INT(-3.D0))))/(MW*SB*SW)

 amplitudes(2) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.0078125D0*CB*B0(MA02, MU2, MU2)*DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**IN&
  &T(-3.D0))*DBLE(PI**INT(-4.D0))* DBLE(RR22**INT(2.D0))*DBLE(SB**INT(-3.D0))*DBLE(SW**INT(-3.D0)) + 0.0078125D0*CB*(-1.D0*MH22 +&
  & 4.D0*MU2)*C0(MA02, MU2, MU2, MU2, MU2, MH22)* DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0&
  &))*DBLE(RR22**INT(2.D0))*DBLE(SB**INT(-3.D0))* DBLE(SW**INT(-3.D0))))/(MW*SB*SW)

 amplitudes(3) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.0078125D0*CB*B0(MA02, MU2, MU2)*DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**IN&
  &T(-3.D0))*DBLE(PI**INT(-4.D0))* DBLE(RR32**INT(2.D0))*DBLE(SB**INT(-3.D0))*DBLE(SW**INT(-3.D0)) + 0.0078125D0*CB*(-1.D0*MH32 +&
  & 4.D0*MU2)*C0(MA02, MU2, MU2, MU2, MU2, MH32)* DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0&
  &))*DBLE(RR32**INT(2.D0))*DBLE(SB**INT(-3.D0))* DBLE(SW**INT(-3.D0))))/(MW*SB*SW)

 amplitudes(4) = (-3.D0*CB*EL*MA02*MU*PI2*((0.0078125D0*CB*B0(MA02, MU2, MU2)*DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**IN&
  &T(-3.D0))*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0)))/SB + (0.0078125D0*CB*MZ2*C0(MA02, MU2, MU2, MU2, MU2, MZ2)*DBLE(EL**INT(&
  &3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**INT(-3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/SB))/(MW*SB*SW)

 amplitudes(5) = (-3.D0*CB*EL*MA02*MU*PI2*(0.0078125D0*B0(MA02, MU2, MU2)*DBLE(CB**INT(3.D0))*DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.&
  &D0))*DBLE(MW**INT(-3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SB**INT(-3.D0))*DBLE(SW**INT(-3.D0)) + 0.0078125D0*MA02*C0(MA02, MU2, MU2&
  &, MU2, MU2, MA02)*DBLE(CB**INT(3.D0))* DBLE(EL**INT(3.D0))*DBLE(MU**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0))*DBLE(&
  &SB**INT(-3.D0))*DBLE(SW**INT(-3.D0))))/(MW*SB*SW)

 amplitudes(6) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.015625D0*CKM11*CKMC11*MD2*MU*YukS2Quark2*YukS3Quark1*B0(MA02, MD2, MD2)*DBLE(EL**IN&
  &T(3.D0))*DBLE(MW**INT(-3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)) - (0.03125D0*CKM11*CKMC11*EL*MD*YukS2Quark2*C0(MA02, &
  &MU2, MU2, MD2, MD2, MW2)*DBLE(PI**INT(-4.D0))* ((0.5D0*EL2*MD*MU*SB2*YukS3Quark1)/SW2 - (0.5D0*EL2*MU*SB2*YukS3Quark1*DBLE(MD*&
  &*INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*SB2*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*SB2*YukS3Quark1*DBLE(MU**INT(3.D0)))&
  &/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MD**INT(3.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(MW*SB2*SW)))/(MW*SB*SW)

 amplitudes(7) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.015625D0*CKM12*CKMC12*MS2*MU*YukS2Quark2*YukS3Quark1*B0(MA02, MS2, MS2)*DBLE(EL**IN&
  &T(3.D0))*DBLE(MW**INT(-3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)) - (0.03125D0*CKM12*CKMC12*EL*MS*YukS2Quark2*C0(MA02, &
  &MU2, MU2, MS2, MS2, MW2)*DBLE(PI**INT(-4.D0))* ((0.5D0*EL2*MS*MU*SB2*YukS3Quark1)/SW2 - (0.5D0*EL2*MU*SB2*YukS3Quark1*DBLE(MS*&
  &*INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MS*SB2*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*EL2*MS*SB2*YukS3Quark1*DBLE(MU**INT(3.D0)))&
  &/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MS**INT(3.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(MW*SB2*SW)))/(MW*SB*SW)

 amplitudes(8) = (-3.D0*CB*EL*MA02*MU*PI2*(-0.015625D0*CKM13*CKMC13*MB2*MU*YukS2Quark2*YukS3Quark1*B0(MA02, MB2, MB2)*DBLE(EL**IN&
  &T(3.D0))*DBLE(MW**INT(-3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)) - (0.03125D0*CKM13*CKMC13*EL*MB*YukS2Quark2*C0(MA02, &
  &MU2, MU2, MB2, MB2, MW2)*DBLE(PI**INT(-4.D0))* ((0.5D0*EL2*MB*MU*SB2*YukS3Quark1)/SW2 - (0.5D0*EL2*MU*SB2*YukS3Quark1*DBLE(MB*&
  &*INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MB*SB2*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*EL2*MB*SB2*YukS3Quark1*DBLE(MU**INT(3.D0)))&
  &/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MB**INT(3.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(MW*SB2*SW)))/(MW*SB*SW)

 amplitudes(9) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CB*CKM11*CKMC11*MD2*MU*YukS2Quark2*YukS3Quark2*B0(MA02, MD2, MD2)*DBLE(EL&
  &**INT(3.D0))* DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/SB - (0.03125D0*CKM11*CKMC11*EL*MD*YukS2Quark2*C&
  &0(MA02, MU2, MU2, MD2, MD2, MHp2)*DBLE(PI**INT(-4.D0))* ((0.5D0*CB*EL2*MD*MHp2*MU*SB*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*MU&
  &*SB*YukS3Quark2*DBLE(MD**INT(3.D0)))/(MW2*SW2) + (0.5D0*CB2*EL2*MD*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*CB*EL2*MD*SB*YukS3Q&
  &uark2*DBLE(MU**INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MD**INT(3.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(MW*SB2&
  &*SW)))/(MW*SB*SW)

 amplitudes(10) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CB*CKM12*CKMC12*MS2*MU*YukS2Quark2*YukS3Quark2*B0(MA02, MS2, MS2)*DBLE(E&
  &L**INT(3.D0))* DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/SB - (0.03125D0*CKM12*CKMC12*EL*MS*YukS2Quark2*&
  &C0(MA02, MU2, MU2, MS2, MS2, MHp2)*DBLE(PI**INT(-4.D0))* ((0.5D0*CB*EL2*MHp2*MS*MU*SB*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*M&
  &U*SB*YukS3Quark2*DBLE(MS**INT(3.D0)))/(MW2*SW2) + (0.5D0*CB2*EL2*MS*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*CB*EL2*MS*SB*YukS3&
  &Quark2*DBLE(MU**INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MS**INT(3.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(MW*SB&
  &2*SW)))/(MW*SB*SW)

 amplitudes(11) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CB*CKM13*CKMC13*MB2*MU*YukS2Quark2*YukS3Quark2*B0(MA02, MB2, MB2)*DBLE(E&
  &L**INT(3.D0))* DBLE(MW**INT(-3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/SB - (0.03125D0*CKM13*CKMC13*EL*MB*YukS2Quark2*&
  &C0(MA02, MU2, MU2, MB2, MB2, MHp2)*DBLE(PI**INT(-4.D0))* ((0.5D0*CB*EL2*MB*MHp2*MU*SB*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*M&
  &U*SB*YukS3Quark2*DBLE(MB**INT(3.D0)))/(MW2*SW2) + (0.5D0*CB2*EL2*MB*DBLE(MU**INT(3.D0)))/(MW2*SW2) - (0.5D0*CB*EL2*MB*SB*YukS3&
  &Quark2*DBLE(MU**INT(3.D0)))/(MW2*SW2) + (0.5D0*EL2*MU*SB2*DBLE(MB**INT(3.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(MW*SB&
  &2*SW)))/(MW*SB*SW)

 amplitudes(12) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f211*EL2*RR12*B0(MU2, MH12, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f211*EL2*RR12*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f211*EL2*(MA02 - 1.D0*MH12 + MZ2)*RR12*C0(MA02, MU2, MU2, MZ2, MH12, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(13) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f212*EL2*RR22*B0(MU2, MH22, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f212*EL2*RR22*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f212*EL2*(MA02 - 1.D0*MH22 + MZ2)*RR22*C0(MA02, MU2, MU2, MZ2, MH22, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(14) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f213*EL2*RR32*B0(MU2, MH32, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f213*EL2*RR32*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f213*EL2*(MA02 - 1.D0*MH32 + MZ2)*RR32*C0(MA02, MU2, MU2, MZ2, MH32, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(15) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f221*EL2*RR12*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f221*EL2*RR12*B0(MU2, MH12, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f221*EL2*(2.D0*MA02 - 1.D0*MH12)*RR12*C0(MA02, MU2, MU2, MA02, MH12, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(16) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f222*EL2*RR22*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f222*EL2*RR22*B0(MU2, MH22, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f222*EL2*(2.D0*MA02 - 1.D0*MH22)*RR22*C0(MA02, MU2, MU2, MA02, MH22, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(17) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f223*EL2*RR32*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f223*EL2*RR32*B0(MU2, MH32, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f223*EL2*(2.D0*MA02 - 1.D0*MH32)*RR32*C0(MA02, MU2, MU2, MA02, MH32, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(18) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f211*EL2*RR12*B0(MU2, MH12, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f211*EL2*RR12*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f211*EL2*(MA02 - 1.D0*MH12 + MZ2)*RR12*C0(MA02, MU2, MU2, MZ2, MH12, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(19) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f212*EL2*RR22*B0(MU2, MH22, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f212*EL2*RR22*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f212*EL2*(MA02 - 1.D0*MH22 + MZ2)*RR22*C0(MA02, MU2, MU2, MZ2, MH22, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(20) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.015625D0*CS2S2S1f213*EL2*RR32*B0(MU2, MH32, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(MA02*MW2*SB*SW2) + (0.015625D0*CS2S2S1f213*EL2*RR32*B0(MU2, MU2, MZ2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MA&
  &02*MW2*SB*SW2) - (0.015625D0*CS2S2S1f213*EL2*(MA02 - 1.D0*MH32 + MZ2)*RR32*C0(MA02, MU2, MU2, MZ2, MH32, MU2)*DBLE(MU**INT(3.D&
  &0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB*SW2)))/(MW*SB*SW)

 amplitudes(21) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f221*EL2*RR12*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f221*EL2*RR12*B0(MU2, MH12, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f221*EL2*(2.D0*MA02 - 1.D0*MH12)*RR12*C0(MA02, MU2, MU2, MA02, MH12, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(22) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f222*EL2*RR22*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f222*EL2*RR22*B0(MU2, MH22, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f222*EL2*(2.D0*MA02 - 1.D0*MH22)*RR22*C0(MA02, MU2, MU2, MA02, MH22, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(23) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CB*CS2S2S1f223*EL2*RR32*B0(MU2, MA02, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**I&
  &NT(-4.D0)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f223*EL2*RR32*B0(MU2, MH32, MU2)*DBLE(MU**INT(3.D0))*DBLE(PI**INT(-4.D0&
  &)))/(MA02*MW2*SB2*SW2) - (0.015625D0*CB*CS2S2S1f223*EL2*(2.D0*MA02 - 1.D0*MH32)*RR32*C0(MA02, MU2, MU2, MA02, MH32, MU2)*DBLE(&
  &MU**INT(3.D0))*DBLE(PI**INT(-4.D0)))/ (MA02*MW2*SB2*SW2)))/(MW*SB*SW)

 amplitudes(24) = 0.D0

 amplitudes(25) = 0.D0

 amplitudes(26) = 0.D0

 amplitudes(27) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MD2, MHp2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MD2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MD2)*((-0.5D0*CB*EL2*MA02*MD2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MD2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MD2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MD2*MHp2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(28) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MD2, MHp2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MD2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM11*CKMC11*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MD2)*((-0.5D0*CB*EL2*MA02*MD2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MD2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MD2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MD2*MHp2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(29) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MHp2, MS2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MS2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MS2)*((-0.5D0*CB*EL2*MA02*MS2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MS2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MS2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MHp2*MS2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(30) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MHp2, MS2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MS2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM12*CKMC12*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MS2)*((-0.5D0*CB*EL2*MA02*MS2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MS2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MS2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MHp2*MS2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(31) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MB2, MHp2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MB2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MB2)*((-0.5D0*CB*EL2*MA02*MB2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MB2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MB2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MB2*MHp2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(32) = (-3.D0*CB*EL*MA02*MU*PI2*((0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.D0*CB2*MW*&
  &SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*B0(M&
  &U2, MB2, MHp2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) - (0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*MU*(-1.D0*CB2 - 1.D0*SB2)*((-2.&
  &D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* ((-0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*&
  &SW2))*B0(MU2, MB2, MW2)*DBLE(PI**INT(-4.D0)))/ (MA02*SB2) + (0.015625D0*CKM13*CKMC13*(Lam4 - 1.D0*Lam5)*(-1.D0*CB2 - 1.D0*SB2)&
  &*((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2*SW)/EL)* C0(MA02, MU2, MU2, MW2, MHp2, MB2)*((-0.5D0*CB*EL2*MA02*MB2*MU*SB*YukS3Quark1)/&
  &(MW2*SW2) + (0.5D0*EL2*MA02*MB2*MU*SB2*YukS3Quark2)/(MW2*SW2) + (0.5D0*EL2*MB2*MU*SB2*YukS3Quark1*YukS3Quark2)/SW2 - (0.5D0*EL&
  &2*MB2*MHp2*MU*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2) - (0.5D0*CB*EL2*SB*DBLE(MU**INT(3.D0)))/SW2 + (0.5D0*CB*EL2*MHp2*SB*DBLE(&
  &MU**INT(3.D0)))/(MW2*SW2))*DBLE(PI**INT(-4.D0)))/(MA02*SB2)))/(MW*SB*SW)

 amplitudes(33) = 0.D0

 amplitudes(34) = 0.D0

 amplitudes(35) = 0.D0

 amplitudes(36) = (-3.D0*CB*EL*MA02*MU*PI2*((0.027777777777777776D0*CB*MU*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MW*SB*SW) - &
  &(0.05555555555555555D0*CB*MU*B0(MU2, 0.D0, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MW*SB*SW) + (0.027777777777777776D0&
  &*CB*MU*(MA02 - 2.D0*MU2)*C0(MA02, MU2, MU2, MU2, MU2, 0.D0)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0)))/(MW*SB*SW)))/ (MW*SB*SW)

 amplitudes(37) = (-3.D0*CB*EL*MA02*MU*PI2*((-0.006944444444444444D0*CB*MU*(3.D0*CW2 - 1.D0*SW2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0)))/(CW2*MW*SB*SW) + (0.013888888888888888D0*CB*MU*(3.D0*CW2 - 1.D0*SW2)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**&
  &INT(-4.D0)))/(CW2*MW*SB*SW) - (0.001736111111111111D0*CB*MU*C0(MA02, MU2, MU2, MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT&
  &(-4.D0))*DBLE(SW**INT(-3.D0))* (12.D0*CW2*MA02*SW2 - 6.D0*CW2*MU2*SW2 + 9.D0*MU2*DBLE(CW**INT(4.D0)) - 4.D0*MA02*DBLE(SW**INT(&
  &4.D0)) + 17.D0*MU2*DBLE(SW**INT(4.D0))))/ (CW2*MW*SB)))/(MW*SB*SW)

 amplitudes(38) = (-0.09375D0*CB*CKM11*CKMC11*MA02*MD2*MU2*YukS2Quark2*C0(MA02, MU2, MU2, MD2, MD2, MW2)*DBLE(EL**INT(4.D0))*DBLE&
  &(SW**INT(-4.D0)))/(MW2*PI2*SB)

 amplitudes(39) = (-0.09375D0*CB*CKM12*CKMC12*MA02*MS2*MU2*YukS2Quark2*C0(MA02, MU2, MU2, MS2, MS2, MW2)*DBLE(EL**INT(4.D0))*DBLE&
  &(SW**INT(-4.D0)))/(MW2*PI2*SB)

 amplitudes(40) = (-0.09375D0*CB*CKM13*CKMC13*MA02*MB2*MU2*YukS2Quark2*C0(MA02, MU2, MU2, MB2, MB2, MW2)*DBLE(EL**INT(4.D0))*DBLE&
  &(SW**INT(-4.D0)))/(MW2*PI2*SB)

 amplitudes(41) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR12*(CB*RR12 - 1.D0*RR11*SB)*B0(MA02, MH12, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR1&
  &2*(CB*RR12 - 1.D0*RR11*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH12 + 3.D0*MA02*MU2 + MH12*MU2 - 1.D0*MU2*MZ2)*RR12* (CB*RR12 - 1.&
  &D0*RR11*SB)*C0(MA02, MU2, MU2, MZ2, MH12, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR12*(CB*RR12 - 1.D0*RR11*SB)*B0(MU2, MH12, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(42) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR22*(CB*RR22 - 1.D0*RR21*SB)*B0(MA02, MH22, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR2&
  &2*(CB*RR22 - 1.D0*RR21*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH22 + 3.D0*MA02*MU2 + MH22*MU2 - 1.D0*MU2*MZ2)*RR22* (CB*RR22 - 1.&
  &D0*RR21*SB)*C0(MA02, MU2, MU2, MZ2, MH22, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR22*(CB*RR22 - 1.D0*RR21*SB)*B0(MU2, MH22, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(43) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR32*(CB*RR32 - 1.D0*RR31*SB)*B0(MA02, MH32, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR3&
  &2*(CB*RR32 - 1.D0*RR31*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH32 + 3.D0*MA02*MU2 + MH32*MU2 - 1.D0*MU2*MZ2)*RR32* (CB*RR32 - 1.&
  &D0*RR31*SB)*C0(MA02, MU2, MU2, MZ2, MH32, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR32*(CB*RR32 - 1.D0*RR31*SB)*B0(MU2, MH32, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(44) = 0.D0

 amplitudes(45) = 0.D0

 amplitudes(46) = 0.D0

 amplitudes(47) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM11*CKMC11*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MD2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MD2, MHp2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM11*CKMC11*EL2*(CB2 + SB2)*B0(MU2, MD2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MD2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM11*CKMC11*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MD2)* ((-0.707106781&
  &1865475D0*CB*EL*MA02*MD2*MU)/(MW*SW) + (1.4142135623730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MD2*M&
  &U*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MD2*MHp2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MD2*MU*MW*&
  &SB*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU*&
  &*INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.00781&
  &25D0*CB*CKM11*CKMC11*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB&
  &)))/(MW*SB*SW)

 amplitudes(48) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM12*CKMC12*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MS2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MHp2, MS2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM12*CKMC12*EL2*(CB2 + SB2)*B0(MU2, MS2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MS2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM12*CKMC12*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MS2)* ((1.4142135623&
  &730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) - (0.7071067811865475D0*CB*EL*MA02*MS2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MS2*MU&
  &*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MHp2*MS2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MS2*MU*MW*S&
  &B*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU**&
  &INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.007812&
  &5D0*CB*CKM12*CKMC12*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB)&
  &))/(MW*SB*SW)

 amplitudes(49) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM13*CKMC13*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MB2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MB2, MHp2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM13*CKMC13*EL2*(CB2 + SB2)*B0(MU2, MB2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MB2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM13*CKMC13*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MB2)* ((-0.707106781&
  &1865475D0*CB*EL*MA02*MB2*MU)/(MW*SW) + (1.4142135623730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MB2*M&
  &U*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MB2*MHp2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MB2*MU*MW*&
  &SB*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU*&
  &*INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.00781&
  &25D0*CB*CKM13*CKMC13*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB&
  &)))/(MW*SB*SW)

 amplitudes(50) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR12*(CB*RR12 - 1.D0*RR11*SB)*B0(MA02, MH12, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR1&
  &2*(CB*RR12 - 1.D0*RR11*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH12 + 3.D0*MA02*MU2 + MH12*MU2 - 1.D0*MU2*MZ2)*RR12* (CB*RR12 - 1.&
  &D0*RR11*SB)*C0(MA02, MU2, MU2, MZ2, MH12, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR12*(CB*RR12 - 1.D0*RR11*SB)*B0(MU2, MH12, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(51) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR22*(CB*RR22 - 1.D0*RR21*SB)*B0(MA02, MH22, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR2&
  &2*(CB*RR22 - 1.D0*RR21*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH22 + 3.D0*MA02*MU2 + MH22*MU2 - 1.D0*MU2*MZ2)*RR22* (CB*RR22 - 1.&
  &D0*RR21*SB)*C0(MA02, MU2, MU2, MZ2, MH22, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR22*(CB*RR22 - 1.D0*RR21*SB)*B0(MU2, MH22, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(52) = (-3.D0*CB*EL*MA02*MU*PI2*((0.00390625D0*MU*RR32*(CB*RR32 - 1.D0*RR31*SB)*B0(MA02, MH32, MZ2)*DBLE(EL**INT(3.D0)&
  &)*DBLE(PI**INT(-4.D0))* DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MW*SB) - (0.0078125D0*MU*(MA02 - 1.D0*MU2)*RR3&
  &2*(CB*RR32 - 1.D0*RR31*SB)*B0(MU2, MU2, MZ2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)**&
  &INT(2.D0)))/(CW2*MA02*MW*SB) + (0.0078125D0*MU*(-1.D0*MA02*MH32 + 3.D0*MA02*MU2 + MH32*MU2 - 1.D0*MU2*MZ2)*RR32* (CB*RR32 - 1.&
  &D0*RR31*SB)*C0(MA02, MU2, MU2, MZ2, MH32, MU2)*DBLE(EL**INT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))* DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MA02*MW*SB) - (0.0078125D0*RR32*(CB*RR32 - 1.D0*RR31*SB)*B0(MU2, MH32, MU2)*DBLE(EL**INT(3.D0))* DBLE(MU**I&
  &NT(3.D0))*DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MA02*MW*SB)))/(MW*SB*SW)

 amplitudes(53) = 0.D0

 amplitudes(54) = 0.D0

 amplitudes(55) = 0.D0

 amplitudes(56) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM11*CKMC11*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MD2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MD2, MHp2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM11*CKMC11*EL2*(CB2 + SB2)*B0(MU2, MD2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MD2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM11*CKMC11*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MD2)* ((-0.707106781&
  &1865475D0*CB*EL*MA02*MD2*MU)/(MW*SW) + (1.4142135623730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MD2*M&
  &U*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MD2*MHp2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MD2*MU*MW*&
  &SB*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU*&
  &*INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.00781&
  &25D0*CB*CKM11*CKMC11*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB&
  &)))/(MW*SB*SW)

 amplitudes(57) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM12*CKMC12*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MS2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MHp2, MS2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM12*CKMC12*EL2*(CB2 + SB2)*B0(MU2, MS2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MS2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM12*CKMC12*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MS2)* ((1.4142135623&
  &730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) - (0.7071067811865475D0*CB*EL*MA02*MS2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MS2*MU&
  &*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MHp2*MS2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MS2*MU*MW*S&
  &B*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU**&
  &INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.007812&
  &5D0*CB*CKM12*CKMC12*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB)&
  &))/(MW*SB*SW)

 amplitudes(58) = (-3.D0*CB*EL*MA02*MU*PI2*((0.011048543456039804D0*CKM13*CKMC13*EL2*MU*(CB2 + SB2)*((-0.7071067811865475D0*CB*EL&
  &*MU2)/(MW*SW) + (0.7071067811865475D0*EL*MB2*SB*YukS3Quark2)/(MW*SW))*B0(MU2, MB2, MHp2)*DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) -&
  & (0.011048543456039804D0*CKM13*CKMC13*EL2*(CB2 + SB2)*B0(MU2, MB2, MW2)*((1.4142135623730951D0*CB*EL*MA02*MU)/(MW*SW) + (0.707&
  &1067811865475D0*EL*MB2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*CB*EL*DBLE(MU**INT(3.D0)))/(MW*SW))*DBLE(PI**INT(-4.&
  &D0)))/ (MA02*SB*SW2) - (0.011048543456039804D0*CKM13*CKMC13*EL2*(CB2 + SB2)*C0(MA02, MU2, MU2, MW2, MHp2, MB2)* ((-0.707106781&
  &1865475D0*CB*EL*MA02*MB2*MU)/(MW*SW) + (1.4142135623730951D0*CB*EL*MA02*MHp2*MU)/(MW*SW) + (2.1213203435596424D0*EL*MA02*MB2*M&
  &U*SB*YukS3Quark2)/(MW*SW) + (0.7071067811865475D0*EL*MB2*MHp2*MU*SB*YukS3Quark2)/(MW*SW) - (0.7071067811865475D0*EL*MB2*MU*MW*&
  &SB*YukS3Quark2)/SW - (1.4142135623730951D0*CB*EL*MA02*DBLE(MU**INT(3.D0)))/(MW*SW) - (0.7071067811865475D0*CB*EL*MHp2*DBLE(MU*&
  &*INT(3.D0)))/(MW*SW) + (0.7071067811865475D0*CB*EL*MW*DBLE(MU**INT(3.D0)))/SW)* DBLE(PI**INT(-4.D0)))/(MA02*SB*SW2) + (0.00781&
  &25D0*CB*CKM13*CKMC13*MU*(CB2 + SB2)*B0(MA02, MHp2, MW2)*DBLE(EL**INT(3.D0))* DBLE(PI**INT(-4.D0))*DBLE(SW**INT(-3.D0)))/(MW*SB&
  &)))/(MW*SB*SW)

  totalAmplitude = (0D0,0D0)
 do j=1,58
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

 A0toUUBarVC = totalAmplitude
end function A0toUUBarVC