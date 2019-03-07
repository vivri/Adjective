package com.victorivri.adjective

import AdjectiveMembership._

object TildaFlow {
  implicit class TupExt2[A1 <: Adjective[N1],A2 <: Adjective[N2],N1,N2] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2])]) {
    def ~ [A3 <: Adjective[N3], N3] (next: AdjectiveMembership[A3,N3]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3])] =
      (v, next) match {
        case (Right((a,b)), c: Includes[A3,N3]) => Right((a,b,c))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt3[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],N1,N2,N3] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3])]) {
    def ~ [A4 <: Adjective[N4], N4] (next: AdjectiveMembership[A4,N4]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4])] =
      (v, next) match {
        case (Right((a,b,c)), d: Includes[A4,N4]) => Right((a,b,c,d))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt4[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],N1,N2,N3,N4] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4])]) {
    def ~ [A5 <: Adjective[N5], N5] (next: AdjectiveMembership[A5,N5]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5])] =
      (v, next) match {
        case (Right((a,b,c,d)), e: Includes[A5,N5]) => Right((a,b,c,d,e))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt5[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],N1,N2,N3,N4,N5] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5])]) {
    def ~ [A6 <: Adjective[N6], N6] (next: AdjectiveMembership[A6,N6]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6])] =
      (v, next) match {
        case (Right((a,b,c,d,e)), f: Includes[A6,N6]) => Right((a,b,c,d,e,f))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt6[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],N1,N2,N3,N4,N5,N6] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6])]) {
    def ~ [A7 <: Adjective[N7], N7] (next: AdjectiveMembership[A7,N7]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f)), g: Includes[A7,N7]) => Right((a,b,c,d,e,f,g))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt7[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],N1,N2,N3,N4,N5,N6,N7] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7])]) {
    def ~ [A8 <: Adjective[N8], N8] (next: AdjectiveMembership[A8,N8]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g)), h: Includes[A8,N8]) => Right((a,b,c,d,e,f,g,h))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt8[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],N1,N2,N3,N4,N5,N6,N7,N8] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8])]) {
    def ~ [A9 <: Adjective[N9], N9] (next: AdjectiveMembership[A9,N9]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h)), i: Includes[A9,N9]) => Right((a,b,c,d,e,f,g,h,i))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt9[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],N1,N2,N3,N4,N5,N6,N7,N8,N9] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9])]) {
    def ~ [A10 <: Adjective[N10], N10] (next: AdjectiveMembership[A10,N10]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i)), j: Includes[A10,N10]) => Right((a,b,c,d,e,f,g,h,i,j))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt10[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10])]) {
    def ~ [A11 <: Adjective[N11], N11] (next: AdjectiveMembership[A11,N11]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j)), k: Includes[A11,N11]) => Right((a,b,c,d,e,f,g,h,i,j,k))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt11[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11])]) {
    def ~ [A12 <: Adjective[N12], N12] (next: AdjectiveMembership[A12,N12]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k)), l: Includes[A12,N12]) => Right((a,b,c,d,e,f,g,h,i,j,k,l))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt12[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12])]) {
    def ~ [A13 <: Adjective[N13], N13] (next: AdjectiveMembership[A13,N13]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l)), m: Includes[A13,N13]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt13[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13])]) {
    def ~ [A14 <: Adjective[N14], N14] (next: AdjectiveMembership[A14,N14]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m)), n: Includes[A14,N14]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt14[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14])]) {
    def ~ [A15 <: Adjective[N15], N15] (next: AdjectiveMembership[A15,N15]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n)), o: Includes[A15,N15]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt15[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15])]) {
    def ~ [A16 <: Adjective[N16], N16] (next: AdjectiveMembership[A16,N16]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)), p: Includes[A16,N16]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt16[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16])]) {
    def ~ [A17 <: Adjective[N17], N17] (next: AdjectiveMembership[A17,N17]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)), q: Includes[A17,N17]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt17[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17])]) {
    def ~ [A18 <: Adjective[N18], N18] (next: AdjectiveMembership[A18,N18]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)), r: Includes[A18,N18]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt18[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18])]) {
    def ~ [A19 <: Adjective[N19], N19] (next: AdjectiveMembership[A19,N19]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)), s: Includes[A19,N19]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt19[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19])]) {
    def ~ [A20 <: Adjective[N20], N20] (next: AdjectiveMembership[A20,N20]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19],Includes[A20,N20])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)), t: Includes[A20,N20]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt20[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],A20 <: Adjective[N20],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19,N20] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19],Includes[A20,N20])]) {
    def ~ [A21 <: Adjective[N21], N21] (next: AdjectiveMembership[A21,N21]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19],Includes[A20,N20],Includes[A21,N21])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)), u: Includes[A21,N21]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
  implicit class TupExt21[A1 <: Adjective[N1],A2 <: Adjective[N2],A3 <: Adjective[N3],A4 <: Adjective[N4],A5 <: Adjective[N5],A6 <: Adjective[N6],A7 <: Adjective[N7],A8 <: Adjective[N8],A9 <: Adjective[N9],A10 <: Adjective[N10],A11 <: Adjective[N11],A12 <: Adjective[N12],A13 <: Adjective[N13],A14 <: Adjective[N14],A15 <: Adjective[N15],A16 <: Adjective[N16],A17 <: Adjective[N17],A18 <: Adjective[N18],A19 <: Adjective[N19],A20 <: Adjective[N20],A21 <: Adjective[N21],N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19,N20,N21] (v: Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19],Includes[A20,N20],Includes[A21,N21])]) {
    def ~ [A22 <: Adjective[N22], N22] (next: AdjectiveMembership[A22,N22]): Either[List[Excludes[_,_]], (Includes[A1,N1],Includes[A2,N2],Includes[A3,N3],Includes[A4,N4],Includes[A5,N5],Includes[A6,N6],Includes[A7,N7],Includes[A8,N8],Includes[A9,N9],Includes[A10,N10],Includes[A11,N11],Includes[A12,N12],Includes[A13,N13],Includes[A14,N14],Includes[A15,N15],Includes[A16,N16],Includes[A17,N17],Includes[A18,N18],Includes[A19,N19],Includes[A20,N20],Includes[A21,N21],Includes[A22,N22])] =
      (v, next) match {
        case (Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)), v: Includes[A22,N22]) => Right((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v))
        case (Left(fails), x) => Left(fails ::: AdjectiveMembership.nonMembershipAsList(x))
        case (Right(_), x)    => Left(AdjectiveMembership.nonMembershipAsList(x))
      }
  }
}
