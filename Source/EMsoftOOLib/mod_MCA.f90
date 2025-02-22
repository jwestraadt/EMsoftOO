! ###################################################################
! Copyright (c) 2013-2025, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

module mod_MCA
  !! author: MDG 
  !! version: 1.0 
  !! date: 08/12/21
  !!
  !! class definition for the Marching Cubes Algorithm (based on http://iqc.udg.es/cat/similarity/ASA/mca.html)

use mod_kinds
use mod_global

IMPLICIT NONE 

private 

! encoding of all possible triangles
integer(kind=irg), parameter  ::  mc1(256,16) = transpose(reshape( (/ &
                                                0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  2, 10,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  9,  4, 10,  9,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  2,  3, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  3, 11,  1,  3, 10,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  9,  4,  3, 11,  9, 11, 10,  9,  0,  0,  0,  0,  0,  0,  0, &
                                                4, 12,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1, 12,  3,  9, 12,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2, 10,  1,  3,  4, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2, 12,  3,  2, 10, 12, 10,  9, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                4, 11,  2, 12, 11,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1, 11,  2,  1,  9, 11,  9, 12, 11,  0,  0,  0,  0,  0,  0,  0, &
                                                4, 10,  1,  4, 12, 10, 12, 11, 10,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  9, 11, 11,  9, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  8,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  4,  1,  8,  4,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  2, 10,  9,  5,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  2, 10,  5,  8,  2,  8,  4,  2,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11,  9,  5,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  5,  8,  4,  1,  5,  2,  3, 11,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  3, 11, 10,  1,  3,  9,  5,  8,  0,  0,  0,  0,  0,  0,  0, &
                                                3, 11, 10,  3, 10,  8,  3,  8,  4,  8, 10,  5,  0,  0,  0,  0, &
                                                9,  5,  8,  4, 12,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  5,  8, 12,  3,  5,  3,  1,  5,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  1,  2,  9,  5,  8,  3,  4, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  8, 12, 10,  5, 12, 10, 12,  3, 10,  3,  2,  0,  0,  0,  0, &
                                                4, 11,  2,  4, 12, 11,  8,  9,  5,  0,  0,  0,  0,  0,  0,  0, &
                                                2, 12, 11,  2,  5, 12,  2,  1,  5,  8, 12,  5,  0,  0,  0,  0, &
                                                5,  8,  9, 10,  1, 12, 10, 12, 11, 12,  1,  4,  0,  0,  0,  0, &
                                                5,  8, 12,  5, 12, 10, 10, 12, 11,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  6,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  6,  5,  1,  9,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  6,  5,  2,  6,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  6,  5,  9,  4,  6,  4,  2,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11, 10,  6,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  1,  9,  2,  3, 11,  5, 10,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                6,  3, 11,  6,  5,  3,  5,  1,  3,  0,  0,  0,  0,  0,  0,  0, &
                                                3, 11,  6,  4,  3,  6,  4,  6,  5,  4,  5,  9,  0,  0,  0,  0, &
                                               10,  6,  5,  3,  4, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1, 12,  3,  1,  9, 12,  5, 10,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  6,  5,  1,  2,  6,  3,  4, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  2,  6,  3,  6,  9,  3,  9, 12,  5,  9,  6,  0,  0,  0,  0, &
                                               11,  4, 12, 11,  2,  4, 10,  6,  5,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 10,  6,  1,  9,  2,  9, 11,  2,  9, 12, 11,  0,  0,  0,  0, &
                                                6,  5,  1,  6,  1, 12,  6, 12, 11, 12,  1,  4,  0,  0,  0,  0, &
                                                6,  5,  9,  6,  9, 11, 11,  9, 12,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  8,  9,  6,  8, 10,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  4,  1, 10,  6,  4,  6,  8,  4,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  8,  9,  1,  2,  8,  2,  6,  8,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  6,  4,  4,  6,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  8,  9, 10,  6,  8, 11,  2,  3,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  2,  3, 10,  6,  1,  6,  4,  1,  6,  8,  4,  0,  0,  0,  0, &
                                                9,  1,  3,  9,  3,  6,  9,  6,  8, 11,  6,  3,  0,  0,  0,  0, &
                                                3, 11,  6,  3,  6,  4,  4,  6,  8,  0,  0,  0,  0,  0,  0,  0, &
                                                8, 10,  6,  8,  9, 10,  4, 12,  3,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  6,  8, 10,  8,  3, 10,  3,  1,  3,  8, 12,  0,  0,  0,  0, &
                                                3,  4, 12,  1,  2,  9,  2,  8,  9,  2,  6,  8,  0,  0,  0,  0, &
                                               12,  3,  2, 12,  2,  8,  8,  2,  6,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  6,  9,  9,  6,  8, 11,  2,  4, 11,  4, 12,  0,  0,  0,  0, &
                                                6,  8,  1,  6,  1, 10,  8, 12,  1,  2,  1, 11, 12, 11,  1,  0, &
                                               12, 11,  1, 12,  1,  4, 11,  6,  1,  9,  1,  8,  6,  8,  1,  0, &
                                               12, 11,  6,  8, 12,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  7,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  6, 11,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  1,  2,  6, 11,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  9,  4,  2, 10,  9,  6, 11,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  7,  6,  3,  7,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  7,  6,  2,  3,  7,  4,  1,  9,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  7,  6, 10,  1,  7,  1,  3,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                6, 10,  9,  6,  9,  3,  6,  3,  7,  4,  3,  9,  0,  0,  0,  0, &
                                                3,  4, 12, 11,  7,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  1,  9, 12,  3,  1, 11,  7,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  2, 10,  3,  4, 12,  6, 11,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                6, 11,  7,  2, 10,  3, 10, 12,  3, 10,  9, 12,  0,  0,  0,  0, &
                                                7,  4, 12,  7,  6,  4,  6,  2,  4,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9, 12,  1, 12,  6,  1,  6,  2,  6, 12,  7,  0,  0,  0,  0, &
                                                4, 12,  7,  1,  4,  7,  1,  7,  6,  1,  6, 10,  0,  0,  0,  0, &
                                                7,  6, 10,  7, 10, 12, 12, 10,  9,  0,  0,  0,  0,  0,  0,  0, &
                                                6, 11,  7,  5,  8,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  4,  1,  5,  8,  4,  7,  6, 11,  0,  0,  0,  0,  0,  0,  0, &
                                                2, 10,  1,  6, 11,  7,  9,  5,  8,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  7,  6,  2, 10,  8,  2,  8,  4,  8, 10,  5,  0,  0,  0,  0, &
                                                7,  2,  3,  7,  6,  2,  5,  8,  9,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3,  6,  6,  3,  7,  4,  1,  5,  4,  5,  8,  0,  0,  0,  0, &
                                                9,  5,  8, 10,  1,  6,  1,  7,  6,  1,  3,  7,  0,  0,  0,  0, &
                                                8,  4, 10,  8, 10,  5,  4,  3, 10,  6, 10,  7,  3,  7, 10,  0, &
                                                4, 12,  3,  8,  9,  5, 11,  7,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                6, 11,  7,  5,  8,  3,  5,  3,  1,  3,  8, 12,  0,  0,  0,  0, &
                                                1,  2, 10,  5,  8,  9,  3,  4, 12,  6, 11,  7,  0,  0,  0,  0, &
                                               10,  3,  2, 10, 12,  3, 10,  5, 12,  8, 12,  5,  6, 11,  7,  0, &
                                                9,  5,  8,  4, 12,  6,  4,  6,  2,  6, 12,  7,  0,  0,  0,  0, &
                                                6,  2, 12,  6, 12,  7,  2,  1, 12,  8, 12,  5,  1,  5, 12,  0, &
                                                1,  6, 10,  1,  7,  6,  1,  4,  7, 12,  7,  4,  9,  5,  8,  0, &
                                                7,  6, 10,  7, 10, 12,  5,  8, 10,  8, 12, 10,  0,  0,  0,  0, &
                                               11,  5, 10,  7,  5, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 11,  7,  5, 10, 11,  1,  9,  4,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  1,  2, 11,  7,  1,  7,  5,  1,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  4,  2,  9,  2,  7,  9,  7,  5,  7,  2, 11,  0,  0,  0,  0, &
                                                2,  5, 10,  2,  3,  5,  3,  7,  5,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  1,  9,  2,  3, 10,  3,  5, 10,  3,  7,  5,  0,  0,  0,  0, &
                                                1,  3,  5,  5,  3,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  4,  3,  9,  3,  5,  5,  3,  7,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  5, 10, 11,  7,  5, 12,  3,  4,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  3,  3,  9, 12,  5, 10, 11,  5, 11,  7,  0,  0,  0,  0, &
                                                4, 12,  3,  1,  2,  7,  1,  7,  5,  7,  2, 11,  0,  0,  0,  0, &
                                                7,  5,  2,  7,  2, 11,  5,  9,  2,  3,  2, 12,  9, 12,  2,  0, &
                                               10,  7,  5, 10,  4,  7, 10,  2,  4, 12,  7,  4,  0,  0,  0,  0, &
                                                9, 12,  2,  9,  2,  1, 12,  7,  2, 10,  2,  5,  7,  5,  2,  0, &
                                                4, 12,  7,  4,  7,  1,  1,  7,  5,  0,  0,  0,  0,  0,  0,  0, &
                                                7,  5,  9, 12,  7,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                8, 11,  7,  8,  9, 11,  9, 10, 11,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  8,  4,  1, 11,  8,  1, 10, 11,  7,  8, 11,  0,  0,  0,  0, &
                                               11,  7,  8,  2, 11,  8,  2,  8,  9,  2,  9,  1,  0,  0,  0,  0, &
                                               11,  7,  8, 11,  8,  2,  2,  8,  4,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3,  7,  2,  7,  9,  2,  9, 10,  9,  7,  8,  0,  0,  0,  0, &
                                                3,  7, 10,  3, 10,  2,  7,  8, 10,  1, 10,  4,  8,  4, 10,  0, &
                                                8,  9,  1,  8,  1,  7,  7,  1,  3,  0,  0,  0,  0,  0,  0,  0, &
                                                8,  4,  3,  7,  8,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  4, 12, 11,  7,  9, 11,  9, 10,  9,  7,  8,  0,  0,  0,  0, &
                                                3,  1,  8,  3,  8, 12,  1, 10,  8,  7,  8, 11, 10, 11,  8,  0, &
                                                2,  9,  1,  2,  8,  9,  2, 11,  8,  7,  8, 11,  3,  4, 12,  0, &
                                               12,  3,  2, 12,  2,  8, 11,  7,  2,  7,  8,  2,  0,  0,  0,  0, &
                                                9, 10,  7,  9,  7,  8, 10,  2,  7, 12,  7,  4,  2,  4,  7,  0, &
                                                1, 10,  2, 12,  7,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                8,  9,  1,  8,  1,  7,  4, 12,  1, 12,  7,  1,  0,  0,  0,  0, &
                                                8, 12,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                8,  7, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  1,  9, 12,  8,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  2, 10, 12,  8,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  2, 10,  9,  4,  2, 12,  8,  7,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  2,  3,  7, 12,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11,  4,  1,  9,  7, 12,  8,  0,  0,  0,  0,  0,  0,  0, &
                                                3, 10,  1,  3, 11, 10,  7, 12,  8,  0,  0,  0,  0,  0,  0,  0, &
                                                7, 12,  8,  3, 11,  4, 11,  9,  4, 11, 10,  9,  0,  0,  0,  0, &
                                                8,  3,  4,  7,  3,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                8,  1,  9,  8,  7,  1,  7,  3,  1,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  8,  7,  3,  4,  8,  1,  2, 10,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  7,  3,  2,  9,  7,  2, 10,  9,  9,  8,  7,  0,  0,  0,  0, &
                                               11,  8,  7, 11,  2,  8,  2,  4,  8,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  8,  7,  2,  8, 11,  2,  9,  8,  2,  1,  9,  0,  0,  0,  0, &
                                                1,  4,  8,  1,  8, 11,  1, 11, 10,  7, 11,  8,  0,  0,  0,  0, &
                                                8,  7, 11,  8, 11,  9,  9, 11, 10,  0,  0,  0,  0,  0,  0,  0, &
                                                7,  9,  5, 12,  9,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  7, 12,  4,  1,  7,  1,  5,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  7, 12,  9,  5,  7, 10,  1,  2,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  5,  7, 10,  7,  4, 10,  4,  2, 12,  4,  7,  0,  0,  0,  0, &
                                                7,  9,  5,  7, 12,  9,  3, 11,  2,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11,  4,  1, 12,  1,  7, 12,  1,  5,  7,  0,  0,  0,  0, &
                                                5, 12,  9,  5,  7, 12,  1,  3, 10,  3, 11, 10,  0,  0,  0,  0, &
                                               11, 10,  4, 11,  4,  3, 10,  5,  4, 12,  4,  7,  5,  7,  4,  0, &
                                                9,  3,  4,  9,  5,  3,  5,  7,  3,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  5,  3,  5,  7,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2, 10,  1,  3,  4,  5,  3,  5,  7,  5,  4,  9,  0,  0,  0,  0, &
                                                2, 10,  5,  2,  5,  3,  3,  5,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  2,  4,  9,  7,  2,  9,  5,  7,  7, 11,  2,  0,  0,  0,  0, &
                                               11,  2,  1, 11,  1,  7,  7,  1,  5,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  7,  4,  5,  4,  9,  7, 11,  4,  1,  4, 10, 11, 10,  4,  0, &
                                               11, 10,  5,  7, 11,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 10,  6,  8,  7, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  5, 10,  6, 12,  8,  7,  0,  0,  0,  0,  0,  0,  0, &
                                                6,  1,  2,  6,  5,  1,  8,  7, 12,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  8,  7,  9,  4,  5,  4,  6,  5,  4,  2,  6,  0,  0,  0,  0, &
                                               10,  6,  5, 11,  2,  3,  8,  7, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                7, 12,  8,  2,  3, 11,  1,  9,  4,  5, 10,  6,  0,  0,  0,  0, &
                                                8,  7, 12,  6,  5, 11,  5,  3, 11,  5,  1,  3,  0,  0,  0,  0, &
                                                4,  5,  9,  4,  6,  5,  4,  3,  6, 11,  6,  3, 12,  8,  7,  0, &
                                                8,  3,  4,  8,  7,  3,  6,  5, 10,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  6,  5,  1,  9,  7,  1,  7,  3,  7,  9,  8,  0,  0,  0,  0, &
                                                4,  7,  3,  4,  8,  7,  2,  6,  1,  6,  5,  1,  0,  0,  0,  0, &
                                                7,  3,  9,  7,  9,  8,  3,  2,  9,  5,  9,  6,  2,  6,  9,  0, &
                                               10,  6,  5, 11,  2,  7,  2,  8,  7,  2,  4,  8,  0,  0,  0,  0, &
                                                2,  7, 11,  2,  8,  7,  2,  1,  8,  9,  8,  1, 10,  6,  5,  0, &
                                                5,  1, 11,  5, 11,  6,  1,  4, 11,  7, 11,  8,  4,  8, 11,  0, &
                                                8,  7, 11,  8, 11,  9,  6,  5, 11,  5,  9, 11,  0,  0,  0,  0, &
                                                7, 10,  6,  7, 12, 10, 12,  9, 10,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  7, 12,  1,  7,  4,  1,  6,  7,  1, 10,  6,  0,  0,  0,  0, &
                                                1, 12,  9,  1,  6, 12,  1,  2,  6,  6,  7, 12,  0,  0,  0,  0, &
                                                7, 12,  4,  7,  4,  6,  6,  4,  2,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 11, 10,  6, 12, 10, 12,  9, 12,  6,  7,  0,  0,  0,  0, &
                                                1, 12,  4,  1,  7, 12,  1, 10,  7,  6,  7, 10,  2,  3, 11,  0, &
                                               12,  9,  6, 12,  6,  7,  9,  1,  6, 11,  6,  3,  1,  3,  6,  0, &
                                                7, 12,  4,  7,  4,  6,  3, 11,  4, 11,  6,  4,  0,  0,  0,  0, &
                                                6,  9, 10,  6,  3,  9,  6,  7,  3,  4,  9,  3,  0,  0,  0,  0, &
                                               10,  6,  7, 10,  7,  1,  1,  7,  3,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  6,  9,  2,  9,  1,  6,  7,  9,  4,  9,  3,  7,  3,  9,  0, &
                                                2,  6,  7,  3,  2,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  4,  7,  2,  7, 11,  4,  9,  7,  6,  7, 10,  9, 10,  7,  0, &
                                               11,  2,  1, 11,  1,  7, 10,  6,  1,  6,  7,  1,  0,  0,  0,  0, &
                                                1,  4,  9,  6,  7, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  6,  7,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  6, 11,  8,  6, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  6, 11, 12,  8,  6,  9,  4,  1,  0,  0,  0,  0,  0,  0,  0, &
                                                6, 12,  8,  6, 11, 12,  2, 10,  1,  0,  0,  0,  0,  0,  0,  0, &
                                               11,  8,  6, 11, 12,  8, 10,  9,  2,  9,  4,  2,  0,  0,  0,  0, &
                                               12,  2,  3, 12,  8,  2,  8,  6,  2,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  2,  3,  8,  2,  8,  6,  8,  3, 12,  0,  0,  0,  0, &
                                               10,  8,  6, 10,  3,  8, 10,  1,  3,  3, 12,  8,  0,  0,  0,  0, &
                                                8,  6,  3,  8,  3, 12,  6, 10,  3,  4,  3,  9, 10,  9,  3,  0, &
                                                3,  6, 11,  3,  4,  6,  4,  8,  6,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  3,  1,  9,  6,  3,  9,  8,  6, 11,  3,  6,  0,  0,  0,  0, &
                                               10,  1,  2,  6, 11,  4,  6,  4,  8,  4, 11,  3,  0,  0,  0,  0, &
                                               10,  9,  3, 10,  3,  2,  9,  8,  3, 11,  3,  6,  8,  6,  3,  0, &
                                                2,  4,  6,  4,  8,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  8,  1,  8,  2,  2,  8,  6,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  1,  4, 10,  4,  6,  6,  4,  8,  0,  0,  0,  0,  0,  0,  0, &
                                               10,  9,  8,  6, 10,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                6,  9,  5,  6, 11,  9, 11, 12,  9,  0,  0,  0,  0,  0,  0,  0, &
                                                6,  1,  5,  6, 12,  1,  6, 11, 12, 12,  4,  1,  0,  0,  0,  0, &
                                                1,  2, 10,  9,  5, 11,  9, 11, 12, 11,  5,  6,  0,  0,  0,  0, &
                                               11, 12,  5, 11,  5,  6, 12,  4,  5, 10,  5,  2,  4,  2,  5,  0, &
                                                3,  6,  2,  3,  9,  6,  3, 12,  9,  5,  6,  9,  0,  0,  0,  0, &
                                                1,  5, 12,  1, 12,  4,  5,  6, 12,  3, 12,  2,  6,  2, 12,  0, &
                                                1,  3,  6,  1,  6, 10,  3, 12,  6,  5,  6,  9, 12,  9,  6,  0, &
                                               10,  5,  6,  3, 12,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  6, 11,  4,  6,  3,  4,  5,  6,  4,  9,  5,  0,  0,  0,  0, &
                                                6, 11,  3,  6,  3,  5,  5,  3,  1,  0,  0,  0,  0,  0,  0,  0, &
                                                4, 11,  3,  4,  6, 11,  4,  9,  6,  5,  6,  9,  1,  2, 10,  0, &
                                                6, 11,  3,  6,  3,  5,  2, 10,  3, 10,  5,  3,  0,  0,  0,  0, &
                                                9,  5,  6,  9,  6,  4,  4,  6,  2,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  5,  6,  2,  1,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                9,  5,  6,  9,  6,  4, 10,  1,  6,  1,  4,  6,  0,  0,  0,  0, &
                                               10,  5,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 12,  8,  5, 10, 12, 10, 11, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  9,  4,  5, 10,  8, 10, 12,  8, 10, 11, 12,  0,  0,  0,  0, &
                                                2, 11, 12,  2, 12,  5,  2,  5,  1,  8,  5, 12,  0,  0,  0,  0, &
                                                4,  2,  5,  4,  5,  9,  2, 11,  5,  8,  5, 12, 11, 12,  5,  0, &
                                                5, 12,  8, 10, 12,  5, 10,  3, 12, 10,  2,  3,  0,  0,  0,  0, &
                                               10,  8,  5, 10, 12,  8, 10,  2, 12,  3, 12,  2,  1,  9,  4,  0, &
                                               12,  8,  5, 12,  5,  3,  3,  5,  1,  0,  0,  0,  0,  0,  0,  0, &
                                               12,  8,  5, 12,  5,  3,  9,  4,  5,  4,  3,  5,  0,  0,  0,  0, &
                                                3, 10, 11,  3,  8, 10,  3,  4,  8,  8,  5, 10,  0,  0,  0,  0, &
                                               10, 11,  8, 10,  8,  5, 11,  3,  8,  9,  8,  1,  3,  1,  8,  0, &
                                                4,  8, 11,  4, 11,  3,  8,  5, 11,  2, 11,  1,  5,  1, 11,  0, &
                                                2, 11,  3,  9,  8,  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 10,  2,  5,  2,  8,  8,  2,  4,  0,  0,  0,  0,  0,  0,  0, &
                                                5, 10,  2,  5,  2,  8,  1,  9,  2,  9,  8,  2,  0,  0,  0,  0, &
                                                5,  1,  4,  8,  5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                5,  9,  8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                               10, 11,  9, 11, 12,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  1, 10,  4, 10, 12, 12, 10, 11,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  2, 11,  1, 11,  9,  9, 11, 12,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  2, 11, 12,  4, 11,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  3, 12,  2, 12, 10, 10, 12,  9,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  1, 10,  4, 10, 12,  2,  3, 10,  3, 12, 10,  0,  0,  0,  0, &
                                                1,  3, 12,  9,  1, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                4,  3, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  4,  9,  3,  9, 11, 11,  9, 10,  0,  0,  0,  0,  0,  0,  0, &
                                               10, 11,  3,  1, 10,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                3,  4,  9,  3,  9, 11,  1,  2,  9,  2, 11,  9,  0,  0,  0,  0, &
                                                2, 11,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                2,  4,  9, 10,  2,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1, 10,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                1,  4,  9,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                                                0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /), (/16,256/) ) )

integer(kind=irg), parameter :: mc2(12,2) =  reshape( (/1,2,3,1,5,6,7,5,1,2,3,4,2,3,4,4,6,7,8,8,5,6,7,8 /), (/12,2/) )

! the algorithm produces a 1D linked list of MCAtriangles compatible with the Stereolithography (.stl) file format
type, public :: MCAtriangle
  real(kind=sgl)            :: v1(3)
  real(kind=sgl)            :: v2(3)
  real(kind=sgl)            :: v3(3)
  type(MCAtriangle),pointer :: next 
end type MCAtriangle

! class definition
type, public :: MCA_T
  type(MCAtriangle),pointer       :: MCAtriangles
  integer(kind=irg)               :: Ntriangles 

contains
private 
  procedure, pass(self) :: doMCA_             ! implementation of the Marching Cubes algoritm
  procedure, pass(self) :: getMCAptr_ 
  procedure, pass(self) :: getNtriangles_

  generic, public :: doMCA => doMCA_
  generic, public :: getMCAptr => getMCAptr_
  generic, public :: getNtriangles => getNtriangles_

end type MCA_T

! the constructor routine for this class 
interface MCA_T
  module procedure MCA_constructor
end interface MCA_T

contains

!--------------------------------------------------------------------------
type(MCA_T) function MCA_constructor(  ) result(MCA)
!DEC$ ATTRIBUTES DLLEXPORT :: MCA_constructor
!! author: MDG 
!! version: 1.0 
!! date: 08/12/21
!!
!! constructor for the MCA_T Class; 
 
IMPLICIT NONE

MCA%Ntriangles = 0 
nullify(MCA%MCAtriangles)

end function MCA_constructor

!--------------------------------------------------------------------------
subroutine MCA_destructor(self) 
!DEC$ ATTRIBUTES DLLEXPORT :: MCA_constructor
!! author: MDG 
!! version: 1.0 
!! date: 08/12/21
!!
!! destructor for the MCA_T Class
 
IMPLICIT NONE

type(MCA_T), INTENT(INOUT)  :: self 

call reportDestructor('MCA_T')

end subroutine MCA_destructor

!--------------------------------------------------------------------------
function getMCAptr_(self) result(MCAtriangles)
!DEC$ ATTRIBUTES DLLEXPORT :: getMCAptr_
!! author: MDG
!! version: 1.0 
!! date: 08/12/21
!!
!! get the pointer to the triangle linked list 

class(MCA_T), INTENT(INOUT) :: self 
type(MCAtriangle),pointer   :: MCAtriangles

MCAtriangles => self%MCAtriangles

end function getMCAptr_

!--------------------------------------------------------------------------
function getNtriangles_(self) result(Ntriangles)
!DEC$ ATTRIBUTES DLLEXPORT :: getNtriangles_
!! author: MDG
!! version: 1.0 
!! date: 08/12/21
!!
!! get the number of triangles in the linked list 

class(MCA_T), INTENT(INOUT) :: self 
integer(kind=irg)           :: Ntriangles

Ntriangles = self%Ntriangles

end function getNtriangles_

!--------------------------------------------------------------------------
subroutine doMCA_(self, vol, dims, dxyz, isovalue )
!DEC$ ATTRIBUTES DLLEXPORT :: doMCA_
!! author: MDG, based on routine from http://iqc.udg.es/cat/similarity/ASA/mca.html 
!! version: 1.0 
!! date: 08/12/21
!!
!! Marching Cubes Algorithm

use mod_IO 

IMPLICIT NONE 

class(MCA_T), INTENT(INOUT)     :: self
integer(kind=irg),INTENT(IN)    :: dims(3)        ! must be odd numbers !!!
real(kind=sgl), INTENT(IN)      :: vol(dims(1),dims(2),dims(3))
real(kind=sgl),INTENT(IN)       :: dxyz           ! stepsize identical in all directions
real(kind=sgl),INTENT(IN)       :: isovalue 

type(MCAtriangle),pointer       :: tail    
type(IO_T)                      :: Message
real(kind=sgl),allocatable      :: x(:), y(:), z(:), v(:) 
real(kind=sgl)                  :: dx, dy, dz, xc(8), yc(8), zc(8), vc(8), xt, yt, zt, pre
integer(kind=irg)               :: i, j, k, ii, npx, npy, npz, npts, nc, nz, inum, nf, indc, num1, num2, icon, tcnt, io_int(1)
logical                         :: l1 
integer(kind=irg),parameter     :: ivals(8) = (/ 1, 2, 4, 8, 16, 32, 64, 128/)

npts = product(dims)
npx = dims(1)
npy = dims(2)
npz = dims(3)

! first generate the 1-D coordinate arrays (z first, then y, then x)
allocate( x(npts), y(npts), z(npts), v(npts) )
ii = 1
dx = -dxyz * (dims(1)-1)/2
dy = -dxyz * (dims(2)-1)/2
dz = -dxyz * (dims(3)-1)/2
do i = 1, npx 
  do j = 1, npy 
    do k = 1, npz 
      v(ii) = vol(i,j,k)
      x(ii) = dx
      y(ii) = dy 
      z(ii) = dz 
      ii = ii+1
      dz = dz + dxyz 
    end do 
    dz = -dxyz * (dims(3)-1)/2 
    dy = dy + dxyz
  end do 
  dy = -dxyz * (dims(2)-1)/2 
  dx = dx + dxyz
end do 

nc = product(dims-1)
nz = 1
inum = 1 
tcnt = 1
if (.not.associated(self%MCAtriangles)) allocate(self%MCAtriangles)
tail => self%MCAtriangles
nullify(tail%next)
do i=1,nc
  if (nz.lt.npz) then
    xc = (/ x(i+npz*npy), x(i+npz*npy+1), x(i+npz*npy+npz+1), x(i+npz*npy+npz), x(i), x(i+1), x(i+npz+1), x(i+npz) /)
    yc = (/ y(i+npz*npy), y(i+npz*npy+1), y(i+npz*npy+npz+1), y(i+npz*npy+npz), y(i), y(i+1), y(i+npz+1), y(i+npz) /)
    zc = (/ z(i+npz*npy), z(i+npz*npy+1), z(i+npz*npy+npz+1), z(i+npz*npy+npz), z(i), z(i+1), z(i+npz+1), z(i+npz) /)
    vc = (/ v(i+npz*npy), v(i+npz*npy+1), v(i+npz*npy+npz+1), v(i+npz*npy+npz), v(i), v(i+1), v(i+npz+1), v(i+npz) /)
    indc = 1
    do j=1,8 
      if (vc(j).lt.isovalue) indc = indc + ivals(j)
    end do 
    !
    l1=.TRUE.    
    k = 1
    do while (l1)
      icon = mc1(indc,k)
      if (icon.eq.0) then
        l1=.false.
      else
        num1 = mc2(icon,1)
        num2 = mc2(icon,2)
        if (vc(num2).eq.vc(num1)) then 
          pre = 0.5 
        else 
          pre = (isovalue-vc(num1))/(vc(num2)-vc(num1))
        end if 
        xt = xc(num1) + pre*(xc(num2)-xc(num1))
        yt = yc(num1) + pre*(yc(num2)-yc(num1))
        zt = zc(num1) + pre*(zc(num2)-zc(num1))
        if (tcnt.eq.1) tail%v1 = (/ xt, yt, zt /)
        if (tcnt.eq.2) tail%v2 = (/ xt, yt, zt /)
        if (tcnt.eq.3) then 
          tail%v3 = (/ xt, yt, zt /)
          self%Ntriangles = self%Ntriangles + 1
          allocate(tail%next)
          tail => tail%next
          nullify(tail%next)
          tcnt = 0
        end if 
        tcnt = tcnt + 1 
        inum = inum + 1
        k = k + 1
        if (k.gt.16) l1 = .FALSE.  ! this test should never be .TRUE.
      end if
    end do
    nz = nz + 1
  else
    nz = 1
  end if
end do

io_int(1) = self%Ntriangles
call Message%WriteValue('   Total number of triangles generated : ',io_int,1) 
deallocate(x, y, z, v)

end subroutine doMCA_

end module mod_MCA
