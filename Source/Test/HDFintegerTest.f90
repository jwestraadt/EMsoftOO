! ###################################################################
! Copyright (c) 2016-2024, Marc De Graef Research Group/Carnegie Mellon University
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

!--------------------------------------------------------------------------
! EMsoft:HDFintegerTest.f90
!--------------------------------------------------------------------------
!
! MODULE: HDFintegerTest
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief test module for writing and reading of nml files to/from HDF5 files
!
!> @date 10/29/16   MDG 1.0 original
!> @date 01/09/20   MDG 1.1 updated for new HDF class
!--------------------------------------------------------------------------

module HDFintegerTest

use stringconstants
use mod_global
use mod_kinds

contains 

subroutine HDFintegerExecuteTest(res) &
           bind(c, name='HDFintegerExecuteTest')    ! this routine is callable from a C/C++ program
!DEC$ ATTRIBUTES DLLEXPORT :: HDFintegerExecuteTest

use,INTRINSIC :: ISO_C_BINDING
use mod_EMsoft
use HDF5
use mod_HDFsupport

IMPLICIT NONE

integer(C_INT32_T),INTENT(OUT)  :: res

character(fnlen)                :: HDFfilename, tmppath, groupname, dataset, textfile, progname, progdesc
character(1)                    :: EMsoftnativedelimiter

integer(kind=irg)               :: i1, i2, i3, i4, dim1, dim2, dim3, dim4, hdferr, isum, ival, ival_save
integer(HSIZE_T)                :: dims1(1), dims2(2), dims3(3), dims4(4)
integer(kind=irg),allocatable   :: iarr1(:), iarr2(:,:), iarr3(:,:,:), iarr4(:,:,:,:)
integer(kind=irg),allocatable   :: iarr1_save(:), iarr2_save(:,:), iarr3_save(:,:,:), iarr4_save(:,:,:,:)

type(HDF_T)                     :: HDF
type(EMsoft_T)                  :: EMsoft

progname = 'HDFintegerTest'
progdesc = 'Test program for integer reading and writing using the HDF class'
EMsoft = EMsoft_T(progname, progdesc)

!====================================
! generate the integer arrays
dim1 = 5
dim2 = 10
dim3 = 15
dim4 = 20

ALLOCATE (iarr1(dim1))
ALLOCATE (iarr2(dim1,dim2))
ALLOCATE (iarr3(dim1,dim2,dim3))
ALLOCATE (iarr4(dim1,dim2,dim3,dim4))

ival = 123

do i1=1,dim1
  iarr1(i1) = i1
  do i2=1,dim2
    iarr2(i1,i2) = i1 * i2
    do i3=1,dim3
      iarr3(i1,i2,i3) = i1 * i2 * i3
      do i4=1,dim4
        iarr4(i1,i2,i3,i4) = i1 * i2 * i3 * i4
      end do
    end do
  end do
end do

ALLOCATE (iarr1_save(dim1))
ALLOCATE (iarr2_save(dim1,dim2))
ALLOCATE (iarr3_save(dim1,dim2,dim3))
ALLOCATE (iarr4_save(dim1,dim2,dim3,dim4))

ival_save = ival
iarr1_save = iarr1
iarr2_save = iarr2
iarr3_save = iarr3
iarr4_save = iarr4

!====================================
! initialize the HDF class
HDF = HDF_T()

! determine the pathname delimiter character
EMsoftnativedelimiter = EMsoft%getConfigParameter('EMsoftnativedelimiter')

! get the location of the Temporary folder inside the Build folder (it always exists)
tmppath = EMsoft%getConfigParameter('EMsofttestpath')

! create and open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_integer.h5'

write(*,*) 'writing filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF%createFile(HDFfilename)
if (hdferr.ne.0) then
  res = 1
  return
end if

! write the integer and integer arrays to the file
dataset = SC_integer
hdferr = HDF%writeDatasetInteger(dataset, ival)
if (hdferr.ne.0) then
  res = 2
  return
end if

dataset = SC_integer1D
hdferr = HDF%writeDatasetIntegerArray(dataset, iarr1, dim1)
if (hdferr.ne.0) then
  res = 3
  return
end if

dataset = SC_integer2D
hdferr = HDF%writeDatasetIntegerArray(dataset, iarr2, dim1, dim2)
if (hdferr.ne.0) then
  res = 4
  return
end if

dataset = SC_integer3D
hdferr = HDF%writeDatasetIntegerArray(dataset, iarr3, dim1, dim2, dim3)
if (hdferr.ne.0) then
  res = 5
  return
end if

dataset = SC_integer4D
hdferr = HDF%writeDatasetIntegerArray(dataset, iarr4, dim1, dim2, dim3, dim4)
if (hdferr.ne.0) then
  res = 6
  return
end if

call HDF%popall()

!====================================


!====================================
! deallocate the integer arrays (they will be recreated upon reading)
ival = 0
deallocate( iarr1, iarr2, iarr3, iarr4)
!====================================

!====================================
! next, we read the data sets from the HDF5 file

! open the test file
HDFfilename = trim(tmppath)//EMsoftnativedelimiter//'HDFtest_integer.h5'

write(*,*) 'reading filename = <'//trim(HDFfilename)//'>, has length ',len(trim(HDFfilename))
write (*,*) 'cstring : <'//cstringify(HDFfilename)//'> has length ',len(cstringify(HDFfilename))

hdferr =  HDF%openFile(HDFfilename)
if (hdferr.ne.0) then
  res = 7
  return
end if

! read the integer and arrays
dataset = SC_integer
call HDF%readDatasetInteger(dataset, hdferr, ival)
if (hdferr.ne.0) then
  res = 8
  return
end if

dataset = SC_integer1D
call HDF%readDatasetIntegerArray(dataset, dims1, hdferr, iarr1)
if (hdferr.ne.0) then
  res = 9
  return
end if

dataset = SC_integer2D
call HDF%readDatasetIntegerArray(dataset, dims2, hdferr, iarr2)
if (hdferr.ne.0) then
  res = 10 
  return
end if

dataset = SC_integer3D
call HDF%readDatasetIntegerArray(dataset, dims3, hdferr, iarr3)
if (hdferr.ne.0) then
  res = 11
  return
end if

dataset = SC_integer4D
call HDF%readDatasetIntegerArray(dataset, dims4, hdferr, iarr4)
if (hdferr.ne.0) then
  res = 12
  return
end if

call HDF%popall()

!====================================


!====================================
! compare the entries with the stored values
isum = 20
if (ival.ne.ival_save) isum = isum + 1
if (sum(iarr1-iarr1_save).ne.0) isum = isum + 2
if (sum(iarr2-iarr2_save).ne.0) isum = isum + 4
if (sum(iarr3-iarr3_save).ne.0) isum = isum + 8
if (sum(iarr4-iarr4_save).ne.0) isum = isum + 16

if (isum.eq.20) then
  res = 0
else
  res = isum
end if
!====================================

end subroutine HDFintegerExecuteTest


end module HDFintegerTest
