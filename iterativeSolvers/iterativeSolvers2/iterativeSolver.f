      program iterativeSolver

      double precision A(4,4),Ad(4,4),Ab(4,4),y,yd,yb,B(4),xOld(4)
      integer::j,i
      A = transpose(reshape((/10,-1,2,0,-1,11,
     +  -1,3,2,-1,10,-1,0,3,-1,8/),shape(A)))
	  !A=(/10,-1,2,0,-1,11,-1,3,2,-1,10,-1,0,3,-1,8/)
      B = (/6 ,25 ,-11 ,15/)
      xOld=(/0.6 ,2.2727 ,-1.1 ,1.875/)
c      call func(A,B,xOld,y);
c      print *,y

!     Forward Mode
      print *,'Forward'
      Ad=0.0
      do i=1,4
        do j=1,4
           xOld=(/0.6 ,2.2727 ,-1.1 ,1.875/)
           Ad(i,j)=1.0
           call func_d(A,Ad,B,xOld,y,yd)
           print *,i,j,yd
           Ad(i,j)=0.0
        end do
      end do


!      Reverse Mode
      print *,'Reverse'
       xOld=(/0.6 ,2.2727 ,-1.1 ,1.875/)
       yb=1.0
       call func_b(A,Ab,B,xOld,y,yb)
       do i=1,4
          do j=1,4
             print *,i,j,Ab(i,j)
          end do
       end do

      end program iterativeSolver

      subroutine func(A, B, xOld, y)
	  double precision A(4,4),B(4),xOld(4),y,xNew(4),temp,ep
	  integer:: it,i,j,n,flag
	  ep=0.0
!	  A = transpose(reshape((A),shape(A)))
	  temp=0
	  flag=1
	  do while (flag==1)
!	    print *,it
	    do i=1,4
          do j=1,4
            if(i/=j) then
                temp=temp+(A(i,j)*xOld(j));
            end if
		  end do
            xNew(i)=(1.0/A(i,i))*(B(i)-temp);
            
            temp=0;
!            print *,xNew(i)
		end do
		
!		If the difference between the all values of xNew(i) and xOld(i) is less than or equal to ep then flag is set to 0.		
		do j=1,4
			if (abs(xNew(j)-xOld(j)) <= ep) then
				flag=0
			else
				flag=1
				exit
			end if
		end do
		
		do j=1,4
		    xOld(j)=xNew(j)
		end do
		
	  end do
      call obj(xOld,y)
      end subroutine func

      subroutine obj(xOld,y)
      double precision xOld(4),y
c      y = xOld(1) + 2*xOld(2) + 3*xOld(3) + 4*xOld(4) 
      y = xOld(1)**3 + dlog(xOld(2)**2) + exp(xOld(3)) + 4*xOld(4)
      end subroutine obj
	
	
	
	
	
	
	
	
	
