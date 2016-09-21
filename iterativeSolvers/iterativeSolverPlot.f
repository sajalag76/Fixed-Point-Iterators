      program iterativeSolver

      double precision A(4,4), y,yb,yd,B(4),Bd(4),Bb(4),xOld(4)
      integer::j
      A = transpose(reshape((/10,-1,2,0,-1,11,
     +  -1,3,2,-1,10,-1,0,3,-1,8/),shape(A)))
c      A = transpose(reshape((/7,1,-2,1,1,8,
c     +  1,0,-2,1,5,-1,1,0,-1,3/),shape(A)))
      B = (/6 ,25 ,-11 ,15/)
c      B = (/1 ,-1 ,1 ,-1/)
c      xOld=(/0.6 ,2.2727 ,-1.1 ,1.875/)
      xOld=(/ 0 , 0, 0 ,0 /)
      call func(A,B,xOld,y);
      print *,y

!      Forward Mode
c      Bd=0.0
c      do j=1,4
c         xOld=(/0.6 ,2.2727 ,-1.1 ,1.875/)
c         Bd(j)=1.0
c         call func_d(A,B,Bd,xOld,y,yd);
c         print *, j,yd
c         Bd(j)=0.0
c      end do

!     Reverse Mode
c      xOld=(/0.6, 2.2727 , -1.1 , 1.875/)
c      yb=1.0
c      call func_b(A,B,Bb,xOld,y,yb)
c      do j=1,4
c        print *,j,Bb(j)
c      end do

      end program iterativeSolver

      subroutine func(A, B, xOld, y)
	  double precision A(4,4),B(4),xOld(4),y,xNew(4),temp,ep
	  
      integer:: it,i,j,n,flag,z
	open(1,file='errorX.txt',status='new')
	  ep=1e-15
          z=1
!	  A = transpose(reshape((A),shape(A)))
	  temp=0
	  flag=1
	  do while (flag==1) 
	    print *,z
            z=z+1
	  do i=1,4
          do j=1,4
            if(i/=j) then
                temp=temp+(A(i,j)*xOld(j));
            end if
		  end do
            xNew(i)=(1.0/A(i,i))*(B(i)-temp);
            
            temp=0;
            print *,xNew(i)
		end do
		
		write(1,*) abs(xNew(1)-xOld(1)),abs(xNew(2)-xOld(2)),
     +          abs(xNew(3)-xOld(3)),abs(xNew(4)-xOld(4))
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
      double precision xOld(4), y
      y = xOld(1)**3 + dlog(xOld(2)**2) + exp(xOld(3)) + 4*xOld(4)
      end subroutine obj
	
	
	
	
	
	
	
	
	
