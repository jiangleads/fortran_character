	!����ĳ�����Ժܷ���ض����ı��ļ��е�����,��������֪���ļ������ݵĴ�Ÿ�ʽ,
	!ÿ�к�ÿ�е����ݸ������Բ�һ��,ȱ�ٵ������Զ���Ϊ0.0,
	!�Զ�ʶ���ļ�ͷ����Ϣ,����������:
	!  http://www.xuebuyuan.com/1597280.html
	!------------------------------------------------------------
	!include"StrNum.F90"
	!--------------------------------------------------
	program main
	use StrNum
	implicit none
	integer :: count,i,j
	CHARACTER(len=100) :: Filename !�����в���
	Integer::Error,HeadLine,Row,Column
	real(kind=8), allocatable :: Array2D(:,:)
	!!-----------------------------------------------------------------
	!count = command_argument_count() !��ȡ�����������е���������ĸ���
	!!------------------------------------------------------------------
	!if (count>0) then
	!do i=1,count
	!	CALL get_command_argument(i, Filename)
	write(*,*)'------------------------------------------'
	filename="D:\datafile\ec79-15\interdata\output\DP\2000Bergeron700.txt"
	call GetFileRowColumn(Filename,HeadLine,Row,Column) !��ȡ�ļ���,��
	write(*,*)"HeadLine=",HeadLine,"Row=",Row,"Column=",Column
	write(*,*)'------------------------------------------'
	call LoadFromFile(trim(Filename),Array2D,Row,Column,Error)
	if (Error==0)then
		write(*,*)"Array2D(Row,Column):"
		do j=1,Row
			write(*,*)Array2D( j,1:Column)
		end do
		write(*,*)'------------------------------------------'
	else
		write(*,*)"�ļ���д����"
	end if
	!	end do
	!else
	!	write(*,*) 'You should input an argument!'
	!end if
	!---------------------------------------------
	if (Allocated(Array2D)) then
		deallocate(Array2D)
	end if
	!---------------------------------------------
	end program


